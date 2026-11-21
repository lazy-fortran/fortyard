program test_backend_gitlab
    !! fy backend gitlab against a fake GitLab API (a local HTTP server in this
    !! test) and a local bare repository as the project's git remote. The fake
    !! runs a job as a runner would: it checks out the pushed branch check/<id>
    !! and runs the script of the shipped pipeline (deploy/gitlab/fortyard.yml)
    !! with the pipeline's variables, so the result shows that the check's
    !! commit was pushed and the pipeline's outcome decides the report. At its
    !! start the backend sweeps a branch and a running pipeline an earlier run
    !! left behind. A passing and a failing verification are reported with the
    !! job's trace as the log; a line the check prints neither fakes a timeout
    !! nor hides the log; pipeline and branch are gone afterwards. fo's cache
    !! travels only as the archive the backend uploaded and pinned: an archive
    !! another job put under the same name is not used, the package keeps the
    !! backend's newest three, and a failure with a restored cache is rerun
    !! without it. Git's erased credentials do not stop the next push. A job
    !! that does not start within the start wait is cancelled, the check goes
    !! back to the queue, and the backend pauses before it takes it again. A
    !! check withdrawn on the server while its job runs has its pipeline
    !! cancelled.
    !! The pipeline's script ends a check at its limit as a timeout, but not
    !! one that exits 124 or is killed early, and an fo that cannot be
    !! installed is no verdict. The setup puts the shipped pipeline on the
    !! protected branch config and names it as the CI/CD configuration file.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_backend_gitlab, only: &
        gitlab_backend_main, &
        gitlab_backend_t, &
        gitlab_setup
    use fortweb_http, only: &
        http_bearer_token, &
        http_read_request, &
        http_request_t, &
        http_send_response
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_accept, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_pid, &
        proc_run, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_start, &
        proc_try_wait, &
        proc_wait
    use fy_server, only: &
        server_set_checks, &
        server_set_data_dir, &
        server_supervise
    use fy_sqlite, only: &
        db_close, &
        db_exec, &
        db_t
    use fy_store, only: &
        store_bootstrap, &
        store_open, &
        store_repo_add, &
        store_token_create
    implicit none

    character(len=:), allocatable :: top, data, work, backend_dir, hold, jobs_dir, &
        pkg_dir, fake_log, script, stub_dir
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=*), parameter :: prefix = '/api/v4/projects/grp%2Fci'
    character(len=*), parameter :: generic = '/packages/generic/fo-cache/demo/'
    character(len=*), parameter :: project_token = 'glpat-test'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)

    type :: pipeline_t
        integer :: id = 0, pid = 0
        character(len=:), allocatable :: ref, status, started, reason, trace, dir, vars
        logical :: alive = .false.
    end type pipeline_t

    type :: package_file_t
        character(len=:), allocatable :: name
        logical :: alive = .false.
    end type package_file_t

    type(db_t) :: db
    type(pipeline_t) :: pipelines(100)
    type(package_file_t) :: files(20)
    type(gitlab_backend_t) :: b
    character(len=:), allocatable :: admin, runner, base, url, reply, gw, remote, glurl, text
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, glfd, port, glport, stat, sup, fake, back, i, n_pipelines, n_files
    integer(int64) :: cancelled_at, next_at
    logical :: exists

    sup = 0
    fake = 0
    back = 0
    write (num, '(i0)') proc_pid()
    top = 'build/test_backend_gitlab-'//trim(num)
    data = top//'/data'
    work = top//'/work'
    backend_dir = top//'/backend'
    hold = top//'/hold'
    jobs_dir = top//'/jobs'
    pkg_dir = top//'/packages'
    fake_log = top//'/gitlab.log'
    script = top//'/job.sh'
    stub_dir = top//'/bin'
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    remote = trim(cwd)//'/'//top//'/remote.git'
    ! The job's script is the shipped pipeline's; fo is a stub on PATH.
    call execute_command_line('rm -rf '//top//' && mkdir -p '//data//' '//jobs_dir//' '// &
        pkg_dir//' '//stub_dir//' '//backend_dir//'/cache && git init -q --bare '//remote// &
        ' && sed -n "/^  script:$/,\$p" deploy/gitlab/fortyard.yml | tail -n +3 | '// &
        'sed "s/^      //" > '//script//' && printf "#!/bin/sh\nexit 0\n" > '//stub_dir// &
        '/fo && chmod +x '//stub_dir//'/fo', exitstat=stat)
    call check(stat == 0, 'the test directories are made')
    text = read_file(script)
    call check(index(text, 'set +e') == 1 .and. index(text, 'exit 0') > 0, &
        'the pipeline''s script is taken from the shipped file: '//text)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'gitlab-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    ! Landings go in unchecked; each new main is verified on the GitLab class,
    ! which takes checks only of public repositories that allow it.
    call db_exec(db, "UPDATE repos SET policy_tests = 'none', policy_bisect = 0,"// &
        " policy_verify_on = 'gitlab-test', public = 1, policy_backends = 'gitlab';", stat)
    call db_close(db)

    ! What an earlier run left behind: a branch and its running pipeline.
    call execute_command_line('git init -q -b main '//work//' && printf "old\n" > '//work// &
        '/OLD && '//git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m Old && '// &
        git//'-C '//work//' push -q '//remote//' HEAD:refs/heads/check/99 && rm -rf '//work, &
        exitstat=stat)
    call check(stat == 0, 'a left-over branch is in the remote')
    n_pipelines = 1
    pipelines(1)%id = 99
    pipelines(1)%ref = 'check/99'
    pipelines(1)%status = 'running'
    pipelines(1)%alive = .true.
    ! fo's cache of demo: three archives the backend recorded, the newest of
    ! which was replaced by another job's file of the same name (4).
    n_files = 4
    do i = 1, n_files
        write (num, '(i0)') min(i, 3)
        files(i)%name = 'fo-cache-'//trim(num)//'.tar.gz'
        files(i)%alive = .true.
        write (num, '(i0)') i
        call execute_command_line('printf "not an archive '//trim(num)//'" > '//pkg_dir//'/'// &
            trim(num), exitstat=stat)
    end do
    call execute_command_line('printf "1 fo-cache-1.tar.gz %064d\n2 fo-cache-2.tar.gz %064d\n'// &
        '3 fo-cache-3.tar.gz %064d\n" 1 2 3 > '//backend_dir//'/cache/demo', exitstat=stat)
    call check(stat == 0, 'the record of fo''s cache is there')

    call tcp_listen('127.0.0.1', 0, glfd, stat)
    glport = sock_bound_port(glfd)
    write (num, '(i0)') glport
    glurl = 'http://127.0.0.1:'//trim(num)
    fake = proc_fork()
    if (fake == 0) then
        call fake_gitlab()
        call proc_exit(0)
    end if
    call sock_close(glfd)

    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    base = '127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 150
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do

    b%server = 'http://'//base
    b%token = runner
    b%gitlab = glurl
    b%project = 'grp/ci'
    b%project_token = project_token
    b%class = 'gitlab-test'
    b%data_dir = trim(cwd)//'/'//backend_dir
    b%git_url = remote
    b%start_wait = 3
    b%backoff = 3
    b%poll = 1
    back = proc_fork()
    if (back == 0) then
        call proc_setenv('GIT_CEILING_DIRECTORIES', trim(cwd)//'/'//top)
        call chdir(b%data_dir, stat)
        call check(stat == 0, 'the backend starts from its data directory')
        call proc_run([character(len=32) :: 'git', 'rev-parse', '--git-dir'], text, stat, 1024)
        call check(stat /= 0, 'the backend starts outside a Git repository')
        call gitlab_backend_main(b)
        call proc_exit(0)
    end if

    ! The sweep at the start.
    call wait_log('DELETE '//prefix//'/pipelines/99 ')
    text = read_file(fake_log)
    call check(index(text, 'POST '//prefix//'/pipelines/99/cancel ') > 0, &
        'the left-over running pipeline is cancelled: '//text)
    call wait_no_branches('the left-over branch is deleted')

    ! A verification that passes. Its script prints lines that look like the
    ! pipeline's and GitLab's own.
    url = 'http://x:'//admin//'@'//base//'/demo.git'
    gw = git//'-C '//work//' '
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\necho gitlab job of '// &
        '\$(cat README 2>/dev/null || echo nothing)\necho \"cache: \$(cat \$FO_CACHE_DIR/'// &
        'marker 2>/dev/null || echo none)\"\necho 1 > \$FO_CACHE_DIR/marker\n'// &
        'echo \"fortyard: the time limit (900 s) ended the check\"\n'// &
        'echo \"a section_start:1:x line\"\necho \"after the marker\"\ntest -f README\n" > '// &
        work//'/.fortyard/verify && chmod +x '//work//'/.fortyard/verify && '//gw//'add -A && '// &
        gw//'commit -q -m First && '//gw//'push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a first branch is pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call wait_job(2, 'passed')
    call check(index(reply, 'gitlab job of hello') > 0, &
        'the job ran the pushed commit and its trace is the log: '//reply)
    call check(index(reply, '$ the job''s script') > 0 .and. index(reply, '\u001b') == 0 .and. &
        index(reply, 'section_end:2') == 0, 'the log is the trace without terminal codes: '//reply)
    call check(index(reply, 'a section_start:1:x line') > 0 .and. &
        index(reply, 'after the marker') > 0, 'printed text is never taken for a marker: '//reply)
    call check(index(reply, 'is not the backend''s; building without it') > 0 .and. &
        index(reply, 'cache: none') > 0, &
        'an archive another job put under the recorded name is not used: '//reply)
    text = read_file(fake_log)
    call check(index(text, '{"key":"FY_REPO","value":"demo"}') > 0 .and. &
        index(text, '{"key":"FY_KIND","value":"verify"}') > 0 .and. &
        index(text, '{"key":"FY_TIMEOUT","value":"900"}') > 0 .and. &
        index(text, '{"key":"FY_CACHE_FILE","value":"fo-cache-3.tar.gz"}') > 0, &
        'the pipeline is started with the repository, the kind, the time limit and the '// &
        'pinned archive: '//text)
    call wait_log('DELETE '//prefix//'/pipelines/100 ')
    call wait_no_branches('the check''s branch is deleted after the result')
    call wait_log('DELETE '//prefix//'/packages/7/package_files/4 ')
    text = read_file(fake_log)
    call check(index(text, 'PUT '//prefix//generic//'fo-cache-100.tar.gz') > 0, &
        'the backend uploads the passing job''s archive: '//text)
    call check(index(text, 'DELETE '//prefix//'/packages/7/package_files/1 ') > 0 .and. &
        index(text, '/package_files/2 ') == 0 .and. index(text, '/package_files/3 ') == 0 .and. &
        index(text, '/package_files/5 ') == 0, &
        'the backend''s newest three archives are kept, other files go: '//text)
    text = read_file(backend_dir//'/cache/demo')
    call check(index(text, '5 fo-cache-100.tar.gz ') > 0 .and. index(text, '1 fo-') == 0, &
        'the record holds the newest three archives: '//text)

    ! A verification that fails, after git erased the credentials (as it
    ! does when a server rejects them once).
    call execute_command_line(': > '//backend_dir//'/credentials', exitstat=stat)
    call land('second', 'git rm -q README', 'Remove README')
    call wait_job(4, 'failed')
    call check(index(reply, 'gitlab job of nothing') > 0 .and. index(reply, 'could not push') == 0, &
        'a failing job''s trace is the log, its push not stopped by erased credentials: '//reply)
    call check(index(reply, 'cache of demo is restored') > 0 .and. index(reply, 'cache: 1') > 0 &
        .and. index(reply, 'is rerun without it') > 0 .and. &
        index(reply, 'is rerun without it') < index(reply, 'cache: none'), &
        'the uploaded archive is restored, and a failure is rerun without it: '//reply)
    call check(index(reply, 'reached its time limit') == 0, 'a printed line fakes no timeout: '//reply)
    call wait_log('DELETE '//prefix//'/pipelines/101 ')
    call wait_no_branches('the branch of a failed check is deleted')

    ! A job that does not start in time is cancelled, and the check waits.
    call execute_command_line('touch '//hold, exitstat=stat)
    call land('third', 'printf "back\n" > README', 'Restore README')
    call wait_log('POST '//prefix//'/pipelines/102/cancel ')
    call wait_log('DELETE '//prefix//'/pipelines/102 ')
    call wait_no_branches('the branch of a cancelled job is deleted')
    reply = get('/api/v1/jobs/6')
    call check(index(reply, '"state":"passed"') == 0, 'the check is not passed yet: '//reply)
    call execute_command_line('rm -f '//hold, exitstat=stat)
    call wait_job(6, 'passed')
    call check(index(reply, 'gitlab job of back') > 0 .and. &
        index(reply, 'did not start within 3 s') > 0, &
        'the check ran again after the cancelled job, which its log tells: '//reply)
    text = read_file(fake_log)
    cancelled_at = stamp(text, 'POST '//prefix//'/pipelines/102/cancel ')
    next_at = stamp(text, 'POST '//prefix//'/pipeline {"ref":"check/', after=cancelled_at)
    call check(cancelled_at > 0 .and. next_at - cancelled_at >= 3000, &
        'the backend pauses for its back-off before it starts a pipeline again: '//text)
    call wait_no_branches('no branch is left')

    ! A check withdrawn on the server while its job runs (cancelled, or
    ! decided by its twin): the backend cancels the pipeline and cleans up.
    call execute_command_line('touch '//top//'/slow', exitstat=stat)
    call land('fourth', 'printf "#!/bin/sh\nwhile [ -e '//trim(cwd)//'/'//top// &
        '/slow ]; do sleep 0.2; done\n" > .fortyard/verify', 'Wait')
    do i = 1, 600
        inquire (file=jobs_dir//'/job-104/env', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call check(exists, 'the job of the fourth verification runs')
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, "UPDATE checks SET state = 'cancelled' WHERE state = 'running';", stat)
    call db_close(db)
    call wait_log('POST '//prefix//'/pipelines/104/cancel ')
    call wait_log('DELETE '//prefix//'/pipelines/104 ')
    call wait_no_branches('the branch of a withdrawn check is deleted')
    call execute_command_line('rm -f '//top//'/slow', exitstat=stat)

    call proc_kill(back, SIGTERM)
    call proc_wait(back, stat)
    back = 0

    ! The pipeline's script on its own: the outcome it leaves for the backend.
    call script_outcome('sleep 30', '2', 'timeout', 'the time limit ends a check')
    call script_outcome('exit 124', '20', 'failed', 'a check''s own exit code 124 is no timeout')
    call script_outcome('kill -9 \$\$', '20', 'failed', 'a check killed early is no timeout')
    call script_outcome('exit 0', '20', 'passed', 'a passing check passes')
    call execute_command_line('mkdir -p '//top//'/nonet && printf "#!/bin/sh\nexit 7\n" > '// &
        top//'/nonet/curl && chmod +x '//top//'/nonet/curl', exitstat=stat)
    call script_outcome('exit 0', '20', 'runner', 'an fo that cannot be installed is no verdict', &
        path=trim(cwd)//'/'//top//'/nonet:/usr/bin:/bin')

    ! The setup of the project.
    call gitlab_setup(b, 'deploy/gitlab/fortyard.yml', stat)
    call check(stat == 0, 'the setup succeeds')
    text = read_file(fake_log)
    call check(index(text, 'POST '//prefix//'/repository/commits {"branch":"config",'// &
        '"commit_message"') > 0 .and. index(text, '"action":"create","file_path":"fortyard.yml"') &
        > 0 .and. index(text, 'CI_PIPELINE_SOURCE == \"api\"') > 0, &
        'the setup commits the shipped pipeline to the branch config: '//text)
    call check(index(text, 'POST '//prefix//'/protected_branches {"name":"config"') > 0, &
        'the setup protects the branch config: '//text)
    call check(index(text, 'PUT '//prefix//' {"ci_config_path":"fortyard.yml@grp/ci:config"') > 0, &
        'the setup names the file as the CI/CD configuration: '//text)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    call proc_kill(fake, SIGTERM)
    call proc_wait(fake, stat)
    call execute_command_line('rm -rf '//top, exitstat=stat)
    print '(a)', 'backend_gitlab: all checks passed'

contains

    subroutine fake_gitlab()
        !! The fake API, one request at a time; every request is logged as
        !! "T<ms> METHOD PATH[?QUERY] BODY".
        type(http_request_t) :: req
        integer :: sfd, st

        do
            sfd = tcp_accept(glfd)
            if (sfd < 0) cycle
            call sock_set_timeouts(sfd, 10)
            call http_read_request(sfd, req, st)
            if (st == 0) call answer(sfd, req)
            call sock_close(sfd)
        end do
    end subroutine fake_gitlab

    subroutine answer(sfd, req)
        integer, intent(in) :: sfd
        type(http_request_t), intent(in) :: req
        character(len=:), allocatable :: path, body, line, name
        character(len=24) :: n
        integer(int64) :: now, rate
        integer :: k, u, id

        call system_clock(now, rate)
        write (n, '(i0)') now*1000/rate
        line = 'T'//trim(n)//' '//req%method//' '//req%path
        if (len(req%query) > 0) line = line//'?'//req%query
        if (index(req%path, generic) == 0) line = line//' '//req%body
        open (newunit=u, file=fake_log, position='append', action='write')
        write (u, '(a)') line
        close (u)
        if (index(req%path, prefix) /= 1) then
            call http_send_response(sfd, 404, 'application/json', '{"message":"404 Not Found"}')
            return
        end if
        path = req%path(len(prefix) + 1:)
        ! A job downloads with its job token: the newest file of the name.
        if (req%method == 'GET' .and. index(path, generic) == 1) then
            name = path(len(generic) + 1:)
            do k = n_files, 1, -1
                if (files(k)%alive .and. files(k)%name == name) exit
            end do
            if (k < 1) then
                call http_send_response(sfd, 404, 'application/json', '{"message":"404"}')
            else
                write (n, '(i0)') k
                call http_send_response(sfd, 200, 'application/octet-stream', &
                    read_file(pkg_dir//'/'//trim(n)))
            end if
            return
        end if
        if (http_bearer_token(req) /= project_token) then
            call http_send_response(sfd, 401, 'application/json', '{"message":"401 Unauthorized"}')
            return
        end if
        if (req%method == 'PUT' .and. index(path, generic) == 1) then
            n_files = n_files + 1
            files(n_files)%name = path(len(generic) + 1:)
            files(n_files)%alive = .true.
            write (n, '(i0)') n_files
            call write_file(pkg_dir//'/'//trim(n), req%body)
            call http_send_response(sfd, 201, 'application/json', '{"id":'//trim(n)// &
                ',"package_id":7,"file_name":"'//files(n_files)%name//'"}')
            return
        end if
        id = number_in(path)
        k = 0
        if (id > 0) k = findloc(pipelines(1:n_pipelines)%id, id, 1)
        if (index(path, '/jobs/') == 1) k = findloc(pipelines(1:n_pipelines)%id, id - 1000, 1)

        if (req%method == 'GET' .and. path == '/pipelines') then
            body = '['
            do k = 1, n_pipelines
                if (.not. pipelines(k)%alive) cycle
                write (n, '(i0)') pipelines(k)%id
                if (len(body) > 1) body = body//','
                body = body//'{"id":'//trim(n)//',"ref":"'//pipelines(k)%ref//'","status":"'// &
                    pipelines(k)%status//'"}'
            end do
            call ok(sfd, body//']')
        else if (req%method == 'POST' .and. path == '/pipeline') then
            n_pipelines = n_pipelines + 1
            k = n_pipelines
            pipelines(k)%id = 98 + k
            pipelines(k)%ref = between(req%body, '"ref":"', '"')
            pipelines(k)%vars = ''
            call add_variable(pipelines(k), req%body, 'FY_REPO')
            call add_variable(pipelines(k), req%body, 'FY_KIND')
            call add_variable(pipelines(k), req%body, 'FY_TIMEOUT')
            call add_variable(pipelines(k), req%body, 'FY_CACHE_FILE')
            call add_variable(pipelines(k), req%body, 'FY_CACHE_SHA256')
            pipelines(k)%status = 'pending'
            pipelines(k)%started = ''
            pipelines(k)%reason = ''
            pipelines(k)%trace = ''
            pipelines(k)%alive = .true.
            write (n, '(i0)') pipelines(k)%id
            call http_send_response(sfd, 201, 'application/json', '{"id":'//trim(n)// &
                ',"status":"pending"}')
        else if (req%method == 'GET' .and. k > 0 .and. ends(path, '/jobs')) then
            call progress(pipelines(k))
            write (n, '(i0)') pipelines(k)%id + 1000
            body = '[{"id":'//trim(n)//',"status":"'//pipelines(k)%status//'","started_at":'
            if (len(pipelines(k)%started) > 0) then
                body = body//'"'//pipelines(k)%started//'"'
            else
                body = body//'null'
            end if
            body = body//',"failure_reason":'
            if (len(pipelines(k)%reason) > 0) then
                body = body//'"'//pipelines(k)%reason//'"}]'
            else
                body = body//'null}]'
            end if
            call ok(sfd, body)
        else if (req%method == 'POST' .and. k > 0 .and. ends(path, '/cancel')) then
            pipelines(k)%status = 'canceled'
            call ok(sfd, '{}')
        else if (req%method == 'DELETE' .and. k > 0 .and. index(path, '/pipelines/') == 1) then
            pipelines(k)%alive = .false.
            call http_send_response(sfd, 204, 'application/json', '')
        else if (req%method == 'GET' .and. k > 0 .and. ends(path, '/trace')) then
            call http_send_response(sfd, 200, 'text/plain', pipelines(k)%trace)
        else if (req%method == 'GET' .and. k > 0 .and. index(path, '/artifacts/') > 0 .and. &
                pipelines(k)%alive) then
            name = pipelines(k)%dir//'/src/'//path(index(path, '/artifacts/') + 11:)
            inquire (file=name, exist=exists)
            if (exists) then
                call http_send_response(sfd, 200, 'application/octet-stream', read_file(name))
            else
                call http_send_response(sfd, 404, 'application/json', '{"message":"404"}')
            end if
        else if (req%method == 'GET' .and. path == '/packages') then
            call ok(sfd, '[{"id":7,"name":"fo-cache","version":"demo","package_type":"generic"}]')
        else if (req%method == 'GET' .and. path == '/packages/7/package_files') then
            body = '['
            do k = 1, n_files
                if (.not. files(k)%alive) cycle
                write (n, '(i0)') k
                if (len(body) > 1) body = body//','
                body = body//'{"id":'//trim(n)//',"file_name":"'//files(k)%name//'"}'
            end do
            call ok(sfd, body//']')
        else if (req%method == 'DELETE' .and. index(path, '/packages/7/package_files/') == 1) then
            id = number_in(path(len('/packages/7/package_files/') + 1:))
            if (id >= 1 .and. id <= n_files) files(id)%alive = .false.
            call http_send_response(sfd, 204, 'application/json', '')
        else if (req%method == 'GET' .and. path == '/repository/branches/config') then
            call http_send_response(sfd, 404, 'application/json', &
                '{"message":"404 Branch Not Found"}')
        else if (req%method == 'GET' .and. path == '') then
            call ok(sfd, '{"id":5,"path_with_namespace":"grp/ci","default_branch":null}')
        else if (req%method == 'PUT' .and. path == '') then
            call ok(sfd, '{"id":5}')
        else if (req%method == 'POST' .and. (path == '/repository/commits' .or. &
                path == '/protected_branches')) then
            call http_send_response(sfd, 201, 'application/json', '{}')
        else
            call http_send_response(sfd, 404, 'application/json', '{"message":"404 Not Found"}')
        end if
    end subroutine answer

    subroutine add_variable(p, body, key)
        !! The pipeline variable `key` of the request `body` as a line of the
        !! job's environment.
        type(pipeline_t), intent(inout) :: p
        character(len=*), intent(in) :: body, key
        p%vars = p%vars//'export '//key//'="'// &
            between(body, '{"key":"'//key//'","value":"', '"')//'"'//new_line('a')
    end subroutine add_variable

    subroutine ok(sfd, text)
        integer, intent(in) :: sfd
        character(len=*), intent(in) :: text
        call http_send_response(sfd, 200, 'application/json', text)
    end subroutine ok

    subroutine progress(p)
        !! A pending job starts at a poll unless the hold file exists: the
        !! pipeline's script runs in a checkout of the pushed branch, in the
        !! background as on a runner; the job ends with it.
        type(pipeline_t), intent(inout) :: p
        character(len=16) :: n
        logical :: held, done
        integer :: rc

        select case (p%status)
        case ('pending')
            inquire (file=hold, exist=held)
            if (held) return
            p%status = 'running'
            p%started = '2026-11-12T10:00:00.000Z'
            write (n, '(i0)') p%id
            p%dir = trim(cwd)//'/'//jobs_dir//'/job-'//trim(n)
            call execute_command_line('rm -rf '//p%dir//' && mkdir -p '//p%dir//'/src', &
                exitstat=rc)
            call write_file(p%dir//'/env', p%vars//'export FY_WORK="'//p%dir//'/work"'// &
                new_line('a')//'export CI_API_V4_URL="'//glurl//'/api/v4"'//new_line('a')// &
                'export CI_PROJECT_ID="grp%2Fci" CI_JOB_TOKEN=job-token CI_JOB_IMAGE=gcc:14 '// &
                'FO_VERSION=v0 FPM_VERSION=0 PATH="'//trim(cwd)//'/'//stub_dir//':$PATH"'// &
                new_line('a'))
            call proc_start([character(len=1024) :: 'sh', '-c', &
                'git --git-dir="$1" archive "refs/heads/$2" | tar -x -C "$3/src" && '// &
                'cd "$3/src" && . "$3/env" && printf "section_start:1:step_script\r\033[0K'// &
                '\033[32;1m\$ the job''s script\033[0;m\n" && sh "$4"; rc=$?; '// &
                'printf "section_end:2:step_script\r\033[0K\n"; exit $rc', 'job', remote, &
                p%ref, p%dir, trim(cwd)//'/'//script], p%dir//'/trace', p%pid)
        case ('running')
            if (p%pid <= 0) return
            call proc_try_wait(p%pid, done, rc)
            if (.not. done) return
            p%pid = 0
            p%trace = read_file(p%dir//'/trace')
            if (rc == 0) then
                p%status = 'success'
            else
                p%status = 'failed'
                p%reason = 'script_failure'
            end if
        end select
    end subroutine progress

    subroutine script_outcome(command, limit, outcome, what, path)
        !! The pipeline's script run on a check that runs `command`, with the
        !! time limit `limit` (s), leaves `outcome` for the backend.
        character(len=*), intent(in) :: command, limit, outcome, what
        character(len=*), intent(in), optional :: path
        character(len=:), allocatable :: d, env_path

        d = trim(cwd)//'/'//top//'/alone'
        env_path = trim(cwd)//'/'//stub_dir//':/usr/bin:/bin'
        if (present(path)) env_path = path
        call execute_command_line('rm -rf '//d//' && mkdir -p '//d//'/src/.fortyard && printf '// &
            '"#!/bin/sh\n'//command//'\n" > '//d//'/src/.fortyard/check && chmod +x '//d// &
            '/src/.fortyard/check && cd '//d//'/src && env PATH="'//env_path//'" FY_WORK='//d// &
            '/work FY_REPO=demo FY_KIND=check FY_TIMEOUT='//limit//' sh '//trim(cwd)//'/'// &
            script//' > '//d//'/trace 2>&1', exitstat=stat)
        text = read_file(d//'/src/fy-out/outcome')
        call check(text == outcome//new_line('a'), what//': '//text//read_file(d//'/trace'))
    end subroutine script_outcome

    subroutine write_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: u
        open (newunit=u, file=path, access='stream', form='unformatted', status='replace', &
            action='write')
        write (u) content
        close (u)
    end subroutine write_file
    integer function number_in(path)
        !! The first number in path, or 0.
        character(len=*), intent(in) :: path
        integer :: s, e, ios

        number_in = 0
        s = scan(path, '0123456789')
        if (s == 0) return
        e = s
        do while (e < len(path))
            if (index('0123456789', path(e + 1:e + 1)) == 0) exit
            e = e + 1
        end do
        read (path(s:e), *, iostat=ios) number_in
    end function number_in

    function between(text, open, close) result(s)
        character(len=*), intent(in) :: text, open, close
        character(len=:), allocatable :: s
        integer :: a, e

        s = ''
        a = index(text, open)
        if (a == 0) return
        a = a + len(open)
        e = index(text(a:), close)
        if (e == 0) return
        s = text(a:a + e - 2)
    end function between

    logical function ends(s, suffix)
        character(len=*), intent(in) :: s, suffix
        ends = .false.
        if (len(s) >= len(suffix)) ends = s(len(s) - len(suffix) + 1:) == suffix
    end function ends

    function stamp(log, what, after) result(t)
        !! The time of the first request in `log` that contains `what`
        !! (and comes after the time `after`), or -1.
        character(len=*), intent(in) :: log, what
        integer(int64), intent(in), optional :: after
        integer(int64) :: t, at
        integer :: s, e, ios

        t = -1
        s = 1
        do while (s <= len(log))
            e = index(log(s:), new_line('a'))
            if (e == 0) e = len(log) - s + 2
            if (index(log(s:s + e - 2), what) > 0) then
                read (log(s + 1:s + index(log(s:), ' ') - 2), *, iostat=ios) at
                if (ios == 0) then
                    if (.not. present(after)) then
                        t = at
                        return
                    end if
                    if (at > after) then
                        t = at
                        return
                    end if
                end if
            end if
            s = s + e
        end do
    end function stamp

    function read_file(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer(int64) :: n
        integer :: u, ios

        text = ''
        inquire (file=path, size=n)
        if (n <= 0) return
        deallocate (text)
        allocate (character(len=n) :: text)
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            iostat=ios)
        if (ios == 0) read (u, iostat=ios) text
        close (u)
    end function read_file

    subroutine wait_log(what)
        !! Until the fake has seen a request that contains `what`.
        character(len=*), intent(in) :: what
        integer :: k
        do k = 1, 1800
            text = read_file(fake_log)
            if (index(text, what) > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'GitLab is asked for '//what//': '//text)
    end subroutine wait_log

    subroutine wait_no_branches(what)
        character(len=*), intent(in) :: what
        character(len=:), allocatable :: out
        integer :: k, st
        do k = 1, 1800
            call proc_run([character(len=1024) :: 'git', '--git-dir='//remote, 'for-each-ref', &
                'refs/heads/check'], out, st, 65536)
            if (st == 0 .and. len(out) == 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., what//': '//out)
    end subroutine wait_no_branches

    subroutine land(branch, edit, subject)
        !! A branch from main with one commit, landed.
        character(len=*), intent(in) :: branch, edit, subject
        call execute_command_line(gw//'fetch -q '//url//' main && '//gw// &
            'checkout -q -B '//branch//' FETCH_HEAD && (cd '//work//' && '//edit//') && '//gw// &
            'add -A && '//gw//'commit -q -m "'//subject//'" && '//gw//'push -q '//url// &
            ' HEAD:'//branch, exitstat=stat)
        call check(stat == 0, 'branch '//branch//' is pushed')
        reply = post('/api/v1/repos/demo/land', '{"branch":"'//branch//'"}')
    end subroutine land

    subroutine wait_job(number, state)
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        integer :: k
        character(len=16) :: n
        write (n, '(i0)') number
        do k = 1, 1800
            reply = get('/api/v1/jobs/'//trim(n))
            ! The job's own state, not one of its checks'.
            if (index(reply, '"state":"'//state//'","queued"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'job '//trim(n)//' '//state//': '//reply)
    end subroutine wait_job

    function post(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)//CRLF//CRLF//body)
    end function post

    function get(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        r = exchange('GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF//CRLF)
    end function get

    function exchange(request) result(r)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, n, st
        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, request, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            r = r//buf(1:n)
        end do
        call sock_close(cfd)
    end function exchange

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (back > 0) call proc_kill(back, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            if (fake > 0) call proc_kill(fake, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_backend_gitlab
