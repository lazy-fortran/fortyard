program test_backend_github
    !! The GitHub backend against a fake GitHub API (an HTTP server in this
    !! test that checks the App's RS256 signature with the App's public key)
    !! and a local bare repository acme/demo as the GitHub mirror of the
    !! fortyard repository fx, so the backend works on the upstream's name,
    !! which the fake gives runs a workflow path with @ref. A passing and a
    !! failing run are reported with their job logs, after polls that the
    !! fake answers with 304 for an unchanged run; their runs and branches are
    !! deleted. A run whose job never starts is cancelled and its check given
    !! back (error runner), and so is a check whose workflow file differs from
    !! the shipped one or that brings another active workflow, before
    !! anything is pushed. GitHub's own time limit ends a check as timeout;
    !! a check withdrawn on the server has its run cancelled. The sweep
    !! deletes old runs of check branches and cancels the runs of checks that
    !! do not run, and touches nothing else. Setup refuses a repository with secrets and
    !! prepares a clean one. The workflow built into fy is the shipped file.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_backend_github, only: github_configure, github_main, github_setup, github_token_file, &
        github_workflow
    use fy_http, only: http_request_t, http_read_request, http_send_response, http_query_param
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_set_timeouts, &
        tcp_accept, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM, &
        proc_setenv, proc_run
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create, &
        store_check_add, store_check_cancel, store_check_get
    implicit none

    character(len=*), parameter :: dir = 'build/test_backend_github'
    character(len=*), parameter :: data = dir//'/data', work = dir//'/work', up = dir//'/acme/demo.git'
    character(len=*), parameter :: api_log = dir//'/api.log'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=*), parameter :: RID0 = '90000000'
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, base, root, log, shipped, state
    character(len=16) :: num
    character(len=4096) :: cwd
    integer(int64) :: id
    integer :: lfd, flfd, port, fport, stat, sup, fake, back, i, age, mark
    logical :: exists
    ! The fake GitHub's state.
    type(http_request_t) :: req
    character(len=:), allocatable :: body, headers, path, auth, branch, sha, tag
    character(len=64) :: scen(32)
    integer :: gets(32), code, n
    logical :: cancelled(32), wf_read

    sup = 0
    fake = 0
    back = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    root = trim(cwd)//'/'//dir
    call execute_command_line('rm -rf '//dir//' && mkdir -p '//data//' '//dir//'/tmp && '// &
        'git init -q --bare '//up//' && openssl genpkey -algorithm RSA -pkeyopt '// &
        'rsa_keygen_bits:2048 -out '//dir//'/app.pem 2>/dev/null && openssl pkey -in '//dir// &
        '/app.pem -pubout -out '//dir//'/app.pub', exitstat=stat)
    call check(stat == 0, 'keys and upstream are made')
    call proc_setenv('TMPDIR', root//'/tmp')

    shipped = slurp('deploy/github/fortyard.yml')
    call check(len(shipped) > 0 .and. github_workflow() == shipped, &
        'the workflow built into fy is deploy/github/fortyard.yml')

    ! The fake GitHub.
    call tcp_listen('127.0.0.1', 0, flfd, stat)
    fport = sock_bound_port(flfd)
    fake = proc_fork()
    if (fake == 0) then
        call fake_github(flfd)
        call proc_exit(0)
    end if
    call sock_close(flfd)
    write (num, '(i0)') fport
    call github_configure('4242', dir//'/app.pem', 'acme', 'http://127.0.0.1:'//trim(num), '', &
        1, 3)

    call github_setup('leaky', stat)
    call check(stat == 0, 'setup accepts a repository whose own workflows have secrets')
    call github_setup('demo', stat)
    log = slurp(api_log)
    call check(stat == 0, 'setup accepts a clean repository')
    call check(index(log, 'PUT /repos/acme/demo/actions/permissions {"enabled":true,'// &
        '"allowed_actions":"all"}') > 0, 'setup enables Actions')
    call check(index(log, 'selected-actions') == 0 .and. index(log, '/disable') == 0 .and. &
        index(log, 'retention') == 0 .and. index(log, 'PUT /repos/acme/demo/actions/'// &
        'permissions/workflow') == 0, 'setup leaves the repository''s own workflows and '// &
        'permissions as they are')

    ! The server, with the bare repository as upstream of demo.
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'github-backend', 'runner', runner, stat)
    call store_repo_add(db, 'fx', root//'/acme/demo.git', stat, mode='primary')
    ! A GitHub class takes checks only of public repositories that allow it.
    call db_exec(db, "UPDATE repos SET public = 1, policy_backends = 'github';", stat)
    call db_close(db)
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
    do i = 1, 300
        inquire (file=data//'/repos/fx.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('git init -q -b main '//work//' && mkdir -p '//work// &
        '/.github/workflows && cp deploy/github/fortyard.yml '//work//'/.github/workflows/ && '// &
        'echo "on: push" > '//work//'/.github/workflows/old.yml && cd '//work//' && '//git// &
        'add -A && '//git//'commit -q -m base && for s in pass fail never timedout hold; do '// &
        git//'checkout -q -B $s main && echo $s > scenario && '//git//'add -A && '//git// &
        'commit -q -m $s && '//git//'push -q http://x:'//admin//'@'//base//'/fx.git $s; done'// &
        ' && '//git//'checkout -q -B changed main && echo "# changed" >> .github/workflows/'// &
        'fortyard.yml && '//git//'commit -q -am changed && '//git//'push -q http://x:'//admin// &
        '@'//base//'/fx.git changed && '//git//'checkout -q -B extra main && printf "on: push'// &
        '\npermissions: {contents: write}\n" > .github/workflows/x.yml && '//git//'add -A && '// &
        git//'commit -q -m extra && '//git//'push -q http://x:'//admin//'@'//base//'/fx.git extra', &
        exitstat=stat)
    call check(stat == 0, 'the branches are pushed')
    call store_open(data//'/fortyard.db', db, stat)
    call store_check_add(db, 'fx', 'refs/heads/pass', id, stat, class='github-linux')
    call store_check_add(db, 'fx', 'refs/heads/fail', id, stat, class='github-linux')
    call store_check_add(db, 'fx', 'refs/heads/never', id, stat, class='github-linux')
    call store_check_add(db, 'fx', 'refs/heads/changed', id, stat, class='github-linux')
    call store_check_add(db, 'fx', 'refs/heads/extra', id, stat, class='github-linux')
    call store_check_add(db, 'fx', 'refs/heads/timedout', id, stat, class='github-linux')
    call store_check_add(db, 'fx', 'refs/heads/hold', id, stat, class='github-linux')
    call db_close(db)
    call execute_command_line('printf "%s\n" '//runner//' > '//dir//'/token')

    back = proc_fork()
    if (back == 0) then
        call github_main('http://'//base, runner, 'github-linux', 20, 5, 360)
        call proc_exit(0)
    end if

    call wait_check(1, 'passed', '')
    call wait_check(2, 'failed', '')
    call check_log(1, 'GitHub log of check 1: pass', 'the job log of a pass is copied')
    call check_log(2, 'GitHub log of check 2: fail', 'the job log of a failure is copied')
    call wait_check(4, '', 'is not the workflow fortyard ships')
    call cancel(4)
    call wait_check(3, '', 'The job did not start within 3 s')
    call cancel(3)
    call wait_check(5, '', '.github/workflows/x.yml is new or changed in this check')
    call cancel(5)
    call wait_check(6, 'timeout', '')
    call check_log(6, 'GitHub ended the job at the time limit', &
        'a job GitHub timed out ends as timeout')
    do i = 1, 1200
        if (index(slurp(api_log), 'actions/runs/'//RID0//'07') > 0) exit
        call proc_sleep_ms(100)
    end do
    call cancel(7)
    do i = 1, 1200
        log = slurp(api_log)
        if (index(log, 'POST /repos/acme/demo/actions/runs/'//RID0//'07/cancel') > 0 .and. &
            index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'07') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(log, 'POST /repos/acme/demo/actions/runs/'//RID0//'07/cancel') > 0 .and. &
        index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'07') > 0, &
        'the run of a check withdrawn on the server is cancelled and deleted')
    log = slurp(api_log)
    call check(index(log, '304 GET /repos/acme/demo/actions/runs/'//RID0//'01') > 0, &
        'an unchanged run is answered with 304 to the backend''s ETag')
    call check(index(log, 'POST /repos/acme/demo/actions/runs/'//RID0//'03/cancel') > 0, &
        'the run that never started is cancelled')
    call check(index(log, 'branch=fortyard%2Fcheck%2F4') == 0, &
        'the check with a changed workflow is never looked for on GitHub')
    call check(index(log, 'branch=fortyard%2Fcheck%2F5') == 0, &
        'the check that brings another workflow is never looked for on GitHub')
    call check(index(log, 'POST /repos/acme/demo/actions/runs/8000000003/cancel') > 0 .and. &
        index(log, '8000000004/cancel') == 0, &
        'the sweep cancels a run of a check that does not run, not a run of main')
    call check(index(log, '/access_tokens {"repositories":["demo"]}') > 0 .and. &
        index(log, '401 ') == 0, 'tokens are signed by the App and limited to the repository')
    do i = 1, 600
        log = slurp(api_log)
        if (index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'01') > 0 .and. &
            index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'02') > 0 .and. &
            index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'03') > 0 .and. &
            index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'06') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'01') > 0 .and. &
        index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'02') > 0 .and. &
        index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'03') > 0 .and. &
        index(log, 'DELETE /repos/acme/demo/actions/runs/'//RID0//'06') > 0, &
        'the runs are deleted')
    call check(index(log, 'DELETE /repos/acme/demo/actions/runs/8000000001') > 0 .and. &
        index(log, 'DELETE /repos/acme/demo/actions/runs/8000000002') == 0, &
        'the sweep deletes an old run of a check branch and nothing else')
    do i = 1, 1200
        if (len(line_of('git --git-dir='//up//' for-each-ref refs/heads/fortyard')) == 0) exit
        call proc_sleep_ms(100)
    end do
    call check(len(line_of('git --git-dir='//up//' for-each-ref refs/heads/fortyard')) == 0, &
        'no check branch is left upstream')

    ! With a fine-grained token in a file instead of the App: the same check
    ! passes, and no App token is asked for or refused.
    call proc_kill(back, SIGTERM)
    call proc_wait(back, stat)
    back = 0
    mark = len(slurp(api_log))
    call execute_command_line('printf "ghs_test\n" > '//dir//'/gh.token')
    call store_open(data//'/fortyard.db', db, stat)
    call store_check_add(db, 'fx', 'refs/heads/pass', id, stat, class='github-linux')
    call db_close(db)
    back = proc_fork()
    if (back == 0) then
        write (num, '(i0)') fport
        call github_configure('', '', 'acme', 'http://127.0.0.1:'//trim(num), '', 1, 3)
        call github_token_file(dir//'/gh.token')
        call github_main('http://'//base, runner, 'github-linux', 20, 5, 360)
        call proc_exit(0)
    end if
    call wait_check(int(id), 'passed', '')
    log = slurp(api_log)
    log = log(mark + 1:)
    call check(index(log, '/actions/runs') > 0 .and. index(log, 'access_tokens') == 0 .and. &
        index(log, '/orgs/acme/installation') == 0 .and. index(log, '401 ') == 0, &
        'a token file stands in for the App: '//log)

    call stop_all()
    call execute_command_line('rm -rf '//dir)
    print '(a)', 'backend github: all checks passed'

contains

    subroutine wait_check(n, final, text)
        !! Until check n is in state `final`, or its log has `text`.
        integer, intent(in) :: n
        character(len=*), intent(in) :: final, text
        integer :: k
        do k = 1, 1200
            call store_open(data//'/fortyard.db', db, stat)
            call store_check_get(db, int(n, int64), state, log, age)
            call db_close(db)
            if (len(final) > 0 .and. state == final) return
            if (len(text) > 0 .and. index(log, text) > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'check '//itoa(n)//' '//final//text//': '//state//' '//log)
    end subroutine wait_check

    subroutine check_log(n, text, what)
        integer, intent(in) :: n
        character(len=*), intent(in) :: text, what
        call store_open(data//'/fortyard.db', db, stat)
        call store_check_get(db, int(n, int64), state, log, age)
        call db_close(db)
        call check(index(log, text) > 0, what//': '//log)
    end subroutine check_log

    subroutine cancel(n)
        !! A check given back would be taken again by the same class here,
        !! where no other class exists; it is withdrawn instead.
        integer, intent(in) :: n
        call store_open(data//'/fortyard.db', db, stat)
        call store_check_cancel(db, int(n, int64))
        call db_close(db)
    end subroutine cancel

    subroutine fake_github(listen)
        !! GitHub's REST API as far as the backend and setup use it.
        integer, intent(in) :: listen
        integer :: cfd, st, u

        gets = 0
        cancelled = .false.
        scen = ''
        wf_read = .false.
        do
            cfd = tcp_accept(listen)
            if (cfd < 0) cycle
            call sock_set_timeouts(cfd, 30)
            call http_read_request(cfd, req, st)
            if (st /= 0) then
                call sock_close(cfd)
                cycle
            end if
            code = 200
            body = ''
            headers = ''
            path = req%path
            auth = req%authorization
            ! The App's own routes need its signed JWT; the rest a token.
            if (index(path, '/app/') == 1 .or. path == '/orgs/acme/installation') then
                if (.not. app_signed(auth)) code = 401
            else if (index(path, '/raw/') /= 1) then
                if (auth /= 'Bearer ghs_test') code = 401
            end if
            if (code == 200) call answer()
            open (newunit=u, file=api_log, position='append', action='write')
            if (code == 304) then
                write (u, '(a)') '304 '//req%method//' '//path
            else if (code == 401) then
                write (u, '(a)') '401 '//req%method//' '//path
            else if (len(req%query) > 0) then
                write (u, '(a)') req%method//' '//path//'?'//req%query//' '//req%body
            else
                write (u, '(a)') req%method//' '//path//' '//req%body
            end if
            close (u)
            call http_send_response(cfd, code, 'application/json', body, headers)
            call sock_close(cfd)
        end do

    end subroutine fake_github

    subroutine answer()
        character(len=:), allocatable :: rest, rid
        integer :: st
        select case (path)
        case ('/orgs/acme/installation')
            body = '{"id":77}'
            return
        case ('/app/installations/77/access_tokens')
            code = 201
            body = '{"token":"ghs_test","expires_at":"2099-01-01T00:00:00Z"}'
            return
        case ('/installation/repositories')
            body = '{"total_count":1,"repositories":[{"name":"demo"}]}'
            return
        case ('/orgs/acme/repos')
            body = '[{"name":"demo"}]'
            return
        case ('/repos/acme/demo', '/repos/acme/leaky')
            body = '{"name":"x","private":false}'
            return
        case ('/repos/acme/demo/actions/secrets', '/repos/acme/demo/actions/'// &
                'organization-secrets', '/repos/acme/leaky/actions/organization-secrets', &
                '/repos/acme/demo/environments/pages/secrets')
            body = '{"total_count":0,"secrets":[]}'
            return
        case ('/repos/acme/leaky/actions/secrets')
            body = '{"total_count":1,"secrets":[{"name":"TOKEN"}]}'
            return
        case ('/repos/acme/demo/environments', '/repos/acme/leaky/environments')
            body = '{"total_count":1,"environments":[{"id":5,"name":"pages"}]}'
            return
        case ('/repos/acme/demo/contents/.github/workflows/fortyard.yml', &
                '/repos/acme/leaky/contents/.github/workflows/fortyard.yml')
            body = shipped
            return
        case ('/repos/acme/demo/actions/permissions/workflow')
            if (req%method == 'PUT') then
                wf_read = index(req%body, '"default_workflow_permissions":"read"') > 0
                code = 204
            else
                body = '{"default_workflow_permissions":"'// &
                    trim(merge('read ', 'write', wf_read))//'"}'
            end if
            return
        case ('/repos/acme/demo/actions/permissions', '/repos/acme/leaky/actions/permissions')
            code = 204
            return
        case ('/repos/acme/demo/contents/.github/workflows/old.yml')
            ! main's own workflow, the same in every check branch.
            body = 'on: push'//new_line('a')
            return
        case ('/repos/acme/demo/actions/workflows')
            body = '{"total_count":3,"workflows":[{"id":1,"path":".github/workflows/'// &
                'fortyard.yml","state":"active"},{"id":2,"path":".github/workflows/ci.yml",'// &
                '"state":"active"},{"id":3,"path":".github/workflows/old.yml","state":'// &
                '"disabled_manually"}]}'
            return
        case ('/repos/acme/demo/actions/runs')
            body = '{"total_count":2,"workflow_runs":[{"id":8000000001,"head_branch":'// &
                '"fortyard/check/999","status":"completed","updated_at":"2020-01-01T00:00:'// &
                '00Z"},{"id":8000000002,"head_branch":"main","status":"completed",'// &
                '"updated_at":"2020-01-01T00:00:00Z"},{"id":8000000003,"head_branch":'// &
                '"fortyard/check/998-macos","status":"in_progress","updated_at":'// &
                '"2020-01-01T00:00:00Z"},{"id":8000000004,"head_branch":"main","status":'// &
                '"in_progress","updated_at":"2020-01-01T00:00:00Z"}]}'
            return
        case ('/repos/acme/demo/actions/workflows/fortyard.yml/runs')
            branch = http_query_param(req%query, 'branch')
            sha = line_of('git --git-dir='//up//' rev-parse -q --verify refs/heads/'// &
                branch//' 2>/dev/null; true')
            body = '{"total_count":0,"workflow_runs":[]}'
            if (index(branch, 'fortyard/check/') /= 1 .or. len(sha) == 0) return
            if (sha /= http_query_param(req%query, 'head_sha')) return
            read (branch(16:), *, iostat=st) n
            if (st /= 0 .or. n < 1 .or. n > 32) return
            scen(n) = line_of('git --git-dir='//up//' show '//sha//':scenario')
            body = '{"total_count":1,"workflow_runs":['//run_json(n)//']}'
            return
        end select
        if (index(path, '/raw/') == 1) then
            read (path(6:), *, iostat=st) n
            body = 'GitHub log of check '//itoa(n - 700)//': '//trim(scen(n - 700))//new_line('a')
            return
        end if
        if (index(path, '/repos/acme/demo/actions/jobs/') == 1) then
            rest = path(31:)
            headers = 'Location: http://127.0.0.1:'//itoa(fport)//'/raw/'// &
                rest(1:index(rest, '/') - 1)//CRLF
            code = 302
            return
        end if
        if (index(path, '/repos/acme/demo/actions/runs/'//RID0) == 1) then
            rest = path(39:)
            read (rest(1:2), *, iostat=st) n
            if (st /= 0 .or. n < 1 .or. n > 32) then
                code = 404
                return
            end if
            rid = RID0//rest(1:2)
            if (req%method == 'DELETE') then
                code = 204
            else if (rest(3:) == '/cancel') then
                cancelled(n) = .true.
                code = 202
            else if (rest(3:) == '/jobs') then
                body = '{"total_count":1,"jobs":[{"id":'//itoa(700 + n)//',"name":"check",'// &
                    '"status":"'//job_status(n)//'","started_at":'//trim(merge( &
                    'null                  ', '"2026-11-12T10:00:00Z"', job_status(n) == &
                    'queued'))//'}]}'
            else
                tag = '"r'//itoa(n)//'-'//itoa(phase(n))//'"'
                if (req%if_none_match == tag) then
                    code = 304
                else
                    body = run_json(n)
                    headers = 'ETag: '//tag//CRLF
                end if
                gets(n) = gets(n) + 1
            end if
            return
        end if
        code = 404
        body = '{"message":"Not Found"}'
    end subroutine answer

    integer function phase(n)
        !! 0 queued, 1 in progress, 2 completed; each kept for two polls.
        integer, intent(in) :: n
        if (cancelled(n)) then
            phase = 3
        else if (scen(n) == 'never') then
            phase = 0
        else if (scen(n) == 'hold') then
            phase = 1
        else
            phase = min(2, gets(n)/2)
        end if
    end function phase

    function job_status(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        select case (phase(n))
        case (0); s = 'queued'
        case (1); s = 'in_progress'
        case default; s = 'completed'
        end select
    end function job_status

    function run_json(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s, conclusion
        select case (phase(n))
        case (2)
            conclusion = '"success"'
            if (scen(n) == 'fail') conclusion = '"failure"'
            if (scen(n) == 'timedout') conclusion = '"timed_out"'
        case (3)
            conclusion = '"cancelled"'
        case default
            conclusion = 'null'
        end select
        s = '{"id":'//RID0//itoa2(n)//',"head_branch":"fortyard/check/'//itoa(n)// &
            '","head_sha":"'//sha_of(n)//'","path":".github/workflows/fortyard.yml@'// &
            'fortyard/check/'//itoa(n)//'",'// &
            '"status":"'//job_status(n)//'","conclusion":'//conclusion// &
            ',"updated_at":"2099-01-01T00:00:00Z"}'
    end function run_json

    function sha_of(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        s = line_of('git --git-dir='//up//' rev-parse -q --verify refs/heads/fortyard/check/'// &
            itoa(n)//' 2>/dev/null; true')
    end function sha_of

    logical function app_signed(header)
        !! Whether `header` carries a JWT of App 4242 signed with its key.
        character(len=*), intent(in) :: header
        character(len=:), allocatable :: out
        integer :: s
        app_signed = .false.
        if (index(header, 'Bearer ') /= 1) return
        call proc_run([character(len=2048) :: 'sh', '-c', &
            't=$1; d=$2; s=$(printf %s "$t" | cut -d. -f3 | tr "_-" "/+"); '// &
            'while [ $(( ${#s} % 4 )) -ne 0 ]; do s="$s="; done; '// &
            'printf "%s" "$s" | openssl base64 -d -A > "$d/sig" && '// &
            'printf %s "$t" | cut -d. -f1,2 | tr -d "\n" > "$d/msg" && '// &
            'openssl dgst -sha256 -verify "$d/app.pub" -signature "$d/sig" "$d/msg" '// &
            '> /dev/null && p=$(printf %s "$t" | cut -d. -f2 | tr "_-" "/+"); '// &
            'while [ $(( ${#p} % 4 )) -ne 0 ]; do p="$p="; done; '// &
            'printf "%s" "$p" | openssl base64 -d -A | grep -q ''"iss":"4242"''', 'verify', &
            header(8:), dir], out, s, 4096)
        app_signed = s == 0
    end function app_signed

    function itoa(v) result(s)
        integer, intent(in) :: v
        character(len=:), allocatable :: s
        character(len=24) :: b
        write (b, '(i0)') v
        s = trim(b)
    end function itoa

    function itoa2(v) result(s)
        integer, intent(in) :: v
        character(len=2) :: s
        write (s, '(i2.2)') v
    end function itoa2

    function slurp(path) result(s)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: s
        integer :: u, ios, n
        s = ''
        open (newunit=u, file=path, access='stream', form='unformatted', status='old', &
            action='read', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (s)
        allocate (character(len=n) :: s)
        if (n > 0) read (u, iostat=ios) s
        close (u)
    end function slurp

    function line_of(command) result(s)
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: s
        integer :: st
        call proc_run([character(len=2048) :: 'sh', '-c', command], s, st, 65536)
        do while (len(s) > 0)
            if (s(len(s):) /= new_line('a')) exit
            s = s(1:len(s) - 1)
        end do
    end function line_of

    subroutine stop_all()
        if (back > 0) then
            call proc_kill(back, SIGTERM)
            call proc_wait(back, stat)
        end if
        if (sup > 0) then
            call proc_kill(sup, SIGTERM)
            call proc_wait(sup, stat)
        end if
        if (fake > 0) then
            call proc_kill(fake, SIGTERM)
            call proc_wait(fake, stat)
        end if
        back = 0
        sup = 0
        fake = 0
    end subroutine stop_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            call stop_all()
            error stop 1
        end if
    end subroutine check

end program test_backend_github
