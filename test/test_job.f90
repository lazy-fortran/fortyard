program test_job
    !! Jobs name their change. A change whose first entry fails its check and
    !! whose second lands shows as two entries of one change: each job carries
    !! the number and title of the change, which entry of it the job is and its
    !! checks with their runner; the failed one names the entry that followed
    !! and the first line of its log with Error or FAIL. The list is filtered
    !! by kind and state. fy job, fy jobs and the MCP tools show the same, and
    !! so does the page of the job as the server sends it, with an address for
    !! every line of the log.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        tcp_listen
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_wait
    use fy_runner, only: &
        runner_main
    use fy_server, only: &
        server_set_checks, &
        server_set_data_dir, &
        server_set_web_dir, &
        server_supervise
    use fy_sqlite, only: &
        db_close, &
        db_t
    use fy_store, only: &
        store_bootstrap, &
        store_open, &
        store_repo_add, &
        store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_job_data', work = 'build/test_job_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, server, fy, env, out, in, line
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, run_pid, i, at, u
    logical :: exists

    ! runners ask for work every 100 ms instead of 5 s
    call proc_setenv('FY_RUNNER_POLL_MS', '100')
    sup = 0
    run_pid = 0
    call getcwd(cwd)
    ! The runner's git must not use the machine's configuration.
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    ! The page template, as the build of the browser application leaves it.
    call execute_command_line('mkdir -p '//data//'/web')
    open (newunit=u, file=data//'/web/index.html', status='replace', action='write')
    write (u, '(a)') '<title>t</title><div id="app"></div><!--fy-data-->'
    close (u)
    call server_set_web_dir(data//'/web')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
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
    run_pid = proc_fork()
    if (run_pid == 0) then
        call runner_main(server, runner, data, '')
        call proc_exit(0)
    end if
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = trim(read_line(data//'/fy'))
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 && '
    in = env//'cd '//work//' && '
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! main exists, from a landing of a branch that has no change: job 1.
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\necho check ran\n'// &
        'if test -f BROKEN; then echo \"Error: BROKEN is there\"; exit 1; fi\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land --wait')
    call check(stat == 0, 'a first landing: '//out)

    ! Change #2: its first version fails the check (job 2), its second lands
    ! (job 3).
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git checkout -q -b feature FETCH_HEAD && '// &
        'printf "two\n" >> README && touch BROKEN && '//git//'add -A && '//git// &
        'commit -q -m "Add a line" -m "Change-Id: I0123abcd" && '//fy//' push && '//fy// &
        ' change land 2 --wait')
    call check(stat /= 0 .and. index(out, 'change #2') > 0 .and. index(out, 'failed') > 0, &
        'the first entry of change #2 fails its check: '//out)
    call run(in//'git rm -q -f BROKEN && '//git//'commit -q --amend --no-edit && '//fy// &
        ' push && '//fy//' change land 2 --wait')
    call check(stat == 0 .and. index(out, 'landed as') > 0, 'its second entry lands: '//out)

    ! The API: both jobs name the change, and are two entries of it.
    call run(env//fy//' job 2 --json')
    call check(index(out, '"id":2,"kind":"landing"') > 0 .and. index(out, '"state":"failed"') > 0 &
        .and. index(out, '"change":2,"title":"Add a line","version":null,"reason":null,'// &
        '"session":"bootstrap","candidate":null,"entry":1,"entries":2') > 0, &
        'a failed job names its change and is entry 1 of 2: '//out)
    call check(index(out, '"runner":"runner-1","checks":[{"id":') > 0 .and. &
        index(out, '"attempt":1,"kind":"landing","state":"failed","runner":"runner-1"') > 0 &
        .and. index(out, '"tree":null,"command":null,"image":null,"fo_version":null,'// &
        '"exit_status":null}') > 0, 'it carries its check with the runner: '//out)
    call check(index(out, '"wait":null') == 0 .and. index(out, '"run":null') == 0, &
        'a finished check has its wait and run time: '//out)
    call check(index(out, '"next":{"id":3,"state":"landed"}') > 0, &
        'it names the entry that followed: '//out)
    ! The line the job's log has the check's complaint on.
    at = index(out, '"error_line":')
    call check(at > 0 .and. index(out, '"error_line":null') == 0, &
        'the first line with Error or FAIL is named: '//out)
    line = out(at + 13:)
    line = line(1:index(line, ',') - 1)
    call run(env//fy//' job 3 --json')
    call check(index(out, '"change":2,"title":"Add a line"') > 0 .and. &
        index(out, '"entry":2,"entries":2') > 0 .and. index(out, '"next":null') > 0 .and. &
        index(out, '"error_line":null') > 0 .and. index(out, '"state":"passed"') > 0, &
        'the landed job is entry 2 of 2, the newest, with a passed check: '//out)
    call run(in//fy//' verify --no-wait && '//fy//' jobs --json --kind verify')
    call check(index(out, '"kind":"verify"') > 0 .and. index(out, '"kind":"landing"') == 0 &
        .and. index(out, '"change":null,"title":null') > 0 .and. &
        index(out, '"entry":null,"entries":null') > 0, &
        'kind=verify keeps the verification, a job without a change: '//out)

    ! The list carries the same members and is filtered by kind and state.
    call run(env//fy//' jobs --json')
    call check(index(out, '"id":3,"kind":"landing"') > 0 .and. index(out, '"id":2,') > 0 .and. &
        index(out, '"change":2,"title":"Add a line"') > 0 .and. index(out, '"checks":[{') > 0 &
        .and. index(out, '"candidate":null') > 0 .and. index(out, '"log"') == 0, &
        'the list names changes and checks: '//out)
    call run(env//fy//' jobs --json --state failed --kind landing')
    call check(index(out, '"id":2,') > 0 .and. index(out, '"id":3,') == 0 .and. &
        index(out, '"id":1,') == 0, 'state=failed keeps the failed job: '//out)
    call run(env//fy//' jobs --json --state all')
    call check(index(out, '"id":2,') > 0 .and. index(out, '"id":3,') > 0, &
        'state=all keeps all: '//out)
    call run(env//fy//' jobs --json --kind test')
    call check(index(out, '"jobs":[]') > 0, 'kind=test has no jobs yet: '//out)
    call run(env//fy//' jobs --json demo --kind landing')
    call check(index(out, '"id":3,') > 0 .and. index(out, '"kind":"verify"') == 0, &
        'kind=landing keeps the entries: '//out)
    call run(env//fy//' jobs --kind nothing')
    call check(stat /= 0 .and. index(out, 'kind is landing, test or verify') > 0, &
        'an unknown kind is refused: '//out)
    call run(env//fy//' jobs --state "failed&repo=other"')
    call check(stat /= 0 .and. index(out, '--kind and --state take one word') > 0, &
        'a state that would add a parameter to the query is refused: '//out)

    ! fy job and fy jobs print the same.
    call run(env//fy//' job 2')
    call check(index(out, 'job 2: landing of demo feature, failed'//new_line('a')// &
        'change #2 Add a line, entry 1 of 2'//new_line('a')// &
        'next entry of this change: job 3, landed'//new_line('a')//'check ') == 1 .and. &
        index(out, ', attempt 1, failed after ') > 0 .and. index(out, ', runner runner-1') > 0 &
        .and. index(out, 'first line with Error or FAIL: '//line//new_line('a')) > 0 .and. &
        index(out, 'Error: BROKEN is there') > 0, 'fy job: '//out)
    call run(env//fy//' jobs --state failed')
    call check(index(out, 'ID  KIND     REPO  STATE   STARTED') == 1 .and. &
        index(out, 'BRANCH   CHANGE  RUNNER    TITLE') > 0 .and. &
        index(out, 'feature  2       runner-1  Add a line') > 0, 'fy jobs: '//out)

    ! Agents get the same through fy mcp.
    call run('printf ''%s\n'' ''{"jsonrpc":"2.0","id":1,"method":"tools/call","params":'// &
        '{"name":"job","arguments":{"id":2,"lines":0}}}'' ''{"jsonrpc":"2.0","id":2,'// &
        '"method":"tools/call","params":{"name":"jobs","arguments":{"state":"failed"}}}'' '// &
        '''{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"jobs",'// &
        '"arguments":{"kind":"nothing"}}}'' > '//data//'/requests && '//env//fy//' mcp < '// &
        data//'/requests')
    call check(index(out, 'job 2: landing of demo feature, failed\nchange #2 Add a line, '// &
        'entry 1 of 2\nnext entry of this change: job 3, landed\ncheck ') > 0 .and. &
        index(out, 'first line with Error or FAIL: '//line//'\n"}]') > 0, &
        'the MCP tool job: '//out)
    call check(index(out, '\"change\":2,\"title\":\"Add a line\"') > 0 .and. &
        index(out, '\"id\":3,') == 0, 'the MCP tool jobs takes a state: '//out)
    call check(index(out, 'jobs needs a valid kind') > 0, 'and refuses an unknown kind: '//out)

    ! The page of the job, as the server sends it to a signed-in browser.
    call run('curl -s -o /dev/null -c '//data//'/jar -H "Content-Type: application/json" -d '''// &
        '{"token":"'//admin//'"}'' '//server//'/api/v1/session && curl -s -b '//data// &
        '/jar '//server//'/jobs/2')
    call check(index(out, '<title>Job 2 · fortyard</title>') > 0 .and. &
        index(out, '>demo #2 Add a line</a>') > 0 .and. index(out, '>entry 1 of 2<') > 0 .and. &
        index(out, '>next entry of this change:<') > 0 .and. index(out, '>job 3, landed</a>') > 0, &
        'the page of the job names its change and the next entry: '//out)
    call check(index(out, '>attempt 1<') > 0 .and. index(out, '>runner runner-1<') > 0, &
        'it shows the check with its runner: '//out)
    call check(index(out, ' id="L'//line//'"') > 0 .and. index(out, '/jobs/2#L'//line//'"') > 0 &
        .and. index(out, 'Error: BROKEN is there') > 0, &
        'every line of the log has an address, and the first with Error is linked: '//out)

    ! Filters the server refuses match no job, also after the list was shown;
    ! a job has one address.
    call run('curl -s -o /dev/null -b '//data//'/jar '//server//'/jobs && curl -s -b '//data// &
        '/jar "'//server//'/jobs?kind=bogus"')
    call check(index(out, '>No jobs match.<') > 0 .and. index(out, 'data-box="row-j') == 0 .and. &
        index(out, '>Loading...<') == 0, 'an unknown kind in the address matches no job: '//out)
    call run('curl -s -o /dev/null -w "%{http_code}" -b '//data//'/jar '//server//'/jobs/02')
    call check(index(out, '404') == 1, '/jobs/02 is not the address of job 2: '//out)

    ! Without a runner the first of three landings waits for its check and
    ! holds the queue, so at least two of them are checked together as one
    ! candidate, under the ref of its last member. Every member carries the
    ! candidate's check.
    call proc_kill(run_pid, SIGTERM)
    call proc_wait(run_pid, stat)
    run_pid = 0
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && for b in a b c; do git checkout -q -B $b '// &
        'FETCH_HEAD && echo $b > $b.txt && '//git//'add $b.txt && '//git//'commit -q -m '// &
        '"Add $b" && '//fy//' land --no-wait || exit 1; done')
    call check(stat == 0 .and. index(out, 'landing 5 of demo a queued') > 0 .and. &
        index(out, 'landing 7 of demo c queued') > 0, 'three landings are queued: '//out)
    do i = 1, 1200
        call run(env//fy//' job 5 --json')
        if (index(out, '"checks":[{') > 0 .and. index(out, '"state":"queued","runner"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(out, '"state":"queued","runner":null') > 0, &
        'the first landing shows its check waiting for a runner: '//out)

    ! A runner that restarts hands its checks back; the pages of their jobs
    ! hear of it, since the jobs no longer name that runner.
    call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" '//server// &
        '/api/v1/checks/claim')
    call check(index(out, '"kind":"check"') > 0, 'the check is claimed by hand: '//out)
    call execute_command_line('curl -sN -H "Authorization: Bearer '//admin//'" "'//server// &
        '/api/v1/events?topic=jobs" > '//data//'/stream 2>/dev/null & echo $! > '//data// &
        '/stream.pid')
    do i = 1, 1200
        if (index(read_all(data//'/stream'), 'data: now') > 0) exit
        call proc_sleep_ms(100)
    end do
    call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" '//server// &
        '/api/v1/checks/release')
    call check(index(out, '"released":1') > 0, 'the check is handed back: '//out)
    do i = 1, 300
        if (occurrences(read_all(data//'/stream'), 'event: jobs') >= 2) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('kill $(cat '//data//'/stream.pid)')
    call check(occurrences(read_all(data//'/stream'), 'event: jobs') >= 2, &
        'handing a check back tells the pages of jobs: '//read_all(data//'/stream'))

    run_pid = proc_fork()
    if (run_pid == 0) then
        call runner_main(server, runner, data, '')
        call proc_exit(0)
    end if
    call wait_job('5', 'landed')
    call wait_job('6', 'landed')
    call wait_job('7', 'landed')
    line = ''
    do i = 5, 7
        write (num, '(i0)') i
        call run(env//fy//' job '//trim(num)//' --json')
        call check(index(out, '"runner":"runner-1","checks":[{"id":') > 0 .and. &
            index(out, '"attempt":1,"kind":"landing","state":"passed","runner":"runner-1"') > 0, &
            'job '//trim(num)//' carries the check that landed it: '//out)
        if (index(out, 'other landing(s) in one candidate') > 0) line = line//trim(num)
    end do
    call check(len(line) >= 2, 'two of the landings were checked as one candidate: '//line)

    call proc_kill(run_pid, SIGTERM)
    call proc_wait(run_pid, stat)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'job: all checks passed'

contains

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    subroutine wait_job(id, want)
        !! Until job `id` itself, not one of its checks, is in state `want`.
        character(len=*), intent(in) :: id, want
        integer :: k
        do k = 1, 1200
            call run(env//fy//' job '//id//' --json')
            if (index(out, '"state":"'//want//'","queued"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'job '//id//' is '//want//': '//out)
    end subroutine wait_job

    integer function occurrences(text, part) result(n)
        character(len=*), intent(in) :: text, part
        integer :: from, at
        n = 0
        from = 1
        do
            at = index(text(from:), part)
            if (at == 0) return
            n = n + 1
            from = from + at + len(part) - 1
        end do
    end function occurrences

    function read_line(path) result(text)
        character(len=*), intent(in) :: path
        character(len=4096) :: text
        integer :: u, ios
        text = ''
        open (newunit=u, file=path, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) text
        close (u)
    end function read_line

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: u, ios, n
        text = ''
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=ios) text
        close (u)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (run_pid > 0) call proc_kill(run_pid, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_job
