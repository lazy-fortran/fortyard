program test_verify
    !! Eventual correctness with a runner: the landing check passes a commit
    !! that the full verification (.fortyard/verify) fails. Verifications of
    !! main find it: the commits between the last pass and the failure are
    !! bisected to the one that broke it, an issue names it with the failing
    !! log, its change gets a comment, and the issue is closed once main
    !! passes again. Landing checks are never held up by verifications.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_wait
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_runner, only: runner_main
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_data', work = 'build/test_verify_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, macos, base, url, reply, gw, issue
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, run, mac, i, k
    logical :: exists

    sup = 0
    run = 0
    mac = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_token_create(db, 1_int64, 'runner-2', 'runner', macos, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    ! Main is verified on two runner classes, landings are checked on linux.
    call db_exec(db, "UPDATE repos SET policy_verify_on = 'linux,macos-arm64';", stat)
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
    do i = 1, 150
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    run = proc_fork()
    if (run == 0) then
        call runner_main('http://'//base, runner, data, '')
        call proc_exit(0)
    end if
    mac = proc_fork()
    if (mac == 0) then
        ! Each runner has a temporary directory, and so a work tree, of its own.
        call getcwd(cwd)
        call execute_command_line('mkdir -p '//data//'/tmp-macos', exitstat=stat)
        call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp-macos')
        call runner_main('http://'//base, macos, data, '', 'macos-arm64')
        call proc_exit(0)
    end if
    url = 'http://x:'//admin//'@'//base//'/demo.git'
    gw = git//'-C '//work//' '

    ! The landing check passes as long as README exists; the verification
    ! also fails on a file named BROKEN.
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && printf "#!/bin/sh\necho full suite\ntest -f README && '// &
        'test ! -f BROKEN && { test \"\$FORTYARD_RUNNER_CLASS\" != macos-arm64 || '// &
        'test ! -f MACBROKEN; }\n" > '//work//'/.fortyard/verify && chmod +x '//work// &
        '/.fortyard/check '//work//'/.fortyard/verify && '//gw//'add -A && '//gw// &
        'commit -q -m First && '//gw//'push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a first branch is pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call wait_job(1, 'landed')
    call wait_verified(1)

    ! Three landings, the second of which breaks the verification only.
    call land('second', 'printf "2\n" > two', 'Second')
    call land('third', 'printf "x\n" > BROKEN', 'Break the full suite')
    call land('fourth', 'printf "4\n" > four', 'Fourth')
    reply = ''
    do i = 1, 1800
        reply = get('/api/v1/repos/demo/issues')
        if (index(reply, 'Verification fails since') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, 'Break the full suite (#3)') > 0, &
        'the regression is reported, blamed on the commit that broke it: '//reply)
    ! Its number depends on when the bisection ends; it is the only issue.
    k = index(reply, '"number":') + 9
    issue = ''
    do while (index('0123456789', reply(k:k)) > 0)
        issue = issue//reply(k:k)
        k = k + 1
    end do
    reply = get('/api/v1/repos/demo/issues/'//issue)
    call check(index(reply, 'full suite') > 0 .and. index(reply, 'landed change #3') > 0, &
        'the issue has the failing log and names the change: '//reply)
    reply = get('/api/v1/repos/demo/changes/3')
    call check(index(reply, 'The verification of main fails since this change landed: issue #'// &
        issue) > 0, 'the change that broke it gets a comment: '//reply)

    ! A fix lands; main passes again and the issue is closed.
    call land('fix', 'git rm -q BROKEN', 'Remove BROKEN')
    do i = 1, 1800
        reply = get('/api/v1/repos/demo/issues/'//issue)
        if (index(reply, '"state":"closed"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"closed"') > 0 .and. index(reply, 'passes again') > 0, &
        'the issue is closed when main passes again: '//reply)
    reply = get('/api/v1/jobs')
    call check(index(reply, '"kind":"verify"') > 0 .and. index(reply, '"state":"passed"') > 0, &
        'verifications are jobs: '//reply)

    ! A commit that fails on one runner class only fails the verification,
    ! and the report names that class.
    call land('mac', 'printf "m\n" > MACBROKEN', 'Break macOS')
    do i = 1, 1800
        reply = get('/api/v1/repos/demo/issues')
        if (index(reply, 'Break macOS') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, 'Break macOS (#7)') > 0, 'a failure on macos-arm64 is reported: '//reply)
    k = index(reply, '"number":') + 9
    issue = ''
    do while (index('0123456789', reply(k:k)) > 0)
        issue = issue//reply(k:k)
        k = k + 1
    end do
    reply = get('/api/v1/repos/demo/issues/'//issue)
    call check(index(reply, 'On the runner class macos-arm64') > 0, &
        'the report names the runner class that failed: '//reply)

    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    call proc_kill(mac, SIGTERM)
    call proc_wait(mac, stat)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'verify: all checks passed'

contains

    subroutine land(branch, edit, subject)
        !! A branch from main with one commit, landed.
        character(len=*), intent(in) :: branch, edit, subject
        call execute_command_line(gw//'fetch -q '//url//' main && '//gw// &
            'checkout -q -B '//branch//' FETCH_HEAD && (cd '//work//' && '//edit//') && '//gw// &
            'add -A && '//gw//'commit -q -m "'//subject//'" && '//gw//'push -q '//url// &
            ' HEAD:'//branch, exitstat=stat)
        call check(stat == 0, 'branch '//branch//' is pushed')
        reply = post('/api/v1/repos/demo/land', '{"branch":"'//branch//'"}')
        call wait_landing_of(branch)
    end subroutine land

    subroutine wait_landing_of(branch)
        character(len=*), intent(in) :: branch
        integer :: k
        do k = 1, 1800
            reply = get('/api/v1/repos/demo/landings')
            if (index(reply, '"branch":"'//branch//'","account":"admin","state":"landed"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing of '//branch//': '//reply)
    end subroutine wait_landing_of

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

    subroutine wait_verified(count)
        !! Until `count` verifications passed.
        integer, intent(in) :: count
        integer :: k, seen, at
        do k = 1, 1800
            reply = get('/api/v1/jobs')
            seen = 0
            at = 1
            do
                i = index(reply(at:), '"kind":"verify"')
                if (i == 0) exit
                at = at + i
                if (index(reply(at:at + 200), '"state":"passed"') > 0) seen = seen + 1
            end do
            if (seen >= count) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'verifications passed: '//reply)
    end subroutine wait_verified

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
            if (run > 0) call proc_kill(run, SIGTERM)
            if (mac > 0) call proc_kill(mac, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_verify
