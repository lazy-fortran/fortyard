program test_policy_bisect
    !! The policy setting bisect: with bisect=off, a failing verification of
    !! main is reported once, without further runs and without blaming a
    !! change; with bisect=on, the commit that broke it is found.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
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
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_policy_bisect_data', work = 'build/test_policy_bisect_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, base, url, reply, gw, issue
    character(len=16) :: num
    integer :: lfd, port, stat, sup, run, i, k
    logical :: exists

    ! runners ask for work every 100 ms instead of 5 s
    call proc_setenv('FY_RUNNER_POLL_MS', '100')
    sup = 0
    run = 0
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
    url = 'http://x:'//admin//'@'//base//'/demo.git'
    gw = git//'-C '//work//' '

    ! The landing check passes as long as README exists; the verification
    ! also fails on a file named BROKEN.
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && printf "#!/bin/sh\necho full suite\ntest -f README && '// &
        'test ! -f BROKEN\n" > '//work//'/.fortyard/verify && chmod +x '//work// &
        '/.fortyard/check '//work//'/.fortyard/verify && '//gw//'add -A && '//gw// &
        'commit -q -m First && '//gw//'push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a first branch is pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call wait_job(1, 'landed')
    call wait_verified(1)

    ! Without bisection, a failing verification of main is reported once and
    ! no change is blamed.
    reply = put('/api/v1/repos/demo/policy', '{"bisect":"off"}')
    call check(index(reply, ' 200 ') > 0 .and. index(reply, '"bisect":"off","batch":8') > 0, &
        'bisection is switched off: '//reply)
    call land('second', 'printf "2\n" > two', 'Second')
    call land('third', 'printf "x\n" > BROKEN', 'Break the full suite')
    call land('fourth', 'printf "4\n" > four', 'Fourth')
    reply = ''
    do i = 1, 600
        reply = get('/api/v1/repos/demo/issues')
        if (index(reply, 'The verification of main fails') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, 'The verification of main fails') > 0 .and. &
        index(reply, 'Verification fails since') == 0, &
        'the failure is reported without a culprit: '//reply)
    k = index(reply, '"number":') + 9
    issue = ''
    do while (index('0123456789', reply(k:k)) > 0)
        issue = issue//reply(k:k)
        k = k + 1
    end do
    reply = get('/api/v1/repos/demo/issues/'//issue)
    call check(index(reply, 'bisect=off') > 0, 'the issue says why nothing is blamed: '//reply)
    ! Main's newest commit is verified; nothing else runs, and nothing is blamed.
    call wait_idle()
    reply = get('/api/v1/repos/demo/issues')
    call check(index(reply, 'Verification fails since') == 0, 'no commit is blamed later: '//reply)
    reply = get('/api/v1/repos/demo/changes/3')
    call check(index(reply, 'The verification of main fails since') == 0, &
        'the change that broke it gets no comment: '//reply)

    ! Switched on again, the same failure is bisected to the commit that broke it.
    reply = put('/api/v1/repos/demo/policy', '{"bisect":"on"}')
    do i = 1, 600
        reply = get('/api/v1/repos/demo/issues')
        if (index(reply, 'Verification fails since') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, 'Break the full suite (#3)') > 0, &
        'with bisection, the commit that broke it is blamed: '//reply)

    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'policy_bisect: all checks passed'

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
        do k = 1, 600
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
        do k = 1, 600
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
        do k = 1, 600
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

    subroutine wait_idle()
        !! Until no job has been queued or running for a second.
        integer :: k, quiet
        quiet = 0
        do k = 1, 600
            reply = get('/api/v1/jobs')
            quiet = quiet + 1
            if (index(reply, '"state":"running"') > 0 .or. index(reply, '"state":"queued"') > 0) &
                quiet = 0
            if (quiet >= 10) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'the jobs come to an end: '//reply)
    end subroutine wait_idle

    function put(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange('PUT '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)//CRLF//CRLF//body)
    end function put

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
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_policy_bisect
