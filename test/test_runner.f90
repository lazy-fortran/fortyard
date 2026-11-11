program test_runner
    !! With checks left to a runner, a landing waits for a separate runner
    !! process: a passing check lands, a failing one does not, and the commit's
    !! temporary check ref is gone afterwards. A remote runner, with its own
    !! empty data directory, checks a third landing through the API alone. The
    !! build directory is kept between checks, and a failure is confirmed from
    !! an empty one before it is reported. While one repository's check runs,
    !! another repository lands, and a second landing of the first waits.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM, proc_setenv
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_runner, only: runner_main
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_runner_data', work = 'build/test_runner_work'
    character(len=*), parameter :: remote_dir = 'build/test_runner_remote'
    character(len=*), parameter :: gate = 'build/test_runner_gate'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, base, url, reply
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, run, run2, i
    logical :: exists

    sup = 0
    run = 0
    ! The runners' git must not use the machine's configuration, such as a
    ! credential helper that supplies (and stores) the test's tokens.
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call store_repo_add(db, 'other', '', stat, mode='primary')
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
        if (exists) inquire (file=data//'/repos/other.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    run = proc_fork()
    if (run == 0) then
        call runner_main('http://'//base, runner, data, '')
        call proc_exit(0)
    end if

    url = 'http://x:'//admin//'@'//base//'/demo.git'
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\necho runner says hi\n'// &
        'mkdir -p build && echo x >> build/count && echo checks seen \$(wc -l < build/count)\n'// &
        'test -f README\n" > '//work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check'// &
        ' && '//git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m "First" && '// &
        git//'-C '//work//' push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a branch is pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call wait_landing(1, 'landed')
    reply = get('/api/v1/jobs/1')
    call check(index(reply, 'Checked by a runner.') > 0, 'the runner checked the landing')
    call execute_command_line('! git --git-dir='//data//'/repos/demo.git rev-parse -q --verify '// &
        'refs/fortyard/checks/1', exitstat=stat)
    call check(stat == 0, 'the check ref is removed')

    call execute_command_line(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '// &
        work//' checkout -q -B second FETCH_HEAD && '//git//'-C '//work//' rm -q README && '// &
        git//'-C '//work//' commit -q -m "Remove README" && '//git//'-C '//work//' push -q '// &
        url//' HEAD:second', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"second"}')
    call wait_landing(2, 'failed')
    reply = get('/api/v1/jobs/2')
    call check(index(reply, 'runner says hi') > 0, 'a failing check reports the runner''s output')
    call check(index(reply, 'checks seen 2') > 0, 'the build directory is kept between checks')
    call check(index(reply, 'rerun from an empty build directory') > 0 .and. &
        index(reply, 'checks seen 1') > 0, 'a failure is confirmed from an empty build directory')
    call check(index(reply, 'runner disk:') > 0, 'the check log reports the runner''s disk use')

    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    call execute_command_line('rm -rf '//remote_dir//' '//remote_dir//'-2 && mkdir -p '// &
        remote_dir//' '//remote_dir//'-2', exitstat=stat)
    run = proc_fork()
    if (run == 0) then
        call runner_main('http://'//base, runner, remote_dir, '')
        call proc_exit(0)
    end if
    call execute_command_line(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '// &
        work//' checkout -q -B third FETCH_HEAD && printf "more\n" >> '//work//'/README && '// &
        git//'-C '//work//' commit -q -am "More" && '//git//'-C '//work//' push -q '// &
        url//' HEAD:third', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"third"}')
    call wait_landing(3, 'landed')
    inquire (file=remote_dir//'/mirrors/demo.git/HEAD', exist=exists)
    call check(exists, 'the remote runner checked out from its own mirror')
    call execute_command_line(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '// &
        work//' checkout -q -B fourth FETCH_HEAD && printf "again\n" >> '//work//'/README && '// &
        git//'-C '//work//' commit -q -am "Again" && '//git//'-C '//work//' push -q '// &
        url//' HEAD:fourth', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"fourth"}')
    call wait_landing(4, 'landed')

    ! A check of demo that waits for the gate file holds one runner; other
    ! lands meanwhile on a second one, and demo's next landing waits its turn.
    run2 = proc_fork()
    if (run2 == 0) then
        call runner_main('http://'//base, runner, remote_dir//'-2', '')
        call proc_exit(0)
    end if
    call getcwd(cwd)
    call execute_command_line('rm -f '//gate//' && '//git//'-C '//work//' fetch -q '//url// &
        ' main && '//git//'-C '//work//' checkout -q -B slow FETCH_HEAD && printf '// &
        '"#!/bin/sh\nwhile [ ! -f '//trim(cwd)//'/'//gate//' ]; do sleep 0.1; done\n" > '// &
        work//'/.fortyard/check && '//git//'-C '//work//' commit -q -am "Slow" && '//git// &
        '-C '//work//' push -q '//url//' HEAD:slow && '//git//'-C '//work//' checkout -q -B '// &
        'after FETCH_HEAD && printf "after\n" >> '//work//'/README && '//git//'-C '//work// &
        ' commit -q -am "After" && '//git//'-C '//work//' push -q '//url//' HEAD:after && '// &
        git//'-C '//work//' checkout -q -B other FETCH_HEAD && '//git//'-C '//work// &
        ' push -q http://x:'//admin//'@'//base//'/other.git HEAD:first', exitstat=stat)
    call check(stat == 0, 'branches of two repositories are pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"slow"}')
    call wait_landing(5, 'running')
    reply = post('/api/v1/repos/other/land', '{"branch":"first"}')
    reply = post('/api/v1/repos/demo/land', '{"branch":"after"}')
    call wait_landing(6, 'landed')
    reply = get('/api/v1/jobs/7')
    call check(index(reply, '"state":"queued"') > 0, 'a second landing of demo waits: '//reply)
    reply = get('/api/v1/jobs/5')
    call check(index(reply, '"state":"running"') > 0, 'demo''s check still runs')
    call execute_command_line('touch '//gate)
    call wait_landing(5, 'landed')
    call wait_landing(7, 'landed')
    call proc_kill(run2, SIGTERM)
    call proc_wait(run2, stat)
    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'runner: all checks passed'

contains

    subroutine wait_landing(number, state)
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        integer :: k
        character(len=16) :: n
        write (n, '(i0)') number
        do k = 1, 300
            reply = get('/api/v1/jobs/'//trim(n))
            ! The job's own state, not one of its checks'.
            if (index(reply, '"state":"'//state//'","queued"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//trim(n)//' '//state//': '//reply)
    end subroutine wait_landing

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

end program test_runner
