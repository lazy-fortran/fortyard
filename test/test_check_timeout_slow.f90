program test_check_timeout_slow
    !! A runner ends a check at the time limit of its repository (one minute
    !! here), reports it as an error and not as a failure, and the check runs
    !! again: its second run passes and lands, and the landing's log says what
    !! ended the first. Slow, since the limit is counted in minutes.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_policy, only: policy_get, policy_put, policy_t
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM, proc_setenv
    use fy_runner, only: runner_main
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_check_timeout_slow_data'
    character(len=*), parameter :: work = 'build/test_check_timeout_slow_work'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    type(policy_t) :: policy
    character(len=:), allocatable :: admin, runner, base, url, reply
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, run, i
    logical :: exists, found

    sup = 0
    run = 0
    call getcwd(cwd)
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')

    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'slowlimits', '', stat, mode='primary')
    call policy_get(db, 'slowlimits', policy)
    policy%check_timeout = 1
    call policy_put(db, 'slowlimits', policy, found)
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
        inquire (file=data//'/repos/slowlimits.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    run = proc_fork()
    if (run == 0) then
        call runner_main('http://'//base, runner, data, '')
        call proc_exit(0)
    end if

    ! The first run sleeps past the limit of one minute; the runner ends it
    ! and reports an error, the check is queued again and its second run
    ! passes.
    url = 'http://x:'//admin//'@'//base//'/slowlimits.git'
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "#!/bin/sh\nif [ ! -f '//trim(cwd)//'/'//data//'/ran ]; then touch '// &
        trim(cwd)//'/'//data//'/ran; echo first run; sleep 600; fi\necho second run passes\n"'// &
        ' > '//work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && '//git//'-C '// &
        work//' add -A && '//git//'-C '//work//' commit -q -m First && '//git//'-C '//work// &
        ' push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a branch is pushed')
    reply = post('/api/v1/repos/slowlimits/land', '{"branch":"first"}', admin)
    call wait_landing(1, 'running')
    do i = 1, 300
        reply = get('/api/v1/jobs')
        if (index(reply, '"checks":1') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"name":"slowlimits"') > 0 .and. index(reply, '"checks":1') > 0, &
        'the jobs list shows the runner the repository holds: '//reply)
    call wait_landing(1, 'landed')
    reply = get('/api/v1/jobs/1')
    call check(index(reply, 'The time limit (1 min) ended the check') > 0 .and. &
        index(reply, 'first run') > 0 .and. index(reply, 'second run passes') > 0, &
        'a check ended by its time limit runs again and its log says so: '//reply)
    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    run = 0

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'check_timeout_slow: all checks passed'

contains

    subroutine wait_landing(number, state)
        !! Waits for the state; a check that reaches its time limit of a
        !! minute takes that long and more under load.
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        integer :: k
        character(len=16) :: n
        write (n, '(i0)') number
        do k = 1, 3000
            reply = get('/api/v1/jobs/'//trim(n))
            if (index(reply, '"state":"'//state//'"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//trim(n)//' '//state//': '//reply)
    end subroutine wait_landing

    function post(path, body, token) result(r)
        character(len=*), intent(in) :: path, body, token
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//token//CRLF// &
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

end program test_check_timeout_slow
