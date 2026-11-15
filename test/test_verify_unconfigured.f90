program test_verify_unconfigured
    !! A repository whose main has neither .fortyard/verify nor fpm.toml has
    !! no verification a runner could run: a request already queued for it
    !! (as a history rewrite queues one) is cancelled with the reason instead
    !! of waiting for ever, and so is a new request.
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_setenv, proc_wait, proc_sleep_ms, &
        SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_unconfigured_data', &
        work = 'build/test_verify_unconfigured_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, reply
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, i
    logical :: exists

    sup = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_exec(db, "INSERT INTO landings (id, repo, branch, account, created_at, kind,"// &
        " commit_id) VALUES (1, 'demo', 'main', 'fortyard', '2026-01-01T00:00:00Z', 'verify',"// &
        " 'abc');", stat)
    call check(stat == 0, 'a verification request is queued')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
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
    ! Main of a project that is not built with fpm, written past any hook.
    call getcwd(cwd)
    call execute_command_line('git init -q -b main '//work//' && printf "x\n" > '//work// &
        '/pyproject.toml && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && git --git-dir='//data//'/repos/demo.git fetch -q '// &
        trim(cwd)//'/'//work//' main:refs/heads/main', exitstat=stat)
    call check(stat == 0, 'main without a verification exists')

    do i = 1, 300
        reply = get('/api/v1/jobs/1')
        if (index(reply, '"state":"cancelled"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"cancelled"') > 0 .and. &
        index(reply, 'neither .fortyard/verify nor fpm.toml') > 0, &
        'the queued request is cancelled with the reason: '//reply)
    reply = post('/api/v1/repos/demo/verify')
    call check(index(reply, '"job":2') > 0, 'a new request is queued: '//reply)
    do i = 1, 300
        reply = get('/api/v1/jobs/2')
        if (index(reply, '"state":"cancelled"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"cancelled"') > 0 .and. &
        index(reply, 'neither .fortyard/verify nor fpm.toml') > 0, &
        'the new request is cancelled with the reason: '//reply)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'verify unconfigured: all checks passed'

contains

    function post(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: 2'//CRLF//CRLF//'{}')
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
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_verify_unconfigured
