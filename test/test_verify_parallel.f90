program test_verify_parallel
    !! Verifications of different repositories wait for runners side by side:
    !! while the verification of one repository waits for its check, another
    !! repository's verification queues its own check instead of waiting for
    !! the first to end. No runner takes them here, so both checks stay queued.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: SIGTERM, proc_exit, proc_fork, proc_kill, proc_setenv, proc_sleep_ms, proc_wait
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_prepare, stmt_t, stmt_step, stmt_int, stmt_finalize, &
        SQLITE_ROW
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_parallel_data', &
        work = 'build/test_verify_parallel_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=*), parameter :: names(2) = ['one', 'two']
    type(db_t) :: db
    character(len=:), allocatable :: admin, reply
    integer :: lfd, port, stat, sup, i, k
    logical :: exists

    sup = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    do k = 1, size(names)
        call store_repo_add(db, names(k), '', stat, mode='primary')
    end do
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
    do k = 1, size(names)
        do i = 1, 150
            inquire (file=data//'/repos/'//trim(names(k))//'.git/HEAD', exist=exists)
            if (exists) exit
            call proc_sleep_ms(100)
        end do
    end do

    ! A main with a full suite in each repository, written straight into it.
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "#!/bin/sh\ntrue\n" > '//work//'/.fortyard/verify && chmod +x '//work// &
        '/.fortyard/verify && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && git --git-dir '//data//'/repos/one.git fetch -q '//work// &
        ' main:main && git --git-dir '//data//'/repos/two.git fetch -q '//work//' main:main > '//data// &
        '/setup.log 2>&1', exitstat=stat)
    if (stat /= 0) call execute_command_line('cat '//data//'/setup.log')
    call check(stat == 0, 'main is in both repositories')
    do k = 1, size(names)
        reply = post('/api/v1/repos/'//trim(names(k))//'/verify')
        call check(index(reply, 'HTTP/1.1 202') == 1, 'a verification is queued: '//reply)
    end do

    do i = 1, 100
        if (queued_verify_checks() == 2) exit
        call proc_sleep_ms(100)
    end do
    call check(queued_verify_checks() == 2, 'both repositories wait for a runner at once')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)

contains

    integer function queued_verify_checks() result(n)
        !! Repositories with a verification check waiting for a runner.
        type(db_t) :: d
        type(stmt_t) :: st
        integer :: s

        n = 0
        call store_open(data//'/fortyard.db', d, s)
        if (s /= 0) return
        call db_prepare(d, "SELECT count(DISTINCT repo) FROM checks WHERE kind = 'verify'"// &
            " AND state = 'queued';", st, s)
        if (s == 0) then
            if (stmt_step(st) == SQLITE_ROW) n = int(stmt_int(st, 0))
            call stmt_finalize(st)
        end if
        call db_close(d)
    end function queued_verify_checks

    function post(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Length: 0'//CRLF//CRLF)
    end function post

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

end program test_verify_parallel
