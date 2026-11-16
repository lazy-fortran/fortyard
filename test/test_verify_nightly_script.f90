program test_verify_nightly_script
    !! A repository verified only at night whose main has neither
    !! .fortyard/verify nor fpm.toml but a .fortyard/nightly script (a CMake
    !! project) gets its nightly verification: the nightly run takes that
    !! script first.
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
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_close, &
        db_prepare, &
        db_t, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_nightly_script_data', &
        work = 'build/test_verify_nightly_script_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=*), parameter :: NIGHTLY = "SELECT count(*) FROM landings WHERE kind = 'verify'"// &
        " AND verify_class = 'linux' AND state <> 'cancelled'"
    type(db_t) :: db
    character(len=:), allocatable :: admin, reply
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, i
    logical :: exists

    sup = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data//'/tmp', &
        exitstat=stat)
    ! The server's landing work trees are named by landing number, which other
    ! tests' servers share.
    call getcwd(cwd)
    call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp')
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    ! Inside the night window.
    call proc_setenv('FY_VERIFY_NOW', '2026-11-13T01:30')
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
    reply = send('/api/v1/repos/demo/policy', '{"verify_on":"linux@nightly"}', 'PUT')
    call check(index(reply, '"verify_on":"linux@nightly"') > 0, &
        'the repository is verified at night only: '//reply)
    ! Main of a CMake project with only a nightly script, written past any hook.
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work// &
        '/.fortyard && printf "#!/bin/sh\nexit 0\n" > '//work//'/.fortyard/nightly && '// &
        'chmod 755 '//work//'/.fortyard/nightly && printf "project(x)\n" > '//work// &
        '/CMakeLists.txt && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && git --git-dir='//data//'/repos/demo.git fetch -q '// &
        trim(cwd)//'/'//work//' main:refs/heads/main', exitstat=stat)
    call check(stat == 0, 'main with only a nightly script exists')
    ! The verifier looked before main existed and sleeps until a job changes:
    ! a request (cancelled, as nothing runs by day) wakes it.
    reply = send('/api/v1/repos/demo/verify', '{}', 'POST')

    do i = 1, 300
        if (count(NIGHTLY) >= 1) exit
        call proc_sleep_ms(100)
    end do
    call check(count(NIGHTLY) == 1, 'main is verified on the nightly class')
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'verify nightly script: all checks passed'

contains

    integer function count(sql) result(n)
        !! The number `sql` counts in the server's database.
        character(len=*), intent(in) :: sql
        type(db_t) :: d
        type(stmt_t) :: st
        integer :: s
        n = -1
        call store_open(data//'/fortyard.db', d, s)
        if (s /= 0) return
        call db_prepare(d, sql//';', st, s)
        if (s == SQLITE_OK) then
            if (stmt_step(st) == SQLITE_ROW) n = int(stmt_int(st, 0))
            call stmt_finalize(st)
        end if
        call db_close(d)
    end function count

    function send(path, body, method) result(r)
        character(len=*), intent(in) :: path, body, method
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange(method//' '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)//CRLF//CRLF//body)
    end function send

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

end program test_verify_nightly_script
