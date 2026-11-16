program test_verify_cancel
    !! A verification of main can be cancelled by an administrator (DELETE
    !! /api/v1/repos/NAME/landings/ID, as fy land --cancel sends): the job
    !! ends cancelled and its check is withdrawn, so no runner or backend
    !! takes it again. A token with only the land scope cannot cancel it.
    use, intrinsic :: iso_fortran_env, only: int64
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
        db_exec, &
        db_prepare, &
        db_t, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_cancel_data'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, lander, reply
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, i
    logical :: exists

    sup = 0
    call execute_command_line('rm -rf '//data//' && mkdir -p '//data//'/tmp', exitstat=stat)
    call getcwd(cwd)
    call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp')
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'lander', 'land', lander, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    ! A nightly verification, started now, whose check no runner of its class runs.
    call db_exec(db, "INSERT INTO landings (id, repo, branch, account, created_at, kind,"// &
        " commit_id, state, started_at, verify_class) VALUES (1, 'demo', 'main', 'fortyard',"// &
        " strftime('%Y-%m-%dT%H:%M:%SZ', 'now'), 'verify', 'abc', 'running',"// &
        " strftime('%Y-%m-%dT%H:%M:%SZ', 'now'),"// &
        " 'github-windows');", stat)
    call check(stat == 0, 'a verification runs')
    call db_exec(db, "INSERT INTO checks (id, repo, ref, state, created_at, kind, class)"// &
        " VALUES (1, 'demo', 'refs/fortyard/verify/1', 'queued', strftime('%Y-%m-%dT%H:%M:%SZ',"// &
        " 'now'),"// &
        " 'nightly', 'github-windows');", stat)
    call check(stat == 0, 'its check waits')
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

    reply = delete('/api/v1/repos/demo/landings/1', lander)
    call check(index(reply, ' 404 ') > 0 .and. &
        count("SELECT count(*) FROM landings WHERE id = 1 AND state = 'running'") == 1, &
        'the land scope cannot cancel a verification: '//reply)
    reply = delete('/api/v1/repos/demo/landings/1', admin)
    call check(index(reply, ' 200 ') > 0, 'an administrator cancels it: '//reply)
    call check(count("SELECT count(*) FROM landings WHERE id = 1 AND state = 'cancelled'"// &
        " AND log LIKE '%Cancelled by%'") == 1, 'the verification ends cancelled')
    call check(count("SELECT count(*) FROM checks WHERE id = 1 AND state = 'cancelled'") == 1, &
        'its check is withdrawn')
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'verify cancel: all checks passed'

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

    function delete(path, token) result(r)
        character(len=*), intent(in) :: path, token
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, n, st
        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, 'DELETE '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//CRLF, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            r = r//buf(1:n)
        end do
        call sock_close(cfd)
    end function delete

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_verify_cancel
