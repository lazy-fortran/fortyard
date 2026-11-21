program test_verify_resume
    !! A server that restarts (a new release) while main is being verified
    !! follows the verification's runner check to its end instead of dropping
    !! it: a running verification whose check passed is recorded as passed, one
    !! without a check is cancelled. The runners are told of the checks of
    !! the verifications it follows, as the stopped verifier may have queued
    !! one without telling them.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_events, only: events_wait, events_watch_close, events_watch_open
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_resume_data'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, reply
    integer :: lfd, port, stat, sup, i, watch

    sup = 0
    call execute_command_line('rm -rf '//data//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    ! As a restart leaves them: two running verifications, the first with a
    ! runner check that has since passed.
    call db_exec(db, "INSERT INTO landings (id, repo, branch, account, created_at, kind,"// &
        " commit_id, state, started_at) VALUES (1, 'demo', 'main', 'fortyard', '2026-01-01T00:00:00Z',"// &
        " 'verify', 'abc', 'running', '2026-01-01T00:00:00Z'), (2, 'demo', 'main', 'fortyard',"// &
        " '2026-01-01T00:00:00Z', 'verify', 'def', 'running', '2026-01-01T00:00:00Z'),"// &
        " (3, 'demo', 'main', 'fortyard', '2026-01-01T00:00:00Z', 'verify', 'ghi',"// &
        " 'running', '2026-01-01T00:00:00Z');"// &
        "INSERT INTO checks (repo, ref, state, log, created_at, kind) VALUES ('demo',"// &
        " 'refs/fortyard/verify/1', 'passed', 'full suite passed', '2026-01-01T00:00:00Z',"// &
        " 'verify'), ('demo', 'refs/fortyard/verify/3', 'queued', NULL,"// &
        " '2026-01-01T00:00:00Z', 'verify');", stat)
    call check(stat == 0, 'the state of a restart is prepared')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call execute_command_line('mkdir -p '//data//'/events', exitstat=stat)
    watch = events_watch_open()
    call check(watch >= 0, 'the events are watched')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 300
        reply = get('/api/v1/jobs/2')
        if (index(reply, '"state":"cancelled"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"cancelled"') > 0, 'a verification without a check is cancelled: '// &
        reply)
    reply = get('/api/v1/jobs/1')
    call check(index(reply, '"state":"passed"') > 0 .and. index(reply, 'full suite passed') > 0, &
        'a verification whose check passed is recorded as passed: '//reply)
    call check(events_wait(watch, 'checks', 60000), 'the runners are told of the queued check'// &
        ' of a verification that goes on')
    call events_watch_close(watch)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'verify resume: all checks passed'

contains

    function get(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, n, st
        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, 'GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            admin//CRLF//CRLF, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            r = r//buf(1:n)
        end do
        call sock_close(cfd)
    end function get

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_verify_resume
