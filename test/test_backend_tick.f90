program test_backend_tick
    !! A backend process whose periodic sweep takes long still claims checks:
    !! the sweep runs in a child of its own. Here the sweep never ends, and the
    !! queued check of the backend's class is still taken.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_backend, only: backend_main
    use fortweb_posix, only: sock_bound_port, sock_close, tcp_listen
    use fy_proc, only: SIGTERM, proc_exit, proc_fork, proc_kill, proc_sleep_ms, proc_wait
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create, &
        store_check_add
    implicit none

    character(len=*), parameter :: data = 'build/test_backend_tick_data'
    character(len=*), parameter :: marker = 'build/test_backend_tick_data/taken'
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner
    character(len=16) :: num
    integer(int64) :: id
    integer :: lfd, stat, sup, back, i
    logical :: exists

    sup = 0
    back = 0
    call execute_command_line('rm -rf '//data//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'github-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_exec(db, "UPDATE repos SET public = 1, policy_backends = 'github';", stat)
    call store_check_add(db, 'demo', 'refs/heads/topic', id, stat, class='github-linux')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    back = proc_fork()
    if (back == 0) then
        call backend_main('http://127.0.0.1:'//trim(num), runner, ['github-linux'], [1], 1, 60, &
            take, endless_sweep, 200, 600)
        call proc_exit(0)
    end if
    do i = 1, 300
        inquire (file=marker, exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call check(exists, 'the check is taken while the sweep still runs')

    call stop_all()
    call execute_command_line('rm -rf '//data)
    print '(a)', 'backend tick: all checks passed'

contains

    subroutine take(server, token, class, id, repo, ref, kind, timeout)
        character(len=*), intent(in) :: server, token, class, id, repo, ref, kind
        integer, intent(in) :: timeout
        integer :: st
        call execute_command_line('touch '//marker, exitstat=st)
    end subroutine take

    subroutine endless_sweep()
        do
            call proc_sleep_ms(1000)
        end do
    end subroutine endless_sweep

    subroutine stop_all()
        integer :: st
        if (back > 0) then
            call proc_kill(back, SIGTERM)
            call proc_wait(back, st)
        end if
        if (sup > 0) then
            call proc_kill(sup, SIGTERM)
            call proc_wait(sup, st)
        end if
    end subroutine stop_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            call stop_all()
            error stop 1
        end if
    end subroutine check

end program test_backend_tick
