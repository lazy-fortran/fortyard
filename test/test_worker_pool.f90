program test_worker_pool
    !! The server in a forked supervisor with 2 worker processes of 2 threads:
    !! idle connections must not block other clients, and killed workers must
    !! be replaced without the service going away.
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: &
        SIGKILL, &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_sleep_ms, &
        proc_wait
    use fy_server, only: server_supervise
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open
    implicit none

    character(len=*), parameter :: path = 'build/test_worker_pool.db'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    integer :: lfd, port, stat, sup, i, idle(3)
    integer, allocatable :: before(:), after(:)

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call db_close(db)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'listens')
    port = sock_bound_port(lfd)

    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, path, 2, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    call check(healthy_within(5000), 'server answers after start')
    before = workers()
    call check(size(before) == 2, 'two worker processes')

    ! Three of four threads wait on clients that send nothing.
    do i = 1, 3
        call tcp_connect('127.0.0.1', port, idle(i), stat)
        call check(stat == 0, 'idle client connects')
    end do
    call proc_sleep_ms(200)
    call check(health() , 'served while three connections are idle')
    do i = 1, 3
        call sock_close(idle(i))
    end do

    ! Past the crash-loop window, so replacements start at once.
    call proc_sleep_ms(1100)
    call proc_kill(before(1), SIGKILL)
    call check(reaped_within(before(1), 2000), 'killed worker reaped')
    call check(health(), 'served while one worker is dead')
    call proc_kill(before(2), SIGKILL)
    call check(healthy_within(3000), 'served again after all workers were killed')
    do i = 1, 150
        after = workers()
        if (size(after) == 2) then
            if (all(after /= before(1)) .and. all(after /= before(2))) exit
        end if
        call proc_sleep_ms(20)
    end do
    call check(i <= 150, 'both killed workers replaced by new ones')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    do i = 1, size(after)
        call check(gone_within(after(i), 2000), 'workers exit with the supervisor')
    end do

contains

    logical function health()
        character(len=:), allocatable :: reply
        reply = exchange('GET /healthz HTTP/1.1'//CRLF//CRLF)
        health = index(reply, 'HTTP/1.1 200') == 1
    end function health

    logical function healthy_within(ms)
        integer, intent(in) :: ms
        integer :: waited
        waited = 0
        do
            healthy_within = health()
            if (healthy_within .or. waited >= ms) return
            call proc_sleep_ms(100)
            waited = waited + 100
        end do
    end function healthy_within

    function exchange(request) result(response)
        !! Empty when the server does not answer within 3 s.
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: response
        character(len=4096) :: buf
        integer :: cfd, n, st

        response = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 3)
        call sock_send_all(cfd, request, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            response = response//buf(1:n)
        end do
        call sock_close(cfd)
    end function exchange

    function workers() result(pids)
        integer, allocatable :: pids(:)
        character(len=256) :: line
        character(len=16) :: s
        integer :: u, ios, k, p, pos

        write (s, '(i0)') sup
        allocate (pids(0))
        open (newunit=u, file='/proc/'//trim(s)//'/task/'//trim(s)//'/children', &
            action='read', iostat=ios)
        if (ios /= 0) return
        line = ''
        read (u, '(a)', iostat=ios) line
        close (u)
        pos = 1
        do
            line = adjustl(line(pos:))
            if (len_trim(line) == 0) exit
            k = index(line, ' ')
            read (line(1:k - 1), *) p
            pids = [pids, p]
            pos = k
        end do
    end function workers

    logical function reaped_within(pid, ms)
        !! The supervisor reaps a worker only after all its threads have ended;
        !! until then a dying thread can still take a connection off the queue.
        integer, intent(in) :: pid, ms
        character(len=16) :: s
        integer :: waited
        write (s, '(i0)') pid
        waited = 0
        do
            inquire (file='/proc/'//trim(s)//'/stat', exist=reaped_within)
            reaped_within = .not. reaped_within
            if (reaped_within .or. waited >= ms) return
            call proc_sleep_ms(20)
            waited = waited + 20
        end do
    end function reaped_within

    logical function gone_within(pid, ms)
        integer, intent(in) :: pid, ms
        integer :: waited
        waited = 0
        do
            gone_within = .not. alive(pid)
            if (gone_within .or. waited >= ms) return
            call proc_sleep_ms(20)
            waited = waited + 20
        end do
    end function gone_within

    logical function alive(pid)
        !! Zombies count as ended: their new parent reaps them in its own time.
        integer, intent(in) :: pid
        character(len=256) :: line
        character(len=16) :: s
        integer :: u, ios, k
        write (s, '(i0)') pid
        alive = .false.
        open (newunit=u, file='/proc/'//trim(s)//'/stat', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
        k = index(line, ')', back=.true.)
        alive = ios == 0 .and. k > 0 .and. line(k + 2:k + 2) /= 'Z'
    end function alive

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_worker_pool
