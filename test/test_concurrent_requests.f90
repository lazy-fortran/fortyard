program test_concurrent_requests
    !! One worker serving eight connections at once, under eight clients that
    !! send heads with many headers of different lengths and read their
    !! account back: every request is answered with the same intact text, and
    !! the worker never dies (#91: concurrent heads corrupted its heap).
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
        proc_exit_status, &
        proc_fork, &
        proc_kill, &
        proc_sleep_ms, &
        proc_wait
    use fy_server, only: server_supervise
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_bootstrap, store_open
    implicit none

    character(len=*), parameter :: path = 'build/test_concurrent_requests.db'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    integer, parameter :: CLIENTS = 8, REQUESTS = 60
    type(db_t) :: db
    character(len=:), allocatable :: token, expected
    integer :: lfd, port, stat, sup, i, bad, pids(CLIENTS)
    integer, allocatable :: before(:), after(:)

    sup = 0
    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call store_bootstrap(db, token, stat)
    call check(stat == 0, 'administrator token')
    call db_close(db)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'listens')
    port = sock_bound_port(lfd)

    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, path, 1, CLIENTS)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    do i = 1, 100
        expected = body_of(exchange(request(0)))
        if (index(expected, '"account":"admin"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(expected, '"account":"admin"') > 0, 'whoami answers: '//expected)
    before = workers()
    call check(size(before) == 1, 'one worker process')

    do i = 1, CLIENTS
        pids(i) = proc_fork()
        if (pids(i) == 0) call client(i)
    end do
    bad = 0
    do i = 1, CLIENTS
        bad = bad + proc_exit_status(pids(i))
    end do
    call check(bad == 0, 'every request answered with the same account')
    after = workers()
    call check(size(after) == 1, 'still one worker')
    call check(all(after == before), 'the worker never died')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'concurrent requests: all checks passed'

contains

    subroutine client(k)
        !! Exits with the number of wrong or missing answers, at most 255.
        integer, intent(in) :: k
        integer :: n, wrong

        wrong = 0
        do n = 1, REQUESTS
            if (body_of(exchange(request(k * REQUESTS + n))) /= expected) wrong = wrong + 1
        end do
        call proc_exit(min(wrong, 255))
    end subroutine client

    function request(seed) result(text)
        !! whoami with 40 cookie headers whose lengths vary with `seed`.
        integer, intent(in) :: seed
        character(len=:), allocatable :: text
        integer :: j

        text = 'GET /api/v1/whoami HTTP/1.1'//CRLF//'Authorization: Bearer '//token//CRLF
        do j = 1, 40
            text = text//'Cookie:  '//repeat('c', mod(seed * 7 + j * 13, 300) + 1)//' '//CRLF
        end do
        text = text//CRLF
    end function request

    function body_of(response) result(body)
        character(len=*), intent(in) :: response
        character(len=:), allocatable :: body
        integer :: k

        body = ''
        if (index(response, 'HTTP/1.1 200') /= 1) return
        k = index(response, CRLF//CRLF)
        if (k > 0) body = response(k + 4:)
    end function body_of

    function exchange(text) result(response)
        !! Empty when the server does not answer within 20 s.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: response
        character(len=4096) :: buf
        integer :: cfd, n, st

        response = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 20)
        call sock_send_all(cfd, text, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            response = response//buf(1:n)
        end do
        call sock_close(cfd)
    end function exchange

    function workers() result(found)
        integer, allocatable :: found(:)
        character(len=256) :: line
        character(len=16) :: s
        integer :: u, ios, k, p, pos

        write (s, '(i0)') sup
        allocate (found(0))
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
            found = [found, p]
            pos = k
        end do
    end function workers

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_concurrent_requests
