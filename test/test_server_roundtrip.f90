program test_server_roundtrip
    !! Real sockets in one thread: the client's request waits in the kernel
    !! until the server accepts and answers it.
    use fy_posix, only: tcp_listen, tcp_accept, tcp_connect, sock_send_all, &
        sock_recv, sock_close, sock_bound_port
    use fy_server, only: serve_connection
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap
    implicit none

    character(len=*), parameter :: path = 'build/test_server_roundtrip.db'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: token, reply
    integer :: lfd, port, stat

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call store_bootstrap(db, token, stat)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'listens on an ephemeral port')
    port = sock_bound_port(lfd)
    call check(port > 0, 'bound port known')

    reply = exchange('GET /healthz HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. index(reply, 'ok ') > 0, 'health check')

    reply = exchange('GET /api/v1/whoami HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 401') == 1, 'whoami without token is 401')

    reply = exchange('GET /api/v1/whoami HTTP/1.1'//CRLF//'Authorization: Bearer '// &
        token//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. index(reply, '"account":"admin"') > 0, &
        'whoami with token')

    reply = exchange('POST /api/v1/tokens HTTP/1.1'//CRLF//'Authorization: Bearer '// &
        token//CRLF//'Content-Length: 15'//CRLF//CRLF//'{"name":"ci-1"}')
    call check(index(reply, 'HTTP/1.1 201') == 1 .and. index(reply, '"token":"fy_') > 0, &
        'token creation')

    reply = exchange('DELETE /healthz HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 405') == 1, 'other methods rejected')

    reply = exchange('GET /nowhere HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1, 'unknown path is 404')

    reply = exchange('garbage'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 400') == 1, 'malformed request is 400')

    call sock_close(lfd)
    call db_close(db)

contains

    function exchange(request) result(response)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: response
        character(len=4096) :: buf
        integer :: cfd, sfd, n, st

        call tcp_connect('127.0.0.1', port, cfd, st)
        call check(st == 0, 'client connects')
        call sock_send_all(cfd, request, st)
        sfd = tcp_accept(lfd)
        call serve_connection(sfd, db)
        call sock_close(sfd)
        response = ''
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            response = response//buf(1:n)
        end do
        call sock_close(cfd)
    end function exchange

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_server_roundtrip
