program test_oidc_proxy
    !! Behind the egress proxy the server has no route and no DNS of its own,
    !! so the sign-in's token request goes to the proxy named in FY_PROXY: a
    !! fake proxy answers for a provider whose name does not resolve.
    use fy_oidc, only: oidc_config_t, oidc_exchange
    use fy_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, tcp_accept, &
        tcp_listen
    use fy_proc, only: SIGKILL, proc_exit, proc_fork, proc_kill, proc_setenv, proc_wait
    implicit none

    character(len=*), parameter :: issuer = 'http://idp.invalid/realms/x'
    type(oidc_config_t) :: cfg
    character(len=:), allocatable :: id_token, error
    character(len=16) :: num
    integer :: lfd, port, stat, pid

    pid = 0
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'the fake proxy listens')
    port = sock_bound_port(lfd)
    flush (6)
    pid = proc_fork()
    if (pid == 0) then
        call fake_proxy(lfd)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    cfg%enabled = .true.
    cfg%issuer = issuer
    cfg%client = 'yard'
    cfg%secret = 'secret'
    cfg%url = 'http://yard.example/yard'
    cfg%admins = ''
    write (num, '(i0)') port
    call proc_setenv('FY_PROXY', 'http://127.0.0.1:'//trim(num))
    call oidc_exchange(cfg, 'code1', 'verifier1', id_token, error)
    call check(len(error) == 0, 'the token request is answered through the proxy: '//error)
    call check(id_token == 'through-proxy', 'the ID token comes from the proxied answer')
    call proc_kill(pid, SIGKILL)
    call proc_wait(pid, stat)
    write (*, '(a)') 'test_oidc_proxy: ok'

contains

    subroutine fake_proxy(lfd)
        !! One request: a proxy request (absolute URL) for the token endpoint
        !! gets an ID token, anything else an error.
        integer, intent(in) :: lfd
        character(len=8192) :: chunk
        character(len=:), allocatable :: req, body
        integer :: cfd, n, st

        cfd = tcp_accept(lfd)
        req = ''
        do while (index(req, achar(13)//achar(10)//achar(13)//achar(10)) == 0)
            n = sock_recv(cfd, chunk)
            if (n <= 0) exit
            req = req//chunk(1:n)
        end do
        if (index(req, 'POST '//issuer//'/protocol/openid-connect/token HTTP/') == 1) then
            body = '{"id_token":"through-proxy"}'
        else
            body = '{"error":"not a proxy request"}'
        end if
        write (num, '(i0)') len(body)
        call sock_send_all(cfd, 'HTTP/1.1 200 OK'//achar(13)//achar(10)// &
            'Content-Type: application/json'//achar(13)//achar(10)// &
            'Content-Length: '//trim(num)//achar(13)//achar(10)// &
            'Connection: close'//achar(13)//achar(10)//achar(13)//achar(10)//body, st)
        call sock_close(cfd)
    end subroutine fake_proxy

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (pid > 0) call proc_kill(pid, SIGKILL)
            error stop 1
        end if
    end subroutine check

end program test_oidc_proxy
