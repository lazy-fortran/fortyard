program test_runner_reach
    !! A runner reaches its server by name and through its egress proxy: with
    !! --server http://localhost:PORT it connects (a container name such as
    !! fortyard is resolved the same way), and with FY_PROXY set it sends an
    !! https:// server's requests to that proxy, so a runner on an internal
    !! network without DNS gets through.
    use fortweb_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, &
        sock_set_timeouts, tcp_accept, tcp_listen
    use fy_proc, only: SIGKILL, proc_kill, proc_setenv, proc_start, proc_wait
    implicit none

    character(len=*), parameter :: work = 'build/test_runner_reach_work'
    character(len=:), allocatable :: fy, req
    character(len=16) :: num
    character(len=4096) :: line
    integer :: lfd, port, stat, pid, u

    pid = 0
    call execute_command_line('rm -rf '//work//' && mkdir -p '//work//' && '// &
        'echo token > '//work//'/token && '// &
        'ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1 > '//work//'/fy', &
        exitstat=stat)
    call check(stat == 0, 'work directory written')
    open (newunit=u, file=work//'/fy', action='read')
    read (u, '(a)') line
    close (u)
    fy = trim(line)
    call check(len(fy) > 0, 'fy is built')

    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'the fake server listens')
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    ! A missing connection fails the test instead of hanging it.
    call sock_set_timeouts(lfd, 300)

    call proc_setenv('FY_PROXY', '')
    req = first_request('http://localhost:'//trim(num))
    call check(index(req, 'POST /api/v1/checks/') == 1, &
        'a runner given a server name connects to it: '//req(1:min(len(req), 80)))

    call proc_setenv('FY_PROXY', 'http://127.0.0.1:'//trim(num))
    req = first_request('https://yard.invalid/yard')
    call check(index(req, 'CONNECT yard.invalid:443 ') == 1, &
        'a runner with FY_PROXY reaches an https server through the proxy: '// &
        req(1:min(len(req), 80)))

    call sock_close(lfd)
    call execute_command_line('rm -rf '//work)
    write (*, '(a)') 'test_runner_reach: ok'

contains

    function first_request(server) result(head)
        !! Starts fy runner against server and returns the head of the first
        !! request that arrives at the listener; empty if none comes.
        character(len=*), intent(in) :: server
        character(len=:), allocatable :: head
        character(len=8192) :: chunk
        character(len=256) :: argv(8)
        integer :: cfd, n, st

        argv = [character(len=256) :: '', 'runner', '--server', '', '--token-file', &
            work//'/token', '--data', work//'/data']
        argv(1) = fy
        argv(4) = server
        call proc_start(argv, work//'/runner.log', pid)
        call check(pid > 0, 'fy runner starts')
        head = ''
        cfd = tcp_accept(lfd)
        if (cfd >= 0) then
            call sock_set_timeouts(cfd, 60)
            do while (index(head, achar(13)//achar(10)//achar(13)//achar(10)) == 0)
                n = sock_recv(cfd, chunk)
                if (n <= 0) exit
                head = head//chunk(1:n)
            end do
            call sock_send_all(cfd, 'HTTP/1.1 503 Service Unavailable'//achar(13)//achar(10)// &
                'Content-Length: 0'//achar(13)//achar(10)// &
                'Connection: close'//achar(13)//achar(10)//achar(13)//achar(10), st)
            call sock_close(cfd)
        end if
        call proc_kill(pid, SIGKILL)
        call proc_wait(pid, st)
        pid = 0
    end function first_request

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (pid > 0) call proc_kill(pid, SIGKILL)
            error stop 1
        end if
    end subroutine check

end program test_runner_reach
