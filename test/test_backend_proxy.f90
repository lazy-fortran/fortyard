program test_backend_proxy
    !! Behind its egress proxy a backend process has no route and no DNS of its
    !! own, so its requests to the server go to the proxy named in FY_PROXY,
    !! as a runner's do: a fake proxy sees them for a server whose name does
    !! not resolve.
    use fy_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, tcp_accept, &
        tcp_listen
    use fy_proc, only: SIGKILL, proc_exit, proc_fork, proc_kill, proc_sleep_ms, proc_wait
    implicit none

    character(len=*), parameter :: dir = 'build/test_backend_proxy'
    character(len=*), parameter :: seen = dir//'/seen'
    character(len=:), allocatable :: fy, text
    character(len=16) :: num
    integer :: lfd, port, stat, pid, i

    pid = 0
    call execute_command_line('rm -rf '//dir//' && mkdir -p '//dir//' && printf "fy_x\n" > '// &
        dir//'/token && printf "gh_x\n" > '//dir//'/github.token', exitstat=stat)
    call check(stat == 0, 'the files are written')
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//dir//'/fy', exitstat=stat)
    fy = read_all(dir//'/fy')
    fy = fy(1:len_trim(fy) - 1)
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

    write (num, '(i0)') port
    call execute_command_line('FY_PROXY=http://127.0.0.1:'//trim(num)//' timeout 8 '//fy// &
        ' backend github --owner acme --github-token-file '//dir//'/github.token --classes'// &
        ' github-linux --server http://yard.invalid --token-file '//dir//'/token >/dev/null 2>&1', &
        exitstat=stat)
    text = ''
    do i = 1, 50
        text = read_all(seen)
        if (index(text, 'POST http://yard.invalid/api/v1/checks/') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(text, 'POST http://yard.invalid/api/v1/checks/') > 0, &
        'the backend reaches its server through FY_PROXY: '//text)
    call proc_kill(pid, SIGKILL)
    call proc_wait(pid, stat)
    call execute_command_line('rm -rf '//dir)
    write (*, '(a)') 'test_backend_proxy: ok'

contains

    subroutine fake_proxy(lfd)
        !! Records the request line of every request and answers 204.
        integer, intent(in) :: lfd
        character(len=8192) :: chunk
        character(len=:), allocatable :: req
        integer :: cfd, n, st, u

        do
            cfd = tcp_accept(lfd)
            if (cfd < 0) cycle
            req = ''
            do while (index(req, achar(13)//achar(10)) == 0)
                n = sock_recv(cfd, chunk)
                if (n <= 0) exit
                req = req//chunk(1:n)
            end do
            if (index(req, achar(13)//achar(10)) > 0) then
                open (newunit=u, file=seen, position='append', action='write')
                write (u, '(a)') req(1:index(req, achar(13)//achar(10)) - 1)
                close (u)
            end if
            call sock_send_all(cfd, 'HTTP/1.1 204 No Content'//achar(13)//achar(10)// &
                'Connection: close'//achar(13)//achar(10)//achar(13)//achar(10), st)
            call sock_close(cfd)
        end do
    end subroutine fake_proxy

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        character(len=4096) :: line
        integer :: u, ios
        text = ''
        open (newunit=u, file=path, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text//trim(line)//new_line('a')
        end do
        close (u)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (pid > 0) call proc_kill(pid, SIGKILL)
            error stop 1
        end if
    end subroutine check

end program test_backend_proxy
