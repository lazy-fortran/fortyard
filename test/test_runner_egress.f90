program test_runner_egress
    !! A runner controller with FY_EGRESS_EXTRA_FILE opens the extra hosts of
    !! the check it runs on its executor's proxy. While no check of its runs
    !! the file holds only "private", also when a stopped runner left hosts in
    !! it; the repository's hosts, one per line, and "private" for a private
    !! repository come in only once the executor has taken the check, and
    !! stay until the result. A controller that cannot write the file hands
    !! the check back without giving it to the executor, and a runner without
    !! an executor, in which the check runs itself, leaves the file alone.
    !! The server is a fake that hands out one check, whose repository is
    !! missing, so its checkout fails at once.
    use fy_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, &
        sock_set_timeouts, tcp_accept, tcp_listen
    use fy_proc, only: SIGKILL, proc_kill, proc_setenv, proc_sleep_ms, proc_start, proc_wait
    implicit none

    character(len=*), parameter :: work = 'build/test_runner_egress_work'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=*), parameter :: HOSTS = 'a.example'//new_line('a')//'b.example'// &
        new_line('a')//'private'//new_line('a')
    character(len=*), parameter :: IDLE = 'private'//new_line('a')
    character(len=:), allocatable :: fy, extra, top, url, req, path, during
    ! Fixed length: gfortran overflows typed array constructors around
    ! deferred-length strings.
    character(len=4096) :: line, cwd, argv(10)
    character(len=16) :: num
    integer :: lfd, port, stat, ctl, ex, u, k, cfd
    logical :: claimed, reported, cleared, there, idle_before

    ctl = 0
    ex = 0
    call getcwd(cwd)
    top = trim(cwd)//'/'//work
    call execute_command_line('rm -rf '//work//' && mkdir -p '//work//'/extra '//work// &
        '/data/repos '//work//'/data/events && echo token > '//work//'/token && '// &
        'printf "stale.example\n" > '//work//'/extra/executor.txt && '// &
        'ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1 > '//work//'/fy', &
        exitstat=stat)
    call check(stat == 0, 'work directory written')
    open (newunit=u, file=work//'/fy', action='read')
    read (u, '(a)') line
    close (u)
    fy = trim(line)
    call check(len(fy) > 0, 'fy is built')
    extra = work//'/extra/executor.txt'

    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'the fake server listens')
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    url = 'http://127.0.0.1:'//trim(num)
    ! A missing connection fails the test instead of hanging it.
    call sock_set_timeouts(lfd, 300)
    call proc_setenv('FY_PROXY', '')

    ! A split runner: the hosts open once the executor took the check.
    call proc_setenv('FY_EGRESS_EXTRA_FILE', extra)
    call start_controller(top//'/x1', ctl)
    claimed = .false.
    reported = .false.
    cleared = .false.
    idle_before = .false.
    during = ''
    do k = 1, 100
        call next_request()
        if (.not. claimed) then
            call check(read_all(extra) == IDLE, 'a starting runner closes the stale hosts: '// &
                read_all(extra))
        end if
        if (path == 'POST /api/v1/checks/claim' .and. .not. claimed) then
            claimed = .true.
            call answer(cfd, 200, claim('true'))
            call sock_close(cfd)
            call wait_for(top//'/x1/exchange/job', there)
            call check(there, 'the controller hands the check over')
            idle_before = read_all(extra) == IDLE
            argv(1) = fy
            argv(2) = 'executor'
            argv(3) = top//'/x1'
            call proc_start(argv(1:3), work//'/executor.log', ex)
            call check(ex > 0, 'fy executor starts')
            cycle
        else if (path == 'POST /api/v1/checks/7/result') then
            during = read_all(extra)
            reported = .true.
            call answer(cfd, 200, '{}')
        else if (path == 'POST /api/v1/checks/claim') then
            cleared = reported .and. read_all(extra) == IDLE
            call answer(cfd, 204, '')
            call sock_close(cfd)
            exit
        else
            call answer(cfd, 200, '{}')
        end if
        call sock_close(cfd)
    end do
    call halt(ctl)
    call halt(ex)
    call check(claimed .and. reported, 'the runner takes the check and reports it')
    call check(idle_before, 'no hosts open before the executor takes the check')
    call check(during == HOSTS, 'the hosts and "private" are open until the result: '//during)
    call check(cleared, 'only "private" stays after the check: '//read_all(extra))

    ! A file it cannot write: the check goes back to the server.
    call proc_setenv('FY_EGRESS_EXTRA_FILE', work//'/missing/executor.txt')
    call start_controller(top//'/x2', ctl)
    reported = .false.
    claimed = .false.
    do k = 1, 100
        call next_request()
        if (path == 'POST /api/v1/checks/claim') then
            claimed = .true.
            call answer(cfd, 200, claim('true'))
        else if (path == 'POST /api/v1/checks/7/result') then
            reported = index(req, '"error":"runner"') > 0
            call answer(cfd, 200, '{}')
            call sock_close(cfd)
            exit
        else
            call answer(cfd, 200, '{}')
        end if
        call sock_close(cfd)
    end do
    inquire (file=top//'/x2/exchange/job', exist=there)
    call halt(ctl)
    call check(claimed .and. reported, 'a runner that cannot write the file hands the check back')
    call check(.not. there, 'and does not give it to the executor')

    ! Without an executor the check could rewrite the file: nothing is written.
    call execute_command_line('printf "stale.example\n" > '//extra, exitstat=stat)
    call proc_setenv('FY_EGRESS_EXTRA_FILE', extra)
    call runner_argv()
    call proc_start(argv(1:8), work//'/runner.log', ctl)
    call check(ctl > 0, 'fy runner starts')
    reported = .false.
    do k = 1, 100
        call next_request()
        if (path == 'POST /api/v1/checks/claim' .and. .not. reported) then
            call answer(cfd, 200, claim('true'))
        else if (path == 'POST /api/v1/checks/7/result') then
            reported = .true.
            call answer(cfd, 200, '{}')
            call sock_close(cfd)
            exit
        else
            call answer(cfd, 200, '{}')
        end if
        call sock_close(cfd)
    end do
    call halt(ctl)
    call check(reported, 'the runner without an executor runs the check')
    call check(read_all(extra) == 'stale.example'//new_line('a'), &
        'and leaves the file alone: '//read_all(extra))

    call sock_close(lfd)
    call execute_command_line('rm -rf '//work)
    write (*, '(a)') 'test_runner_egress: ok'

contains

    function claim(private) result(body)
        character(len=*), intent(in) :: private
        character(len=:), allocatable :: body
        body = '{"id":7,"repo":"demo","ref":"refs/heads/x","kind":"check","timeout":60,'// &
            '"egress":"a.example,b.example","private":'//private//'}'
    end function claim

    subroutine start_controller(shared, pid)
        character(len=*), intent(in) :: shared
        integer, intent(out) :: pid
        call runner_argv()
        argv(9) = '--executor'
        argv(10) = shared
        call proc_start(argv, work//'/runner.log', pid)
        call check(pid > 0, 'fy runner starts')
    end subroutine start_controller

    subroutine runner_argv()
        argv(1) = fy
        argv(2) = 'runner'
        argv(3) = '--server'
        argv(4) = url
        argv(5) = '--token-file'
        argv(6) = work//'/token'
        argv(7) = '--data'
        argv(8) = work//'/data'
    end subroutine runner_argv

    subroutine next_request()
        cfd = tcp_accept(lfd)
        call check(cfd >= 0, 'the runner sends its next request')
        call sock_set_timeouts(cfd, 60)
        req = request(cfd)
        path = req(1:max(0, index(req, ' HTTP/') - 1))
    end subroutine next_request

    subroutine wait_for(file, there)
        !! Until `file` exists, for at most a minute.
        character(len=*), intent(in) :: file
        logical, intent(out) :: there
        integer :: i
        do i = 1, 600
            inquire (file=file, exist=there)
            if (there) return
            call proc_sleep_ms(100)
        end do
    end subroutine wait_for

    subroutine halt(pid)
        integer, intent(inout) :: pid
        integer :: st
        if (pid <= 0) return
        call proc_kill(pid, SIGKILL)
        call proc_wait(pid, st)
        pid = 0
    end subroutine halt

    function request(fd) result(text)
        !! The request on `fd`, head and body.
        integer, intent(in) :: fd
        character(len=:), allocatable :: text
        character(len=8192) :: chunk
        integer :: n, e, s, want, ios

        text = ''
        do while (index(text, CRLF//CRLF) == 0)
            n = sock_recv(fd, chunk)
            if (n <= 0) return
            text = text//chunk(1:n)
        end do
        e = index(text, CRLF//CRLF) + 3
        want = 0
        s = index(text, 'Content-Length: ')
        if (s > 0) then
            n = index(text(s:), achar(13))
            read (text(s + 16:s + n - 2), *, iostat=ios) want
        end if
        do while (len(text) - e < want)
            n = sock_recv(fd, chunk)
            if (n <= 0) exit
            text = text//chunk(1:n)
        end do
    end function request

    subroutine answer(fd, code, body)
        integer, intent(in) :: fd, code
        character(len=*), intent(in) :: body
        character(len=16) :: c, n
        integer :: st
        write (c, '(i0)') code
        write (n, '(i0)') len(body)
        call sock_send_all(fd, 'HTTP/1.1 '//trim(c)//' X'//CRLF//'Content-Type: application/json'// &
            CRLF//'Content-Length: '//trim(n)//CRLF//'Connection: close'//CRLF//CRLF//body, st)
    end subroutine answer

    function read_all(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text
        integer :: v, ios, n
        text = ''
        open (newunit=v, file=file, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) then
            text = '(missing)'
            return
        end if
        inquire (unit=v, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (v, iostat=ios) text
        close (v)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (ctl > 0) call proc_kill(ctl, SIGKILL)
            if (ex > 0) call proc_kill(ex, SIGKILL)
            error stop 1
        end if
    end subroutine check

end program test_runner_egress
