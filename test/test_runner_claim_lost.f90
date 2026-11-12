program test_runner_claim_lost
    !! A claim the server committed but never answered, because it stopped in
    !! between, is handed back: after a claim without an answer the runner
    !! asks the server to release its checks before it claims again, so the
    !! check runs instead of waiting as running until it is taken as lost.
    !! Both the runner beside the server (a data directory with repos/) and
    !! the remote runner do so.
    use fy_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, &
        sock_set_timeouts, tcp_accept, tcp_listen
    use fy_proc, only: SIGKILL, proc_kill, proc_start, proc_wait
    implicit none

    character(len=*), parameter :: work = 'build/test_runner_claim_lost_work'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=:), allocatable :: fy, seen
    character(len=16) :: num
    character(len=4096) :: line
    integer :: lfd, port, stat, pid, u

    pid = 0
    call execute_command_line('rm -rf '//work//' && mkdir -p '//work//'/local/repos '// &
        work//'/local/events '//work//'/remote && echo token > '//work//'/token && '// &
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
    call sock_set_timeouts(lfd, 120)

    seen = claims(work//'/local')
    call check(index(seen, 'claim release claim') > 0, &
        'the runner beside the server releases its checks after a lost claim: '//seen)
    seen = claims(work//'/remote')
    call check(index(seen, 'claim release claim') > 0, &
        'the remote runner releases its checks after a lost claim: '//seen)

    call sock_close(lfd)
    call execute_command_line('rm -rf '//work)
    write (*, '(a)') 'test_runner_claim_lost: ok'

contains

    function claims(data) result(seen)
        !! Starts fy runner with `data` and serves it until its second claim:
        !! the first claim is left without an answer. The claims and releases
        !! it sent, in order.
        character(len=*), intent(in) :: data
        character(len=:), allocatable :: seen, head
        character(len=8192) :: chunk
        character(len=256) :: argv(8)
        integer :: cfd, n, st, k, asked

        argv = [character(len=256) :: '', 'runner', '--server', '', '--token-file', &
            work//'/token', '--data', data]
        argv(1) = fy
        argv(4) = 'http://127.0.0.1:'//trim(num)
        call proc_start(argv, data//'.log', pid)
        call check(pid > 0, 'fy runner starts')
        seen = ''
        asked = 0
        do k = 1, 20
            cfd = tcp_accept(lfd)
            if (cfd < 0) exit
            call sock_set_timeouts(cfd, 60)
            head = ''
            do while (index(head, CRLF//CRLF) == 0)
                n = sock_recv(cfd, chunk)
                if (n <= 0) exit
                head = head//chunk(1:n)
            end do
            if (index(head, 'POST /api/v1/checks/claim ') == 1) then
                seen = seen//' claim'
                asked = asked + 1
                ! The first claim: the server stops before it answers.
                if (asked > 1) call sock_send_all(cfd, 'HTTP/1.1 204 No Content'//CRLF// &
                    'Connection: close'//CRLF//CRLF, st)
            else if (index(head, 'POST /api/v1/checks/release ') == 1) then
                seen = seen//' release'
                call sock_send_all(cfd, 'HTTP/1.1 200 OK'//CRLF//'Content-Length: 14'//CRLF// &
                    'Connection: close'//CRLF//CRLF//'{"released":0}', st)
            else
                call sock_send_all(cfd, 'HTTP/1.1 503 Service Unavailable'//CRLF// &
                    'Content-Length: 0'//CRLF//'Connection: close'//CRLF//CRLF, st)
            end if
            call sock_close(cfd)
            if (asked > 1) exit
        end do
        call proc_kill(pid, SIGKILL)
        call proc_wait(pid, st)
        pid = 0
    end function claims

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (pid > 0) call proc_kill(pid, SIGKILL)
            error stop 1
        end if
    end subroutine check

end program test_runner_claim_lost
