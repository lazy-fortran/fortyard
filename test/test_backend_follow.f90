program test_backend_follow
    !! A backend process follows the server's release as a remote runner does.
    !! Against a fake server it keeps claiming while the reported release is
    !! its own; once the release changes it claims no more, waits for its
    !! running check to be reported, stops its sweep (which never ends here)
    !! and returns, so that remote-runner.sh starts the new release.
    use fy_backend, only: backend_main, backend_report
    use fy_build_info, only: FY_COMMIT
    use fy_http, only: http_request_t, http_read_request, http_send_response
    use fy_posix, only: sock_bound_port, sock_close, sock_set_timeouts, tcp_accept, tcp_listen
    use fy_proc, only: SIGTERM, proc_exit, proc_fork, proc_kill, proc_sleep_ms, proc_try_wait, &
        proc_wait
    implicit none

    character(len=*), parameter :: dir = 'build/test_backend_follow'
    character(len=*), parameter :: api_log = dir//'/api.log', release = dir//'/release', &
        go = dir//'/go'
    character(len=:), allocatable :: text, server
    character(len=16) :: num
    integer :: lfd, stat, fake, back, i, st, claims
    logical :: done

    fake = 0
    back = 0
    call execute_command_line('rm -rf '//dir//' && mkdir -p '//dir, exitstat=stat)
    call write_file(release, FY_COMMIT)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'the fake server listens')
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
    flush (6)
    fake = proc_fork()
    if (fake == 0) then
        call fake_server(lfd)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    back = proc_fork()
    if (back == 0) then
        call backend_main(server, 'fy_x', ['github-linux'], [2], 2, 60, take, endless_sweep, &
            100, 600, version_seconds=0)
        call proc_exit(0)
    end if

    ! Check 7 runs (its take waits for `go`) while the backend claims on.
    call wait_log('POST /api/v1/checks/claim 7', 'check 7 is given')
    claims = 0
    do i = 1, 600
        text = read_file(api_log)
        claims = count_of(text(index(text, 'POST /api/v1/checks/claim 7'):), &
            'POST /api/v1/checks/claim none')
        if (claims >= 3) exit
        call proc_sleep_ms(100)
    end do
    call check(claims >= 3, 'with the release unchanged the backend keeps claiming: '//text)
    call proc_try_wait(back, done, st)
    call check(.not. done, 'the backend runs on while the release is its own')

    ! The server moves on: no claim follows the new release's answer, and the
    ! backend waits for its running check.
    call write_file(release, 'f00dfeed')
    call wait_log('GET /api/v1/version f00dfeed', 'the backend asks for the release')
    call proc_try_wait(back, done, st)
    call check(.not. done, 'the backend does not return while check 7 runs')
    call write_file(go, 'go')
    do i = 1, 600
        call proc_try_wait(back, done, st)
        if (done) exit
        call proc_sleep_ms(100)
    end do
    call check(done, 'the backend returns once its check is reported')
    back = 0
    call check(st == 0, 'the backend returns rather than dies')
    text = read_file(api_log)
    call check(index(text, 'POST /api/v1/checks/7/result {"passed":true') > 0, &
        'check 7 is reported before the backend returns: '//text)
    text = text(index(text, 'GET /api/v1/version f00dfeed'):)
    call check(index(text, 'POST /api/v1/checks/claim') == 0, &
        'no check is claimed once the server runs another release: '//text)

    call stop_all()
    call execute_command_line('rm -rf '//dir)
    print '(a)', 'backend follow: all checks passed'

contains

    subroutine take(server, token, class, id, repo, ref, kind, timeout)
        character(len=*), intent(in) :: server, token, class, id, repo, ref, kind
        integer, intent(in) :: timeout
        logical :: there
        do
            inquire (file=go, exist=there)
            if (there) exit
            call proc_sleep_ms(50)
        end do
        call backend_report(server, token, id, 'passed', 'checked', '')
    end subroutine take

    subroutine endless_sweep()
        do
            call proc_sleep_ms(1000)
        end do
    end subroutine endless_sweep

    subroutine fake_server(listen)
        !! The runner API as a backend process uses it: one check (7) for the
        !! first claim, none after; the release from the file `release`.
        integer, intent(in) :: listen
        type(http_request_t) :: req
        character(len=:), allocatable :: body, line
        integer :: cfd, st, code
        logical :: given

        given = .false.
        do
            cfd = tcp_accept(listen)
            if (cfd < 0) cycle
            call sock_set_timeouts(cfd, 30)
            call http_read_request(cfd, req, st)
            if (st /= 0) then
                call sock_close(cfd)
                cycle
            end if
            code = 200
            body = '{}'
            line = req%method//' '//req%path
            if (req%path == '/api/v1/version') then
                ! One read serves reply and log, so the log names the commit sent.
                body = read_file(release)
                body = body(1:len_trim(body) - 1)
                line = line//' '//body
                body = '{"version":"0","commit":"'//body//'"}'
            else if (req%path == '/api/v1/checks/claim') then
                if (given) then
                    code = 204
                    body = ''
                    line = line//' none'
                else
                    given = .true.
                    body = '{"id":7,"repo":"demo","ref":"refs/heads/topic","kind":"check"}'
                    line = line//' 7'
                end if
            else if (index(req%path, '/result') > 0) then
                line = line//' '//req%body
            end if
            call append_line(line)
            call http_send_response(cfd, code, 'application/json', body)
            call sock_close(cfd)
        end do
    end subroutine fake_server

    subroutine wait_log(what, why)
        character(len=*), intent(in) :: what, why
        integer :: j
        do j = 1, 600
            text = read_file(api_log)
            if (index(text, what) > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., why//': '//text)
    end subroutine wait_log

    integer function count_of(hay, needle)
        character(len=*), intent(in) :: hay, needle
        integer :: p, q
        count_of = 0
        p = 1
        do
            q = index(hay(p:), needle)
            if (q == 0) return
            count_of = count_of + 1
            p = p + q + len(needle) - 1
        end do
    end function count_of

    subroutine append_line(line)
        character(len=*), intent(in) :: line
        integer :: u
        open (newunit=u, file=api_log, position='append', action='write')
        write (u, '(a)') line
        close (u)
    end subroutine append_line

    subroutine write_file(path, content)
        !! Written aside and renamed, so a reader never sees it half written.
        character(len=*), intent(in) :: path, content
        integer :: u, st2
        open (newunit=u, file=path//'.new', status='replace', action='write')
        write (u, '(a)') content
        close (u)
        call execute_command_line('mv -f '//path//'.new '//path, exitstat=st2)
    end subroutine write_file

    function read_file(path) result(s)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: s
        integer :: u, ios, n
        s = ''
        open (newunit=u, file=path, access='stream', form='unformatted', status='old', &
            action='read', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        if (n > 0) then
            deallocate (s)
            allocate (character(len=n) :: s)
            read (u, iostat=ios) s
        end if
        close (u)
    end function read_file

    subroutine stop_all()
        integer :: st2
        if (back > 0) then
            call proc_kill(back, SIGTERM)
            call proc_wait(back, st2)
        end if
        if (fake > 0) then
            call proc_kill(fake, SIGTERM)
            call proc_wait(fake, st2)
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

end program test_backend_follow
