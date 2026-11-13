program test_mirror
    !! A server with a data directory mirrors a repository on its own and
    !! answers the repository API from the mirror.
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_mirror_interval
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_mirror_data', up = 'build/test_mirror_up'
    character(len=*), parameter :: g = 'git -c user.name=T -c user.email=t@example.org -C '// &
        up//' '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: token, reply, upstream
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, i

    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//up//' && mkdir -p '//data//' '//up// &
        ' && git init -q -b main '//up//' && printf "hello\n" > '//up//'/README && '//g// &
        'add README && '//g//'commit -q -m "Initial commit"', exitstat=stat)
    call check(stat == 0, 'upstream created')
    call getcwd(cwd)
    upstream = trim(cwd)//'/'//up
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, token, stat)
    call store_repo_add(db, 'demo', upstream, stat)
    call check(stat == 0, 'repository registered')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_mirror_interval(1)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    do i = 1, 150
        reply = get('/api/v1/repos/demo/log?ref=main')
        if (index(reply, 'HTTP/1.1 200') == 1) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"subject":"Initial commit"') > 0, 'mirrored without being asked')
    reply = get('/api/v1/repos')
    call check(index(reply, '"name":"demo"') > 0 .and. index(reply, '"ready":true') > 0, &
        'repository listed as ready')
    reply = get('/api/v1/repos/demo/tree?ref=main')
    call check(index(reply, '"name":"README","type":"blob"') > 0, 'tree')
    reply = get('/api/v1/repos/demo/blob?ref=main&path=README')
    call check(index(reply, CRLF//CRLF//'hello') > 0, 'file')
    reply = get('/api/v1/repos/demo/commits/main')
    call check(index(reply, '+hello') > 0, 'commit with patch')
    reply = get('/api/v1/repos/demo/blob?ref=main&path=..%2Ffortyard.db')
    call check(index(reply, 'HTTP/1.1 400') == 1, 'paths cannot leave the tree')
    reply = get('/api/v1/repos/demo/log?ref=--output%3D%2Ftmp%2Fx')
    call check(index(reply, 'HTTP/1.1 400') == 1, 'refs cannot be git options')
    ! A new upstream commit arrives with the next fetch.
    call execute_command_line('printf "more\n" > '//up//'/MORE && '//g//'add MORE && '//g// &
        'commit -q -m "Second commit"', exitstat=stat)
    do i = 1, 100
        reply = get('/api/v1/repos/demo/log?ref=main')
        if (index(reply, '"subject":"Second commit"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"subject":"Second commit"') > 0, 'fetched again after the interval')
    reply = post('/api/v1/repos/demo/verify', '')
    call check(index(reply, 'HTTP/1.1 409') == 1, 'a mirror is not verified: '//reply)
    reply = get('/api/v1/repos/nothere/log')
    call check(index(reply, 'HTTP/1.1 404') == 1, 'unknown repository')
    reply = post('/api/v1/repos', '{"name":"evil","upstream":"--upload-pack=x"}')
    call check(index(reply, 'HTTP/1.1 400') == 1, 'upstream must be an https or ssh URL')
    reply = post('/api/v1/repos', '{"name":"fx","upstream":"https://example.invalid/fx.git"}')
    call check(index(reply, 'HTTP/1.1 201') == 1, 'administrator adds a repository')
    reply = post('/api/v1/repos/fx/mode', '{"mode":"primary"}')
    call check(index(reply, 'HTTP/1.1 409') == 1, 'a repository not cloned yet stays a mirror')
    reply = get('/api/v1/repos')
    call check(index(reply, '"name":"fx","mode":"mirror"') > 0, 'its mode is unchanged: '//reply)
    reply = post('/api/v1/repos/demo/mode', '{"mode":"primary"}')
    call check(index(reply, 'HTTP/1.1 200') == 1, 'a cloned mirror becomes primary')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)

contains

    function get(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        r = exchange('GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//token// &
            CRLF//CRLF)
    end function get

    function post(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//token// &
            CRLF//'Content-Length: '//trim(n)//CRLF//CRLF//body)
    end function post

    function exchange(request) result(response)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: response
        character(len=4096) :: buf
        integer :: cfd, n, st

        response = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, request, st)
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
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_mirror
