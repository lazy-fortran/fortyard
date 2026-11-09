program test_server_roundtrip
    !! Real sockets in one thread: the client's request waits in the kernel
    !! until the server accepts and answers it.
    use fy_posix, only: tcp_listen, tcp_accept, tcp_connect, sock_send_all, &
        sock_recv, sock_close, sock_bound_port
    use fy_server, only: serve_connection, server_set_web_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap
    implicit none

    character(len=*), parameter :: path = 'build/test_server_roundtrip.db'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: token, reply, cookie
    integer :: lfd, port, stat, k, u

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

    reply = exchange('POST /api/v1/session HTTP/1.1'//CRLF// &
        'Content-Type: application/json'//CRLF//'Content-Length: 10'//CRLF//CRLF//'{"token":}')
    call check(index(reply, 'HTTP/1.1 401') == 1, 'session needs a valid token')

    reply = exchange('POST /api/v1/session HTTP/1.1'//CRLF// &
        'Content-Type: application/json'//CRLF//'Content-Length: 79'//CRLF//CRLF// &
        '{"token":"'//token//'"}')
    k = index(reply, 'Set-Cookie: fy_session=')
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. k > 0 .and. &
        index(reply, 'HttpOnly; SameSite=Strict') > 0, 'session opens with an HttpOnly cookie')
    cookie = reply(k + 12:k + 12 + len('fy_session=') + 67 - 1)

    reply = exchange('GET /api/v1/whoami HTTP/1.1'//CRLF//'Cookie: a=b; '//cookie//CRLF//CRLF)
    call check(index(reply, '"token":"browser session"') > 0, 'the session cookie authenticates')

    reply = exchange('POST /api/v1/tokens HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF// &
        'Content-Type: application/x-www-form-urlencoded'//CRLF//'Content-Length: 15'//CRLF// &
        CRLF//'{"name":"ci-2"}')
    call check(index(reply, 'HTTP/1.1 415') == 1, 'a cookie request that changes state must be JSON')

    reply = exchange('GET /api/v1/tokens HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, '"name":"ci-1"') > 0 .and. index(reply, '"current":true') > 0 &
        .and. index(reply, '"token":"fy_') == 0, 'tokens listed without secrets')

    reply = exchange('POST /api/v1/tokens/2/revoke HTTP/1.1'//CRLF//'Cookie: '//cookie// &
        CRLF//'Content-Type: application/json'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1, 'own token revoked')
    reply = exchange('POST /api/v1/tokens/999/revoke HTTP/1.1'//CRLF//'Cookie: '//cookie// &
        CRLF//'Content-Type: application/json'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1, 'unknown token not revoked')

    reply = exchange('DELETE /api/v1/session HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, 'Max-Age=0') > 0, 'session closes and clears the cookie')
    reply = exchange('GET /api/v1/whoami HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 401') == 1, 'a closed session no longer authenticates')

    reply = exchange('GET / HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, '<h1>fortyard</h1>') > 0, 'status page at / without a web application')
    call execute_command_line('rm -rf build/test_web && mkdir -p build/test_web' // &
        ' && printf code > build/test_web/fy_app.wasm')
    open (newunit=u, file='build/test_web/index.html', status='replace', action='write')
    write (u, '(a)') '<p>page</p><div id="app"></div><!--fy-data-->'
    close (u)
    call server_set_web_dir('build/test_web')
    reply = exchange('GET / HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, '<p>page</p><div id="app"><div class="fui-root') > 0 .and. &
        index(reply, 'data-id="token"') > 0, 'the page carries its first frame: sign-in')
    call check(index(reply, 'id="fy-version"') > 0 .and. index(reply, 'fy-whoami') == 0 &
        .and. index(reply, 'Cache-Control: no-store') > 0, 'anonymous page: version only, not cached')
    reply = exchange('POST /api/v1/session HTTP/1.1'//CRLF// &
        'Content-Type: application/json'//CRLF//'Content-Length: 79'//CRLF//CRLF// &
        '{"token":"'//token//'"}')
    k = index(reply, 'Set-Cookie: fy_session=')
    cookie = reply(k + 12:k + 12 + len('fy_session=') + 67 - 1)
    reply = exchange('GET / HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, 'id="fy-whoami">{"account":"admin"') > 0 .and. &
        index(reply, 'id="fy-repos">{"repos":[]}') > 0 .and. index(reply, 'data-id="signout"') > 0, &
        'signed-in page: account, repositories and their first frame')
    reply = exchange('GET /app/fy_app.wasm HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'application/wasm') > 0 .and. index(reply, CRLF//CRLF//'code') > 0, &
        'WebAssembly served with its type')
    call execute_command_line('printf zipped > build/test_web/fy_app.wasm.gz')
    reply = exchange('GET /app/fy_app.wasm HTTP/1.1'//CRLF//'Accept-Encoding: gzip, br'//CRLF//CRLF)
    call check(index(reply, 'Content-Encoding: gzip') > 0 .and. index(reply, CRLF//CRLF//'zipped') > 0, &
        'the gzip copy goes to clients that accept it')
    reply = exchange('GET /app/fy_app.wasm HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'Content-Encoding') == 0 .and. index(reply, CRLF//CRLF//'code') > 0, &
        'the plain file goes to the others')
    reply = exchange('GET /app/..%2ffortyard.db HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1, 'no path leaves the web directory')
    reply = exchange('GET /app/../test_store.db HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1, 'no parent directory')
    reply = exchange('GET /status HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, '<h1>fortyard</h1>') > 0, 'status page at /status')

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
