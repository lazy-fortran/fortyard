program test_server_roundtrip
    !! Real sockets in one thread: the client's request waits in the kernel
    !! until the server accepts and answers it.
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        tcp_accept, &
        tcp_connect, &
        tcp_listen
    use fy_server, only: serve_connection, server_set_web_dir, server_set_data_dir
    use fx_hash, only: sha256_string
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap
    implicit none

    character(len=*), parameter :: path = 'build/test_server_roundtrip.db'
    character(len=*), parameter :: data_dir = 'build/test_server_roundtrip_data'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: token, reply, cookie, narrow
    integer :: lfd, port, stat, k, u

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call execute_command_line('rm -rf '//data_dir//' && mkdir -p '//data_dir//'/repos')
    call server_set_data_dir(data_dir)
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

    reply = exchange('POST /api/v1/tokens HTTP/1.1'//CRLF//'Authorization: Bearer '// &
        token//CRLF//'Content-Length: 37'//CRLF//CRLF//'{"name":"runner-1","scopes":"runner"}')
    call check(index(reply, 'HTTP/1.1 201') == 1 .and. index(reply, '"scopes":"runner"') > 0, &
        'a token with a subset of the scopes')
    narrow = reply(index(reply, '"token":"') + 9:)
    narrow = narrow(1:index(narrow, '"') - 1)
    reply = exchange('POST /api/v1/tokens HTTP/1.1'//CRLF//'Authorization: Bearer '// &
        narrow//CRLF//'Content-Length: 36'//CRLF//CRLF//'{"name":"other-1","scopes":"runner"}')
    call check(index(reply, 'HTTP/1.1 403') == 1, &
        'a token without admin creates none, not even with its own scopes')

    call check_cache()

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
    write (u, '(a)') '<title>t</title><p>page</p><div id="app"></div><!--fy-data-->'
    close (u)
    call server_set_web_dir('build/test_web')
    reply = exchange('GET / HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, '<p>page</p><div id="app"><div class="fui-root') > 0 .and. &
        index(reply, '/token"') > 0, 'the page carries its first frame: sign-in')
    call check(index(reply, '"p":"/api/v1/version","s":200') > 0 .and. &
        index(reply, '"p":"/api/v1/whoami","s":401') > 0 .and. &
        index(reply, 'Cache-Control: no-store') > 0, 'anonymous page: the answers it used, not cached')
    call check(index(reply, '<title>Sign in · fortyard</title>') > 0, 'the page has its own title')
    reply = exchange('POST /api/v1/session HTTP/1.1'//CRLF// &
        'Content-Type: application/json'//CRLF//'Content-Length: 79'//CRLF//CRLF// &
        '{"token":"'//token//'"}')
    k = index(reply, 'Set-Cookie: fy_session=')
    cookie = reply(k + 12:k + 12 + len('fy_session=') + 67 - 1)
    reply = exchange('GET / HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, '"p":"/api/v1/whoami","s":200,"b":"{'//achar(92)//'"account'//achar(92)//'":'// &
        achar(92)//'"admin') > 0 .and. &
        index(reply, '"p":"/api/v1/repos","s":200') > 0 .and. index(reply, '/account"') > 0, &
        'signed-in page: account, repositories and their first frame')
    reply = exchange('POST /api/v1/tokens HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF// &
        'Content-Type: application/json'//CRLF//'Content-Length: 22'//CRLF//CRLF//'{"name":"a</script>b"}')
    call check(index(reply, 'HTTP/1.1 201') == 1, 'a token named with markup')
    reply = exchange('GET /tokens HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. &
        index(reply, '<title>Access tokens · fortyard</title>') > 0 .and. &
        index(reply, '"p":"/api/v1/tokens"') > 0, 'a deep link renders its own page')
    call check(index(reply, '</script>b') == 0 .and. index(reply, achar(92)//'u003c/script>b') > 0, &
        'data in the page cannot end its script element')
    reply = exchange('GET /no/such/-/thing HTTP/1.1'//CRLF//'Cookie: '//cookie//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1 .and. index(reply, 'Page not found') > 0, &
        'an unknown location is a page that says so, with 404')
    reply = exchange('GET /healthz/x HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1 .and. index(reply, 'fui-root') == 0, &
        'reserved names are never pages')
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

    subroutine check_cache()
        !! The shared build cache: upload with checksum, download with it,
        !! refused checksums, other namespaces and unknown keys.
        character(len=*), parameter :: key = &
            '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
        character(len=*), parameter :: object = 'compiled object bytes'
        character(len=64) :: sha
        character(len=16) :: n

        sha = sha256_string(object)
        write (n, '(i0)') len(object)
        reply = exchange('PUT /cache/v1/shared/'//key//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//'X-Content-SHA256: '//sha//CRLF//'Content-Length: '//trim(n)//CRLF// &
            CRLF//object)
        call check(index(reply, 'HTTP/1.1 201') == 1, 'an object is uploaded to the shared cache')
        reply = exchange('GET /cache/v1/shared/'//key//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//CRLF)
        call check(index(reply, 'HTTP/1.1 200') == 1 .and. index(reply, object) > 0 .and. &
            index(reply, 'X-Content-SHA256: '//sha) > 0, 'it is downloaded with its checksum')
        reply = exchange('PUT /cache/v1/shared/'//key//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//'X-Content-SHA256: '//repeat('0', 64)//CRLF//'Content-Length: '// &
            trim(n)//CRLF//CRLF//object)
        call check(index(reply, 'HTTP/1.1 422') == 1, 'a wrong checksum is refused')
        reply = exchange('PUT /cache/v1/u999/'//key//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//'X-Content-SHA256: '//sha//CRLF//'Content-Length: '//trim(n)//CRLF// &
            CRLF//object)
        call check(index(reply, 'HTTP/1.1 403') == 1, 'another account''s namespace is refused')
        reply = exchange('GET /cache/v1/shared/'//repeat('f', 64)//' HTTP/1.1'//CRLF// &
            'Authorization: Bearer '//token//CRLF//CRLF)
        call check(index(reply, 'HTTP/1.1 404') == 1, 'an unknown key is not cached')
        reply = exchange('GET /cache/v1/shared/'//key//' HTTP/1.1'//CRLF//CRLF)
        call check(index(reply, 'HTTP/1.1 401') == 1, 'the cache needs a token')
    end subroutine check_cache

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
