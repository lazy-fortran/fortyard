program test_login
    !! Sign-in through the provider, end to end over real sockets. The token
    !! endpoint is a file:// URL, so curl returns a prepared answer offline.
    use, intrinsic :: iso_c_binding, only: c_long
    use, intrinsic :: iso_fortran_env, only: int8
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        tcp_accept, &
        tcp_connect, &
        tcp_listen
    use fy_server, only: serve_connection, server_set_data_dir
    use fy_sqlite, only: db_t
    use fy_store, only: store_open, store_bootstrap
    use fy_oidc, only: pkce_challenge, base64url_encode, base64url_decode, oidc_value_ok
    implicit none

    interface
        integer(c_long) function c_time(t) bind(C, name='time')
            import :: c_long
            integer(c_long), intent(in), optional :: t
        end function c_time
    end interface

    character(len=*), parameter :: dir = 'build/test_login'
    character(len=*), parameter :: ADMIN_SUB = 'f3a1c0de-0000-4000-8000-000000000001'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: token, reply, issuer, state, nonce, pending, session, cwd
    character(len=4096) :: buf
    integer :: lfd, port, stat, u

    ! RFC 7636, appendix B.
    call check(pkce_challenge('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk') == &
        'E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM', 'PKCE S256 challenge')
    call check(base64url_decode(base64url_encode(bytes('{"a":"b?>"}'))) == '{"a":"b?>"}', &
        'base64url round trip')
    call check(.not. oidc_value_ok('a"b') .and. .not. oidc_value_ok('a'//new_line('a')), &
        'codes with quotes or line breaks are refused')

    call execute_command_line('rm -rf '//dir//' && mkdir -p '//dir// &
        '/idp/protocol/openid-connect')
    call get_environment_variable('PWD', buf)
    cwd = trim(buf)
    issuer = 'file://'//cwd//'/'//dir//'/idp'
    open (newunit=u, file=dir//'/oidc.conf', status='replace', action='write')
    write (u, '(a)') '# test provider'
    write (u, '(a)') 'issuer = '//issuer
    write (u, '(a)') 'client = fortyard'
    write (u, '(a)') 'secret = s3cret'
    write (u, '(a)') 'url = http://127.0.0.1'
    write (u, '(a)') 'admin = '//ADMIN_SUB
    close (u)
    call server_set_data_dir(dir)
    call store_open(dir//'/fortyard.db', db, stat)
    call store_bootstrap(db, token, stat)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)

    reply = exchange('GET /api/v1/version HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, '"login":true') > 0, 'version tells the page that sign-in is offered')

    reply = exchange('GET /login HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 302') == 1, '/login redirects')
    call check(index(reply, 'Location: '//issuer//'/protocol/openid-connect/auth?') > 0 .and. &
        index(reply, '&code_challenge_method=S256') > 0 .and. &
        index(reply, 'client_id=fortyard') > 0, 'to the provider with PKCE')
    call check(index(reply, 'SameSite=Lax') > 0 .and. index(reply, 'Secure') == 0, &
        'login cookie is Lax, and not Secure over http')
    state = param(reply, 'state')
    nonce = param(reply, 'nonce')
    pending = header_value(reply, 'Set-Cookie: fy_login=')
    call check(index(pending, state//'.') == 1, 'cookie carries the state')

    call answer(ADMIN_SUB, 'fortyard', nonce, 300)
    reply = exchange('GET /-/login/callback?code=abc&state=wrong HTTP/1.1'//CRLF// &
        'Cookie: fy_login='//pending//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 400') == 1, 'a foreign state is refused')

    reply = callback()
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. &
        index(reply, 'Set-Cookie: fy_session=fy_') > 0, 'the administrator gets a session')
    session = header_value(reply, 'Set-Cookie: fy_session=')
    reply = exchange('GET /api/v1/whoami HTTP/1.1'//CRLF//'Cookie: fy_session='//session// &
        CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. index(reply, '"account":"admin"') > 0, &
        'the session identifies the administrator')

    call answer('someone-else', 'fortyard', nonce, 300)
    reply = callback()
    call check(index(reply, 'HTTP/1.1 403') == 1 .and. index(reply, 'fy_session=fy_') == 0, &
        'an account without access is refused')

    call answer(ADMIN_SUB, 'computor-backend', nonce, 300)
    reply = callback()
    call check(index(reply, 'HTTP/1.1 502') == 1, 'a token for another client is refused')

    call answer(ADMIN_SUB, 'fortyard', 'other-nonce', 300)
    reply = callback()
    call check(index(reply, 'HTTP/1.1 502') == 1, 'a replayed token (wrong nonce) is refused')

    call answer(ADMIN_SUB, 'fortyard', nonce, -60)
    reply = callback()
    call check(index(reply, 'HTTP/1.1 502') == 1, 'an expired token is refused')

    write (*, '(a)') 'test_login: all checks passed'

contains

    function callback() result(r)
        character(len=:), allocatable :: r
        r = exchange('GET /-/login/callback?code=abc.DEF-1&state='//state//' HTTP/1.1'//CRLF// &
            'Cookie: fy_login='//pending//CRLF//CRLF)
    end function callback

    subroutine answer(sub, aud, nonce_claim, lifetime)
        !! What the token endpoint returns next: an ID token with these claims.
        character(len=*), intent(in) :: sub, aud, nonce_claim
        integer, intent(in) :: lifetime
        character(len=24) :: exp
        integer :: v

        write (exp, '(i0)') c_time() + lifetime
        open (newunit=v, file=dir//'/idp/protocol/openid-connect/token', status='replace', &
            action='write')
        write (v, '(a)') '{"access_token":"x","id_token":"'// &
            base64url_encode(bytes('{"alg":"RS256"}'))//'.'// &
            base64url_encode(bytes('{"iss":"'//issuer//'","aud":"'//aud// &
            '","azp":"fortyard","sub":"'//sub//'","exp":'//trim(exp)//',"nonce":"'// &
            nonce_claim//'","preferred_username":"tester"}'))//'.c2ln"}'
        close (v)
    end subroutine answer

    function bytes(s) result(b)
        character(len=*), intent(in) :: s
        integer(int8), allocatable :: b(:)
        integer :: k
        allocate (b(len(s)))
        do k = 1, len(s)
            b(k) = int(merge(iachar(s(k:k)) - 256, iachar(s(k:k)), iachar(s(k:k)) > 127), int8)
        end do
    end function bytes

    function param(r, key) result(v)
        character(len=*), intent(in) :: r, key
        character(len=:), allocatable :: v
        integer :: a, e
        a = index(r, '&'//key//'=') + len(key) + 2
        e = a
        do while (e <= len(r))
            if (r(e:e) == '&' .or. r(e:e) == achar(13)) exit
            e = e + 1
        end do
        v = r(a:e - 1)
    end function param

    function header_value(r, prefix) result(v)
        character(len=*), intent(in) :: r, prefix
        character(len=:), allocatable :: v
        integer :: a, e
        a = index(r, prefix) + len(prefix)
        e = index(r(a:), ';') + a - 2
        v = r(a:e)
    end function header_value

    function exchange(request) result(response)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: response
        character(len=4096) :: chunk
        integer :: cfd, sfd, n, st

        call tcp_connect('127.0.0.1', port, cfd, st)
        call check(st == 0, 'client connects')
        call sock_send_all(cfd, request, st)
        sfd = tcp_accept(lfd)
        call serve_connection(sfd, db)
        call sock_close(sfd)
        response = ''
        do
            n = sock_recv(cfd, chunk)
            if (n <= 0) exit
            response = response//chunk(1:n)
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

end program test_login
