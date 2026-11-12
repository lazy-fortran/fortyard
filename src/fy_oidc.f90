module fy_oidc
    !! Sign-in through an OpenID Connect provider (Keycloak): authorization code
    !! flow with PKCE, the code exchanged by the server with its client secret.
    use, intrinsic :: iso_c_binding, only: &
        c_long
    use, intrinsic :: iso_fortran_env, only: &
        int64, &
        int8
    use fx_hash, only: &
        sha256_string
    use fx_json_parse, only: &
        JSON_ARRAY_END, &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_KEY, &
        JSON_STRING, &
        json_event_t, &
        json_extract_int, &
        json_extract_string, &
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    use fy_proc, only: &
        fd_close, &
        fd_read, &
        fd_write_all, &
        proc_exit_status, &
        proc_find, &
        proc_spawn
    implicit none
    private
    public :: &
        base64url_decode, &
        base64url_encode, &
        oidc_authorize_url, &
        oidc_check, &
        oidc_claims_t, &
        oidc_config_t, &
        oidc_exchange, &
        oidc_is_admin, &
        oidc_load, &
        oidc_value_ok, &
        pkce_challenge

    type :: oidc_config_t
        logical :: enabled = .false.
        !! Issuer, e.g. https://code.tugraz.at/auth/realms/computor.
        character(len=:), allocatable :: issuer, client, secret
        !! Public URL of fortyard (with its base path), for the callback.
        character(len=:), allocatable :: url
        !! Subjects (`sub`) signed in as the administrator account.
        character(len=:), allocatable :: admins
    end type oidc_config_t

    type :: oidc_claims_t
        character(len=:), allocatable :: sub, username, name
    end type oidc_claims_t

    interface
        integer(c_long) function c_time(t) bind(C, name='time')
            import :: c_long
            integer(c_long), intent(in), optional :: t
        end function c_time
    end interface

contains

    subroutine oidc_load(path, cfg)
        !! `key = value` lines: issuer, client, secret, url, admin (repeatable).
        !! A missing file leaves sign-in through the provider off.
        character(len=*), intent(in) :: path
        type(oidc_config_t), intent(out) :: cfg
        character(len=4096) :: line
        character(len=:), allocatable :: key, value
        integer :: u, ios, eq

        cfg%issuer = ''
        cfg%client = ''
        cfg%secret = ''
        cfg%url = ''
        cfg%admins = ' '
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (line(1:1) == '#') cycle
            eq = index(line, '=')
            if (eq == 0) cycle
            key = trim(adjustl(line(1:eq - 1)))
            value = trim(adjustl(line(eq + 1:)))
            select case (key)
            case ('issuer'); cfg%issuer = value
            case ('client'); cfg%client = value
            case ('secret'); cfg%secret = value
            case ('url'); cfg%url = value
            case ('admin'); cfg%admins = cfg%admins//value//' '
            end select
        end do
        close (u)
        cfg%enabled = len(cfg%issuer) > 0 .and. len(cfg%client) > 0 .and. &
            len(cfg%secret) > 0 .and. len(cfg%url) > 0
    end subroutine oidc_load

    logical function oidc_is_admin(cfg, sub)
        type(oidc_config_t), intent(in) :: cfg
        character(len=*), intent(in) :: sub
        oidc_is_admin = len(sub) > 0 .and. index(cfg%admins, ' '//sub//' ') > 0
    end function oidc_is_admin

    function oidc_authorize_url(cfg, state, verifier, nonce) result(url)
        !! Keycloak's endpoint layout below the issuer.
        type(oidc_config_t), intent(in) :: cfg
        character(len=*), intent(in) :: state, verifier, nonce
        character(len=:), allocatable :: url

        url = cfg%issuer//'/protocol/openid-connect/auth?response_type=code'// &
            '&client_id='//enc(cfg%client)//'&redirect_uri='//enc(callback(cfg))// &
            '&scope=openid%20profile&state='//state//'&nonce='//nonce// &
            '&code_challenge='//pkce_challenge(verifier)//'&code_challenge_method=S256'
    end function oidc_authorize_url

    function callback(cfg) result(s)
        type(oidc_config_t), intent(in) :: cfg
        character(len=:), allocatable :: s
        s = cfg%url//'/-/login/callback'
    end function callback

    subroutine oidc_exchange(cfg, code, verifier, id_token, error)
        !! Trades the code for tokens at the token endpoint. The secret goes to
        !! curl on standard input, never on its command line.
        type(oidc_config_t), intent(in) :: cfg
        character(len=*), intent(in) :: code, verifier
        character(len=:), allocatable, intent(out) :: id_token, error
        character(len=:), allocatable :: curl, conf, body
        character(len=65536) :: buf
        character(len=16) :: argv(3)
        character(len=40) :: env(1)
        integer :: pid, to_child, from_child, n, stat, status
        logical :: found

        id_token = ''
        error = ''
        ! The code comes from the browser; it is pasted into curl's config.
        if (.not. oidc_value_ok(code) .or. .not. oidc_value_ok(verifier)) then
            error = 'malformed code'
            return
        end if
        curl = proc_find('curl')
        if (len(curl) == 0) then
            error = 'curl not found'
            return
        end if
        conf = 'url = "'//cfg%issuer//'/protocol/openid-connect/token"'//new_line('a')// &
            'user = "'//cfg%client//':'//cfg%secret//'"'//new_line('a')// &
            'data = "grant_type=authorization_code"'//new_line('a')// &
            'data = "code='//code//'"'//new_line('a')// &
            'data-urlencode = "redirect_uri='//callback(cfg)//'"'//new_line('a')// &
            'data = "code_verifier='//verifier//'"'//new_line('a')// &
            'max-time = 15'//new_line('a')//'silent'//new_line('a')
        if (index(cfg%issuer, 'https://') == 1) conf = conf//'proto = "=https"'//new_line('a')
        ! Behind fortyard's egress proxy the provider is reached only through it.
        call get_environment_variable('FY_PROXY', length=n, status=stat)
        if (stat == 0 .and. n > 0 .and. n <= len(buf)) then
            call get_environment_variable('FY_PROXY', buf(1:n))
            if (scan(buf(1:n), '"\'//achar(10)//achar(13)) == 0) &
                conf = conf//'proxy = "'//buf(1:n)//'"'//new_line('a')
        end if
        argv(1) = 'curl'
        argv(2) = '-K'
        argv(3) = '-'
        env(1) = 'PATH=/usr/local/bin:/usr/bin:/bin'
        call proc_spawn(curl, argv, env, pid, to_child, from_child)
        if (pid < 0) then
            error = 'curl did not start'
            return
        end if
        call fd_write_all(to_child, conf, stat)
        call fd_close(to_child)
        body = ''
        do
            n = fd_read(from_child, buf)
            if (n <= 0) exit
            if (len(body) + n > 262144) exit
            body = body//buf(1:n)
        end do
        call fd_close(from_child)
        status = proc_exit_status(pid)
        if (status /= 0) then
            error = 'token endpoint not reachable'
            return
        end if
        call json_extract_string(body, 'id_token', id_token, found)
        if (.not. found .or. len(id_token) == 0) then
            call json_extract_string(body, 'error', error, found)
            if (.not. found) error = 'no ID token in the answer'
            id_token = ''
        end if
    end subroutine oidc_exchange

    subroutine oidc_check(cfg, id_token, nonce, claims, error)
        !! The ID token came straight from the token endpoint over TLS, so the
        !! connection vouches for the issuer and the signature is not checked
        !! (OpenID Connect Core 3.1.3.7, item 6); the claims still are.
        type(oidc_config_t), intent(in) :: cfg
        character(len=*), intent(in) :: id_token, nonce
        type(oidc_claims_t), intent(out) :: claims
        character(len=:), allocatable, intent(out) :: error
        character(len=:), allocatable :: payload, s
        integer :: d1, d2, exp
        logical :: found

        claims%sub = ''
        claims%username = ''
        claims%name = ''
        error = 'malformed ID token'
        d1 = index(id_token, '.')
        if (d1 == 0) return
        d2 = index(id_token(d1 + 1:), '.') + d1
        if (d2 == d1) return
        payload = base64url_decode(id_token(d1 + 1:d2 - 1))
        if (len(payload) == 0) return
        call json_extract_string(payload, 'iss', s, found)
        if (.not. found .or. s /= cfg%issuer) then
            error = 'wrong issuer'
            return
        end if
        if (.not. audience_ok(payload, cfg%client)) then
            error = 'wrong audience'
            return
        end if
        call json_extract_string(payload, 'azp', s, found)
        if (found .and. s /= cfg%client) then
            error = 'wrong authorized party'
            return
        end if
        call json_extract_int(payload, 'exp', exp, found)
        if (.not. found .or. int(exp, int64) <= int(c_time(), int64)) then
            error = 'expired ID token'
            return
        end if
        call json_extract_string(payload, 'nonce', s, found)
        if (.not. found .or. s /= nonce .or. len(nonce) == 0) then
            error = 'wrong nonce'
            return
        end if
        call json_extract_string(payload, 'sub', claims%sub, found)
        if (.not. found .or. len(claims%sub) == 0) then
            error = 'no subject'
            return
        end if
        call json_extract_string(payload, 'preferred_username', claims%username, found)
        call json_extract_string(payload, 'name', claims%name, found)
        error = ''
    end subroutine oidc_check

    logical function audience_ok(payload, client)
        !! `aud` is the client, or an array that contains it.
        character(len=*), intent(in) :: payload, client
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: s
        logical :: found, in_aud
        integer :: depth

        call json_extract_string(payload, 'aud', s, found)
        if (found) then
            audience_ok = s == client
            return
        end if
        audience_ok = .false.
        in_aud = .false.
        depth = 0
        call json_parser_init(p, payload)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_KEY)
                in_aud = ev%string_val == 'aud'
            case (JSON_STRING)
                if (in_aud .and. ev%string_val == client) audience_ok = .true.
            case (JSON_ARRAY_END)
                in_aud = .false.
            end select
        end do
    end function audience_ok

    pure logical function oidc_value_ok(s)
        !! Characters of codes, states and verifiers that need no escaping.
        character(len=*), intent(in) :: s
        oidc_value_ok = len(s) > 0 .and. len(s) <= 4096 .and. verify(s, &
            'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._~-') == 0
    end function oidc_value_ok

    function pkce_challenge(verifier) result(s)
        !! base64url(SHA-256(verifier)) without padding (RFC 7636, S256).
        character(len=*), intent(in) :: verifier
        character(len=:), allocatable :: s
        character(len=64) :: hex
        integer(int8) :: bytes(32)
        integer :: k, v

        hex = sha256_string(verifier)
        do k = 1, 32
            read (hex(2*k - 1:2*k), '(z2)') v
            bytes(k) = int(merge(v - 256, v, v > 127), int8)
        end do
        s = base64url_encode(bytes)
    end function pkce_challenge

    function base64url_encode(bytes) result(s)
        integer(int8), intent(in) :: bytes(:)
        character(len=:), allocatable :: s
        character(len=*), parameter :: A = &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
        integer :: k, n, w, i

        s = ''
        n = size(bytes)
        do k = 1, n, 3
            w = 0
            do i = 0, 2
                w = w * 256
                if (k + i <= n) w = w + iand(int(bytes(k + i)), 255)
            end do
            s = s//A(ishft(w, -18) + 1:ishft(w, -18) + 1)// &
                A(iand(ishft(w, -12), 63) + 1:iand(ishft(w, -12), 63) + 1)
            if (k + 1 <= n) s = s//A(iand(ishft(w, -6), 63) + 1:iand(ishft(w, -6), 63) + 1)
            if (k + 2 <= n) s = s//A(iand(w, 63) + 1:iand(w, 63) + 1)
        end do
    end function base64url_encode

    function base64url_decode(s) result(out)
        !! Empty on a character outside the alphabet; padding is accepted.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        character(len=*), parameter :: A = &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
        integer :: k, v, bits, nbits

        out = ''
        bits = 0
        nbits = 0
        do k = 1, len(s)
            if (s(k:k) == '=') exit
            v = index(A, s(k:k)) - 1
            if (v < 0) then
                out = ''
                return
            end if
            bits = iand(ishft(bits, 6), int(z'FFFFFF')) + v
            nbits = nbits + 6
            if (nbits >= 8) then
                nbits = nbits - 8
                out = out//achar(iand(ishft(bits, -nbits), 255))
            end if
        end do
    end function base64url_decode

    function enc(s) result(e)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: e
        character(len=*), parameter :: hexd = '0123456789ABCDEF'
        integer :: k, c

        e = ''
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (index('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._~-', &
                s(k:k)) > 0) then
                e = e//s(k:k)
            else
                e = e//'%'//hexd(c / 16 + 1:c / 16 + 1)//hexd(mod(c, 16) + 1:mod(c, 16) + 1)
            end if
        end do
    end function enc

end module fy_oidc
