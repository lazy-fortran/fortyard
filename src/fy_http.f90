module fy_http
    !! HTTP/1.1 requests and responses, one request per connection.
    use fy_posix, only: sock_recv, sock_send_all
    implicit none
    private

    public :: http_request_t, http_parse_head, http_read_request
    public :: http_send_response, http_status_text, http_bearer_token, http_cookie
    public :: http_query_param, http_percent_decode, http_read_head, http_read_body
    public :: http_basic_password

    integer, parameter, public :: HTTP_MAX_HEAD = 65536
    integer, parameter, public :: HTTP_MAX_BODY = 1048576

    type :: http_request_t
        character(len=:), allocatable :: method
        character(len=:), allocatable :: path
        character(len=:), allocatable :: query
        character(len=:), allocatable :: authorization
        character(len=:), allocatable :: cookie
        character(len=:), allocatable :: content_type
        character(len=:), allocatable :: accept_encoding
        character(len=:), allocatable :: if_none_match
        character(len=:), allocatable :: body
        !! Git's protocol version request and the request body's encoding.
        character(len=:), allocatable :: git_protocol, content_encoding
        !! SHA-256 the sender states for the body (uploads of release files).
        character(len=:), allocatable :: content_sha256
        integer :: content_length = 0
        !! Content-Length was given; a chunked body has none.
        logical :: has_length = .false., chunked = .false.
    end type http_request_t

    character(len=2), parameter :: CRLF = achar(13)//achar(10)

contains

    subroutine http_parse_head(head, req, status, streaming)
        !! Parses request line and headers; status is 0 or an HTTP error code.
        !! With `streaming`, chunked and large bodies are accepted, for routes
        !! that pass the body on as it arrives.
        character(len=*), intent(in) :: head
        type(http_request_t), intent(out) :: req
        integer, intent(out) :: status
        logical, intent(in), optional :: streaming
        integer :: eol, pos, sp1, sp2, colon, ios
        character(len=:), allocatable :: line, target, name, value

        req%method = ''
        req%path = ''
        req%query = ''
        req%authorization = ''
        req%cookie = ''
        req%content_type = ''
        req%accept_encoding = ''
        req%if_none_match = ''
        req%body = ''
        req%git_protocol = ''
        req%content_encoding = ''
        req%content_sha256 = ''
        status = 400
        eol = index(head, CRLF)
        if (eol < 1) return
        line = head(1:eol - 1)
        sp1 = index(line, ' ')
        sp2 = index(line, ' ', back=.true.)
        if (sp1 < 2 .or. sp2 <= sp1 + 1) return
        if (line(sp2 + 1:) /= 'HTTP/1.1' .and. line(sp2 + 1:) /= 'HTTP/1.0') return
        req%method = line(1:sp1 - 1)
        target = line(sp1 + 1:sp2 - 1)
        if (target(1:1) /= '/') return
        pos = index(target, '?')
        if (pos > 0) then
            req%path = target(1:pos - 1)
            req%query = target(pos + 1:)
        else
            req%path = target
        end if

        pos = eol + 2
        do while (pos <= len(head))
            eol = index(head(pos:), CRLF)
            if (eol == 0) exit
            line = head(pos:pos + eol - 2)
            pos = pos + eol + 1
            if (len(line) == 0) exit
            colon = index(line, ':')
            if (colon < 2) return
            name = lower(line(1:colon - 1))
            value = trim(adjustl(line(colon + 1:)))
            select case (name)
            case ('authorization')
                req%authorization = value
            case ('cookie')
                req%cookie = value
            case ('content-type')
                req%content_type = lower(value)
            case ('accept-encoding')
                req%accept_encoding = lower(value)
            case ('if-none-match')
                req%if_none_match = value
            case ('content-length')
                read (value, *, iostat=ios) req%content_length
                if (ios /= 0 .or. req%content_length < 0) return
                req%has_length = .true.
            case ('transfer-encoding')
                if (lower(value) /= 'chunked') then
                    status = 501
                    return
                end if
                req%chunked = .true.
            case ('git-protocol')
                req%git_protocol = value
            case ('content-encoding')
                req%content_encoding = lower(value)
            case ('x-content-sha256')
                req%content_sha256 = lower(value)
            end select
        end do
        status = 0
        if (present(streaming)) then
            if (streaming) return
        end if
        if (req%chunked) status = 501
        if (req%content_length > HTTP_MAX_BODY) status = 413
    end subroutine http_parse_head

    subroutine http_read_request(fd, req, status)
        !! Reads one request from a socket; status is 0 or an HTTP error code.
        integer, intent(in) :: fd
        type(http_request_t), intent(out) :: req
        integer, intent(out) :: status
        character(len=:), allocatable :: rest

        call http_read_head(fd, req, rest, status)
        if (status /= 0) return
        call http_read_body(fd, req, rest, status)
    end subroutine http_read_request

    subroutine http_read_head(fd, req, rest, status)
        !! Reads and parses the request head only, accepting chunked and large
        !! bodies; `rest` holds the bytes read past the head. Status is 0, -1
        !! when the peer sent nothing, or an HTTP error code.
        integer, intent(in) :: fd
        type(http_request_t), intent(out) :: req
        character(len=:), allocatable, intent(out) :: rest
        integer, intent(out) :: status
        character(len=8192) :: buf
        character(len=:), allocatable :: data
        integer :: n, head_end

        data = ''
        rest = ''
        head_end = 0
        status = 400
        do
            n = sock_recv(fd, buf)
            if (n <= 0) then
                if (len(data) == 0) status = -1
                return
            end if
            data = data//buf(1:n)
            head_end = index(data, CRLF//CRLF)
            if (head_end > 0) exit
            if (len(data) > HTTP_MAX_HEAD) then
                status = 431
                return
            end if
        end do
        call http_parse_head(data(1:head_end + 3), req, status, streaming=.true.)
        rest = data(head_end + 4:)
    end subroutine http_read_head

    subroutine http_read_body(fd, req, rest, status)
        !! Reads a bounded, non-chunked body into req%body after http_read_head.
        integer, intent(in) :: fd
        type(http_request_t), intent(inout) :: req
        character(len=*), intent(in) :: rest
        integer, intent(out) :: status
        character(len=8192) :: buf
        integer :: n

        status = 501
        if (req%chunked) return
        status = 413
        if (req%content_length > HTTP_MAX_BODY) return
        status = 400
        req%body = rest
        do while (len(req%body) < req%content_length)
            n = sock_recv(fd, buf)
            if (n <= 0) return
            req%body = req%body//buf(1:n)
        end do
        if (len(req%body) > req%content_length) req%body = req%body(1:req%content_length)
        status = 0
    end subroutine http_read_body

    function http_basic_password(req) result(password)
        !! The password of HTTP Basic authorization, or empty; git sends an
        !! access token this way.
        type(http_request_t), intent(in) :: req
        character(len=:), allocatable :: password
        character(len=:), allocatable :: plain
        integer :: colon

        password = ''
        if (len(req%authorization) <= 6) return
        if (lower(req%authorization(1:6)) /= 'basic ') return
        plain = base64_decode(trim(adjustl(req%authorization(7:))))
        colon = index(plain, ':')
        if (colon > 0) password = plain(colon + 1:)
    end function http_basic_password

    function base64_decode(s) result(d)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: d
        character(len=*), parameter :: ALPHA = &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
        integer :: i, v, bits, acc

        d = ''
        acc = 0
        bits = 0
        do i = 1, len(s)
            if (s(i:i) == '=') exit
            v = index(ALPHA, s(i:i)) - 1
            if (v < 0) then
                d = ''
                return
            end if
            acc = ior(ishft(iand(acc, 65535), 6), v)
            bits = bits + 6
            if (bits >= 8) then
                bits = bits - 8
                d = d//achar(iand(ishft(acc, -bits), 255))
            end if
        end do
    end function base64_decode

    subroutine http_send_response(fd, code, content_type, body, headers, cache)
        !! `headers` are further header lines, each ending in CRLF; `cache` is the
        !! Cache-Control value, no-store unless given.
        integer, intent(in) :: fd, code
        character(len=*), intent(in) :: content_type, body
        character(len=*), intent(in), optional :: headers, cache
        character(len=16) :: num, clen
        character(len=:), allocatable :: extra, policy
        integer :: stat

        extra = ''
        if (present(headers)) extra = headers
        policy = 'no-store'
        if (present(cache)) policy = cache
        write (num, '(i0)') code
        write (clen, '(i0)') len(body)
        call sock_send_all(fd, 'HTTP/1.1 '//trim(num)//' '//http_status_text(code)//CRLF// &
            'Content-Type: '//content_type//CRLF// &
            'Content-Length: '//trim(clen)//CRLF// &
            'Cache-Control: '//policy//CRLF//extra// &
            'X-Content-Type-Options: nosniff'//CRLF// &
            'Connection: close'//CRLF//CRLF//body, stat)
    end subroutine http_send_response

    function http_query_param(query, key) result(value)
        !! Decoded value of `key` in a query string, or empty.
        character(len=*), intent(in) :: query, key
        character(len=:), allocatable :: value
        integer :: pos, amp, eq
        character(len=:), allocatable :: pair

        value = ''
        pos = 1
        do while (pos <= len(query))
            amp = index(query(pos:), '&')
            if (amp == 0) then
                pair = query(pos:)
                pos = len(query) + 1
            else
                pair = query(pos:pos + amp - 2)
                pos = pos + amp
            end if
            eq = index(pair, '=')
            if (eq == 0) cycle
            if (http_percent_decode(pair(1:eq - 1)) == key) then
                value = http_percent_decode(pair(eq + 1:))
                return
            end if
        end do
    end function http_query_param

    function http_percent_decode(s) result(d)
        !! %XX escapes and '+' for a space; a malformed escape stays as it is.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: d
        integer :: i, v, ios

        d = ''
        i = 1
        do while (i <= len(s))
            if (s(i:i) == '%' .and. i + 2 <= len(s)) then
                read (s(i + 1:i + 2), '(z2)', iostat=ios) v
                if (ios == 0 .and. verify(s(i + 1:i + 2), '0123456789abcdefABCDEF') == 0) then
                    d = d//achar(v)
                    i = i + 3
                    cycle
                end if
            end if
            if (s(i:i) == '+') then
                d = d//' '
            else
                d = d//s(i:i)
            end if
            i = i + 1
        end do
    end function http_percent_decode

    function http_cookie(req, name) result(value)
        !! Value of cookie `name`, or empty.
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: value
        character(len=:), allocatable :: rest, pair
        integer :: semi, eq

        value = ''
        rest = req%cookie
        do while (len(rest) > 0)
            semi = index(rest, ';')
            if (semi == 0) then
                pair = trim(adjustl(rest))
                rest = ''
            else
                pair = trim(adjustl(rest(1:semi - 1)))
                rest = rest(semi + 1:)
            end if
            eq = index(pair, '=')
            if (eq < 2) cycle
            if (pair(1:eq - 1) == name) then
                value = pair(eq + 1:)
                return
            end if
        end do
    end function http_cookie

    function http_bearer_token(req) result(token)
        type(http_request_t), intent(in) :: req
        character(len=:), allocatable :: token

        token = ''
        if (len(req%authorization) > 7) then
            if (lower(req%authorization(1:7)) == 'bearer ') &
                token = trim(adjustl(req%authorization(8:)))
        end if
    end function http_bearer_token

    function http_status_text(code) result(text)
        integer, intent(in) :: code
        character(len=:), allocatable :: text

        select case (code)
        case (200); text = 'OK'
        case (201); text = 'Created'
        case (302); text = 'Found'
        case (304); text = 'Not Modified'
        case (415); text = 'Unsupported Media Type'
        case (400); text = 'Bad Request'
        case (401); text = 'Unauthorized'
        case (403); text = 'Forbidden'
        case (404); text = 'Not Found'
        case (405); text = 'Method Not Allowed'
        case (413); text = 'Content Too Large'
        case (431); text = 'Request Header Fields Too Large'
        case (500); text = 'Internal Server Error'
        case (501); text = 'Not Implemented'
        case default; text = 'Unknown'
        end select
    end function http_status_text

    pure function lower(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: i, c

        r = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 65 .and. c <= 90) r(i:i) = achar(c + 32)
        end do
    end function lower

end module fy_http
