module fy_http
    !! HTTP/1.1 requests and responses, one request per connection.
    use fy_posix, only: sock_recv, sock_send_all
    implicit none
    private

    public :: http_request_t, http_parse_head, http_read_request
    public :: http_send_response, http_status_text, http_bearer_token, http_cookie

    integer, parameter, public :: HTTP_MAX_HEAD = 65536
    integer, parameter, public :: HTTP_MAX_BODY = 1048576

    type :: http_request_t
        character(len=:), allocatable :: method
        character(len=:), allocatable :: path
        character(len=:), allocatable :: query
        character(len=:), allocatable :: authorization
        character(len=:), allocatable :: cookie
        character(len=:), allocatable :: content_type
        character(len=:), allocatable :: body
        integer :: content_length = 0
    end type http_request_t

    character(len=2), parameter :: CRLF = achar(13)//achar(10)

contains

    subroutine http_parse_head(head, req, status)
        !! Parses request line and headers; status is 0 or an HTTP error code.
        character(len=*), intent(in) :: head
        type(http_request_t), intent(out) :: req
        integer, intent(out) :: status
        integer :: eol, pos, sp1, sp2, colon, ios
        character(len=:), allocatable :: line, target, name, value

        req%method = ''
        req%path = ''
        req%query = ''
        req%authorization = ''
        req%cookie = ''
        req%content_type = ''
        req%body = ''
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
            case ('content-length')
                read (value, *, iostat=ios) req%content_length
                if (ios /= 0 .or. req%content_length < 0) return
            case ('transfer-encoding')
                status = 501
                return
            end select
        end do
        status = 0
        if (req%content_length > HTTP_MAX_BODY) status = 413
    end subroutine http_parse_head

    subroutine http_read_request(fd, req, status)
        !! Reads one request from a socket; status is 0 or an HTTP error code.
        integer, intent(in) :: fd
        type(http_request_t), intent(out) :: req
        integer, intent(out) :: status
        character(len=8192) :: buf
        character(len=:), allocatable :: data
        integer :: n, head_end

        data = ''
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
        call http_parse_head(data(1:head_end + 3), req, status)
        if (status /= 0) return
        req%body = data(head_end + 4:)
        do while (len(req%body) < req%content_length)
            n = sock_recv(fd, buf)
            if (n <= 0) then
                status = 400
                return
            end if
            req%body = req%body//buf(1:n)
        end do
        if (len(req%body) > req%content_length) req%body = req%body(1:req%content_length)
    end subroutine http_read_request

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
