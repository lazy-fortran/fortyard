program test_http_parse
    use fortweb_http, only: &
        HTTP_MAX_BODY, &
        http_bearer_token, &
        http_parse_head, &
        http_percent_decode, &
        http_query_param, &
        http_request_t
    implicit none

    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(http_request_t) :: req
    integer :: status
    character(len=16) :: big

    call http_parse_head('GET /api/v1/whoami?x=1 HTTP/1.1'//CRLF// &
        'Host: a'//CRLF//'AUTHORIZATION: Bearer  fy_abc '//CRLF//CRLF, req, status)
    call check(status == 0, 'valid request parses')
    call check(req%method == 'GET', 'method')
    call check(req%path == '/api/v1/whoami', 'path without query')
    call check(req%query == 'x=1', 'query')
    call check(http_bearer_token(req) == 'fy_abc', 'bearer token, case-insensitive header')

    call http_parse_head('GET relative HTTP/1.1'//CRLF//CRLF, req, status)
    call check(status == 400, 'non-absolute target rejected')

    call http_parse_head('GET / HTTP/2.0'//CRLF//CRLF, req, status)
    call check(status == 400, 'unsupported version rejected')

    call http_parse_head('GET / HTTP/1.1'//CRLF//'no colon here'//CRLF//CRLF, req, status)
    call check(status == 400, 'malformed header rejected')

    call http_parse_head('POST / HTTP/1.1'//CRLF//'Transfer-Encoding: chunked'//CRLF// &
        CRLF, req, status)
    call check(status == 501, 'chunked bodies not accepted')

    write (big, '(i0)') HTTP_MAX_BODY + 1
    call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: '//trim(big)//CRLF// &
        CRLF, req, status)
    call check(status == 413, 'oversized body rejected before reading it')

    call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: -5'//CRLF//CRLF, &
        req, status)
    call check(status == 400, 'negative content length rejected')

    call check(http_query_param('ref=main&path=src%2Fa%20b.f90&x', 'path') == 'src/a b.f90' &
        .and. http_query_param('ref=main', 'path') == '' .and. &
        http_percent_decode('100%zz+1') == '100%zz 1', 'query parameters decoded')

    call http_parse_head('GET / HTTP/1.1'//CRLF//'Authorization: Basic eDp5'//CRLF//CRLF, &
        req, status)
    call check(http_bearer_token(req) == '', 'non-bearer authorization ignored')

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_http_parse
