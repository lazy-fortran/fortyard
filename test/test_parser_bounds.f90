program test_parser_bounds
    !! Malformed and truncated requests. The parsing modules keep their bounds
    !! checks in every build, release included, and guard their indices
    !! themselves; the routes behind them are compiled without. With runtime
    !! checks everywhere (fo test --debug test_parser_bounds) a read outside a
    !! string anywhere in this corpus stops the program.
    use fx_json_parse, only: json_extract_string
    use fy_change, only: change_sync
    use fy_http, only: &
        HTTP_MAX_HEAD, &
        http_basic_password, &
        http_bearer_token, &
        http_chunk_size, &
        http_compiler_options, &
        http_cookie, &
        http_parse_head, &
        http_percent_decode, &
        http_query_param, &
        http_request_t
    use fy_git_http, only: git_http_route, git_http_copy_chunked
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_accept, &
        tcp_connect, &
        tcp_listen
    use fy_server, only: serve_connection, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap
    implicit none

    character(len=*), parameter :: path = 'build/test_parser_bounds.db'
    character(len=*), parameter :: data_dir = 'build/test_parser_bounds_data'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    !! Bytes that end or split a field where the parser does not expect it.
    character(len=*), parameter :: HOSTILE = achar(0)//achar(13)//achar(10)// &
        ' :%/?&=;+"\{[,'//achar(255)
    type(db_t) :: db
    type(http_request_t) :: req
    character(len=:), allocatable :: token, reply, text, out
    integer :: lfd, port, stat, status, k

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call execute_command_line('rm -rf '//data_dir//' && mkdir -p '//data_dir//'/repos')
    call server_set_data_dir(data_dir)
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call store_bootstrap(db, token, stat)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'listens on an ephemeral port')
    port = sock_bound_port(lfd)

    ! gfortran records -fcheck=bounds under its older name.
    call check(index(http_compiler_options(), '-fbounds-check') > 0 .or. &
        index(http_compiler_options(), '-fcheck=all') > 0, &
        'the parsing modules are compiled with bounds checks')
    call check_length()
    call check_heads()
    call check_fields()
    call check_json()
    call check_chunks()
    call check_server()

    call sock_close(lfd)
    call db_close(db)

contains

    subroutine check_length()
        !! Content-Length is a string of digits and nothing else.
        character(len=*), parameter :: BAD(11) = [character(len=12) :: '/', '2*7', '5 6', &
            '1,2', '+5', '-5', '0x10', '1e3', '', '5'//achar(0), '.']
        integer :: i

        do i = 1, size(BAD)
            call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: '//trim(BAD(i))// &
                CRLF//CRLF, req, status, streaming=.true.)
            call check(status == 400, 'content length "'//trim(BAD(i))//'" is refused')
        end do
        call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: 0012'//CRLF//CRLF, req, &
            status)
        call check(status == 0 .and. req%content_length == 12 .and. req%has_length, &
            'a content length of digits is read')
        call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length:'//achar(9)//' 12 '// &
            achar(9)//CRLF//'Content-Type:'//achar(9)//'A/b'//achar(9)//CRLF//CRLF, req, status)
        call check(status == 0 .and. req%content_length == 12 .and. req%content_type == 'a/b', &
            'tabs and blanks around a header value are no part of it')
        call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: 1'//achar(9)//'2'//CRLF// &
            CRLF, req, status)
        call check(status == 400, 'a tab inside a content length is refused')
        call http_parse_head('GET / HTTP/1.1'//CRLF//'Cookie:'//achar(9)//' '//CRLF//'X:'//CRLF// &
            CRLF, req, status)
        call check(status == 0 .and. req%cookie == '', 'a header value of whitespace is empty')
        call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: 2147483647'//CRLF//CRLF, &
            req, status, streaming=.true.)
        call check(status == 0 .and. req%content_length == huge(0), &
            'the largest default integer is still a length')
        call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: 2147483648'//CRLF//CRLF, &
            req, status, streaming=.true.)
        call check(status == 413, 'a length past the default integer is too large')
        call http_parse_head('POST / HTTP/1.1'//CRLF//'Content-Length: '//repeat('9', 4000)// &
            CRLF//CRLF, req, status, streaming=.true.)
        call check(status == 413, 'a length of thousands of digits is too large')
    end subroutine check_length

    subroutine check_heads()
        !! Every prefix of a request head, and every byte of it replaced by a
        !! hostile one, parses to a status without leaving the string.
        character(len=*), parameter :: LINES(9) = [character(len=24) :: '', 'GET', 'GET /', &
            'GET  HTTP/1.1', ' / HTTP/1.1', 'GET / HTTP/1.1x', '/ HTTP/1.1', &
            'GET ? HTTP/1.1', 'HTTP/1.1']
        character(len=:), allocatable :: head, bent
        integer :: i, j

        head = 'POST /demo.git/git-receive-pack?x=%41&y=%&service=git-receive-pack HTTP/1.1'// &
            CRLF//'Host: h'//CRLF//'Authorization: Basic eDp5'//CRLF// &
            'Cookie: a=1; fy_session=abc; =x;;'//CRLF//'Content-Length: 12'//CRLF// &
            'Transfer-Encoding: chunked'//CRLF//':'//CRLF//CRLF
        do i = 0, len(head)
            call probe(head(1:i))
        end do
        do i = 1, len(head)
            do j = 1, len(HOSTILE)
                bent = head
                bent(i:i) = HOSTILE(j:j)
                call probe(bent)
            end do
        end do
        do i = 1, size(LINES)
            call probe(trim(LINES(i)))
            call http_parse_head(trim(LINES(i))//CRLF//CRLF, req, status)
            call check(status == 400, 'request line "'//trim(LINES(i))//'" is refused')
        end do
        call http_parse_head(' '//CRLF, req, status)
        call check(status == 400, 'a request line of one space is refused')
        call http_parse_head('GET  '//CRLF, req, status)
        call check(status == 400, 'a request line that ends after the method is refused')
        call probe(achar(10)//achar(10))
        call probe(achar(13))
        call probe(CRLF)
        call probe(CRLF//CRLF)
        call http_parse_head('GET / HTTP/1.1'//CRLF//'X: '//repeat('a', 2*HTTP_MAX_HEAD)//CRLF// &
            CRLF, req, status)
        call check(status == 0, 'a long header line of no interest is passed over')
        call http_parse_head('GET / HTTP/1.1'//CRLF//'Authorization: Bearer'//CRLF//CRLF, req, &
            status)
        call check(http_bearer_token(req) == '' .and. http_basic_password(req) == '', &
            'an authorization without a credential gives none')
        call http_parse_head('GET / HTTP/1.1'//CRLF//'Authorization: Basic '//CRLF//CRLF, req, &
            status)
        call check(http_basic_password(req) == '', 'an empty Basic credential gives none')
        call http_parse_head('GET / HTTP/1.1'//CRLF//'Authorization: Basic e!=='//CRLF//CRLF, &
            req, status)
        call check(http_basic_password(req) == '', 'a Basic credential outside base64 gives none')
    end subroutine check_heads

    subroutine probe(head)
        !! Parses `head` and reads everything the server reads from a request.
        character(len=*), intent(in) :: head
        character(len=:), allocatable :: repo
        logical :: write, routed

        call http_parse_head(head, req, status, streaming=.true.)
        call check(status == 0 .or. status == 400 .or. status == 413 .or. status == 501, &
            'a bent request head parses to a status')
        text = http_bearer_token(req)//http_basic_password(req)//http_cookie(req, 'fy_session')// &
            http_cookie(req, '')//http_query_param(req%query, 'x')//http_query_param(req%query, &
            'y')//http_query_param(req%query, '')//http_percent_decode(req%path)
        routed = git_http_route(req%path, req%query, repo, write)
    end subroutine probe

    subroutine check_fields()
        character(len=*), parameter :: PATHS(8) = [character(len=24) :: '', '/', '/info/refs', &
            '//info/refs', '/git-receive-pack', '/x/git-upload-pac', &
            'x/git-upload-pack', '/.git']
        character(len=:), allocatable :: repo
        logical :: write
        integer :: i

        call check(http_percent_decode('%') == '%' .and. http_percent_decode('a%4') == 'a%4' &
            .and. http_percent_decode('%%%') == '%%%' .and. http_percent_decode('%+1') == '% 1' &
            .and. http_percent_decode('%4') == '%4' .and. http_percent_decode('') == '' .and. &
            http_percent_decode('%41%') == 'A%' .and. http_percent_decode('% 1') == '% 1' .and. &
            http_percent_decode('%-1') == '%-1', 'an escape cut off at the end stays as it is')
        call check(http_query_param('', 'a') == '' .and. http_query_param('&', 'a') == '' .and. &
            http_query_param('=', '') == '' .and. http_query_param('&&=&a', 'a') == '' .and. &
            http_query_param('a=', 'a') == '' .and. http_query_param('a=%', 'a') == '%' .and. &
            http_query_param('=b&a=1&', 'a') == '1', 'query strings without keys or values')
        do i = 1, size(PATHS)
            call check(.not. git_http_route(trim(PATHS(i)), '', repo, write), &
                'path "'//trim(PATHS(i))//'" names no repository')
        end do
        call check(http_chunk_size('0') == 0 .and. http_chunk_size('1f') == 31 .and. &
            http_chunk_size('7FFFFFFF') == huge(0) .and. http_chunk_size('00000010') == 16, &
            'chunk sizes are hexadecimal')
        call check(http_chunk_size('') == -1 .and. http_chunk_size('g') == -1 .and. &
            http_chunk_size('-1') == -1 .and. http_chunk_size('+1') == -1 .and. &
            http_chunk_size(' 1') == -1 .and. http_chunk_size('0x1') == -1 .and. &
            http_chunk_size('80000000') == -1 .and. http_chunk_size('ffffffff') == -1 .and. &
            http_chunk_size('100000000') == -1 .and. http_chunk_size(repeat('0', 16)//'1') == -1 &
            .and. http_chunk_size(repeat('f', 4000)) == -1, &
            'chunk sizes that are no number or too large are refused')
    end subroutine check_fields

    subroutine check_json()
        !! Bodies cut off or bent anywhere give a value or none.
        character(len=:), allocatable :: body, bent, value
        logical :: found
        integer :: i, j

        body = '{"name":"a\"b\\","upstream":"é😀x","n":{"deep":[1,-2.5e3,'// &
            '{"name":"z"},true,null]},"scopes":"a b"}'
        call json_extract_string(body, 'name', value, found)
        call check(found .and. value == 'a"b\', 'the whole body gives its field')
        do i = 0, len(body) - 1
            call json_extract_string(body(1:i), 'name', value, found)
            call check(.not. found, 'a body cut off gives no field')
            call json_extract_string(body(1:i), 'scopes', value, found)
        end do
        do i = 1, len(body)
            do j = 1, len(HOSTILE)
                bent = body
                bent(i:i) = HOSTILE(j:j)
                call json_extract_string(bent, 'name', value, found)
                call json_extract_string(bent, 'scopes', value, found)
            end do
        end do
        call json_extract_string(repeat('[', 100000), 'name', value, found)
        call check(.not. found, 'brackets without end give no field')
        call json_extract_string(repeat('{"name":', 20000)//'"x"', 'name', value, found)
        call check(.not. found, 'objects without end give no field')
        call json_extract_string('{"name":"\u', 'name', value, found)
        call check(.not. found, 'an escape cut off gives no field')
    end subroutine check_json

    subroutine check_chunks()
        call dechunk('5'//CRLF//'hello'//CRLF//'0'//CRLF//CRLF, 0)
        call check(stat == 0 .and. out == 'hello', 'a chunked body is decoded')
        call dechunk('5;a=b'//CRLF//'hello'//CRLF//'3'//CRLF//' yo'//CRLF//'0'//CRLF//CRLF, 9)
        call check(stat == 0 .and. out == 'hello yo', 'chunks with an extension, split by the head')
        call dechunk('1'//CRLF//'aXX0'//CRLF//CRLF, 0)
        call check(stat /= 0, 'a chunk without CRLF after its data is refused')
        call dechunk('1'//CRLF//'a'//achar(13), 0)
        call check(stat /= 0, 'a chunk cut off inside its CRLF is refused')
        call dechunk(repeat('0', 16)//'1'//CRLF//'a'//CRLF//'0'//CRLF//CRLF, 0)
        call check(stat /= 0, 'a chunk size of seventeen digits is refused')
        call dechunk('100000000'//CRLF//'a'//CRLF//'0'//CRLF//CRLF, 0)
        call check(stat /= 0 .and. out == '', 'a chunk size past 32 bits is refused')
        call dechunk('ffffffff'//CRLF//'a'//CRLF//'0'//CRLF//CRLF, 0)
        call check(stat /= 0 .and. out == '', 'a negative chunk size is refused')
        call dechunk('1'//CRLF//'a'//CRLF//'7fffffff'//CRLF//'b', 0)
        call check(stat /= 0 .and. out == 'a', 'chunks that sum past the limit are refused')
        call dechunk(CRLF//'0'//CRLF//CRLF, 0)
        call check(stat /= 0, 'an empty chunk size is refused')
        call dechunk('-1'//CRLF//'0'//CRLF//CRLF, 0)
        call check(stat /= 0, 'a signed chunk size is refused')
        call dechunk(';'//CRLF, 0)
        call check(stat /= 0, 'an extension without a size is refused')
        call dechunk('5', 0)
        call check(stat /= 0, 'a size line without CRLF is refused')
        call dechunk(repeat('1', 2000), 0)
        call check(stat /= 0, 'an endless size line is refused')
        call dechunk('5'//CRLF//'hel', 0)
        call check(stat /= 0 .and. out == 'hel', 'a chunk cut off in its data is refused')
        call dechunk('', 0)
        call check(stat /= 0, 'an empty chunked body is refused')
    end subroutine check_chunks

    subroutine dechunk(body, split)
        !! Decodes `body`, whose first `split` bytes arrived with the head; sets
        !! `stat` and `out`.
        character(len=*), intent(in) :: body
        integer, intent(in) :: split
        character(len=4096) :: buf
        integer :: cfd, sfd, wfd, rfd, n, st

        call tcp_connect('127.0.0.1', port, cfd, st)
        sfd = tcp_accept(lfd)
        call tcp_connect('127.0.0.1', port, rfd, st)
        wfd = tcp_accept(lfd)
        call sock_send_all(cfd, body(split + 1:), st)
        call sock_close(cfd)
        call git_http_copy_chunked(sfd, body(1:split), wfd, stat)
        call sock_close(sfd)
        call sock_close(wfd)
        out = ''
        do
            n = sock_recv(rfd, buf)
            if (n <= 0) exit
            out = out//buf(1:n)
        end do
        call sock_close(rfd)
    end subroutine dechunk

    subroutine check_server()
        !! The routes cut their paths apart and take fields from the body; each
        !! answers a path that ends where the next part is expected, below a
        !! repository with an issue (#1) and a change (#2) and below none.
        character(len=*), parameter :: PATHS(58) = [character(len=48) :: '/api/v1/repos/', &
            '/api/v1/repos/x', '/api/v1/repos//issues', '/api/v1/repos/x/issues', &
            '/api/v1/repos/x/issues/', '/api/v1/repos/x/issues/1/', '/api/v1/repos/x/issuesx', &
            '/api/v1/repos/x/issues/1', '/api/v1/repos/x/issues/1/comments', &
            '/api/v1/repos/x/issues/1/x', '/api/v1/repos/x/issues//comments', &
            '/api/v1/repos/x/issues/0', '/api/v1/repos/x/issues/99999999999999999999', &
            '/api/v1/repos/x/changes', '/api/v1/repos/x/changes/', '/api/v1/repos/x/changes//', &
            '/api/v1/repos/x/changesx', '/api/v1/repos/x/changes/2', &
            '/api/v1/repos/x/changes/2/', '/api/v1/repos/x/changes/2/diff', &
            '/api/v1/repos/x/changes/2/diff?version=&from=%', &
            '/api/v1/repos/x/changes/2/diff?version=9&from=-1', &
            '/api/v1/repos/x/changes/2/comments', '/api/v1/repos/x/changes/2/x', &
            '/api/v1/repos/x/changes/1', '/api/v1/repos/x/changes/99999999999999999999', &
            '/api/v1/repos/x/releases', '/api/v1/repos/x/releases/', &
            '/api/v1/repos/x/releases//release.tar', '/api/v1/repos/x/releases/release.tar', &
            '/api/v1/repos/x/mode', '/api/v1/repos//mode', '/api/v1/repos/mode', &
            '/api/v1/repos/x/land', '/api/v1/repos/x/numbers', '/api/v1/repos/x/verify', &
            '/api/v1/repos/x/landings', '/api/v1/repos/x/commits/', '/api/v1/repos/x/refs', &
            '/api/v1/repos/x/log?ref=', '/api/v1/repos/x/tree?ref=topic&path=%', &
            '/api/v1/repos/x/blob?path=README&ref=topic', '/api/v1/repos/y/issues/1', &
            '/api/v1/repos/y/changes/2', '/api/v1/tokens/', '/api/v1/tokens/revoke', &
            '/api/v1/tokens//revoke', '/api/v1/tokens/99999999999999999999/revoke', &
            '/api/v1/checks/', '/api/v1/checks/result', '/api/v1/checks//result', &
            '/api/v1/checks/1/result', '/api/v1/jobs/', '/api/v1/jobs/99999999999999999999', &
            '/app/', '/cache/v1/u/ab', '/x/info/refs', '/-/login/callback?state=&code=%']
        character(len=*), parameter :: METHODS(4) = [character(len=6) :: 'GET', 'POST', 'PUT', &
            'DELETE']
        !! Cut off in a string, cut off in an escape, and fields of the wrong kind.
        character(len=*), parameter :: BODIES(3) = [character(len=48) :: '{"name":"x\', &
            '{"title":"a\u00', '{"title":7,"body":[],"state":"","branch":{}}']
        character(len=*), parameter :: git = 'env GIT_CONFIG_GLOBAL=/dev/null '// &
            'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -C '// &
            data_dir//'/work '
        character(len=:), allocatable :: auth
        integer :: i, j, b

        auth = 'Authorization: Bearer '//token//CRLF//'Cookie: fy_login=.; fy_session='//CRLF
        reply = send('POST', '/api/v1/repos', '{"name":"x"}')
        call check(index(reply, 'HTTP/1.1 201') == 1, 'a repository is created')
        reply = send('POST', '/api/v1/repos/x/issues', '{"title":"first","body":"b"}')
        call check(index(reply, '{"issue":1}') > 0, 'an issue is opened')
        call execute_command_line('git init -q -b topic '//data_dir//'/work && printf "r\n" > '// &
            data_dir//'/work/README && '//git//'add README && '//git//'commit -q -m Topic && '// &
            git//'push -q ../repos/x.git topic', exitstat=stat)
        call check(stat == 0, 'a branch is pushed')
        call change_sync(db, 'x', data_dir//'/repos/x.git', 'admin')
        reply = send('GET', '/api/v1/repos/x/changes/2', '')
        call check(index(reply, '"number":2,"title":"Topic"') > 0, 'the branch opened a change')

        ! The answer to an edit is read again from the store.
        reply = send('POST', '/api/v1/repos/x/issues/1', '{"title":"second"}')
        call check(index(reply, 'HTTP/1.1 200') == 1 .and. &
            index(reply, '{"number":1,"title":"second","state":"open","author":"admin"') > 0, &
            'an edited issue is answered as it is now')
        reply = send('POST', '/api/v1/repos/x/changes/2', '{"title":"Renamed"}')
        call check(index(reply, 'HTTP/1.1 200') == 1 .and. &
            index(reply, '"number":2,"title":"Renamed"') > 0, &
            'an edited change is answered as it is now')

        do i = 1, size(PATHS)
            do j = 1, size(METHODS)
                do b = 1, size(BODIES)
                    reply = send(trim(METHODS(j)), trim(PATHS(i)), trim(BODIES(b)))
                    call check(index(reply, 'HTTP/1.1 ') == 1, &
                        trim(METHODS(j))//' '//trim(PATHS(i))//' is answered')
                end do
            end do
        end do
        ! The bent requests reached the issue and the change and left them.
        reply = send('GET', '/api/v1/repos/x/issues/1', '')
        call check(index(reply, '{"number":1,"title":"second","state":"open"') > 0, &
            'bent requests leave the issue as it was')
        reply = send('GET', '/api/v1/repos/x/changes/2', '')
        call check(index(reply, '"number":2,"title":"Renamed","state":"open"') > 0, &
            'bent requests leave the change as it was')
        reply = send('POST', '/api/v1/repos/x/issues/1/comments', '{"body":"c"}')
        call check(index(reply, 'HTTP/1.1 201') == 1, 'a comment on the issue is taken')
        reply = send('POST', '/api/v1/repos/x/changes/2/comments', '{"body":"c"}')
        call check(index(reply, 'HTTP/1.1 201') == 1, 'a comment on the change is taken')

        reply = exchange('POST /api/v1/repos HTTP/1.1'//CRLF//auth//'Content-Length: /'//CRLF// &
            CRLF)
        call check(index(reply, 'HTTP/1.1 400') == 1, 'a content length that is no number is 400')
        reply = exchange('POST /api/v1/repos HTTP/1.1'//CRLF//auth//'Content-Length: '// &
            repeat('9', 20)//CRLF//CRLF)
        call check(index(reply, 'HTTP/1.1 413') == 1, 'a huge content length is 413')
        reply = exchange('POST /api/v1/repos HTTP/1.1'//CRLF//auth//'Content-Length: -1'//CRLF// &
            CRLF)
        call check(index(reply, 'HTTP/1.1 400') == 1, 'a negative content length is 400')
        reply = exchange('GET / HTTP/1.1'//CRLF//'X: '//repeat('a', HTTP_MAX_HEAD + 1))
        call check(index(reply, 'HTTP/1.1 431') == 1, 'a header line past the limit is 431')
        reply = exchange('GET / HTTP/1.1'//achar(10)//achar(10))
        call check(index(reply, 'HTTP/1.1 400') == 1, 'a head without CRLF is 400')
        reply = exchange('GET / HTTP/1.1'//CRLF//'Host: h'//CRLF)
        call check(index(reply, 'HTTP/1.1 400') == 1, 'a head cut off is 400')
        reply = exchange('POST /api/v1/repos HTTP/1.1'//CRLF//auth//'Content-Length: 50'//CRLF// &
            CRLF//'{"name":"x\')
        call check(index(reply, 'HTTP/1.1 400') == 1, 'a body shorter than its length is 400')
    end subroutine check_server

    function send(method, target, body) result(response)
        !! One request with the administrator's token.
        character(len=*), intent(in) :: method, target, body
        character(len=:), allocatable :: response
        character(len=16) :: num

        write (num, '(i0)') len(body)
        response = exchange(method//' '//target//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//'Cookie: fy_login=.; fy_session='//CRLF//'Content-Length: '// &
            trim(num)//CRLF//CRLF//body)
    end function send

    function exchange(request) result(response)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: response
        character(len=4096) :: buf
        integer :: cfd, sfd, n, st

        call tcp_connect('127.0.0.1', port, cfd, st)
        call check(st == 0, 'client connects')
        call sock_send_all(cfd, request, st)
        sfd = tcp_accept(lfd)
        ! The client never closes its side, so a request cut off ends here.
        call sock_set_timeouts(sfd, 1)
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

end program test_parser_bounds
