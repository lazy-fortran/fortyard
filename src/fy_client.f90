module fy_client
    !! HTTP client for a fortyard server, and the stored login. Plain HTTP
    !! inside a private network is spoken directly; an https:// server is
    !! reached through curl (as the sign-in does), so runners on other hosts
    !! can use the public address.
    use fy_posix, only: tcp_connect, sock_send_all, sock_recv, sock_close, &
        sock_set_timeouts
    use fy_proc, only: proc_find, proc_spawn, fd_read, fd_write_all, fd_close, &
        proc_exit_status
    implicit none
    private

    public :: client_request, parse_url, config_t, config_load, config_save, &
        config_path, client_set_proxy

    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    !! A proxy for every request (curl's syntax, e.g. socks5h://127.0.0.1:1080),
    !! for servers reached only through a tunnel; empty for none.
    character(len=:), allocatable :: proxy

    type :: config_t
        character(len=:), allocatable :: server
        character(len=:), allocatable :: token
        character(len=:), allocatable :: proxy
    end type config_t

contains

    subroutine client_set_proxy(url)
        character(len=*), intent(in) :: url
        proxy = url
    end subroutine client_set_proxy

    subroutine curl_request(server, method, path, token, payload, code, body, stat)
        !! One request through curl. Token and payload go to curl on standard
        !! input as its configuration, never on its command line; the status
        !! code follows the body on a line of its own.
        character(len=*), intent(in) :: server, method, path, token, payload
        integer, intent(out) :: code, stat
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable :: curl, conf
        character(len=65536) :: buf
        character(len=16) :: argv(3)
        character(len=40) :: env(1)
        integer :: pid, to_child, from_child, n, cut, ios

        code = 0
        body = ''
        stat = 1
        curl = proc_find('curl')
        if (len(curl) == 0) return
        conf = 'url = "'//quoted(server//path)//'"'//new_line('a')// &
            'request = "'//method//'"'//new_line('a')// &
            'max-time = 60'//new_line('a')//'silent'//new_line('a')// &
            'proto = "=http,https"'//new_line('a')// &
            'write-out = "\n%{http_code}"'//new_line('a')
        if (allocated(proxy)) then
            if (len(proxy) > 0) conf = conf//'proxy = "'//quoted(proxy)//'"'//new_line('a')
        end if
        if (len(token) > 0) conf = conf//'header = "Authorization: Bearer '// &
            quoted(token)//'"'//new_line('a')
        if (method /= 'GET') conf = conf//'header = "Content-Type: application/json"'// &
            new_line('a')//'data-binary = "'//quoted(payload)//'"'//new_line('a')
        argv(1) = 'curl'
        argv(2) = '-K'
        argv(3) = '-'
        env(1) = 'PATH=/usr/local/bin:/usr/bin:/bin'
        call proc_spawn(curl, argv, env, pid, to_child, from_child)
        if (pid < 0) return
        call fd_write_all(to_child, conf, stat)
        call fd_close(to_child)
        do
            n = fd_read(from_child, buf)
            if (n <= 0) exit
            if (len(body) + n > 16777216) exit
            body = body//buf(1:n)
        end do
        call fd_close(from_child)
        stat = proc_exit_status(pid)
        if (stat /= 0) return
        cut = index(body, new_line('a'), back=.true.)
        read (body(cut + 1:), *, iostat=ios) code
        if (ios /= 0) then
            stat = 1
            return
        end if
        body = body(1:max(0, cut - 1))
    end subroutine curl_request

    function quoted(text) result(out)
        !! text inside a double-quoted curl configuration value.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i

        out = ''
        do i = 1, len(text)
            select case (text(i:i))
            case ('"', '\')
                out = out//'\'//text(i:i)
            case (achar(10))
                out = out//'\n'
            case (achar(13))
                out = out//'\r'
            case (achar(9))
                out = out//'\t'
            case default
                out = out//text(i:i)
            end select
        end do
    end function quoted

    subroutine parse_url(url, host, port, base, stat)
        !! Accepts http://HOST[:PORT][/PREFIX]; HOST must be an IPv4 address.
        character(len=*), intent(in) :: url
        character(len=:), allocatable, intent(out) :: host, base
        integer, intent(out) :: port, stat
        character(len=:), allocatable :: rest, hostport
        integer :: slash, colon, ios

        host = ''
        base = ''
        port = 80
        stat = 1
        if (len(url) < 8) return
        if (url(1:7) /= 'http://') return
        rest = url(8:)
        slash = index(rest, '/')
        if (slash > 0) then
            hostport = rest(1:slash - 1)
            base = rest(slash:)
            if (base(len(base):) == '/') base = base(1:len(base) - 1)
        else
            hostport = rest
        end if
        colon = index(hostport, ':')
        if (colon > 0) then
            host = hostport(1:colon - 1)
            read (hostport(colon + 1:), *, iostat=ios) port
            if (ios /= 0) return
        else
            host = hostport
        end if
        if (len(host) == 0) return
        stat = 0
    end subroutine parse_url

    subroutine client_request(server, method, path, token, payload, code, body, stat)
        character(len=*), intent(in) :: server, method, path, token, payload
        integer, intent(out) :: code, stat
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable :: host, base, req, data
        character(len=8192) :: buf
        character(len=16) :: clen
        integer :: port, fd, n, head_end, ios
        logical :: use_curl

        code = 0
        body = ''
        use_curl = index(server, 'https://') == 1
        if (allocated(proxy)) use_curl = use_curl .or. len(proxy) > 0
        if (use_curl) then
            call curl_request(server, method, path, token, payload, code, body, stat)
            return
        end if
        call parse_url(server, host, port, base, stat)
        if (stat /= 0) return
        call tcp_connect(host, port, fd, stat)
        if (stat /= 0) return
        call sock_set_timeouts(fd, 30)
        write (clen, '(i0)') len(payload)
        req = method//' '//base//path//' HTTP/1.1'//CRLF//'Host: '//host//CRLF// &
            'User-Agent: fy'//CRLF//'Connection: close'//CRLF
        if (len(token) > 0) req = req//'Authorization: Bearer '//token//CRLF
        if (len(payload) > 0) req = req//'Content-Type: application/json'//CRLF
        req = req//'Content-Length: '//trim(clen)//CRLF//CRLF//payload
        call sock_send_all(fd, req, stat)
        data = ''
        if (stat == 0) then
            do
                n = sock_recv(fd, buf)
                if (n <= 0) exit
                data = data//buf(1:n)
            end do
        end if
        call sock_close(fd)
        stat = 4
        head_end = index(data, CRLF//CRLF)
        if (head_end == 0 .or. len(data) < 12) return
        read (data(10:12), *, iostat=ios) code
        if (ios /= 0) return
        body = data(head_end + 4:)
        stat = 0
    end subroutine client_request

    function config_path() result(path)
        character(len=:), allocatable :: path
        character(len=4096) :: value
        integer :: ios

        call get_environment_variable('XDG_CONFIG_HOME', value, status=ios)
        if (ios == 0 .and. len_trim(value) > 0) then
            path = trim(value)//'/fortyard/credentials'
        else
            call get_environment_variable('HOME', value)
            path = trim(value)//'/.config/fortyard/credentials'
        end if
    end function config_path

    subroutine config_load(cfg)
        !! Stored login, overridden by FY_SERVER, FY_TOKEN and FY_PROXY; the proxy
        !! applies to every request of this process.
        type(config_t), intent(out) :: cfg
        character(len=4096) :: line
        integer :: u, ios, eq

        cfg%server = ''
        cfg%token = ''
        cfg%proxy = ''
        open (newunit=u, file=config_path(), status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                eq = index(line, '=')
                if (eq < 2) cycle
                select case (line(1:eq - 1))
                case ('server'); cfg%server = trim(line(eq + 1:))
                case ('token'); cfg%token = trim(line(eq + 1:))
                case ('proxy'); cfg%proxy = trim(line(eq + 1:))
                end select
            end do
            close (u)
        end if
        call get_environment_variable('FY_SERVER', line, status=ios)
        if (ios == 0 .and. len_trim(line) > 0) cfg%server = trim(line)
        call get_environment_variable('FY_TOKEN', line, status=ios)
        if (ios == 0 .and. len_trim(line) > 0) cfg%token = trim(line)
        call get_environment_variable('FY_PROXY', line, status=ios)
        if (ios == 0 .and. len_trim(line) > 0) cfg%proxy = trim(line)
        call client_set_proxy(cfg%proxy)
    end subroutine config_load

    subroutine config_save(cfg, stat)
        type(config_t), intent(in) :: cfg
        integer, intent(out) :: stat
        character(len=:), allocatable :: path, dir
        integer :: u

        path = config_path()
        dir = path(1:index(path, '/', back=.true.) - 1)
        call execute_command_line("mkdir -p '"//dir//"' && umask 077 && : > '"//path//"'", &
            exitstat=stat)
        if (stat /= 0) return
        open (newunit=u, file=path, status='old', action='write', iostat=stat)
        if (stat /= 0) return
        write (u, '(a)') 'server='//cfg%server
        write (u, '(a)') 'token='//cfg%token
        if (allocated(cfg%proxy)) then
            if (len(cfg%proxy) > 0) write (u, '(a)') 'proxy='//cfg%proxy
        end if
        close (u)
        call execute_command_line("chmod 600 '"//path//"'", exitstat=stat)
    end subroutine config_save

end module fy_client
