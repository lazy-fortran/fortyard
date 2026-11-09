module fy_client
    !! Plain-HTTP client for a fortyard server inside a private network, and
    !! the stored login. HTTPS servers will go through libcurl.
    use fy_posix, only: tcp_connect, sock_send_all, sock_recv, sock_close, &
        sock_set_timeouts
    implicit none
    private

    public :: client_request, parse_url, config_t, config_load, config_save, &
        config_path

    character(len=2), parameter :: CRLF = achar(13)//achar(10)

    type :: config_t
        character(len=:), allocatable :: server
        character(len=:), allocatable :: token
    end type config_t

contains

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

        code = 0
        body = ''
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
        !! Stored login, overridden by FY_SERVER and FY_TOKEN.
        type(config_t), intent(out) :: cfg
        character(len=4096) :: line
        integer :: u, ios, eq

        cfg%server = ''
        cfg%token = ''
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
                end select
            end do
            close (u)
        end if
        call get_environment_variable('FY_SERVER', line, status=ios)
        if (ios == 0 .and. len_trim(line) > 0) cfg%server = trim(line)
        call get_environment_variable('FY_TOKEN', line, status=ios)
        if (ios == 0 .and. len_trim(line) > 0) cfg%token = trim(line)
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
        close (u)
        call execute_command_line("chmod 600 '"//path//"'", exitstat=stat)
    end subroutine config_save

end module fy_client
