module fy_posix
    !! TCP sockets through the C library. Linux constants and struct layouts;
    !! macOS clients will go through libcurl instead.
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: tcp_listen, tcp_accept, tcp_connect, sock_close
    public :: sock_send_all, sock_recv, sock_set_timeouts, sock_bound_port
    public :: activated_listen_fd

    integer(c_int), parameter :: AF_INET = 2, SOCK_STREAM = 1
    integer(c_int), parameter :: SOL_SOCKET = 1, SO_REUSEADDR = 2
    integer(c_int), parameter :: SO_RCVTIMEO = 20, SO_SNDTIMEO = 21
    integer(c_int), parameter :: MSG_NOSIGNAL = 16384

    type, bind(C) :: sockaddr_in_t
        integer(c_int16_t) :: family = 0
        integer(c_int16_t) :: port = 0
        integer(c_int32_t) :: addr = 0
        character(kind=c_char) :: zero(8) = c_null_char
    end type sockaddr_in_t

    type, bind(C) :: timeval_t
        integer(c_long) :: sec = 0
        integer(c_long) :: usec = 0
    end type timeval_t

    interface
        function c_socket(domain, kind, protocol) bind(C, name='socket') result(fd)
            import :: c_int
            integer(c_int), value :: domain, kind, protocol
            integer(c_int) :: fd
        end function c_socket

        function c_setsockopt(fd, level, name, val, length) &
                bind(C, name='setsockopt') result(rc)
            import :: c_int, c_ptr
            integer(c_int), value :: fd, level, name
            type(c_ptr), value :: val
            integer(c_int), value :: length
            integer(c_int) :: rc
        end function c_setsockopt

        function c_bind(fd, addr, length) bind(C, name='bind') result(rc)
            import :: c_int, sockaddr_in_t
            integer(c_int), value :: fd
            type(sockaddr_in_t), intent(in) :: addr
            integer(c_int), value :: length
            integer(c_int) :: rc
        end function c_bind

        function c_connect(fd, addr, length) bind(C, name='connect') result(rc)
            import :: c_int, sockaddr_in_t
            integer(c_int), value :: fd
            type(sockaddr_in_t), intent(in) :: addr
            integer(c_int), value :: length
            integer(c_int) :: rc
        end function c_connect

        function c_listen(fd, backlog) bind(C, name='listen') result(rc)
            import :: c_int
            integer(c_int), value :: fd, backlog
            integer(c_int) :: rc
        end function c_listen

        function c_accept(fd, addr, length) bind(C, name='accept') result(cfd)
            import :: c_int, c_ptr
            integer(c_int), value :: fd
            type(c_ptr), value :: addr, length
            integer(c_int) :: cfd
        end function c_accept

        function c_getsockname(fd, addr, length) bind(C, name='getsockname') result(rc)
            import :: c_int, sockaddr_in_t
            integer(c_int), value :: fd
            type(sockaddr_in_t), intent(out) :: addr
            integer(c_int), intent(inout) :: length
            integer(c_int) :: rc
        end function c_getsockname

        function c_recv(fd, buf, length, flags) bind(C, name='recv') result(n)
            import :: c_int, c_size_t, c_intptr_t, c_char
            integer(c_int), value :: fd
            character(kind=c_char) :: buf(*)
            integer(c_size_t), value :: length
            integer(c_int), value :: flags
            integer(c_intptr_t) :: n
        end function c_recv

        function c_send(fd, buf, length, flags) bind(C, name='send') result(n)
            import :: c_int, c_size_t, c_intptr_t, c_char
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: buf(*)
            integer(c_size_t), value :: length
            integer(c_int), value :: flags
            integer(c_intptr_t) :: n
        end function c_send

        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close

        function c_inet_pton(af, src, dst) bind(C, name='inet_pton') result(rc)
            import :: c_int, c_char, c_int32_t
            integer(c_int), value :: af
            character(kind=c_char), intent(in) :: src(*)
            integer(c_int32_t), intent(out) :: dst
            integer(c_int) :: rc
        end function c_inet_pton

        function c_htons(x) bind(C, name='htons') result(y)
            import :: c_int16_t
            integer(c_int16_t), value :: x
            integer(c_int16_t) :: y
        end function c_htons

        function c_getpid() bind(C, name='getpid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid
    end interface

contains

    subroutine make_addr(host, port, addr, stat)
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        type(sockaddr_in_t), intent(out) :: addr
        integer, intent(out) :: stat

        addr%family = int(AF_INET, c_int16_t)
        addr%port = c_htons(int(port, c_int16_t))
        stat = 0
        if (c_inet_pton(AF_INET, trim(host)//c_null_char, addr%addr) /= 1) stat = 1
    end subroutine make_addr

    subroutine tcp_listen(host, port, fd, stat)
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        integer, intent(out) :: fd, stat
        type(sockaddr_in_t) :: addr
        integer(c_int), target :: one

        call make_addr(host, port, addr, stat)
        fd = -1
        if (stat /= 0) return
        fd = c_socket(AF_INET, SOCK_STREAM, 0_c_int)
        if (fd < 0) then
            stat = 2
            return
        end if
        one = 1
        if (c_setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, c_loc(one), 4_c_int) /= 0) stat = 3
        if (stat == 0 .and. c_bind(fd, addr, int(c_sizeof(addr), c_int)) /= 0) stat = 4
        if (stat == 0 .and. c_listen(fd, 128_c_int) /= 0) stat = 5
        if (stat /= 0) then
            call sock_close(fd)
            fd = -1
        end if
    end subroutine tcp_listen

    function activated_listen_fd() result(fd)
        !! The socket passed by systemd socket activation, or -1.
        integer :: fd
        character(len=32) :: value
        integer :: n, pid, ios

        fd = -1
        call get_environment_variable('LISTEN_FDS', value, status=ios)
        if (ios /= 0) return
        read (value, *, iostat=ios) n
        if (ios /= 0 .or. n < 1) return
        call get_environment_variable('LISTEN_PID', value, status=ios)
        if (ios /= 0) return
        read (value, *, iostat=ios) pid
        if (ios /= 0 .or. pid /= c_getpid()) return
        fd = 3
    end function activated_listen_fd

    function tcp_accept(lfd) result(cfd)
        integer, intent(in) :: lfd
        integer :: cfd

        cfd = c_accept(int(lfd, c_int), c_null_ptr, c_null_ptr)
    end function tcp_accept

    subroutine tcp_connect(host, port, fd, stat)
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        integer, intent(out) :: fd, stat
        type(sockaddr_in_t) :: addr

        call make_addr(host, port, addr, stat)
        fd = -1
        if (stat /= 0) return
        fd = c_socket(AF_INET, SOCK_STREAM, 0_c_int)
        if (fd < 0) then
            stat = 2
            return
        end if
        if (c_connect(fd, addr, int(c_sizeof(addr), c_int)) /= 0) then
            stat = 3
            call sock_close(fd)
            fd = -1
        end if
    end subroutine tcp_connect

    function sock_bound_port(fd) result(port)
        integer, intent(in) :: fd
        integer :: port
        type(sockaddr_in_t) :: addr
        integer(c_int) :: length

        length = int(c_sizeof(addr), c_int)
        port = -1
        if (c_getsockname(int(fd, c_int), addr, length) /= 0) return
        port = iand(int(c_htons(addr%port)), 65535)
    end function sock_bound_port

    subroutine sock_set_timeouts(fd, seconds)
        !! Bounds how long one slow client can hold the accept loop.
        integer, intent(in) :: fd, seconds
        type(timeval_t), target :: tv
        integer(c_int) :: rc

        tv%sec = int(seconds, c_long)
        tv%usec = 0
        rc = c_setsockopt(int(fd, c_int), SOL_SOCKET, SO_RCVTIMEO, c_loc(tv), &
            int(c_sizeof(tv), c_int))
        rc = c_setsockopt(int(fd, c_int), SOL_SOCKET, SO_SNDTIMEO, c_loc(tv), &
            int(c_sizeof(tv), c_int))
    end subroutine sock_set_timeouts

    subroutine sock_send_all(fd, data, stat)
        integer, intent(in) :: fd
        character(len=*), intent(in) :: data
        integer, intent(out) :: stat
        integer :: pos
        integer(c_intptr_t) :: n

        stat = 0
        pos = 1
        do while (pos <= len(data))
            n = c_send(int(fd, c_int), data(pos:), int(len(data) - pos + 1, c_size_t), &
                MSG_NOSIGNAL)
            if (n <= 0) then
                stat = 1
                return
            end if
            pos = pos + int(n)
        end do
    end subroutine sock_send_all

    function sock_recv(fd, buf) result(n)
        !! Bytes received into buf; 0 at end of stream, negative on error.
        integer, intent(in) :: fd
        character(len=*), intent(inout) :: buf
        integer :: n

        n = int(c_recv(int(fd, c_int), buf, int(len(buf), c_size_t), 0_c_int))
    end function sock_recv

    subroutine sock_close(fd)
        integer, intent(in) :: fd
        integer(c_int) :: rc

        if (fd >= 0) rc = c_close(int(fd, c_int))
    end subroutine sock_close

end module fy_posix
