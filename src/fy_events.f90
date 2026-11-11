module fy_events
    !! Change notifications for live pages. A process that changes something a
    !! page shows (a landing queued, started, finished) rewrites the file
    !! events/TOPIC in the data directory; a server-sent event stream waits for
    !! that through inotify and for its client in one poll(2), so a waiting
    !! stream costs no work at all until something happens.
    use, intrinsic :: iso_c_binding
    use fy_posix, only: &
        sock_send_all
    implicit none
    private

    public :: &
        events_notify, &
        events_set_dir, &
        events_stream, &
        events_wait, &
        events_watch_close, &
        events_watch_open

    integer(c_int), parameter :: IN_CLOSE_WRITE = 8, IN_MOVED_TO = 128, IN_CLOEXEC = 524288
    integer(c_short), parameter :: POLLIN = 1
    !! A comment line keeps proxies from closing a quiet stream.
    integer, parameter :: KEEPALIVE_MS = 25000

    type, bind(C) :: pollfd_t
        integer(c_int) :: fd
        integer(c_short) :: events, revents
    end type pollfd_t

    interface
        function c_inotify_init1(flags) bind(C, name='inotify_init1') result(fd)
            import :: c_int
            integer(c_int), value :: flags
            integer(c_int) :: fd
        end function c_inotify_init1

        function c_inotify_add_watch(fd, path, mask) bind(C, name='inotify_add_watch') result(wd)
            import :: c_int, c_char
            integer(c_int), value :: fd, mask
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: wd
        end function c_inotify_add_watch

        function c_poll(fds, n, timeout) bind(C, name='poll') result(rc)
            import :: c_int, c_long, pollfd_t
            type(pollfd_t), intent(inout) :: fds(*)
            integer(c_long), value :: n
            integer(c_int), value :: timeout
            integer(c_int) :: rc
        end function c_poll

        function c_read(fd, buf, n) bind(C, name='read') result(got)
            import :: c_int, c_char, c_size_t, c_intptr_t
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
            integer(c_intptr_t) :: got
        end function c_read

        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close
    end interface

    character(len=:), allocatable :: dir

contains

    subroutine events_set_dir(path)
        !! Set once before the processes start; created if missing.
        character(len=*), intent(in) :: path
        dir = path
        call execute_command_line('mkdir -p '''//dir//'''')
    end subroutine events_set_dir

    subroutine events_notify(topic)
        !! Tells the streams of `topic` that it changed.
        character(len=*), intent(in) :: topic
        integer :: u, ios
        if (.not. allocated(dir)) return
        open (newunit=u, file=dir//'/'//topic, status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') topic
        close (u)
    end subroutine events_notify

    subroutine events_stream(fd, topic)
        !! A server-sent event stream on socket `fd`: one event at once (the
        !! page loads its state), then one each time `topic` changes, until the
        !! client goes away.
        integer, intent(in) :: fd
        character(len=*), intent(in) :: topic
        character(len=2), parameter :: CRLF = achar(13)//achar(10)
        character(len=1), parameter :: LF = achar(10)
        type(pollfd_t) :: fds(2)
        character(kind=c_char) :: buf(4096)
        integer(c_int) :: ino, wd, rc
        integer(c_intptr_t) :: got
        integer :: stat

        call sock_send_all(fd, 'HTTP/1.1 200 OK'//CRLF//'Content-Type: text/event-stream'//CRLF// &
            'Cache-Control: no-store'//CRLF//'X-Accel-Buffering: no'//CRLF// &
            'Connection: close'//CRLF//CRLF//'retry: 2000'//LF//LF// &
            'event: '//topic//LF//'data: now'//LF//LF, stat)
        if (stat /= 0 .or. .not. allocated(dir)) return
        ino = c_inotify_init1(IN_CLOEXEC)
        if (ino < 0) return
        wd = c_inotify_add_watch(ino, dir//c_null_char, ior(IN_CLOSE_WRITE, IN_MOVED_TO))
        if (wd < 0) then
            rc = c_close(ino)
            return
        end if
        do
            fds(1) = pollfd_t(ino, POLLIN, 0_c_short)
            fds(2) = pollfd_t(int(fd, c_int), POLLIN, 0_c_short)
            rc = c_poll(fds, 2_c_long, int(KEEPALIVE_MS, c_int))
            if (rc < 0) exit
            if (rc == 0) then
                call sock_send_all(fd, ': keep-alive'//LF//LF, stat)
                if (stat /= 0) exit
                cycle
            end if
            ! An event stream's client never sends: readable means it closed.
            if (fds(2)%revents /= 0) exit
            if (fds(1)%revents /= 0) then
                got = c_read(ino, buf, int(size(buf), c_size_t))
                if (got <= 0) exit
                if (.not. names(buf(1:got), topic)) cycle
                call sock_send_all(fd, 'event: '//topic//LF//'data: changed'//LF//LF, stat)
                if (stat /= 0) exit
            end if
        end do
        rc = c_close(ino)
    end subroutine events_stream

    integer function events_watch_open() result(fd)
        !! A watch on the topics, opened before looking at the state, so that a
        !! change between the look and the wait is not lost; -1 when unavailable.
        integer(c_int) :: ino, wd, rc
        fd = -1
        if (.not. allocated(dir)) return
        ino = c_inotify_init1(IN_CLOEXEC)
        if (ino < 0) return
        wd = c_inotify_add_watch(ino, dir//c_null_char, ior(IN_CLOSE_WRITE, IN_MOVED_TO))
        if (wd < 0) then
            rc = c_close(ino)
            return
        end if
        fd = int(ino)
    end function events_watch_open

    logical function events_wait(fd, topic, timeout_ms) result(changed)
        !! Sleeps until `topic` changes or `timeout_ms` passes (negative: no
        !! limit); true on a change.
        integer, intent(in) :: fd, timeout_ms
        character(len=*), intent(in) :: topic
        type(pollfd_t) :: fds(1)
        character(kind=c_char) :: buf(4096)
        integer(c_intptr_t) :: got
        integer(c_int) :: rc

        changed = .false.
        if (fd < 0) return
        do
            fds(1) = pollfd_t(int(fd, c_int), POLLIN, 0_c_short)
            rc = c_poll(fds, 1_c_long, int(timeout_ms, c_int))
            if (rc <= 0) return
            got = c_read(int(fd, c_int), buf, int(size(buf), c_size_t))
            if (got <= 0) return
            if (names(buf(1:got), topic)) then
                changed = .true.
                return
            end if
        end do
    end function events_wait

    subroutine events_watch_close(fd)
        integer, intent(in) :: fd
        integer(c_int) :: rc
        if (fd >= 0) rc = c_close(int(fd, c_int))
    end subroutine events_watch_close

    logical function names(bytes, topic)
        !! Whether the inotify records in `bytes` name the file `topic`.
        character(kind=c_char), intent(in) :: bytes(:)
        character(len=*), intent(in) :: topic
        character(len=size(bytes)) :: s
        integer :: k
        do k = 1, size(bytes)
            s(k:k) = bytes(k)
        end do
        names = index(s, topic//c_null_char) > 0
    end function names

end module fy_events
