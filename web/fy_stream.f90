module fy_stream
    !! Server-sent event streams: the browser keeps the connection, counts the
    !! events of a topic and wakes the frame loop on each one, so a live page
    !! learns of changes without asking.
    use, intrinsic :: iso_c_binding, only: &
        c_char, &
        c_int
    implicit none
    private

    public :: &
        stream_close, &
        stream_count, &
        stream_count_of, &
        stream_open

    interface
        integer(c_int) function js_open(p, np, t, nt) bind(C, name="fy_stream_open")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: p(*), t(*)
            integer(c_int), value :: np, nt
        end function js_open

        integer(c_int) function js_count(id) bind(C, name="fy_stream_count")
            import :: c_int
            integer(c_int), value :: id
        end function js_count

        integer(c_int) function js_count_of(id, t, nt) bind(C, name="fy_stream_count_of")
            import :: c_int, c_char
            integer(c_int), value :: id, nt
            character(kind=c_char), intent(in) :: t(*)
        end function js_count_of

        subroutine js_close(id) bind(C, name="fy_stream_close")
            import :: c_int
            integer(c_int), value :: id
        end subroutine js_close
    end interface

contains

    integer function stream_open(path, topic)
        !! Opens the stream at `path` (an application path) for events `topic`,
        !! one name or several separated by commas.
        character(len=*), intent(in) :: path, topic
        stream_open = int(js_open(path, len(path, c_int), topic, len(topic, c_int)))
    end function stream_open

    integer function stream_count(id)
        !! Events of the stream so far.
        integer, intent(in) :: id
        stream_count = int(js_count(int(id, c_int)))
    end function stream_count

    integer function stream_count_of(id, topic)
        !! Events of the stream named `topic` so far.
        integer, intent(in) :: id
        character(len=*), intent(in) :: topic
        stream_count_of = int(js_count_of(int(id, c_int), topic, len(topic, c_int)))
    end function stream_count_of

    subroutine stream_close(id)
        integer, intent(in) :: id
        call js_close(int(id, c_int))
    end subroutine stream_close

end module fy_stream
