module fy_stream
    !! The server's stand-in for the browser's event streams (web/fy_stream.f90):
    !! a page the server composes is never told about changes.
    implicit none
    private

    public :: &
        stream_close, &
        stream_count, &
        stream_open

contains

    integer function stream_open(path, topic)
        character(len=*), intent(in) :: path, topic
        stream_open = 0
    end function stream_open

    integer function stream_count(id)
        integer, intent(in) :: id
        stream_count = 0
    end function stream_count

    subroutine stream_close(id)
        integer, intent(in) :: id
    end subroutine stream_close

end module fy_stream
