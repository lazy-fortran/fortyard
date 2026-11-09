module fy_fetch
    !! The server's stand-in for the browser's HTTP requests (web/fy_fetch.f90):
    !! when the server composes a page, the state is already there, so a
    !! request is never sent and never finishes.
    implicit none
    private

    public :: request_t, fetch_start, fetch_ready

    type :: request_t
        integer :: id = 0
        integer :: status = 0
        character(len=:), allocatable :: body
    end type request_t

contains

    subroutine fetch_start(req, method, path, body)
        type(request_t), intent(inout) :: req
        character(len=*), intent(in) :: method, path
        character(len=*), intent(in), optional :: body
        req%id = 0
        req%status = 0
        req%body = ''
    end subroutine fetch_start

    logical function fetch_ready(req)
        type(request_t), intent(inout) :: req
        fetch_ready = .false.
    end function fetch_ready

end module fy_fetch
