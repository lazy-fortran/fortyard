module fy_fetch
    !! The server's stand-in for the browser's HTTP requests (web/fy_fetch.f90).
    !! While the server composes a page, a GET request is answered at once by
    !! the server's own routing (`fetch_serve`), and the answers are kept so the
    !! page can carry them to the browser (`fetch_served_json`). Otherwise a
    !! request is never sent and never finishes.
    use fx_json_build, only: json_escape_string
    implicit none
    private

    public :: request_t, fetch_start, fetch_ready, fetch_serve, fetch_served_json
    public :: fetch_handler_i

    type :: request_t
        integer :: id = 0
        integer :: status = 0
        character(len=:), allocatable :: body
    end type request_t

    abstract interface
        subroutine fetch_handler_i(path, status, body)
            character(len=*), intent(in) :: path
            integer, intent(out) :: status
            character(len=:), allocatable, intent(out) :: body
        end subroutine fetch_handler_i
    end interface

    procedure(fetch_handler_i), pointer :: handler => null()
    character(len=:), allocatable :: served

contains

    subroutine fetch_serve(h)
        !! Answers GET requests with `h` from now on, forgetting earlier
        !! answers; without `h`, requests are no longer answered.
        procedure(fetch_handler_i), optional :: h
        handler => null()
        if (present(h)) handler => h
        served = ''
    end subroutine fetch_serve

    function fetch_served_json() result(s)
        !! The answers given since `fetch_serve`, as the browser application
        !! reads them: [{"p":PATH,"s":STATUS,"b":BODY},...].
        character(len=:), allocatable :: s
        if (.not. allocated(served)) served = ''
        s = '['//served//']'
    end function fetch_served_json

    subroutine fetch_start(req, method, path, body)
        type(request_t), intent(inout) :: req
        character(len=*), intent(in) :: method, path
        character(len=*), intent(in), optional :: body
        character(len=12) :: num

        req%id = 0
        req%status = 0
        req%body = ''
        if (.not. associated(handler) .or. method /= 'GET') return
        call handler(path, req%status, req%body)
        req%id = -1
        write (num, '(i0)') req%status
        if (.not. allocated(served)) served = ''
        if (len(served) > 0) served = served//','
        served = served//'{"p":"'//json_escape_string(path)//'","s":'//trim(num)// &
            ',"b":"'//json_escape_string(req%body)//'"}'
    end subroutine fetch_start

    logical function fetch_ready(req)
        type(request_t), intent(inout) :: req
        fetch_ready = req%id == -1
        if (fetch_ready) req%id = 0
    end function fetch_ready

end module fy_fetch
