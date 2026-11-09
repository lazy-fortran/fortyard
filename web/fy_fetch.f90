module fy_fetch
    !! Asynchronous HTTP requests from the browser application to its server.
    use, intrinsic :: iso_c_binding, only: c_int, c_char
    implicit none
    private

    public :: request_t, fetch_start, fetch_ready, page_data

    type :: request_t
        integer :: id = 0
        !! HTTP status once finished; 0 when the server was not reached.
        integer :: status = 0
        character(len=:), allocatable :: body
    end type request_t

    interface
        integer(c_int) function js_start(m, nm, p, np, b, nb) bind(C, name="fy_fetch_start")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: m(*), p(*), b(*)
            integer(c_int), value :: nm, np, nb
        end function js_start

        integer(c_int) function js_status(id) bind(C, name="fy_fetch_status")
            import :: c_int
            integer(c_int), value :: id
        end function js_status

        integer(c_int) function js_length(id) bind(C, name="fy_fetch_length")
            import :: c_int
            integer(c_int), value :: id
        end function js_length

        integer(c_int) function js_page_data(name, nname, buf, cap) bind(C, name="fy_page_data")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), value :: nname
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_int), value :: cap
        end function js_page_data

        integer(c_int) function js_take(id, buf, cap) bind(C, name="fy_fetch_take")
            import :: c_int, c_char
            integer(c_int), value :: id
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_int), value :: cap
        end function js_take
    end interface

contains

    function page_data(name) result(text)
        !! The text of the page's <script type="application/json" id="NAME">, or
        !! empty: data the server sent with the page.
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: text
        character(kind=c_char), allocatable :: buf(:)
        integer :: n, k

        allocate (buf(1))
        n = int(js_page_data(name, len(name, c_int), buf, 0_c_int))
        deallocate (buf)
        allocate (buf(max(n, 1)))
        n = int(js_page_data(name, len(name, c_int), buf, int(size(buf), c_int)))
        allocate (character(len=max(n, 0)) :: text)
        do k = 1, n
            text(k:k) = buf(k)
        end do
    end function page_data

    subroutine fetch_start(req, method, path, body)
        !! Starts a request; `req%id` is 0 again once `fetch_ready` saw it finish.
        type(request_t), intent(inout) :: req
        character(len=*), intent(in) :: method, path
        character(len=*), intent(in), optional :: body
        character(len=1) :: none

        none = ' '
        req%status = 0
        req%body = ''
        if (present(body)) then
            req%id = int(js_start(method, len(method, c_int), path, len(path, c_int), &
                body, len(body, c_int)))
        else
            req%id = int(js_start(method, len(method, c_int), path, len(path, c_int), &
                none, 0_c_int))
        end if
    end subroutine fetch_start

    logical function fetch_ready(req)
        !! True once, when the request has finished; status and body are then set.
        type(request_t), intent(inout) :: req
        character(kind=c_char), allocatable :: buf(:)
        integer :: status, n, i

        fetch_ready = .false.
        if (req%id == 0) return
        status = int(js_status(int(req%id, c_int)))
        if (status < 0) return
        n = int(js_length(int(req%id, c_int)))
        allocate (buf(max(n, 1)))
        n = int(js_take(int(req%id, c_int), buf, int(size(buf), c_int)))
        req%body = repeat(' ', n)
        do i = 1, n
            req%body(i:i) = buf(i)
        end do
        req%status = status
        req%id = 0
        fetch_ready = .true.
    end function fetch_ready

end module fy_fetch
