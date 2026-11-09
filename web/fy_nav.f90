module fy_nav
    !! The browser's location, history and preferences: each page of the
    !! application has its own address, and Back and Forward move between them.
    use, intrinsic :: iso_c_binding, only: c_int, c_char
    implicit none
    private

    public :: nav_push, nav_popped, nav_title, nav_location, nav_assign, nav_set_pref, &
        nav_pref, nav_copy

    interface
        subroutine js_call(what, p, np, q, nq) bind(C, name="fy_nav_call")
            import :: c_int, c_char
            integer(c_int), value :: what, np, nq
            character(kind=c_char), intent(in) :: p(*), q(*)
        end subroutine js_call

        integer(c_int) function js_get(what, name, nname, buf, cap) bind(C, name="fy_nav_get")
            import :: c_int, c_char
            integer(c_int), value :: what, nname, cap
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(out) :: buf(*)
        end function js_get
    end interface

    ! What fy_nav_call and fy_nav_get do.
    integer(c_int), parameter :: DO_PUSH = 1, DO_TITLE = 2, DO_ASSIGN = 3, DO_PREF = 4, DO_COPY = 5
    integer(c_int), parameter :: GET_LOCATION = 1, GET_POPPED = 2, GET_COOKIE = 3

contains

    subroutine nav_push(path)
        !! A new entry in the history for `path`, shown from its top.
        character(len=*), intent(in) :: path
        call js_call(DO_PUSH, path, len(path, c_int), path, 0_c_int)
    end subroutine nav_push

    logical function nav_popped(path)
        !! True once after Back or Forward, with the location now shown.
        character(len=:), allocatable, intent(out) :: path
        path = get(GET_POPPED, '')
        nav_popped = len(path) > 0
    end function nav_popped

    function nav_location() result(path)
        !! Path and query of the page.
        character(len=:), allocatable :: path
        path = get(GET_LOCATION, '')
    end function nav_location

    subroutine nav_title(title)
        character(len=*), intent(in) :: title
        call js_call(DO_TITLE, title, len(title, c_int), title, 0_c_int)
    end subroutine nav_title

    subroutine nav_assign(path)
        !! Leaves the application for `path`, a page the server makes.
        character(len=*), intent(in) :: path
        call js_call(DO_ASSIGN, path, len(path, c_int), path, 0_c_int)
    end subroutine nav_assign

    subroutine nav_set_pref(name, value)
        !! A preference kept in a cookie, so the server knows it for the next page.
        character(len=*), intent(in) :: name, value
        call js_call(DO_PREF, name, len(name, c_int), value, len(value, c_int))
    end subroutine nav_set_pref

    function nav_pref(name) result(value)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: value
        value = get(GET_COOKIE, name)
    end function nav_pref

    subroutine nav_copy(text)
        !! Puts `text` on the clipboard; a path from the root becomes this
        !! site's absolute address.
        character(len=*), intent(in) :: text
        call js_call(DO_COPY, text, len(text, c_int), text, 0_c_int)
    end subroutine nav_copy

    function get(what, name) result(s)
        integer(c_int), intent(in) :: what
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        character(kind=c_char) :: buf(4096)
        integer :: n, k

        n = int(js_get(what, name, len(name, c_int), buf, int(size(buf), c_int)))
        allocate (character(len=max(0, min(n, size(buf)))) :: s)
        do k = 1, len(s)
            s(k:k) = buf(k)
        end do
    end function get

end module fy_nav
