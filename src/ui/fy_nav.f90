module fy_nav
    !! The server's stand-in for the browser's location, history and
    !! preferences (web/fy_nav.f90): a page the server composes never navigates.
    implicit none
    private

    public :: nav_push, nav_popped, nav_title, nav_assign, nav_set_pref, nav_copy

contains

    subroutine nav_push(path)
        character(len=*), intent(in) :: path
    end subroutine nav_push

    logical function nav_popped(path)
        character(len=:), allocatable, intent(out) :: path
        path = ''
        nav_popped = .false.
    end function nav_popped

    subroutine nav_title(title)
        character(len=*), intent(in) :: title
    end subroutine nav_title

    subroutine nav_assign(path)
        character(len=*), intent(in) :: path
    end subroutine nav_assign

    subroutine nav_set_pref(name, value)
        character(len=*), intent(in) :: name, value
    end subroutine nav_set_pref

    subroutine nav_copy(text)
        character(len=*), intent(in) :: text
    end subroutine nav_copy

end module fy_nav
