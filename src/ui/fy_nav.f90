module fy_nav
    !! The server's stand-in for the browser's location, history and
    !! preferences (web/fy_nav.f90): a page the server composes never navigates.
    implicit none
    private

    public :: &
        nav_assign, &
        nav_at_end, &
        nav_copy, &
        nav_popped, &
        nav_push, &
        nav_scroll_end, &
        nav_set_pref, &
        nav_title

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

    logical function nav_at_end()
        !! A page the server composes starts at its end.
        nav_at_end = .true.
    end function nav_at_end

    subroutine nav_scroll_end()
    end subroutine nav_scroll_end

end module fy_nav
