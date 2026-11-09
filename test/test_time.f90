program test_time
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_time, only: ago, epoch
    implicit none

    call check(epoch(1970, 1, 1, 0, 0, 0) == 0_int64, 'epoch of 1970')
    call check(epoch(2026, 11, 9, 15, 0, 0) == 1794236400_int64, 'epoch of 2026-11-09 15:00')
    call check(epoch(2000, 3, 1, 0, 0, 0) == 951868800_int64, 'epoch after a leap day')
    call check(epoch(1999, 12, 31, 23, 59, 59) == 946684799_int64, 'epoch at the end of a year')

    call check(ago(stamp('-20 sec')) == 'just now', 'seconds ago')
    call check(ago(stamp('-5 min')) == '5 minutes ago', 'minutes ago')
    call check(ago(stamp('-3 hours')) == '3 hours ago', 'hours ago')
    call check(ago(stamp('-30 hours')) == 'yesterday', 'yesterday')
    call check(ago(stamp('-4 days')) == '4 days ago', 'days ago')
    call check(ago('2001-02-03T04:05:06+01:00') == 'on Feb 3, 2001', 'a date when long ago')
    call check(ago(offset_stamp('-2 hours')) == '2 hours ago', 'a time with a zone offset')

contains

    function stamp(rel) result(s)
        !! Now plus rel, as git writes it in UTC.
        character(len=*), intent(in) :: rel
        character(len=:), allocatable :: s
        s = shell('date -u -d "'//rel//'" +%Y-%m-%dT%H:%M:%SZ')
    end function stamp

    function offset_stamp(rel) result(s)
        !! Now plus rel, in the zone +05:30.
        character(len=*), intent(in) :: rel
        character(len=:), allocatable :: s
        s = shell('TZ=Asia/Kolkata date -d "'//rel//'" +%Y-%m-%dT%H:%M:%S+05:30')
    end function offset_stamp

    function shell(cmd) result(s)
        character(len=*), intent(in) :: cmd
        character(len=:), allocatable :: s
        character(len=64) :: line
        integer :: u
        call execute_command_line(cmd//' > build/test_time.out')
        open (newunit=u, file='build/test_time.out', action='read')
        read (u, '(a)') line
        close (u)
        s = trim(line)
    end function shell

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_time
