module fy_time
    !! Times of commits and fetches as people read them.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortui, only: ui_str
    implicit none
    private

    public :: ago, when, epoch, day_of

contains

    function ago(stamp) result(s)
        !! How long ago an ISO 8601 time was ("5 minutes ago"); the date when
        !! it is a month or more.
        character(len=*), intent(in) :: stamp
        character(len=:), allocatable :: s
        integer :: v(8), d, m
        integer(int64) :: t, now

        s = when(stamp)
        if (len(stamp) < 19) return
        t = epoch(digits(stamp(1:4)), digits(stamp(6:7)), digits(stamp(9:10)), &
            digits(stamp(12:13)), digits(stamp(15:16)), digits(stamp(18:19)))
        if (len(stamp) >= 25) then
            if (stamp(20:20) == '+' .or. stamp(20:20) == '-') then
                m = 60 * digits(stamp(21:22)) + digits(stamp(24:25))
                if (stamp(20:20) == '+') m = -m
                t = t + 60_int64 * m
            end if
        end if
        call date_and_time(values=v)
        if (v(1) < 1970) return
        now = epoch(v(1), v(2), v(3), v(5), v(6), v(7))
        if (abs(v(4)) <= 1440) now = now - 60_int64 * v(4)
        d = int(max(0_int64, now - t))
        if (d < 60) then
            s = 'just now'
        else if (d < 120) then
            s = '1 minute ago'
        else if (d < 3600) then
            s = ui_str(d / 60)//' minutes ago'
        else if (d < 7200) then
            s = '1 hour ago'
        else if (d < 86400) then
            s = ui_str(d / 3600)//' hours ago'
        else if (d < 172800) then
            s = 'yesterday'
        else if (d < 30 * 86400) then
            s = ui_str(d / 86400)//' days ago'
        else
            s = 'on '//day_of(stamp)
        end if
    end function ago

    pure integer(int64) function epoch(y, mo, d, h, mi, se)
        !! Seconds since 1970-01-01 of a UTC time (civil-from-days, Howard Hinnant).
        integer, intent(in) :: y, mo, d, h, mi, se
        integer(int64) :: yy, era, yoe, doy, doe
        yy = y
        if (mo <= 2) yy = yy - 1
        era = merge(yy, yy - 399, yy >= 0) / 400
        yoe = yy - era * 400
        doy = (153 * (mo + merge(-3, 9, mo > 2)) + 2) / 5 + d - 1
        doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
        epoch = 86400_int64 * (era * 146097 + doe - 719468) + 3600 * h + 60 * mi + se
    end function epoch

    pure integer function digits(s)
        character(len=*), intent(in) :: s
        integer :: i
        digits = 0
        do i = 1, len(s)
            if (s(i:i) < '0' .or. s(i:i) > '9') exit
            digits = 10 * digits + iachar(s(i:i)) - 48
        end do
    end function digits

    function day_of(stamp) result(s)
        !! "Nov 9, 2026" from an ISO 8601 time.
        character(len=*), intent(in) :: stamp
        character(len=:), allocatable :: s
        character(len=3), parameter :: MONTHS(12) = [character(len=3) :: 'Jan', 'Feb', &
            'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        integer :: m

        s = stamp(1:min(10, len(stamp)))
        if (len(stamp) < 10) return
        m = digits(stamp(6:7))
        if (m < 1 .or. m > 12) return
        s = MONTHS(m)//' '//ui_str(digits(stamp(9:10)))//', '//stamp(1:4)
    end function day_of

    function when(stamp) result(s)
        !! "2026-11-09 14:34" from an ISO 8601 time.
        character(len=*), intent(in) :: stamp
        character(len=:), allocatable :: s
        s = stamp(1:min(16, len(stamp)))
        if (len(s) >= 11) s(11:11) = ' '
    end function when

end module fy_time
