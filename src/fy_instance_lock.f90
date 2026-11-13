module fy_instance_lock
    !! A server and offline maintenance must never open the same instance.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private
    public :: instance_lock, instance_unlock, instance_pending, instance_batch_pending

    interface
        function c_open(path, flags) bind(C, name='open') result(fd)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags
            integer(c_int) :: fd
        end function c_open
        function c_flock(fd, operation) bind(C, name='flock') result(rc)
            import :: c_int
            integer(c_int), value :: fd, operation
            integer(c_int) :: rc
        end function c_flock
        function c_fcntl(fd, command, flags) bind(C, name='fcntl') result(rc)
            import :: c_int
            integer(c_int), value :: fd, command, flags
            integer(c_int) :: rc
        end function c_fcntl
        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close
    end interface

contains

    subroutine instance_lock(data, fd, status)
        character(len=*), intent(in) :: data
        integer, intent(out) :: fd, status
        integer :: unit
        integer(c_int) :: handle, duplicate, rc

        fd = -1
        open (newunit=unit, file=data//'/instance.lock', access='stream', &
              form='unformatted', status='unknown', action='readwrite', iostat=status)
        if (status /= 0) return
        close (unit)
        handle = c_open(data//'/instance.lock'//c_null_char, 2_c_int)
        status = 1
        if (handle < 0) return
        ! Descriptor 3 is replaced by socket activation even when release exec fails.
        if (handle < 4) then
            duplicate = c_fcntl(handle, 0_c_int, 4_c_int)
            rc = c_close(handle)
            if (duplicate < 0) return
            handle = duplicate
        end if
        ! LOCK_EX | LOCK_NB; a pending operator never stalls the live server.
        rc = c_flock(handle, 6_c_int)
        if (rc /= 0) then
            rc = c_close(handle)
            return
        end if
        ! A release re-executes serve, which takes its own lock before opening SQLite.
        rc = c_fcntl(handle, 2_c_int, 1_c_int)
        if (rc /= 0) then
            rc = c_close(handle)
            return
        end if
        fd = int(handle)
        status = 0
    end subroutine instance_lock

    subroutine instance_unlock(fd)
        integer, intent(inout) :: fd
        integer(c_int) :: rc

        if (fd >= 0) rc = c_close(int(fd, c_int))
        fd = -1
    end subroutine instance_unlock

    logical function instance_pending(data)
        character(len=*), intent(in) :: data

        inquire (file=data//'/history-rewrite.pending', exist=instance_pending)
    end function instance_pending

    logical function instance_batch_pending(data)
        character(len=*), intent(in) :: data

        inquire (file=data//'/history-rewrite.batch', exist=instance_batch_pending)
    end function instance_batch_pending

end module fy_instance_lock
