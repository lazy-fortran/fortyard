module fy_proc
    !! Processes through the C library, for the pre-forked worker pool. Linux only.
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: proc_fork, proc_wait_any, proc_wait, proc_kill, proc_exit, proc_pid
    public :: proc_die_with_parent, proc_sleep_ms
    public :: SIGTERM, SIGKILL

    integer, parameter :: SIGKILL = 9, SIGTERM = 15
    integer(c_int), parameter :: PR_SET_PDEATHSIG = 1

    type, bind(C) :: timespec_t
        integer(c_long) :: sec = 0
        integer(c_long) :: nsec = 0
    end type timespec_t

    interface
        function c_fork() bind(C, name='fork') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_fork

        function c_waitpid(pid, status, options) bind(C, name='waitpid') result(rc)
            import :: c_int
            integer(c_int), value :: pid
            integer(c_int), intent(out) :: status
            integer(c_int), value :: options
            integer(c_int) :: rc
        end function c_waitpid

        function c_kill(pid, sig) bind(C, name='kill') result(rc)
            import :: c_int
            integer(c_int), value :: pid, sig
            integer(c_int) :: rc
        end function c_kill

        subroutine c_exit_now(status) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: status
        end subroutine c_exit_now

        function c_getpid() bind(C, name='getpid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid

        function c_getppid() bind(C, name='getppid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getppid

        ! prctl is variadic; integer arguments pass identically on x86-64 and aarch64 Linux.
        function c_prctl(option, arg2, arg3, arg4, arg5) bind(C, name='prctl') result(rc)
            import :: c_int, c_long
            integer(c_int), value :: option
            integer(c_long), value :: arg2, arg3, arg4, arg5
            integer(c_int) :: rc
        end function c_prctl

        function c_nanosleep(req, rem) bind(C, name='nanosleep') result(rc)
            import :: c_int, timespec_t, c_ptr
            type(timespec_t), intent(in) :: req
            type(c_ptr), value :: rem
            integer(c_int) :: rc
        end function c_nanosleep
    end interface

contains

    function proc_fork() result(pid)
        !! 0 in the child, the child's pid in the parent, negative on failure.
        !! Callers flush Fortran units first so buffered output is not duplicated.
        integer :: pid

        pid = int(c_fork())
    end function proc_fork

    subroutine proc_wait_any(pid, status)
        !! Waits for any child; pid is negative when there is none left.
        integer, intent(out) :: pid, status
        integer(c_int) :: st

        pid = int(c_waitpid(-1_c_int, st, 0_c_int))
        status = int(st)
    end subroutine proc_wait_any

    subroutine proc_wait(pid, status)
        integer, intent(in) :: pid
        integer, intent(out) :: status
        integer(c_int) :: st, rc

        rc = c_waitpid(int(pid, c_int), st, 0_c_int)
        status = int(st)
    end subroutine proc_wait

    subroutine proc_kill(pid, sig)
        integer, intent(in) :: pid, sig
        integer(c_int) :: rc

        rc = c_kill(int(pid, c_int), int(sig, c_int))
    end subroutine proc_kill

    subroutine proc_exit(status)
        !! Leaves a forked child without running the parent's exit handlers.
        integer, intent(in) :: status

        call c_exit_now(int(status, c_int))
    end subroutine proc_exit

    function proc_pid() result(pid)
        integer :: pid

        pid = int(c_getpid())
    end function proc_pid

    function proc_die_with_parent(parent) result(ok)
        !! Asks the kernel to send SIGTERM when the parent exits; false if it already has.
        integer, intent(in) :: parent
        logical :: ok

        ok = c_prctl(PR_SET_PDEATHSIG, int(SIGTERM, c_long), 0_c_long, 0_c_long, &
            0_c_long) == 0 .and. int(c_getppid()) == parent
    end function proc_die_with_parent

    subroutine proc_sleep_ms(ms)
        integer, intent(in) :: ms
        type(timespec_t) :: ts
        integer(c_int) :: rc

        ts%sec = int(ms / 1000, c_long)
        ts%nsec = int(mod(ms, 1000), c_long) * 1000000_c_long
        rc = c_nanosleep(ts, c_null_ptr)
    end subroutine proc_sleep_ms

end module fy_proc
