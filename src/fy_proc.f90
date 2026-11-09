module fy_proc
    !! Processes through the C library, for the pre-forked worker pool. Linux only.
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: proc_fork, proc_wait_any, proc_wait, proc_kill, proc_exit, proc_pid
    public :: proc_die_with_parent, proc_sleep_ms, proc_run
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

        function c_pipe(fds) bind(C, name='pipe') result(rc)
            import :: c_int
            integer(c_int), intent(out) :: fds(2)
            integer(c_int) :: rc
        end function c_pipe

        function c_dup2(old, new) bind(C, name='dup2') result(rc)
            import :: c_int
            integer(c_int), value :: old, new
            integer(c_int) :: rc
        end function c_dup2

        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close

        function c_open(path, flags) bind(C, name='open') result(fd)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags
            integer(c_int) :: fd
        end function c_open

        function c_chdir(path) bind(C, name='chdir') result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: path
            integer(c_int) :: rc
        end function c_chdir

        function c_execvp(file, argv) bind(C, name='execvp') result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: file
            type(c_ptr), intent(in) :: argv(*)
            integer(c_int) :: rc
        end function c_execvp

        function c_read(fd, buf, n) bind(C, name='read') result(got)
            import :: c_int, c_char, c_size_t, c_intptr_t
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
            integer(c_intptr_t) :: got
        end function c_read

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


    subroutine proc_run(argv, output, status, max_bytes, dir)
        !! Runs argv(1) with the arguments argv(2:), found on PATH, without a
        !! shell, and captures standard output up to `max_bytes`; standard
        !! input and error are /dev/null. `status` is the exit code, 128 + the
        !! signal number when a signal ended it, or -1 when it could not start.
        !! Everything the child needs is prepared before `fork`: between fork
        !! and exec only async-signal-safe C calls run, as other threads may
        !! hold locks.
        character(len=*), intent(in) :: argv(:)
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: status
        integer, intent(in) :: max_bytes
        character(len=*), intent(in), optional :: dir
        character(kind=c_char), allocatable, target :: strings(:), cdir(:)
        character(kind=c_char), target :: devnull(10)
        type(c_ptr), allocatable, target :: ptrs(:)
        character(kind=c_char) :: chunk(65536)
        integer(c_int) :: fds(2), rc, st, nullfd
        integer(c_intptr_t) :: got
        integer :: i, k, pos, n, pid, kept

        output = ''
        status = -1
        n = 0
        do i = 1, size(argv)
            n = n + len_trim(argv(i)) + 1
        end do
        allocate (strings(n), ptrs(size(argv) + 1))
        pos = 1
        do i = 1, size(argv)
            ptrs(i) = c_loc(strings(pos))
            do k = 1, len_trim(argv(i))
                strings(pos) = argv(i)(k:k)
                pos = pos + 1
            end do
            strings(pos) = c_null_char
            pos = pos + 1
        end do
        ptrs(size(argv) + 1) = c_null_ptr
        if (present(dir)) then
            allocate (cdir(len_trim(dir) + 1))
            do k = 1, len_trim(dir)
                cdir(k) = dir(k:k)
            end do
            cdir(len_trim(dir) + 1) = c_null_char
        end if
        devnull = transfer('/dev/null'//c_null_char, devnull)
        if (c_pipe(fds) /= 0) return

        pid = proc_fork()
        if (pid < 0) then
            rc = c_close(fds(1))
            rc = c_close(fds(2))
            return
        end if
        if (pid == 0) then
            nullfd = c_open(devnull, 0_c_int)
            rc = c_dup2(nullfd, 0_c_int)
            rc = c_dup2(fds(2), 1_c_int)
            rc = c_dup2(nullfd, 2_c_int)
            rc = c_close(fds(1))
            if (allocated(cdir)) then
                if (c_chdir(c_loc(cdir)) /= 0) call c_exit_now(127_c_int)
            end if
            rc = c_execvp(ptrs(1), ptrs)
            call c_exit_now(127_c_int)
        end if

        rc = c_close(fds(2))
        kept = 0
        do
            got = c_read(fds(1), chunk, int(size(chunk), c_size_t))
            if (got <= 0) exit
            ! Past the limit the child is still drained, so it never blocks.
            if (kept < max_bytes) then
                k = min(int(got), max_bytes - kept)
                output = output//transfer(chunk(1:k), repeat(' ', k))
                kept = kept + k
            end if
        end do
        rc = c_close(fds(1))
        rc = c_waitpid(int(pid, c_int), st, 0_c_int)
        if (iand(st, 127) == 0) then
            status = iand(ishft(st, -8), 255)
        else
            status = 128 + iand(st, 127)
        end if
    end subroutine proc_run

end module fy_proc
