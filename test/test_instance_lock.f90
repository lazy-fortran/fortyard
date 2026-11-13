program test_instance_lock
    use fy_instance_lock, only: instance_lock, instance_unlock, instance_pending
    use fy_proc, only: proc_exit, proc_fork, proc_wait, proc_reexec, fd_close, proc_run
    use fy_posix, only: tcp_listen, sock_close
    implicit none
    character(len=*), parameter :: data = 'build/test_instance_lock_data'
    character(len=:), allocatable :: out, fy
    character(len=4096) :: cwd
    integer :: fd, other, status, child, outcome, unit, listening

    call execute_command_line('rm -rf '//data//' && mkdir -p '//data, exitstat=status)
    if (status /= 0) stop 1
    call getcwd(cwd)
    call proc_run([character(len=256) :: 'sh', '-c', &
        'ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1'], out, status, 4096)
    if (status /= 0 .or. len(out) < 2) error stop 'the native application is built'
    fy = trim(cwd)//'/'//out(:len(out) - 1)
    open (newunit=unit, file=data//'/fortyard.db', status='new')
    close (unit)
    call instance_lock(data, fd, status)
    if (status /= 0) error stop 'first instance takes the lock'
    call startup('already running or under maintenance')
    child = proc_fork()
    if (child == 0) then
        call instance_unlock(fd)
        call instance_lock(data, other, status)
        if (status == 0) call proc_exit(1)
        call proc_exit(0)
    end if
    call proc_wait(child, outcome)
    if (outcome /= 0) error stop 'another process cannot open a live instance'
    call instance_unlock(fd)
    call instance_lock(data, other, status)
    if (status /= 0) error stop 'offline maintenance takes the released lock'
    call instance_unlock(other)
    call fd_close(3)
    call instance_lock(data, fd, status)
    if (status /= 0) error stop 'release fallback fixture takes instance lock'
    call tcp_listen('127.0.0.1', 0, listening, status)
    if (status /= 0) error stop 'release fallback fixture opens a socket'
    call proc_reexec('/nonexistent-fortyard-test-release', &
        [character(len=32) :: 'fy', 'serve'], listening)
    child = proc_fork()
    if (child == 0) then
        call instance_unlock(fd)
        call instance_lock(data, other, status)
        if (status == 0) call proc_exit(1)
        call proc_exit(0)
    end if
    call proc_wait(child, outcome)
    if (outcome /= 0) error stop 'a failed release exec retains the live instance lock'
    call sock_close(listening)
    if (listening /= 3) call fd_close(3)
    call instance_unlock(fd)
    if (instance_pending(data)) error stop 'no pending recovery initially'
    open (newunit=unit, file=data//'/history-rewrite.pending', status='new')
    close (unit)
    if (.not. instance_pending(data)) error stop 'an incomplete rewrite needs recovery'
    call startup('interrupted history rewrite needs admin recovery')
    call execute_command_line('rm '//data//'/history-rewrite.pending', exitstat=status)
    if (status /= 0) stop 1
    open (newunit=unit, file=data//'/history-rewrite.batch', status='new')
    close (unit)
    call startup('interrupted history rewrite needs admin recovery')
    call execute_command_line('rm -rf '//data, exitstat=status)
    if (status /= 0) stop 1

contains

    subroutine startup(expected)
        character(len=*), intent(in) :: expected

        call proc_run([character(len=4096) :: 'sh', '-c', &
            'timeout 5 "$1" serve --data "$2" --listen 127.0.0.1:0 2>&1', &
            'startup-lock-test', fy, data], out, status, 4096)
        if (status == 0 .or. index(out, expected) == 0) then
            print '(a)', out
            error stop 'serve refuses an instance that is locked or awaiting recovery'
        end if
    end subroutine startup
end program test_instance_lock
