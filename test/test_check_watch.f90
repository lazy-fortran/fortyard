program test_check_watch
    !! The check watcher used where there is no /proc, timeout or systemd
    !! (the macOS runner), here forced with FY_CHECK_WATCHED: the time limit
    !! ends the check's process group, also a child that ignores SIGTERM; a
    !! check whose processes pass FY_CHECK_MEMORY GiB is ended at once; a
    !! check that ends by itself keeps its exit status and output.
    use fy_land, only: land_check
    use fy_proc, only: proc_setenv
    implicit none

    character(len=*), parameter :: dir = 'build/test_check_watch_work'
    character(len=:), allocatable :: out
    integer :: code, stat
    logical :: timed_out, linux

    call proc_setenv('FY_CHECK_WATCHED', '1')
    call proc_setenv('FY_CHECK_MEMORY', '1')

    call execute_command_line('rm -rf '//dir//' && mkdir -p '//dir//'/.fortyard && '// &
        'printf ''#!/bin/sh\nsh -c "trap \\"\\" TERM; echo \\$\\$ > child; exec sleep 300" &\n'// &
        'trap "" TERM\necho started\nwait\n'' > '//dir//'/.fortyard/check && chmod +x '//dir// &
        '/.fortyard/check', exitstat=stat)
    call land_check(dir, out, code, limit=5, timed_out=timed_out, grace=1)
    call check(timed_out .and. code == 124, 'the time limit ends the check: '//out)
    call check(index(out, 'started') > 0, 'its output is kept: '//out)
    call execute_command_line('c=$(cat '//dir//'/child) && test -n "$c" && i=0; while [ $i -lt 300 ];'// &
        ' do ps -o stat= -p "$c" | grep -qv Z || exit 0; sleep 0.1; i=$((i + 1)); done; exit 1', &
        exitstat=stat)
    call check(stat == 0, 'the child that ignores SIGTERM is gone')

    call execute_command_line('printf ''#!/bin/sh\necho big\nexec python3 -c "import time; '// &
        'b = b\\"x\\" * (1536 << 20); time.sleep(60)"\n'' > '//dir//'/.fortyard/check', exitstat=stat)
    call land_check(dir, out, code, limit=120, timed_out=timed_out, grace=1)
    call check(.not. timed_out .and. code == 137 .and. index(out, 'more than 1 GiB') > 0, &
        'a check over its memory is ended and says why: '//out)

    call execute_command_line('printf "#!/bin/sh\necho quick\nexit 3\n" > '//dir//'/.fortyard/check', &
        exitstat=stat)
    call land_check(dir, out, code, limit=60, timed_out=timed_out)
    call check(.not. timed_out .and. code == 3 .and. index(out, 'quick') > 0, &
        'a check that ends by itself keeps its status and output: '//out)
    ! A process that left the check's session is killed when the check ends.
    ! Only without /proc: there it is adopted by launchd, while a Linux test
    ! harness may be a subreaper that adopts it instead.
    inquire (file='/proc/self/status', exist=linux)
    if (.not. linux) then
        call execute_command_line('printf ''#!/bin/sh\nperl -MPOSIX -e "POSIX::setsid(); '// &
            'open(my \\$f, q(>), q(daemon)); print \\$f \\$\\$; close \\$f; exec q(sleep), 300" &\n'// &
            'while [ ! -s daemon ]; do sleep 0.1; done\necho done\n'' > '//dir//'/.fortyard/check'// &
            ' && rm -f '//dir//'/daemon', exitstat=stat)
        call land_check(dir, out, code, limit=60, timed_out=timed_out, grace=1)
        call check(.not. timed_out .and. code == 0 .and. index(out, 'done') > 0, &
            'a check that leaves a daemon ends: '//out)
        call execute_command_line('d=$(cat '//dir//'/daemon) && test -n "$d" && i=0; while'// &
            ' [ $i -lt 100 ]; do ps -o stat= -p "$d" | grep -qv Z || exit 0; sleep 0.1;'// &
            ' i=$((i + 1)); done; exit 1', exitstat=stat)
        call check(stat == 0, 'the daemon it left is gone')
    end if
    print '(a)', 'check watch: all checks passed'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_check_watch
