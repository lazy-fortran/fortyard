program test_executor
    !! The hand-over between a runner's controller and its executor: the
    !! executor checks out the commit and runs the check; the controller hears
    !! the output while it grows and gets the output, the exit status and the
    !! build directory's presence; a rerun runs without the build directory; a
    !! time limit ends a check as timed out; a cancel marker stops a check and
    !! every process it started; an executor that dies during a check is
    !! reported by the next one as lost; the controller gives a check back that
    !! no executor takes or that is not reported after its time limit, has a
    !! check stopped past its time limit and reports it as timed out, and
    !! takes no cancel marker for a cancel that it did not write itself; a
    !! link in place of the hand-over directory does not lead it elsewhere,
    !! and the executor removes work trees not used for 30 days.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_executor, only: executor_main, executor_run
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, &
        SIGKILL
    implicit none

    character(len=*), parameter :: root = 'build/test_executor'
    character(len=*), parameter :: git = 'env GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 '// &
        'git -c user.name=T -c user.email=t@example.org '
    character(len=:), allocatable :: dir, src, work, out, state, seen
    character(len=4096) :: cwd
    integer :: stat, ex, killer, code, i
    logical :: timed_out, build, gone
    integer(int64) :: t0, t1, rate

    ex = 0
    killer = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    dir = trim(cwd)//'/'//root//'/shared'
    src = trim(cwd)//'/'//root//'/src.git'
    work = dir//'/fy-work/demo'
    call execute_command_line('rm -rf '//root//' && mkdir -p '//root//' && git init -q --bare '// &
        src//' && git init -q -b main '//root//'/w && mkdir '//root//'/w/.fortyard && cd '// &
        root//'/w && '// &
        branch('out', '#!/bin/sh\necho hello from the executor\n'// &
        'echo "class $FORTYARD_RUNNER_CLASS"'// &
        '\nif [ -d build ]; then echo "build there: yes"; else echo "build there: no"; fi\n'// &
        'mkdir -p build\nexit 3\n')//' && '// &
        branch('slow', '#!/bin/sh\necho started\nsleep 60\n')//' && '// &
        branch('wait', '#!/bin/sh\nsleep 300 & echo $! > pid\necho waiting\nwait\necho never\n'), &
        exitstat=stat)
    call check(stat == 0, 'the source repository is prepared')

    ! The executor removes work trees not used for 30 days.
    call execute_command_line('mkdir -p '//dir//'/fy-work/old && touch -d "40 days ago" '// &
        dir//'/fy-work/old', exitstat=stat)
    call check(stat == 0, 'an old work tree is there')

    ex = start_executor()

    gone = .false.
    do i = 1, 600
        inquire (file=dir//'/fy-work/old/.', exist=gone)
        gone = .not. gone
        if (gone) exit
        call proc_sleep_ms(100)
    end do
    call check(gone, 'the executor removes a work tree not used for 30 days')

    ! A link in place of the hand-over directory leads the controller
    ! nowhere: it neither removes nor writes files where the link points.
    call execute_command_line('rm -rf '//dir//'/exchange && mkdir -p '//root//'/victim && '// &
        'for f in status log sent cancel output; do echo mine > '//root//'/victim/$f; done && '// &
        'ln -s '//trim(cwd)//'/'//root//'/victim '//dir//'/exchange', exitstat=stat)
    call check(stat == 0, 'the hand-over directory is a link')

    ! Output, exit status and the build directory.
    seen = ''
    call executor_run(dir, '1', 'demo', src, 'refs/heads/out', work, .false., 60, 'testclass', &
        .false., out, code, timed_out, build, state, sink=sink)
    call check(state == 'done', 'a check is done: '//state//' '//out)
    call check(code == 3, 'the exit status comes back')
    call check(index(out, 'hello from the executor') > 0, 'the output comes back: '//out)
    call check(index(out, 'class testclass') > 0, 'the check sees its runner class')
    call check(index(out, 'build there: no') > 0, 'a fresh work tree has no build directory')
    call check(build, 'the build directory the check made is reported')
    call check(.not. timed_out, 'a check that ends is not timed out')
    call check(index(seen, 'hello from the executor') > 0, 'the sink hears the output: '//seen)
    call check(no_handover_files(), 'the hand-over files are removed')
    call execute_command_line('[ "$(cat '//root//'/victim/status '//root//'/victim/log '// &
        root//'/victim/sent '//root//'/victim/cancel '//root//'/victim/output)" = '// &
        '"$(printf "mine\nmine\nmine\nmine\nmine")" ] && [ $(ls '//root//'/victim | wc -l) = 5 ]', &
        exitstat=stat)
    call check(stat == 0, 'the files where the link pointed are untouched')

    ! The rerun: the same work tree without its build directory.
    call executor_run(dir, '1', 'demo', src, 'refs/heads/out', work, .false., 60, 'testclass', &
        .true., out, code, timed_out, build, state, sink=sink)
    call check(state == 'done' .and. code == 3, 'the rerun is done')
    call check(index(out, 'build there: no') > 0, 'the rerun has no build directory: '//out)

    ! A time limit.
    call executor_run(dir, '2', 'demo', src, 'refs/heads/slow', work, .false., 2, 'linux', &
        .false., out, code, timed_out, build, state, sink=sink)
    call check(state == 'done' .and. timed_out, 'the time limit ends the check: '//state//' '//out)
    call check(index(out, 'started') > 0, 'a timed out check keeps its output')

    ! Cancelled while it runs: stopped with every process it started.
    seen = ''
    call system_clock(t0, rate)
    call executor_run(dir, '3', 'demo', src, 'refs/heads/wait', work, .false., 600, 'linux', &
        .false., out, code, timed_out, build, state, sink=sink, cancel=waiting)
    call system_clock(t1)
    call check(state == 'cancelled', 'the cancel marker stops the check: '//state)
    call check(t1 - t0 < 120*rate, 'the check is stopped long before its end')
    call check(index(out, 'never') == 0, 'the stopped check did not go on')
    gone = .false.
    do i = 1, 100
        ! A process that ended may wait as a zombie for whoever adopted it.
        call execute_command_line('p=$(cat '//work//'/pid); s=$(cut -d" " -f3 /proc/$p/stat '// &
            '2>/dev/null); [ -z "$s" ] || [ "$s" = Z ]', exitstat=stat)
        gone = stat == 0
        if (gone) exit
        call proc_sleep_ms(100)
    end do
    call check(gone, 'no process of the cancelled check is left')

    ! The executor dies during a check: the next one reports it as lost.
    killer = proc_fork()
    if (killer == 0) then
        do
            call execute_command_line('grep -qs waiting '//dir//'/exchange/output', exitstat=stat)
            if (stat == 0) exit
            call proc_sleep_ms(100)
        end do
        call proc_kill(ex, SIGKILL)
        call executor_main(dir)
        call proc_exit(0)
    end if
    call executor_run(dir, '4', 'demo', src, 'refs/heads/wait', work, .false., 600, 'linux', &
        .false., out, code, timed_out, build, state, sink=sink)
    call check(state == 'lost', 'a check of an executor that died is lost: '//state//' '//out)
    call proc_wait(ex, stat)
    ex = 0

    call proc_kill(killer, SIGKILL)
    call proc_wait(killer, stat)
    killer = 0

    ! From here no executor runs; stand-ins show what the controller does.
    ! None takes the check: it is given back.
    call executor_run(dir, '5', 'demo', src, 'refs/heads/out', work, .false., 60, 'linux', &
        .false., out, code, timed_out, build, state, waits=[1, 1, 2])
    call check(state == 'lost' .and. index(out, 'No executor took the check') > 0, &
        'a check no executor takes is lost: '//state//' '//out)
    call check(no_handover_files(), 'the hand-over is withdrawn')

    ! One takes it and never reports: given back after the time limit.
    ex = stand_in('')
    call executor_run(dir, '6', 'demo', src, 'refs/heads/out', work, .false., 1, 'linux', &
        .false., out, code, timed_out, build, state, waits=[60, 1, 2])
    call check(state == 'lost' .and. index(out, 'did not report') > 0, &
        'a check that is not reported after its time limit is lost: '//state//' '//out)
    call proc_wait(ex, stat)

    ! Past its time limit the controller has the check stopped, and that is
    ! a time limit, not a cancel.
    ex = stand_in('while [ ! -e $x/cancel ]; do sleep 0.1; done; ')
    call executor_run(dir, '7', 'demo', src, 'refs/heads/out', work, .false., 1, 'linux', &
        .false., out, code, timed_out, build, state, waits=[60, 1, 60])
    call check(state == 'done' .and. timed_out, 'a check stopped past its time limit is '// &
        'timed out: '//state//' '//out)
    call check(index(out, 'stopped') > 0, 'its output comes back')
    call proc_wait(ex, stat)

    ! A cancel marker the check wrote itself only ends the check.
    ex = stand_in('touch $x/cancel; ')
    call executor_run(dir, '8', 'demo', src, 'refs/heads/out', work, .false., 60, 'linux', &
        .false., out, code, timed_out, build, state, waits=[60, 1, 60])
    call check(state == 'done' .and. code == 143 .and. .not. timed_out, 'a cancel marker '// &
        'the controller did not write is no cancel: '//state)
    call proc_wait(ex, stat)
    ex = 0
    call execute_command_line('rm -rf '//root)
    print '(a)', 'executor: all checks passed'

contains

    function branch(name, script) result(cmd)
        !! Commits `script` as .fortyard/check on branch `name` of the source.
        character(len=*), intent(in) :: name, script
        character(len=:), allocatable :: cmd
        cmd = 'printf '''//script//''' > .fortyard/check && chmod +x .fortyard/check && '// &
            git//'checkout -q -B '//name//' && '//git//'add -A && '//git//'commit -q -m '//name// &
            ' && '//git//'push -q '//src//' '//name
    end function branch

    integer function start_executor() result(pid)
        pid = proc_fork()
        if (pid == 0) then
            call executor_main(dir)
            call proc_exit(0)
        end if
    end function start_executor

    integer function stand_in(before) result(pid)
        !! An executor in a few lines of shell: takes the hand-over, writes
        !! output, runs `before` and reports the check stopped (unless
        !! `before` is empty: then it reports nothing).
        character(len=*), intent(in) :: before
        integer :: st
        character(len=:), allocatable :: x
        x = dir//'/exchange'
        pid = proc_fork()
        if (pid == 0) then
            call execute_command_line('x='//x//'; while [ ! -f $x/job ]; do sleep 0.1; done; '// &
                'mv $x/job $x/taken && id=$(sed -n "s/^id=//p" $x/taken) && '// &
                'echo started > $x/output && '// &
                merge('true                         ', 'exit 0                       ', &
                len(before) > 0)//'; '//before// &
                'echo stopped > $x/log && printf "id=%s\ncode=143\ntimed_out=0\ncancelled=1\n'// &
                'lost=0\nbuild=0\n" "$id" > $x/status.new && mv $x/status.new $x/status', &
                exitstat=st)
            call proc_exit(0)
        end if
    end function stand_in

    subroutine sink(path, used)
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: used
        integer(int64) :: n
        integer :: u, ios
        inquire (file=path, size=n)
        used = 0
        if (n <= 0) return
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        deallocate (seen)
        allocate (character(len=int(n)) :: seen)
        read (u, iostat=ios) seen
        close (u)
        used = n
    end subroutine sink

    logical function waiting()
        waiting = index(seen, 'waiting') > 0
    end function waiting

    logical function no_handover_files()
        integer :: st
        call execute_command_line('[ -z "$(ls -A '//dir//'/exchange)" ]', exitstat=st)
        no_handover_files = st == 0
    end function no_handover_files

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (ex > 0) call proc_kill(ex, SIGKILL)
            if (killer > 0) call proc_kill(killer, SIGKILL)
            error stop 1
        end if
    end subroutine check

end program test_executor
