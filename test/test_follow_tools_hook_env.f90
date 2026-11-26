program test_follow_tools_hook_env
    !! follow-tools.sh started from a Git hook inherits GIT_DIR, GIT_WORK_TREE
    !! and GIT_INDEX_FILE of the committing checkout. It must still update its
    !! own clone and leave that checkout's HEAD alone.
    implicit none
    character(len=*), parameter :: d = 'build/test_follow_tools_hook_env'
    character(len=:), allocatable :: head_before, head_after, origin_head
    integer :: stat

    call execute_command_line('rm -rf '//d//' && mkdir -p '//d//'/bin '//d//'/inst && '// &
        'cd '//d//' && g="git -c user.name=t -c user.email=t@t -c init.defaultBranch=main" && '// &
        '$g init -q origin && echo 1 > origin/f && $g -C origin add f && '// &
        '$g -C origin commit -qm one && mkdir home && '// &
        '$g clone -q origin home/src/fo && echo 2 > origin/f && $g -C origin commit -qam two && '// &
        '$g init -q victim && echo v > victim/f && $g -C victim add f && '// &
        '$g -C victim commit -qm v1 && $g -C victim checkout -qb topic && '// &
        'echo w > victim/f && $g -C victim commit -qam v2 && '// &
        '$g -C victim remote add origin "$PWD/origin" && '// &
        'printf ''#!/bin/sh\n[ "$1" = build ] || exit 0\nmkdir -p build/fo/app\n'// &
        'printf "#!/bin/sh\\necho fo\\n" > build/fo/app/fo\nchmod +x build/fo/app/fo\n'' '// &
        '> bin/fo && chmod +x bin/fo', exitstat=stat)
    if (stat /= 0) error stop 'fixture'
    head_before = shell_line('git -C '//d//'/victim rev-parse HEAD')
    origin_head = shell_line('git -C '//d//'/origin rev-parse HEAD')

    call execute_command_line('s="$PWD/scripts/follow-tools.sh" && cd '//d// &
        ' && t="$PWD" && cd victim && GIT_DIR="$PWD/.git" GIT_WORK_TREE="$PWD" '// &
        'GIT_INDEX_FILE="$PWD/.git/index" FY_TOOLS_HOME="$t/home" '// &
        'FY_TOOLS_BIN="$t/inst" FY_TOOLS=fo PATH="$t/bin:$PATH" sh "$s" > /dev/null 2>&1')

    head_after = shell_line('git -C '//d//'/victim symbolic-ref -q HEAD; '// &
        'git -C '//d//'/victim rev-parse HEAD')
    call check(head_after == 'refs/heads/topic', 'the committing checkout stays on its branch')
    call check(shell_line('git -C '//d//'/victim rev-parse HEAD') == head_before, &
        'the committing checkout keeps its commit')
    call check(shell_line('cat '//d//'/home/fo.commit') == origin_head, &
        'the follower builds main of its own clone')
    call check(shell_line(d//'/inst/fo') == 'fo', 'the built fo is installed')
    call execute_command_line('rm -rf '//d)
    print '(a)', 'follow_tools_hook_env: all checks passed'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            print '(a)', 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

    function shell_line(command) result(line)
        !! First line of the output of command.
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: line
        character(len=4096) :: buffer
        integer :: u, ios
        call execute_command_line('('//command//') > build/.follow_tools_hook_env_out 2>/dev/null', &
            exitstat=ios)
        open (newunit=u, file='build/.follow_tools_hook_env_out', action='read', iostat=ios)
        buffer = ''
        if (ios == 0) read (u, '(a)', iostat=ios) buffer
        if (ios == 0) close (u, status='delete')
        line = trim(buffer)
    end function shell_line
end program test_follow_tools_hook_env
