program test_follow_tools
    !! fy starts an installed follow-tools.sh in the background when its last
    !! check is older than five minutes, not when it is recent, and never
    !! with FO_NO_FOLLOW=1.
    implicit none
    character(len=*), parameter :: state = 'build/test_follow_tools_data'
    character(len=:), allocatable :: fy
    integer :: stat

    fy = shell_line('ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1')
    if (len(fy) == 0) error stop 'no fy built'
    call execute_command_line('rm -rf '//state//' && mkdir -p '//state// &
        '/src/fortyard/scripts && printf '': > "%s/ran"; : > "%s/checked"\n'' '// &
        '"$PWD/'//state//'" "$PWD/'//state//'" > '//state// &
        '/src/fortyard/scripts/follow-tools.sh', exitstat=stat)
    if (stat /= 0) error stop 'fixture'

    call invoke('')
    call check(appears(state//'/ran'), 'a first run starts the follower')
    call execute_command_line('rm -f '//state//'/ran')
    call invoke('')
    call execute_command_line('sleep 1.5')
    call check(.not. exists(state//'/ran'), 'a recent check starts nothing')
    call execute_command_line('rm -f '//state//'/checked')
    call invoke('FO_NO_FOLLOW=1 ')
    call execute_command_line('sleep 1.5')
    call check(.not. exists(state//'/ran'), 'FO_NO_FOLLOW=1 starts nothing')
    ! The stable copy beside the state is preferred to the clone's.
    call execute_command_line('rm -f '//state//'/checked '//state//'/ran && '// &
        'printf '': > "%s/ran-stable"; : > "%s/checked"\n'' "$PWD/'//state// &
        '" "$PWD/'//state//'" > '//state//'/follow-tools.sh')
    call invoke('')
    call check(appears(state//'/ran-stable'), 'the stable copy runs')
    call check(.not. exists(state//'/ran'), 'the clone''s copy does not')
    call execute_command_line('rm -rf '//state)
    print '(a)', 'follow_tools: all checks passed'

contains

    subroutine invoke(extra)
        character(len=*), intent(in) :: extra
        call execute_command_line(extra//'FY_TOOLS_HOME="$PWD/'//state//'" '//fy// &
            ' --version > /dev/null 2>&1')
    end subroutine invoke

    logical function exists(path)
        character(len=*), intent(in) :: path
        inquire (file=path, exist=exists)
    end function exists

    logical function appears(path)
        character(len=*), intent(in) :: path
        integer :: i
        appears = .false.
        do i = 1, 50
            if (exists(path)) then
                appears = .true.
                return
            end if
            call execute_command_line('sleep 0.1')
        end do
    end function appears

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            print '(a)', 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

    function shell_line(command) result(line)
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: line
        character(len=4096) :: buffer
        integer :: u, ios
        call execute_command_line('('//command//') > build/.follow_tools_out', exitstat=ios)
        open (newunit=u, file='build/.follow_tools_out', action='read', iostat=ios)
        buffer = ''
        if (ios == 0) read (u, '(a)', iostat=ios) buffer
        if (ios == 0) close (u, status='delete')
        line = trim(buffer)
    end function shell_line
end program test_follow_tools
