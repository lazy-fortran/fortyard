module fy_follow_tools
    !! Starts scripts/follow-tools.sh, which keeps fy and fo at main, when it
    !! is installed here (~/.cache/fy-tools, or $FY_TOOLS_HOME) and last checked
    !! more than five minutes ago. It runs detached; fy does not wait for it.
    implicit none
    private
    public :: follow_tools_trigger

contains

    subroutine follow_tools_trigger()
        character(len=4096) :: home, value
        character(len=:), allocatable :: state, script
        integer :: status
        logical :: found

        call get_environment_variable('FO_NO_FOLLOW', value, status=status)
        if (status == 0 .and. trim(value) == '1') return
        call get_environment_variable('FY_TOOLS_HOME', home, status=status)
        if (status == 0 .and. len_trim(home) > 0) then
            state = trim(home)
        else
            call get_environment_variable('HOME', home, status=status)
            if (status /= 0 .or. len_trim(home) == 0) return
            state = trim(home)//'/.cache/fy-tools'
        end if
        script = state//'/follow-tools.sh'
        inquire (file=script, exist=found)
        if (.not. found) then
            script = state//'/src/fortyard/scripts/follow-tools.sh'
            inquire (file=script, exist=found)
        end if
        if (.not. found) return
        call execute_command_line('[ -n "$(find "'//state//'/checked" -mmin -5 2>/dev/null)" ] '// &
            '|| { if command -v setsid >/dev/null 2>&1; then setsid sh "'//script// &
            '"; else sh "'//script//'"; fi; } </dev/null >/dev/null 2>&1 &', &
            wait=.true., exitstat=status)
    end subroutine follow_tools_trigger
end module fy_follow_tools
