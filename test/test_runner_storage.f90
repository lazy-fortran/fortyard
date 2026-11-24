program test_runner_storage
    implicit none

    character(len=*), parameter :: root = 'build/test_runner_storage'
    integer :: stat

    call execute_command_line('mkdir -p '//root//'/fy-work/old/frozen '//root//'/fy-work/recent '// &
        root//'/tmp/stale '//root//'/exchange && '// &
        'dd if=/dev/zero of='//root//'/fy-work/old/payload bs=1024 count=800 2>/dev/null && '// &
        'dd if=/dev/zero of='//root//'/fy-work/recent/payload bs=1024 count=800 2>/dev/null && '// &
        'touch -t 200001010000 '//root//'/fy-work/old && '// &
        'touch '//root//'/tmp/stale/artifact '//root//'/fy-work/old/frozen/artifact '// &
        root//'/exchange/job && chmod 555 '//root//'/tmp/stale '//root//'/fy-work/old/frozen', &
        exitstat=stat)
    if (stat /= 0) error stop 'could not create the runner storage fixture'

    call execute_command_line('FY_RUNNER_WORK='//root// &
        ' FY_RUNNER_RETAIN_MIB=1 FY_RUNNER_ACTIVE_MIB=2 FY_RUNNER_RESERVE_MIB=0 '// &
        'sh scripts/runner-storage.sh collect', exitstat=stat)
    if (stat /= 0) error stop 'runner collection failed'

    call execute_command_line('test ! -e '//root//'/fy-work/old && '// &
        'test -f '//root//'/fy-work/recent/payload && test ! -e '//root//'/tmp/stale && '// &
        'test -f '//root//'/exchange/job', exitstat=stat)
    if (stat /= 0) error stop 'collection did not preserve recent work and pending jobs'

    call execute_command_line('if test -r /proc/self/stat && command -v timeout >/dev/null; then '// &
        'export FY_RUNNER_WORK="$PWD/'//root//'" FY_CHECK_WRAP=nested '// &
        'FY_RUNNER_RETAIN_MIB=1 FY_RUNNER_ACTIVE_MIB=2 FY_RUNNER_RESERVE_MIB=0; '// &
        'guard="$PWD/scripts/runner-storage.sh"; cd "$FY_RUNNER_WORK/fy-work/recent" && '// &
        'timeout -k 1 5 sh "$guard" run sh -c ''test -z "${FY_CHECK_WRAP+x}"''; fi', exitstat=stat)
    if (stat /= 0) error stop 'storage wrapper leaked into the check'
end program test_runner_storage
