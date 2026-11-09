program test_events
    !! A watch opened before a change sees it: events_wait wakes for its topic
    !! and keeps sleeping through other topics.
    use fy_events, only: events_set_dir, events_notify, events_watch_open, events_wait
    implicit none
    integer :: fd
    logical :: got

    call execute_command_line('rm -rf build/test_events && mkdir -p build/test_events')
    call events_set_dir('build/test_events')
    fd = events_watch_open()
    if (fd < 0) error stop 'FAIL: no watch'
    call events_notify('checks')
    got = events_wait(fd, 'checks', 1000)
    if (.not. got) error stop 'FAIL: a change of the topic wakes the wait'
    call events_notify('jobs')
    got = events_wait(fd, 'checks', 300)
    if (got) error stop 'FAIL: another topic does not count'
    call events_notify('jobs')
    call events_notify('checks')
    got = events_wait(fd, 'checks', 1000)
    if (.not. got) error stop 'FAIL: the topic is found after another one'
    print '(a)', 'events: all checks passed'
end program test_events
