program test_live_log
    !! The log of a running check grows on the server while the check runs. A
    !! remote runner sends a slow check's output in several appends, and the
    !! server holds each part before the check ends: through GET
    !! /api/v1/jobs/ID/log, the job, the jobs list, fy job --follow and the MCP
    !! tool job. Appends name their offset: a repeated one changes nothing, a
    !! gap is refused with the length the server has, and only the runner that
    !! claimed the check may append. A check that writes more than the bound
    !! keeps its first and last part and says how much was left out. A runner
    !! that sends no appends, as an older release, still finishes its check.
    !! A follower does not ask again at once while the log ends inside a UTF-8
    !! character, a runner goes on sending after a request failed, and the
    !! stored log of a long check keeps its end, its lines numbered as live.
    !! A check queued again after a timeout starts its log again, and the
    !! stored log holds the last attempt first.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use, intrinsic :: iso_c_binding, only: &
        c_char, &
        c_int, &
        c_null_char
    use fy_live, only: &
        LIVE_HEAD, &
        LIVE_TAIL, &
        live_append, &
        live_length, &
        live_read, &
        live_set_dir
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        tcp_listen
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_wait
    use fy_runner, only: &
        runner_main
    use fy_server, only: &
        server_set_checks, &
        server_set_data_dir, &
        server_supervise
    use fy_sqlite, only: &
        db_close, &
        db_t
    use fy_store, only: &
        store_bootstrap, &
        store_open, &
        store_repo_add, &
        store_token_create
    implicit none

    interface
        function c_open(path, flags, mode) bind(C, name='open') result(fd)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags, mode
            integer(c_int) :: fd
        end function c_open

        function c_dup2(old, new) bind(C, name='dup2') result(rc)
            import :: c_int
            integer(c_int), value :: old, new
            integer(c_int) :: rc
        end function c_dup2
    end interface

    character(len=*), parameter :: data = 'build/test_live_log_data'
    character(len=*), parameter :: work = 'build/test_live_log_work'
    character(len=*), parameter :: remote = 'build/test_live_log_remote'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, other, server, fy, env, out, in, check_id, &
        gates
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, run_pid, i, before
    integer(int64) :: t0, t1, rate

    sup = 0
    run_pid = 0
    call getcwd(cwd)
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    ! runners ask for work every 100 ms instead of 5 s
    call proc_setenv('FY_RUNNER_POLL_MS', '100')
    call execute_command_line('rm -rf '//data//' '//work//' '//remote//' && mkdir -p '//data// &
        '/tmp '//remote, exitstat=stat)
    ! The runner's work trees are its own, not those of other tests.
    call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp')

    call bound()

    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_token_create(db, 1_int64, 'runner-2', 'runner', other, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        ! The server's request lines go to a file the test counts in.
        stat = c_dup2(c_open(data//'/server.log'//c_null_char, 1089_c_int, int(o'644', c_int)), &
            2_c_int)
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = read_all(data//'/fy')
    fy = fy(1:len(fy) - 1)
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 && '
    in = env//'cd '//work//' && '
    do i = 1, 300
        call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
        if (stat == 0) exit
        call proc_sleep_ms(100)
    end do
    call check(stat == 0, 'login: '//out)

    ! A check that writes, waits for a gate the test opens, writes again and
    ! the start of a UTF-8 character, waits for a second gate and writes the
    ! rest of it and once more.
    gates = trim(cwd)//'/'//data//'/gate'
    call run('git init -q -b main '//work//' && mkdir '//work//'/.fortyard && printf '// &
        '"#!/bin/sh\necho one\nwhile [ ! -f '//gates//'1 ]; do sleep 0.05; done\necho two\n'// &
        'printf \"partial \\\\342\\\\234\"\n'// &
        'while [ ! -f '//gates//'2 ]; do sleep 0.05; done\nprintf \"\\\\223 three\\\\n\"\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work//' && '// &
        git//'add -A && '//git//'commit -q -m Slow && '//git//'remote add yard '//server// &
        '/demo.git && '//git//'checkout -q -b slow')
    call check(stat == 0, 'a work tree: '//out)
    call run(in//fy//' land --no-wait')
    call check(stat == 0 .and. index(out, 'landing 1 of demo slow queued') > 0, &
        'a landing is queued: '//out)
    call execute_command_line('('//env//fy//' job 1 --follow > '//data//'/follow 2>&1; echo $? > '// &
        data//'/follow.status) & echo $! > '//data//'/follow.pid')
    run_pid = proc_fork()
    if (run_pid == 0) then
        call runner_main(server, runner, remote, '')
        call proc_exit(0)
    end if

    ! The first part, before the check goes on.
    call wait_log('1', 'one')
    call check(index(out, '{"live":true,"ended":false,"state":"running","from":0') == 1 .and. &
        index(out, 'two') == 0, 'the server holds the first part of the log while the check '// &
        'runs: '//out)
    call run('curl -s -H "Authorization: Bearer '//admin//'" '//server//'/api/v1/jobs/1')
    call check(index(out, '"state":"running"') > 0 .and. &
        index(out, '"live_log":{"length":4}') > 0, 'the job names its live log: '//out)
    check_id = out(index(out, '"checks":[{"id":') + 16:)
    check_id = check_id(1:index(check_id, ',') - 1)
    call run('curl -s -H "Authorization: Bearer '//admin//'" "'//server//'/api/v1/jobs?state=all"')
    call check(index(out, '"log_tail":"one"') > 0, 'the jobs list shows its last line: '//out)

    ! Appends: only from the runner that holds the check, at offsets the
    ! server has.
    call append(other, '0', 'x')
    call check(index(out, '403') > 0, 'another runner may not append: '//out)
    call append(admin, '0', 'x')
    call check(index(out, '403') > 0, 'an administrator may not append: '//out)
    call append(runner, '999', 'x')
    call check(index(out, '"length":4') > 0 .and. index(out, '409') > 0, &
        'a gap is refused with the length the server has: '//out)
    call append(runner, '0', 'one')
    call check(index(out, '{"length":4}') > 0 .and. index(out, '200') > 0, &
        'a repeated append changes nothing: '//out)
    call append(runner, '', 'x')
    call check(index(out, '400') > 0, 'an append names its offset: '//out)
    call run('curl -s -H "Authorization: Bearer '//admin//'" "'//server//'/api/v1/jobs/1/log?from=2"')
    call check(index(out, '"from":2,"next":4,"length":4,"text":"e\n"') > 0, &
        'the log is read from an offset: '//out)

    ! The page of job 1 hears of its growth, not of another job's.
    call execute_command_line('curl -sN -H "Authorization: Bearer '//admin//'" "'//server// &
        '/api/v1/events?topic=log-1,log-99" > '//data//'/stream.out 2>&1 & echo $! > '//data// &
        '/events.pid')
    do i = 1, 1200
        if (index(read_all(data//'/stream.out'), 'event: log-99') > 0) exit
        call proc_sleep_ms(50)
    end do

    ! While the server cannot store the log, the runner's appends fail and
    ! are sent again later; the second part arrives once it can.
    call run('chmod 444 '//data//'/live/check-'//check_id//'.log')
    call execute_command_line('touch '//gates//'1')
    do i = 1, 1200
        if (index(read_all(data//'/server.log'), 'POST /api/v1/checks/'//check_id// &
            '/log 500') > 0) exit
        call proc_sleep_ms(50)
    end do
    call check(i <= 1200, 'an append fails while the log cannot be stored')
    call run('chmod 644 '//data//'/live/check-'//check_id//'.log')
    call wait_log('1', '"length":18,')
    call check(index(out, '{"live":true,"ended":false') == 1 .and. index(out, 'three') == 0 .and. &
        index(out, '"next":16,"length":18,"text":"one\ntwo\npartial "') > 0, &
        'the second part arrives in a later append while the check runs, without the '// &
        'incomplete character at its end: '//out)
    out = read_all(data//'/stream.out')
    call check(index(out, 'event: log-1'//new_line('a')//'data: changed') > 0 .and. &
        index(out, 'event: log-99'//new_line('a')//'data: changed') == 0, &
        'the growth is announced to the job''s own topic only: '//out)
    call execute_command_line('kill $(cat '//data//'/events.pid)')
    do i = 1, 1200
        out = read_all(data//'/follow')
        if (index(out, 'one'//new_line('a')//'two'//new_line('a')//'partial ') > 0) exit
        call proc_sleep_ms(50)
    end do
    call check(index(out, 'one'//new_line('a')//'two'//new_line('a')//'partial ') == 1, &
        'fy job --follow prints the log as it grows: '//out)
    ! The log ends inside a character: the follower asks once a second, not
    ! at once again.
    before = count_lines(read_all(data//'/server.log'), 'GET /api/v1/jobs/1/log ')
    call system_clock(t0, rate)
    do
        call proc_sleep_ms(50)
        call system_clock(t1)
        i = count_lines(read_all(data//'/server.log'), 'GET /api/v1/jobs/1/log ') - before
        if (i >= 3 .or. t1 - t0 > 3 * rate) exit
    end do
    call check(i <= 2 + 2 * (t1 - t0) / rate, 'fy job --follow does not ask in a loop '// &
        'while the log ends inside a character')
    call run(env//'printf ''%s\n'' ''{"jsonrpc":"2.0","id":1,"method":"tools/call",'// &
        '"params":{"name":"job","arguments":{"id":1,"offset":4}}}'' | '//fy//' mcp')
    call check(index(out, 'job 1 running, live log bytes 4 to 16 of 18; next offset 16\n'// &
        'two\npartial ') > 0, 'the MCP tool job answers the log from an offset: '//out)

    call execute_command_line('touch '//gates//'2')
    call wait_job('1', 'landed')
    do i = 1, 1200
        if (len(read_all(data//'/follow.status')) > 0) exit
        call proc_sleep_ms(50)
    end do
    out = read_all(data//'/follow')
    call check(index(out, 'one'//new_line('a')//'two'//new_line('a')//'partial '//achar(226)// &
        achar(156)//achar(147)//' three'//new_line('a')) == 1 .and. &
        index(out, 'landing 1 landed as ') > 0 .and. index(out, 'stored log') == 0 .and. &
        read_all(data//'/follow.status') == '0'//new_line('a'), &
        'fy job --follow ends with the outcome and status 0: '//out)
    call run('ls '//data//'/live')
    call check(len(out) == 0, 'the live log is gone when the job ends: '//out)
    call run('curl -s -H "Authorization: Bearer '//admin//'" "'//server//'/api/v1/jobs/1/log"')
    call check(index(out, '{"live":false,"ended":true,"state":"landed","from":0') == 1 .and. &
        index(out, '"text":"one\ntwo\npartial ') > 0 .and. index(out, 'three') > 0 .and. &
        index(out, '\nChecked by a runner.\n') > 0, 'then the log is the stored one, its '// &
        'lines numbered as in the live log and its outcome after them: '//out)
    call append(runner, '0', 'x')
    call check(index(out, '409') > 0 .and. index(out, 'no running check') > 0, &
        'an ended check takes no appends: '//out)

    ! A runner that sends no appends, as an older release does.
    call proc_kill(run_pid, SIGTERM)
    call proc_wait(run_pid, stat)
    run_pid = 0
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy//' git-credential" '// &
        'fetch -q yard main && git checkout -q -b quiet FETCH_HEAD && echo q > q.txt && '// &
        git//'add q.txt && '//git//'commit -q -m Quiet && '//fy//' land --no-wait')
    call check(stat == 0 .and. index(out, 'landing 2 of demo quiet queued') > 0, &
        'a second landing is queued: '//out)
    do i = 1, 1200
        call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" '//server// &
            '/api/v1/checks/claim')
        if (index(out, '"id":') > 0) exit
        call proc_sleep_ms(50)
    end do
    check_id = out(index(out, '"id":') + 5:)
    check_id = check_id(1:index(check_id, ',') - 1)
    call run('curl -s -H "Authorization: Bearer '//admin//'" '//server//'/api/v1/jobs/2')
    call check(index(out, '"live_log":null') > 0, 'a check without appends has no live log: '//out)
    call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" -d '// &
        '''{"passed":true,"log":"checked by an older runner"}'' '//server//'/api/v1/checks/'// &
        check_id//'/result')
    call wait_job('2', 'landed')
    call run(env//fy//' job 2 --follow')
    call check(stat == 0 .and. index(out, 'checked by an older runner') > 0 .and. &
        index(out, 'landing 2 landed as ') > 0, &
        'its result lands the job, and fy job --follow prints its stored log: '//out)

    call long_check()
    call queued_again()

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    call execute_command_line('rm -rf '//data//' '//work//' '//remote)
    print '(a)', 'live log: all checks passed'

contains

    subroutine bound()
        !! A check that writes more than LIVE_HEAD + LIVE_TAIL bytes keeps its
        !! first and last part, and a reader is told how much was left out.
        character(len=:), allocatable :: chunk, text
        character(len=24) :: lost
        integer(int64) :: offset, length, next, total, kept
        integer :: k, st
        logical :: found

        call live_set_dir(data//'/bound')
        offset = 0
        do k = 1, 20
            chunk = repeat(achar(iachar('a') + k - 1), 524287)//new_line('a')
            call live_append('check-1', offset, chunk, length, st)
            call check(st == 0 .and. length == offset + len(chunk), 'an append is taken')
            call live_append('check-1', offset, chunk, length, st)
            call check(st == 0 .and. length == offset + len(chunk), &
                'the same append again changes nothing')
            offset = length
        end do
        total = offset
        call live_append('check-1', total + 1, 'x', length, st)
        call check(st == 1 .and. length == total, 'a gap is refused')
        call check(live_length('check-1') == total, 'the length counts every byte written')
        call live_read('check-1', 0_int64, huge(1), text, next, length, found)
        write (lost, '(i0)') total - LIVE_HEAD - LIVE_TAIL
        call check(found .and. next == total .and. length == total, 'the whole log is read')
        call check(text(1:1) == 'a' .and. text(LIVE_HEAD - 1:LIVE_HEAD - 1) == 'h' .and. &
            text(len(text) - 1:len(text) - 1) == 't' .and. &
            index(text, new_line('a')//'... '//trim(lost)//' bytes left out ...'// &
            new_line('a')) == LIVE_HEAD, 'its first and last part are kept, with a line '// &
            'that says how much was left out')
        call check(len(text, int64) == LIVE_HEAD + LIVE_TAIL + len('... '//trim(lost)// &
            ' bytes left out ...'//new_line('a')), 'and nothing more')
        call live_read('check-1', LIVE_HEAD + 10, huge(1), text, next, length, found)
        call check(index(text, new_line('a')//'... '//trim(lost)//' bytes left out ...') == 1 &
            .and. len(text, int64) == LIVE_TAIL + len(new_line('a')//'... '//trim(lost)// &
            ' bytes left out ...'//new_line('a')), 'a read from the left-out part starts with '// &
            'that line, on a line of its own after what the reader had')
        ! Lines of 100 bytes: the first part ends inside a line. A reader that
        ! has read up to there gets the line that says what was left out on a
        ! line of its own.
        call live_set_dir(data//'/bound2')
        offset = 0
        chunk = repeat(repeat('x', 99)//new_line('a'), 10486)
        do k = 1, 10
            call live_append('check-2', offset, chunk, length, st)
            offset = length
        end do
        call live_read('check-2', 0_int64, int(LIVE_HEAD), text, next, length, found)
        call check(next == LIVE_HEAD .and. text(len(text):) == 'x', 'a reader stops inside a line')
        offset = next
        call live_read('check-2', offset, huge(1), text, next, length, found)
        call check(index(text, new_line('a')//'... ') == 1, 'the left-out line that follows '// &
            'starts on a line of its own: '//text(1:min(len(text), 60)))
        call run('du -cb '//data//'/bound | tail -1')
        read (out, *) kept
        call check(kept <= LIVE_HEAD + LIVE_TAIL + 1048576 + 4096, &
            'the server keeps at most the bound on disk: '//out)
    end subroutine bound

    subroutine long_check()
        !! A check that writes more than the stored log holds, then waits for
        !! a gate and fails with a last line. Its runner is stopped while the
        !! check waits: the check ends with it, and the runner that starts
        !! next removes the output file left behind and runs the check again.
        !! fy job --follow then prints every line once and the last line, and
        !! the stored log keeps the end.
        character(len=:), allocatable :: pids, first, lf
        integer :: k, n

        lf = new_line('a')
        pids = trim(cwd)//'/'//data//'/pids'
        call run(in//'git -c credential.helper= -c "credential.helper=!'//fy//' git-credential" '// &
            'fetch -q yard main && git checkout -q -b long FETCH_HEAD && printf '// &
            '"#!/bin/sh\necho \$\$ >> '//pids//'\nseq 1 20000 | sed \"s/^/line /\"\n'// &
            'while [ ! -f '//gates//'3 ]; do sleep 0.05; done\necho \"FINAL: error at the end\"\n'// &
            'exit 1\n" > .fortyard/check && '//git//'add -A && '//git//'commit -q -m Long && '// &
            fy//' land --no-wait')
        call check(stat == 0 .and. index(out, 'landing 3 of demo long queued') > 0, &
            'a third landing is queued: '//out)
        run_pid = proc_fork()
        if (run_pid == 0) then
            call runner_main(server, runner, remote, '')
            call proc_exit(0)
        end if
        call wait_log('3', 'line 20000')
        call run('ls '//data//'/tmp/fy-check-*.log')
        first = out(1:index(out, lf) - 1)
        call check(index(first, 'fy-check-') > 0, 'the check writes to a file: '//out)
        call proc_kill(run_pid, SIGTERM)
        call proc_wait(run_pid, stat)
        run_pid = 0
        do k = 1, 1200
            call run('kill -0 $(head -1 '//pids//')')
            if (stat /= 0) exit
            call proc_sleep_ms(50)
        end do
        call check(stat /= 0, 'the check ends when its runner is stopped')
        run_pid = proc_fork()
        if (run_pid == 0) then
            call runner_main(server, runner, remote, '')
            call proc_exit(0)
        end if
        do k = 1, 1200
            if (count_lines(read_all(pids), '') >= 2) exit
            call proc_sleep_ms(50)
        end do
        call run('ls '//first)
        call check(stat /= 0, 'the runner that starts next removes the output file left '// &
            'behind: '//out)
        call execute_command_line('('//env//fy//' job 3 --follow > '//data//'/follow3 2>&1; '// &
            'echo $? > '//data//'/follow3.status) & echo $! > '//data//'/follow.pid')
        do k = 1, 1200
            if (index(read_all(data//'/follow3'), 'line 20000'//lf) > 0) exit
            call proc_sleep_ms(50)
        end do
        call execute_command_line('touch '//gates//'3')
        call wait_job('3', 'failed')
        do k = 1, 1200
            if (len(read_all(data//'/follow3.status')) > 0) exit
            call proc_sleep_ms(50)
        end do
        out = read_all(data//'/follow3')
        n = occurrences(lf//out, lf//'line 1'//lf)
        call check(index(out, 'line 1'//lf) == 1 .and. n == 1 .and. &
            index(out, 'line 20000'//lf//'FINAL: error at the end'//lf) > 0 .and. &
            index(out, lf//'The check failed (exit status 1).'//lf) > 0 .and. &
            index(out, 'landing 3 failed'//lf) > 0 .and. &
            read_all(data//'/follow3.status') == '1'//lf, 'fy job --follow prints each line '// &
            'once, the last one and the outcome, and ends with status 1: '//out(max(1, len(out) - &
            400):))
        call run('curl -s -H "Authorization: Bearer '//admin//'" "'//server//'/api/v1/jobs/3/log"')
        call check(index(out, '"text":"line 1\nline 2\n') > 0 .and. &
            index(out, ' bytes left out ...\n') > 0 .and. &
            index(out, '\nline 20000\nFINAL: error at the end\n') > 0 .and. &
            index(out, '\nThe check failed (exit status 1)."}') > 0, &
            'the stored log keeps the first lines, the end and then the outcome: '// &
            out(max(1, len(out) - 400):))
        call proc_kill(run_pid, SIGTERM)
        call proc_wait(run_pid, stat)
        run_pid = 0
    end subroutine long_check

    subroutine queued_again()
        !! A check queued again after a timeout runs again from an empty live
        !! log. fy job --follow, stopped while the check is queued again and
        !! taken again (so the new log is already longer than what it
        !! printed), says that the log starts again and prints the new one
        !! from its start. When the job ends it prints the stored log's
        !! outcome lines after the last attempt's output, not the earlier
        !! attempt again; at a second timeout that includes the reason. The
        !! stored log holds the last attempt's output first.
        character(len=:), allocatable :: lf, two
        integer :: k

        lf = new_line('a')
        two = 'attempt two is longer than what was printed before'
        call land_change('again', '4')
        call follow_start('4')
        call append(runner, '0', 'attempt one line A'//lf//'attempt one line B'//lf)
        call wait_follow('4', 'attempt one line B'//lf)
        call execute_command_line('kill -STOP $(cat '//data//'/follow.pid)')
        call result('{"error":"timeout","log":"attempt one line A\nattempt one line B\n'// &
            'The time limit ended attempt one."}')
        call check(index(out, '"state":"queued"') > 0, 'a first timeout queues the check '// &
            'again: '//out)
        call claim_again()
        call append(runner, '0', two//lf)
        call execute_command_line('kill -CONT $(cat '//data//'/follow.pid)')
        call wait_follow('4', two//lf)
        call result('{"passed":true,"log":"'//two//'\n"}')
        call wait_job('4', 'landed')
        call wait_status('4')
        out = read_all(data//'/follow4')
        call check(index(out, 'attempt one line A'//lf//'attempt one line B'//lf) == 1 .and. &
            occurrences(out, 'attempt one line A') == 1 .and. &
            index(out, lf//'--- the log starts again ---'//lf//two//lf//'Checked by a runner.'// &
            lf//'Landed as ') > 0 .and. index(out, 'ended attempt one') == 0 .and. &
            index(out, 'earlier attempt') == 0 .and. index(out, 'landing 4 landed as ') > 0 .and. &
            read_all(data//'/follow4.status') == '0'//lf, 'fy job --follow prints the log of '// &
            'the check run again from its start and then its outcome, not the earlier attempt: '// &
            out)
        call run('curl -s -H "Authorization: Bearer '//admin//'" "'//server//'/api/v1/jobs/4/log"')
        call check(index(out, '"text":"'//two//'\nChecked by a runner.\nLanded as ') > 0 .and. &
            index(out, '\n--- an earlier attempt of this check ---\nattempt one line A\n'// &
            'attempt one line B\nThe time limit ended attempt one.') > 0, 'the stored log has '// &
            'the last attempt''s output first, its outcome after it and the earlier attempt '// &
            'last: '//out)

        call land_change('twice', '5')
        call follow_start('5')
        call append(runner, '0', 'first try'//lf)
        call wait_follow('5', 'first try'//lf)
        call execute_command_line('kill -STOP $(cat '//data//'/follow.pid)')
        call result('{"error":"timeout","log":"first try\nThe time limit ended the first try."}')
        call claim_again()
        call append(runner, '0', 'second try, longer than the first'//lf)
        call execute_command_line('kill -CONT $(cat '//data//'/follow.pid)')
        call wait_follow('5', 'second try, longer than the first'//lf)
        call result('{"error":"timeout","log":"second try, longer than the first\n'// &
            'The time limit ended the second try."}')
        call check(index(out, '"state":"timeout"') > 0, 'a second timeout ends the check: '//out)
        call wait_job('5', 'failed')
        call wait_status('5')
        out = read_all(data//'/follow5')
        k = index(out, lf//'--- the log starts again ---'//lf//'second try, longer than the '// &
            'first'//lf//'The time limit ended the second try.'//lf//'Reason: timeout.')
        call check(index(out, 'first try'//lf) == 1 .and. k > 0 .and. &
            index(out, 'The check failed (exit status 1).'//lf//'landing 5 failed'//lf) > k .and. &
            index(out, 'ended the first try') == 0 .and. &
            read_all(data//'/follow5.status') == '1'//lf, 'fy job --follow prints the second '// &
            'attempt, the reason timeout and the outcome: '//out)
    end subroutine queued_again

    subroutine land_change(branch, id)
        !! Lands a new commit on `branch` as landing `id` and claims its check
        !! as the runner.
        character(len=*), intent(in) :: branch, id
        integer :: k

        call run(in//'git -c credential.helper= -c "credential.helper=!'//fy//' git-credential" '// &
            'fetch -q yard main && git checkout -q -b '//branch//' FETCH_HEAD && echo '//branch// &
            ' > '//branch//'.txt && '//git//'add -A && '//git//'commit -q -m '//branch//' && '// &
            fy//' land --no-wait')
        call check(stat == 0 .and. index(out, 'landing '//id//' of demo '//branch//' queued') > 0, &
            'landing '//id//' is queued: '//out)
        do k = 1, 1200
            call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" '//server// &
                '/api/v1/checks/claim')
            if (index(out, '"id":') > 0) exit
            call proc_sleep_ms(50)
        end do
        check_id = out(index(out, '"id":') + 5:)
        check_id = check_id(1:index(check_id, ',') - 1)
    end subroutine land_change

    subroutine claim_again()
        !! Claims the check `check_id` again, as a runner after its timeout.
        character(len=:), allocatable :: again
        integer :: k

        do k = 1, 1200
            call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" '//server// &
                '/api/v1/checks/claim')
            if (index(out, '"id":') > 0) exit
            call proc_sleep_ms(50)
        end do
        again = out(index(out, '"id":') + 5:)
        again = again(1:max(0, index(again, ',') - 1))
        call check(again == check_id, 'the same check is claimed again: '//out)
    end subroutine claim_again

    subroutine result(body)
        character(len=*), intent(in) :: body
        call run('curl -s -X POST -H "Authorization: Bearer '//runner//'" -d '''//body//''' '// &
            server//'/api/v1/checks/'//check_id//'/result')
    end subroutine result

    subroutine follow_start(id)
        !! fy job ID --follow in the background, its process in follow.pid.
        character(len=*), intent(in) :: id
        call execute_command_line('rm -f '//data//'/follow.pid && ('//env//'sh -c ''echo $$ > '// &
            data//'/follow.pid; exec '//fy//' job '//id//' --follow'' > '//data//'/follow'//id// &
            ' 2>&1; echo $? > '//data//'/follow'//id//'.status) &')
    end subroutine follow_start

    subroutine wait_follow(id, part)
        character(len=*), intent(in) :: id, part
        integer :: k
        do k = 1, 1200
            if (index(read_all(data//'/follow'//id), part) > 0) return
            call proc_sleep_ms(50)
        end do
        call check(.false., 'fy job '//id//' --follow prints '//part//': '// &
            read_all(data//'/follow'//id))
    end subroutine wait_follow

    subroutine wait_status(id)
        character(len=*), intent(in) :: id
        integer :: k
        do k = 1, 1200
            if (len(read_all(data//'/follow'//id//'.status')) > 0) return
            call proc_sleep_ms(50)
        end do
        call check(.false., 'fy job '//id//' --follow ends')
    end subroutine wait_status

    integer function count_lines(text, part) result(n)
        !! The lines of `text` that have `part`; all of them for ''.
        character(len=*), intent(in) :: text, part
        integer :: a, b
        n = 0
        a = 1
        do while (a <= len(text))
            b = index(text(a:), new_line('a'))
            if (b == 0) b = len(text) - a + 2
            if (index(text(a:a + b - 2), part) > 0 .or. len(part) == 0) n = n + 1
            a = a + b
        end do
    end function count_lines

    integer function occurrences(text, part) result(n)
        !! How often `part` is in `text`.
        character(len=*), intent(in) :: text, part
        integer :: a, b
        n = 0
        a = 1
        do
            b = index(text(a:), part)
            if (b == 0) return
            n = n + 1
            a = a + b
        end do
    end function occurrences

    subroutine append(token, offset, bytes)
        character(len=*), intent(in) :: token, offset, bytes
        character(len=:), allocatable :: query
        query = ''
        if (len(offset) > 0) query = '?offset='//offset
        call run('curl -s -w " %{http_code}" -X POST -H "Authorization: Bearer '//token// &
            '" --data-binary '''//bytes//''' "'//server//'/api/v1/checks/'//check_id//'/log'// &
            query//'"')
    end subroutine append

    subroutine wait_log(id, part)
        !! Until the answer for the log of job `id` has `part`.
        character(len=*), intent(in) :: id, part
        integer :: k
        do k = 1, 1200
            call run('curl -s -H "Authorization: Bearer '//admin//'" "'//server// &
                '/api/v1/jobs/'//id//'/log?from=0"')
            if (index(out, part) > 0) return
            call proc_sleep_ms(50)
        end do
        call check(.false., 'the log of job '//id//' has '//part//': '//out)
    end subroutine wait_log

    subroutine wait_job(id, want)
        character(len=*), intent(in) :: id, want
        integer :: k
        do k = 1, 1200
            call run('curl -s -H "Authorization: Bearer '//admin//'" '//server//'/api/v1/jobs/'//id)
            if (index(out, '"state":"'//want//'","queued"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'job '//id//' is '//want//': '//out)
    end subroutine wait_job

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: u, ios, n
        text = ''
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=ios) text
        close (u)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (run_pid > 0) call proc_kill(run_pid, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            call execute_command_line('kill $(cat '//data//'/follow.pid) 2>/dev/null')
            call execute_command_line('kill $(cat '//data//'/events.pid) 2>/dev/null')
            error stop 1
        end if
    end subroutine check

end program test_live_log
