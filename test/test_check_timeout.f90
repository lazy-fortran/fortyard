module test_check_timeout_live
    !! A live log as the server keeps one: the bytes a running check wrote,
    !! copied from its output file each time it is announced.
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    character(len=:), allocatable :: live_log, first_told, live_path
    integer :: told = 0

contains

    subroutine live_sink(path, used)
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: used
        character(len=:), allocatable :: bytes
        integer(int64) :: have
        integer :: u, ios

        if (.not. allocated(live_log)) live_log = ''
        live_path = path
        told = told + 1
        used = len(live_log, int64)
        inquire (file=path, size=have)
        if (have > used) then
            open (newunit=u, file=path, status='old', access='stream', form='unformatted', &
                action='read', iostat=ios)
            if (ios == 0) then
                allocate (character(len=int(have - used)) :: bytes)
                read (u, pos=used + 1, iostat=ios) bytes
                close (u)
                if (ios == 0) live_log = live_log//bytes
            end if
        end if
        used = len(live_log, int64)
        if (told == 1) first_told = live_log
    end subroutine live_sink

end module test_check_timeout_live

program test_check_timeout
    !! The time limit of a check (policy check_timeout). The limit ends the
    !! check's whole process group, also a child that ignores SIGTERM, and
    !! leaves no process behind, also none that left the check's session. A
    !! check whose output is sent while it runs is ended by the limit too, and
    !! its live log keeps what it wrote before. A check without a limit ends
    !! with the process that runs it. A check that reached it is queued again once; the second timeout fails
    !! the landing with the reason timeout. A check whose runner has not
    !! reported for twice its limit plus 5 minutes is taken from it and queued
    !! again, and that runner's late result is refused. Here the test acts as
    !! the runner;
    !! test_check_timeout_slow has a runner end a check at a one-minute limit.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_events, only: events_notify
    use fy_land, only: land_check, land_set_live
    use test_check_timeout_live, only: first_told, live_log, live_path, live_sink, told
    use fy_policy, only: policy_get, policy_put, policy_t
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGKILL, SIGTERM, &
        proc_setenv
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_check_timeout_data'
    character(len=*), parameter :: work = 'build/test_check_timeout_work'
    character(len=*), parameter :: group = 'build/test_check_timeout_group'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    type(policy_t) :: policy
    character(len=:), allocatable :: admin, manual, other, base, url, reply, out, id
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, run, i, code
    integer(int64) :: t0, t1, rate
    logical :: exists, timed_out, found

    sup = 0
    run = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')

    ! A check that starts a child ignoring SIGTERM: the limit sends SIGTERM to
    ! the group, SIGKILL after the grace, and nothing of the group is left.
    call execute_command_line('rm -rf '//group//' && mkdir -p '//group//'/.fortyard && '// &
        'printf ''#!/bin/sh\nsh -c "trap \\"\\" TERM; echo \\$\\$ > child; exec sleep 300" &\n'// &
        'trap "" TERM\ncut -d" " -f5 /proc/$$/stat > pgid\necho started\nwait\n'' > '// &
        group//'/.fortyard/check && chmod +x '//group//'/.fortyard/check', exitstat=stat)
    call land_check(group, out, code, limit=1, timed_out=timed_out, grace=1)
    call check(timed_out, 'the time limit ends the check: '//out)
    call check(index(out, 'started') > 0, 'its output is kept: '//out)
    ! A process that ended may stay a zombie until its new parent reaps it,
    ! which under a test harness that is a subreaper may be at its end.
    call execute_command_line('c=$(cat '//group//'/child) && test -n "$c" && for i in $(seq 300); '// &
        'do ps -o stat= -p "$c" | grep -qv Z || exit 0; sleep 0.1; done; exit 1', exitstat=stat)
    call check(stat == 0, 'the child that ignores SIGTERM is gone')
    call execute_command_line('g=$(cat '//group//'/pgid) && test -n "$g" && for i in $(seq 300); '// &
        'do ps -e -o pgid=,stat= | awk -v g="$g" ''$1 == g && $2 !~ /^Z/'' | grep -q . || '// &
        'exit 0; sleep 0.1; done; exit 1', exitstat=stat)
    call check(stat == 0, 'no process of the check''s group is left')
    call execute_command_line('printf "#!/bin/sh\necho quick\n" > '//group//'/.fortyard/check', &
        exitstat=stat)
    call land_check(group, out, code, limit=0, timed_out=timed_out)
    call check(.not. timed_out .and. code == 0, 'a check that ends by itself is no timeout')
    ! An fpm project with a script of its own is checked by the script.
    call execute_command_line('printf ''name = "x"\n'' > '//group//'/fpm.toml', exitstat=stat)
    call land_check(group, out, code, limit=60, timed_out=timed_out)
    call check(code == 0 .and. index(out, 'quick') > 0, 'the script, not fo, checks an fpm project: '//out)
    call execute_command_line('rm -f '//group//'/fpm.toml', exitstat=stat)

    ! A process the check starts in a session of its own escapes the group's
    ! signals and holds the output open; the limit still ends the check
    ! (long before the escaped sleep would) and the escaped process with it.
    call execute_command_line('printf ''#!/bin/sh\nsetsid sh -c "echo \\$\\$ > daemon; exec sleep 300" &\n'// &
        'while [ ! -s daemon ]; do sleep 0.1; done\necho started\nsleep 300\n'' > '// &
        group//'/.fortyard/check && rm -f '//group//'/daemon', exitstat=stat)
    call system_clock(t0, rate)
    call land_check(group, out, code, limit=2, timed_out=timed_out, grace=1)
    call system_clock(t1)
    call check(timed_out .and. index(out, 'started') > 0, 'a check with an escaped process times out: '//out)
    call check(t1 - t0 < 120*rate, 'and the escaped process does not hold it up')
    call execute_command_line('d=$(cat '//group//'/daemon) && test -n "$d" && for i in $(seq 300); '// &
        'do ps -o stat= -p "$d" | grep -qv Z || exit 0; sleep 0.1; done; exit 1', exitstat=stat)
    call check(stat == 0, 'the process that left the check''s session is gone')
    ! A check that ends by itself leaves no such process behind either.
    call execute_command_line('printf ''#!/bin/sh\nsetsid sh -c "echo \\$\\$ > daemon; exec sleep 300" &\n'// &
        'while [ ! -s daemon ]; do sleep 0.1; done\necho done\n'' > '// &
        group//'/.fortyard/check && rm -f '//group//'/daemon', exitstat=stat)
    call land_check(group, out, code, limit=600, timed_out=timed_out, grace=1)
    call check(.not. timed_out .and. code == 0 .and. index(out, 'done') > 0, &
        'a check that leaves a daemon ends: '//out)
    call execute_command_line('d=$(cat '//group//'/daemon) && test -n "$d" && for i in $(seq 300); '// &
        'do ps -o stat= -p "$d" | grep -qv Z || exit 0; sleep 0.1; done; exit 1', exitstat=stat)
    call check(stat == 0, 'the daemon it left is gone')

    ! A check whose output is sent while it runs: what it wrote reaches the
    ! live log before the limit, the limit ends it as any other, and the live
    ! log and its output keep what it wrote; the output file is removed.
    call execute_command_line('printf ''#!/bin/sh\necho before the limit\nsleep 300\n'' > '// &
        group//'/.fortyard/check', exitstat=stat)
    call land_set_live(live_sink)
    call system_clock(t0, rate)
    call land_check(group, out, code, limit=3, timed_out=timed_out, grace=1)
    call system_clock(t1)
    call land_set_live()
    call check(timed_out .and. t1 - t0 < 120*rate, 'a check that sends its output is ended '// &
        'by its time limit: '//out)
    call check(told >= 2 .and. index(first_told, 'before the limit') == 1, &
        'its output was sent while it ran: '//first_told)
    call check(live_log == 'before the limit'//new_line('a') .and. &
        index(out, 'before the limit') == 1, 'its live log and its output keep what it wrote: '// &
        live_log)
    inquire (file=live_path, exist=exists)
    call check(.not. exists, 'its output file is removed: '//live_path)

    ! Without a time limit, a check still ends with the runner or lander that
    ! started it, also a process it left in the background: here a child of
    ! the test runs the check and is killed.
    call execute_command_line('printf ''#!/bin/sh\nsleep 300 &\necho $! > grandchild\n'// &
        'while :; do echo tick; sleep 0.2; done\n'' > '//group//'/.fortyard/check && rm -f '// &
        group//'/grandchild', exitstat=stat)
    call getcwd(cwd)
    run = proc_fork()
    if (run == 0) then
        call proc_setenv('TMPDIR', trim(cwd)//'/'//group)
        call land_check(group, out, code, limit=0)
        call proc_exit(0)
    end if
    do i = 1, 300
        inquire (file=group//'/grandchild', size=t0)
        if (t0 > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(t0 > 0, 'the check without a time limit runs')
    call proc_kill(run, SIGKILL)
    call proc_wait(run, stat)
    run = 0
    call execute_command_line('c=$(cat '//group//'/grandchild) && test -n "$c" && for i in '// &
        '$(seq 300); do ps -o stat= -p "$c" | grep -qv Z || exit 0; sleep 0.1; done; exit 1', &
        exitstat=stat)
    call check(stat == 0, 'a check without a time limit ends with the process that ran it')

    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-2', 'runner', manual, stat)
    call store_token_create(db, 1_int64, 'runner-3', 'runner', other, stat)
    call store_repo_add(db, 'limits', '', stat, mode='primary')
    call policy_get(db, 'limits', policy)
    policy%check_timeout = 1
    call policy_put(db, 'limits', policy, found)
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    base = '127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 150
        inquire (file=data//'/repos/limits.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    ! The test is the runner. Two timeouts of one check fail the landing.
    url = 'http://x:'//admin//'@'//base//'/limits.git'
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "#!/bin/sh\necho checked\n" > '//work//'/.fortyard/check && chmod +x '// &
        work//'/.fortyard/check && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && '//git//'-C '//work//' push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a branch is pushed')
    reply = post('/api/v1/repos/limits/land', '{"branch":"first"}', admin)
    id = claim(manual)
    call check(index(reply, '"timeout":60') > 0, 'the claim carries the limit in seconds: '//reply)
    reply = get('/api/v1/jobs')
    call check(index(reply, '"name":"limits"') > 0 .and. index(reply, '"checks":1') > 0, &
        'the jobs list shows the runner the repository holds: '//reply)
    reply = post('/api/v1/checks/'//id//'/result', '{"error":"timeout","log":"limit one"}', manual)
    call check(index(reply, '"state":"queued"') > 0, 'a first timeout queues it again: '//reply)
    call check(claim(manual) == id, 'the same check is claimed again')
    reply = post('/api/v1/checks/'//id//'/result', '{"error":"timeout","log":"limit two"}', manual)
    call check(index(reply, '"state":"timeout"') > 0, 'a second timeout ends it: '//reply)
    call wait_landing(1, 'failed')
    reply = get('/api/v1/jobs/1')
    call check(index(reply, 'Reason: timeout.') > 0 .and. index(reply, 'limit one') > 0 .and. &
        index(reply, 'limit two') > 0, 'the landing fails with the reason timeout: '//reply)
    reply = post('/api/v1/checks/'//id//'/result', '{"error":"later"}', manual)
    call check(index(reply, '400') > 0, 'an unknown error is refused: '//reply)
    ! A runner that took a check and was not heard of for twice the limit of
    ! its claim plus 5 minutes is lost: the server takes the check from it and
    ! queues it again. Its late result, if it comes, is refused and decides
    ! nothing; the result of the runner that took the check over counts.
    reply = post('/api/v1/repos/limits/land', '{"branch":"first"}', admin)
    id = claim(manual)
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, "UPDATE checks SET claimed_at = '2000-01-01T00:00:00Z' WHERE id = "//id// &
        ';', stat)
    call db_close(db)
    call events_notify('checks')
    call check(claim(other) == id, 'the check of a lost runner is queued again and taken')
    reply = post('/api/v1/checks/'//id//'/result', '{"passed":false,"log":"LATE"}', manual)
    call check(index(reply, ' 409 ') > 0, 'the lost runner''s late result is refused: '//reply)
    reply = post('/api/v1/checks/'//id//'/result', '{"error":"timeout","log":"LATE"}', manual)
    call check(index(reply, ' 409 ') > 0, 'and so is its late timeout: '//reply)
    reply = post('/api/v1/checks/'//id//'/result', '{"passed":true,"log":"ok"}', other)
    call check(index(reply, '"state":"passed"') > 0, 'the runner holding it reports: '//reply)
    call wait_landing(2, 'landed')
    reply = get('/api/v1/jobs/2')
    call check(index(reply, 'taken as lost') > 0 .and. index(reply, 'LATE') == 0, &
        'the log says the runner was lost and has nothing of its late result: '//reply)
    call check(index(reply, 'taken as lost') > 0, 'the log says the runner was lost: '//reply)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'check_timeout: all checks passed'

contains

    function claim(token) result(check_id)
        !! Claims a check with the runner's `token`, waiting until one is queued.
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: check_id
        integer :: k, s, e
        check_id = ''
        do k = 1, 600
            reply = post('/api/v1/checks/claim', '{}', token)
            s = index(reply, '"id":')
            if (s > 0) then
                s = s + 5
                e = s
                do while (index('0123456789', reply(e:e)) > 0)
                    e = e + 1
                end do
                check_id = reply(s:e - 1)
                return
            end if
            call proc_sleep_ms(100)
        end do
        call check(.false., 'a check is queued for the runner: '//reply)
    end function claim

    subroutine wait_landing(number, state)
        !! Waits for the state, under load too.
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        integer :: k
        character(len=16) :: n
        write (n, '(i0)') number
        do k = 1, 3000
            reply = get('/api/v1/jobs/'//trim(n))
            if (index(reply, '"state":"'//state//'"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//trim(n)//' '//state//': '//reply)
    end subroutine wait_landing

    function post(path, body, token) result(r)
        character(len=*), intent(in) :: path, body, token
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//token//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)//CRLF//CRLF//body)
    end function post

    function get(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        r = exchange('GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF//CRLF)
    end function get

    function exchange(request) result(r)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, n, st
        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, request, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            r = r//buf(1:n)
        end do
        call sock_close(cfd)
    end function exchange

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (run > 0) call proc_kill(run, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_check_timeout
