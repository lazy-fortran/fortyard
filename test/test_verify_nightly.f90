program test_verify_nightly
    !! A nightly class (verify_on CLASS@nightly) is verified on its own, in the
    !! night's window only: not outside 01:00 to 06:00, once in the window, not
    !! again while main stays, nor for a main that moves later that night; a
    !! failure on it is bisected in the windows only and reported by an issue
    !! naming the class, which is closed when the class passes again, or when
    !! the class is no longer verified at night. The class without schedule is
    !! verified after each move of main as before. The verifier's clock is set
    !! by FY_VERIFY_NOW, so the server is restarted with another time between
    !! the phases.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_wait
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_runner, only: runner_main
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_close, &
        db_exec, &
        db_prepare, &
        db_t, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_nightly_data', &
        work = 'build/test_verify_nightly_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=*), parameter :: NIGHTLY = "SELECT count(*) FROM landings WHERE kind = 'verify'"// &
        " AND verify_class = 'macos-arm64'"
    character(len=*), parameter :: REGULAR = "SELECT count(*) FROM landings WHERE kind = 'verify'"// &
        " AND verify_class = '' AND state = 'passed'"
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, macos, base, url, reply, gw, issue
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, run, mac, i, k

    sup = 0
    run = 0
    mac = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data//'/tmp', &
        exitstat=stat)
    ! The server's landing work trees are named by landing number, which other
    ! tests' servers share.
    call getcwd(cwd)
    call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp')
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_token_create(db, 1_int64, 'runner-2', 'runner', macos, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    ! The test keeps the listening socket, so runners reach each server.
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    base = '127.0.0.1:'//trim(num)

    ! Noon: outside the window.
    call serve('2026-11-12T12:00')
    reply = put('/api/v1/repos/demo/policy', '{"verify_on":"linux,macos-arm64@nightly"}')
    call check(index(reply, '"verify_on":"linux,macos-arm64@nightly"') > 0, &
        'the policy accepts and shows a nightly class: '//reply)
    reply = put('/api/v1/repos/demo/policy', '{"verify_on":"linux,@nightly"}')
    call check(index(reply, 'verify_on is runner classes') > 0, &
        'a schedule without a class is refused: '//reply)
    ! Each runner has a work tree of its own, apart from other tests'.
    run = proc_fork()
    if (run == 0) then
        call getcwd(cwd)
        call execute_command_line('mkdir -p '//data//'/tmp-linux', exitstat=stat)
        call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp-linux')
        call runner_main('http://'//base, runner, data, '')
        call proc_exit(0)
    end if
    mac = proc_fork()
    if (mac == 0) then
        call getcwd(cwd)
        call execute_command_line('mkdir -p '//data//'/tmp-macos', exitstat=stat)
        call proc_setenv('TMPDIR', trim(cwd)//'/'//data//'/tmp-macos')
        call runner_main('http://'//base, macos, data, '', 'macos-arm64')
        call proc_exit(0)
    end if
    url = 'http://x:'//admin//'@'//base//'/demo.git'
    gw = git//'-C '//work//' '

    ! The verification fails on macos-arm64 when MACBROKEN exists.
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && printf "#!/bin/sh\necho full suite\ntest -f README && '// &
        '{ test \"\$FORTYARD_RUNNER_CLASS\" != macos-arm64 || test ! -f MACBROKEN; }\n" > '// &
        work//'/.fortyard/verify && chmod +x '//work//'/.fortyard/check '//work// &
        '/.fortyard/verify && '//gw//'add -A && '//gw//'commit -q -m First && '//gw// &
        'push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'a first branch is pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call wait_count(REGULAR, 1)
    ! The pass that settled it looked at the nightly class first.
    call check(count(NIGHTLY) == 0 .and. count("SELECT count(*) FROM checks WHERE class ="// &
        " 'macos-arm64'") == 0, 'a nightly class is not verified outside the window')

    ! 01:30: the nightly class verifies main once.
    call serve('2026-11-13T01:30')
    call wait_count(NIGHTLY//" AND state = 'passed'", 1)
    call check(count("SELECT count(*) FROM verify_nights WHERE class = 'macos-arm64'"// &
        " AND night = '2026-11-13' AND main = '"//main_sha()//"'") == 1, &
        'the nightly class records the main of its night')

    ! The next night, main has not moved: a requested verification of the
    ! other classes passes, and the nightly class is not verified again.
    call serve('2026-11-14T01:30')
    reply = post('/api/v1/repos/demo/verify', '{}')
    call wait_count(REGULAR, 2)
    call check(count(NIGHTLY) == 1, 'a nightly class is not verified again without a new main')
    call check(count("SELECT count(*) FROM checks WHERE class = 'macos-arm64'") == 1, &
        'the request ran on the classes without schedule only')
    ! Main moves later in that night: it waits for the next night.
    call land('third', 'printf "3\n" > three', 'Third')
    call wait_count(REGULAR//" AND commit_id = '"//main_sha()//"'", 1)
    call check(count(NIGHTLY) == 1, 'a main that moves after the first pass of the night'// &
        ' is not verified on a nightly class that night')

    ! Noon: main moves twice, the first move breaks macOS; only linux verifies.
    call serve('2026-11-14T12:00')
    call land('mac', 'printf "m\n" > MACBROKEN', 'Break macOS')
    call land('more', 'printf "4\n" > four', 'Fourth')
    call wait_count(REGULAR//" AND commit_id = '"//main_sha()//"'", 1)
    call check(count(NIGHTLY) == 1, 'a new main is not verified on a nightly class by day')

    ! The next night main fails on macos-arm64. Morning comes before the
    ! three commits since the last pass are bisected: what was started ends,
    ! and the next step waits. A pass that settles a requested verification
    ! of the other classes has looked at the nightly class first.
    call serve('2026-11-15T01:30')
    call wait_count(NIGHTLY//" AND state = 'failed'", 1)
    call serve('2026-11-15T06:00')
    do i = 1, 3000
        if (count(NIGHTLY//" AND state IN ('queued', 'running')") == 0) exit
        call proc_sleep_ms(100)
    end do
    k = count(NIGHTLY)
    i = count(REGULAR)
    reply = post('/api/v1/repos/demo/verify', '{}')
    call wait_count(REGULAR, i + 1)
    call check(count(NIGHTLY) == k .and. count(NIGHTLY//" AND state = 'failed'") == 1 .and. &
        count("SELECT count(*) FROM verify_reports") == 0, &
        'a bisection on a nightly class waits for the next night')

    ! The next night the bisection goes on, and the commit that broke macOS
    ! is reported naming the class.
    call serve('2026-11-16T01:30')
    reply = ''
    do i = 1, 3000
        reply = get('/api/v1/repos/demo/issues')
        if (index(reply, 'Verification on macos-arm64 fails since') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, 'Verification on macos-arm64 fails since') > 0 .and. &
        index(reply, 'Break macOS') > 0, 'the failure is reported naming the class and the'// &
        ' commit that broke it: '//reply)
    call check(count(NIGHTLY//" AND state = 'failed'") == 2, &
        'main and the commit before it were verified on the nightly class')
    k = index(reply, '"number":') + 9
    issue = ''
    do while (index('0123456789', reply(k:k)) > 0)
        issue = issue//reply(k:k)
        k = k + 1
    end do
    reply = get('/api/v1/repos/demo/issues/'//issue)
    call check(index(reply, 'macos-arm64@nightly') > 0 .and. index(reply, 'full suite') > 0, &
        'the issue names the nightly class and has the failing log: '//reply)
    call check(count("SELECT count(*) FROM verify_reports WHERE culprit NOT LIKE '%@%'") == 0, &
        'the classes without schedule report nothing')

    ! A fix lands by day; the next night the class passes and its issue closes.
    call serve('2026-11-16T12:00')
    call land('fix', 'git rm -q MACBROKEN', 'Remove MACBROKEN')
    call wait_count(REGULAR//" AND commit_id = '"//main_sha()//"'", 1)
    call serve('2026-11-17T01:30')
    do i = 1, 3000
        reply = get('/api/v1/repos/demo/issues/'//issue)
        if (index(reply, '"state":"closed"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"closed"') > 0 .and. &
        index(reply, 'main on macos-arm64 passes again') > 0, &
        'the issue is closed when the class passes again: '//reply)

    ! An open report of a class that verify_on no longer names at night is
    ! closed, as nothing would verify that class again. The report is recorded
    ! only once a verification settled under the new policy: before, the
    ! verifier still sees main passing on macos-arm64 and closes the report as
    ! passing again.
    reply = put('/api/v1/repos/demo/policy', '{"verify_on":"linux"}')
    i = count(REGULAR)
    reply = post('/api/v1/repos/demo/verify', '{}')
    call wait_count(REGULAR, i + 1)
    reply = post('/api/v1/repos/demo/issues', '{"title":"Reported at night","body":""}')
    k = index(reply, '"issue":') + 8
    issue = ''
    do while (index('0123456789', reply(k:k)) > 0)
        issue = issue//reply(k:k)
        k = k + 1
    end do
    call check(len(issue) > 0, 'an issue is opened: '//reply)
    call store_sql("INSERT INTO verify_reports (repo, culprit, issue, created_at) VALUES"// &
        " ('demo', 'unknown@macos-arm64', "//issue//", 'x')")
    reply = post('/api/v1/repos/demo/verify', '{}')
    do i = 1, 3000
        reply = get('/api/v1/repos/demo/issues/'//issue)
        if (index(reply, '"state":"closed"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"closed"') > 0 .and. &
        index(reply, 'no longer verified at night') > 0, &
        'the report of a class no longer verified at night is closed: '//reply)

    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    call proc_kill(mac, SIGTERM)
    call proc_wait(mac, stat)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    call sock_close(lfd)
    call execute_command_line('rm -rf '//data//' '//work, exitstat=stat)
    print '(a)', 'verify nightly: all checks passed'

contains

    subroutine serve(now)
        !! (Re)starts the server with the verifier's clock at `now`. The old
        !! server's processes end after it, so they are waited for: a verifier
        !! left with the old clock could still start a job.
        character(len=*), intent(in) :: now
        character(len=16) :: pid
        integer :: j
        if (sup > 0) then
            write (pid, '(i0)') sup
            call execute_command_line('pgrep -P '//trim(pid)//' > '//data//'/children.txt', &
                exitstat=stat)
            call proc_kill(sup, SIGTERM)
            call proc_wait(sup, stat)
            ! A zombie counts as ended: a subreaper above the test may reap it late.
            call execute_command_line('for i in $(seq 3000); do ps -o stat= -p "$(paste -sd, '// &
                data//'/children.txt)" | grep -qv Z || exit 0; sleep 0.1; done; exit 1', &
                exitstat=stat)
            call check(stat == 0, 'the processes of the old server end')
        end if
        call proc_setenv('FY_VERIFY_NOW', now)
        sup = proc_fork()
        if (sup == 0) then
            call server_supervise(lfd, data//'/fortyard.db', 1, 2)
            call proc_exit(0)
        end if
        ! A request sent before the new server answers can be lost to the
        ! client's timeout, so the test goes on only once it answers.
        do j = 1, 3000
            if (index(get('/api/v1/repos/demo/policy'), '"verify_on"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'the server at '//now//' answers')
    end subroutine serve

    subroutine land(branch, edit, subject)
        !! A branch from main with one commit, landed.
        character(len=*), intent(in) :: branch, edit, subject
        integer :: j
        call execute_command_line(gw//'fetch -q '//url//' main && '//gw// &
            'checkout -q -B '//branch//' FETCH_HEAD && (cd '//work//' && '//edit//') && '//gw// &
            'add -A && '//gw//'commit -q -m "'//subject//'" && '//gw//'push -q '//url// &
            ' HEAD:'//branch, exitstat=stat)
        call check(stat == 0, 'branch '//branch//' is pushed')
        reply = post('/api/v1/repos/demo/land', '{"branch":"'//branch//'"}')
        do j = 1, 3000
            reply = get('/api/v1/repos/demo/landings')
            if (index(reply, '"branch":"'//branch//'","account":"admin","state":"landed"') > 0) &
                return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing of '//branch//': '//reply)
    end subroutine land

    function main_sha() result(sha)
        character(len=:), allocatable :: sha
        character(len=64) :: line
        integer :: u
        call execute_command_line(gw//'ls-remote '//url//' refs/heads/main > '//data//'/main.txt', &
            exitstat=stat)
        open (newunit=u, file=data//'/main.txt', action='read')
        read (u, '(a)') line
        close (u)
        sha = line(1:40)
    end function main_sha

    subroutine wait_count(sql, n)
        character(len=*), intent(in) :: sql
        integer, intent(in) :: n
        integer :: j
        do j = 1, 3000
            if (count(sql) >= n) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'waiting for '//sql)
    end subroutine wait_count

    subroutine store_sql(sql)
        !! Writes to the server's database.
        character(len=*), intent(in) :: sql
        type(db_t) :: d
        integer :: s
        call store_open(data//'/fortyard.db', d, s)
        call check(s == 0, 'the database opens')
        call db_exec(d, sql//';', s)
        call check(s == SQLITE_OK, 'the database takes '//sql)
        call db_close(d)
    end subroutine store_sql

    integer function count(sql) result(n)
        !! The number `sql` counts in the server's database.
        character(len=*), intent(in) :: sql
        type(db_t) :: d
        type(stmt_t) :: st
        integer :: s
        n = -1
        call store_open(data//'/fortyard.db', d, s)
        if (s /= 0) return
        call db_prepare(d, sql//';', st, s)
        if (s == SQLITE_OK) then
            if (stmt_step(st) == SQLITE_ROW) n = int(stmt_int(st, 0))
            call stmt_finalize(st)
        end if
        call db_close(d)
    end function count

    function post(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        r = send('POST', path, body)
    end function post

    function put(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        r = send('PUT', path, body)
    end function put

    function send(method, path, body) result(r)
        character(len=*), intent(in) :: method, path, body
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange(method//' '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)//CRLF//CRLF//body)
    end function send

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
            if (mac > 0) call proc_kill(mac, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_verify_nightly
