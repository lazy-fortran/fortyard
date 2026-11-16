program test_land_resume
    !! A server restart, as every deploy makes, while landings are being
    !! checked: the lander goes on with the same candidate and its runner
    !! check instead of queuing a new one; a check that passed, or a move of
    !! main, before the restart lands the candidate exactly once; a landing
    !! cancelled before the restart stays out of what lands after it; and a
    !! check that ran in the old process is run again. Here the test is the
    !! runner, and it stops the server and starts a new one on the same data.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_prepare, stmt_t, stmt_step, stmt_int, stmt_finalize, &
        SQLITE_ROW
    use fy_store, only: &
        store_bootstrap, &
        store_check_finish, &
        store_landing_cancel, &
        store_open, &
        store_repo_add, &
        store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_land_resume_data'
    character(len=*), parameter :: work = 'build/test_land_resume_work'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, server, url, body, out, id, ref, kind
    character(len=:), allocatable :: h, a, b, made, before, abs_data, old, repo
    character(len=16) :: num
    integer :: stat, sup, code, n
    logical :: found, cancelled

    sup = 0
    repo = 'demo'
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call run('cd '//data//' && pwd')
    abs_data = trim_nl(out)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call store_repo_add(db, 'loc', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call start()

    ! main, from a first landing.
    call run('git init -q -b main '//work//' && printf "one\n" > '//work//'/README && '// &
        git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m Start && '// &
        git//'-C '//work//' push -q '//url//' HEAD:start')
    call check(stat == 0, 'a branch is pushed: '//out)
    h = land('start')
    call pass(claim())
    call wait_state(h, 'landed')

    ! 1. The check of h runs on the runner when the server restarts. The new
    ! lander waits for that check: its result lands the very commit it checked.
    h = land(branch('h', 'H'))
    id = claim()
    made = rev(ref)
    n = checks()
    call restart()
    call pass(id)
    call wait_state(h, 'landed')
    call check(rev('refs/heads/main') == made, 'main is the commit the runner checked: '// &
        rev('refs/heads/main')//' '//made)
    call check(checks() == n, 'no second check was queued')
    call check(.not. has_ref('refs/heads/h'), 'the landed branch is removed')
    call check(.not. has_ref('refs/fortyard/pinned/'//h), 'the pinned ref is removed')

    ! The same for a candidate of two landings, c and d.
    h = land(branch('h1', 'H1'))
    id = claim()
    a = land(branch('c', 'C'))
    b = land(branch('d', 'D'))
    call pass(id)
    call wait_state(h, 'landed')
    id = claim()
    call check(index(files(ref), ' C ') > 0 .and. index(files(ref), ' D ') > 0, &
        'c and d share a candidate: '//files(ref))
    made = rev(ref)
    n = checks()
    call restart()
    call pass(id)
    call wait_state(a, 'landed')
    call wait_state(b, 'landed')
    call check(rev('refs/heads/main') == made, 'main is the candidate the runner checked')
    call check(checks() == n, 'no second check was queued for the candidate')
    call check(.not. has_ref('refs/heads/c') .and. .not. has_ref('refs/heads/d'), &
        'the landed branches are removed')

    ! 2. The check passes while no server runs, before main moved: the next
    ! lander moves main once.
    before = rev('refs/heads/main')
    h = land(branch('p', 'P'))
    id = claim()
    made = rev(ref)
    n = checks()
    call stop()
    call finish_check(id)
    call start()
    call wait_state(h, 'landed')
    call check(rev('refs/heads/main') == made .and. rev('refs/heads/main^') == before, &
        'main moved once, to the checked commit')
    call check(checks() == n, 'no second check was queued after a pass')
    call check(trailers(h) == 1, 'one commit on main lands p')

    ! 3. main moved and the server stopped before the landing was recorded:
    ! the landing is landed and main stays.
    before = rev('refs/heads/main')
    h = land(branch('q', 'Q'))
    id = claim()
    made = rev(ref)
    n = checks()
    call stop()
    call finish_check(id)
    call run('git --git-dir '//data//'/repos/demo.git update-ref refs/heads/main '//made//' '//before)
    call check(stat == 0, 'main is moved as the old lander did: '//out)
    call start()
    call wait_state(h, 'landed')
    call check(rev('refs/heads/main') == made, 'main is not moved again')
    call check(checks() == n, 'no second check was queued after the move')
    call check(trailers(h) == 1, 'one commit on main lands q')
    call check(.not. has_ref('refs/heads/q'), 'the branch of q is removed')
    call http('GET', '/api/v1/jobs/'//h, '', admin)
    call check(index(body, made) > 0 .and. index(body, 'before a restart') > 0, &
        'the job names the commit it landed as: '//body)

    ! 4. A candidate of a and b is checked; b is cancelled while no server
    ! runs. The new lander withdraws that check and checks a alone.
    h = land(branch('h2', 'H2'))
    id = claim()
    a = land(branch('a', 'A'))
    b = land(branch('b', 'B'))
    call pass(id)
    call wait_state(h, 'landed')
    id = claim()
    call check(index(files(ref), ' A ') > 0 .and. index(files(ref), ' B ') > 0, &
        'a and b share a candidate: '//files(ref))
    call stop()
    call store_open(data//'/fortyard.db', db, stat)
    call store_landing_cancel(db, 'demo', to_int(b), 'admin', old, found, cancelled)
    call db_close(db)
    call check(cancelled, 'b is cancelled: '//old)
    call start()
    old = id
    id = claim()
    call check(to_int(id) > to_int(old), 'a new check is queued: '//id)
    call check(index(files(ref), ' A ') > 0 .and. index(files(ref), ' B ') == 0, &
        'the new candidate is built without b: '//files(ref))
    call http('POST', '/api/v1/checks/'//old//'/result', '{"passed":true,"log":"ok"}', runner)
    call check(code == 409, 'the result of the withdrawn check is refused: '//body)
    call pass(id)
    call wait_state(a, 'landed')
    call check(state(b) == 'cancelled', 'b stays cancelled')
    call run('git --git-dir '//data//'/repos/demo.git ls-tree --name-only refs/heads/main')
    call check(index(out, 'A'//new_line('a')) > 0 .and. index(out, 'B'//new_line('a')) == 0, &
        'main has a and not b: '//out)

    ! 5. A check that runs in the server dies with it and is run again. Its
    ! script counts its runs and ends once the file go exists.
    call stop()
    call server_set_checks('local')
    repo = 'loc'
    call start()
    call run('touch '//abs_data//'/go && rm -rf '//work//' && git init -q -b main '//work// &
        ' && mkdir '//work//'/.fortyard && printf ''#!/bin/sh\necho run >> "'//abs_data// &
        '/runs"\nwhile [ ! -e "'//abs_data//'/go" ]; do sleep 0.1; done\n'' > '//work// &
        '/.fortyard/check && chmod 755 '//work//'/.fortyard/check && '//git//'-C '//work// &
        ' add -A && '//git//'-C '//work//' commit -q -m Start && '//git//'-C '//work// &
        ' push -q '//url//' HEAD:start')
    call check(stat == 0, 'a branch with a check is pushed: '//out)
    h = land('start')
    call wait_state(h, 'landed')
    call run('rm '//abs_data//'/go')
    h = land(branch('l', 'L'))
    call wait_runs(2)
    call restart()
    call wait_runs(3)
    call run('touch '//abs_data//'/go')
    call wait_state(h, 'landed')
    call run('git --git-dir '//data//'/repos/loc.git ls-tree --name-only refs/heads/main')
    call check(index(out, 'L'//new_line('a')) > 0, 'main has l: '//out)

    call stop()
    call execute_command_line('rm -rf '//data//' '//work, exitstat=stat)
    print '(a)', 'land resume: all checks passed'

contains

    subroutine start()
        !! A server on a new port, on the same data directory.
        integer :: lfd, k
        logical :: exists

        call tcp_listen('127.0.0.1', 0, lfd, stat)
        write (num, '(i0)') sock_bound_port(lfd)
        server = 'http://127.0.0.1:'//trim(num)
        sup = proc_fork()
        if (sup == 0) then
            call server_supervise(lfd, data//'/fortyard.db', 1, 2)
            call proc_exit(0)
        end if
        call sock_close(lfd)
        do k = 1, 300
            inquire (file=data//'/repos/'//repo//'.git/HEAD', exist=exists)
            if (exists) exit
            call proc_sleep_ms(100)
        end do
        url = 'http://x:'//admin//'@127.0.0.1:'//trim(num)//'/'//repo//'.git'
        do k = 1, 300
            call http('GET', '/api/v1/jobs', '', admin)
            if (code == 200) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'the server answers')
    end subroutine start

    subroutine stop()
        !! Stops the server and waits until its processes are gone.
        character(len=:), allocatable :: children
        integer :: k

        call run('pgrep -P '//itoa(sup)//' | tr "\n" " "')
        children = trim(out)
        call proc_kill(sup, SIGTERM)
        call proc_wait(sup, stat)
        sup = 0
        if (len(children) == 0) return
        do k = 1, 600
            ! A zombie has ended: whoever adopted it may not reap it soon.
            call run('for p in '//children//'; do s=$(ps -o stat= -p $p); '// &
                'case "$s" in ""|Z*) ;; *) exit 1;; esac; done; exit 0')
            if (stat == 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'the server processes end')
    end subroutine stop

    subroutine restart()
        call stop()
        call start()
    end subroutine restart

    subroutine finish_check(check_id)
        !! Records the runner's pass of `check_id` as the API does.
        character(len=*), intent(in) :: check_id
        logical :: ok
        call store_open(data//'/fortyard.db', db, stat)
        call store_check_finish(db, to_int(check_id), 'runner-1', 'passed', 'ok', ok)
        call db_close(db)
        call check(ok, 'the check passes while no server runs')
    end subroutine finish_check

    integer function checks()
        !! The checks of landings queued so far.
        type(stmt_t) :: st
        checks = -1
        call store_open(data//'/fortyard.db', db, stat)
        call db_prepare(db, "SELECT count(*) FROM checks WHERE kind = 'check';", st, stat)
        if (stmt_step(st) == SQLITE_ROW) checks = int(stmt_int(st, 0))
        call stmt_finalize(st)
        call db_close(db)
    end function checks

    integer function trailers(landing)
        !! The commits on main that name `landing` in their Landing: trailer.
        character(len=*), intent(in) :: landing
        integer :: ios
        call run('git --git-dir '//data//'/repos/demo.git log --format=%B refs/heads/main | '// &
            'grep -c "^Landing: '//landing//'@"')
        trailers = -1
        read (out, *, iostat=ios) trailers
    end function trailers

    subroutine wait_runs(want)
        !! Waits until the check script of loc started `want` times.
        integer, intent(in) :: want
        integer :: k, got, ios
        do k = 1, 600
            call run('cat '//abs_data//'/runs 2>/dev/null | wc -l')
            got = 0
            read (out, *, iostat=ios) got
            if (got >= want) then
                call check(got == want, 'the check ran '//itoa(want)//' times: '//out)
                return
            end if
            call proc_sleep_ms(100)
        end do
        call check(.false., 'the check ran '//itoa(want)//' times')
    end subroutine wait_runs

    function branch(name, file) result(nm)
        !! Pushes the branch `name` from main, adding `file`.
        character(len=*), intent(in) :: name, file
        character(len=:), allocatable :: nm
        call run(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '//work// &
            ' checkout -q -B '//name//' FETCH_HEAD && printf "x\n" > '//work// &
            '/'//file//' && '//git//'-C '//work//' add '//file//' && '//git//'-C '//work// &
            ' commit -q -m "Add '//file//'" && '//git//'-C '//work//' push -q '//url//' '//name)
        call check(stat == 0, 'pushed '//name//': '//out)
        nm = name
    end function branch

    function land(name) result(landing)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: landing
        integer :: k, e
        call http('POST', '/api/v1/repos/'//repo//'/land', '{"branch":"'//name//'"}', admin)
        call check(code == 202, 'landing queued: '//body)
        k = index(body, '"landing":') + 10
        e = k
        do while (index('0123456789', body(e:e)) > 0)
            e = e + 1
        end do
        landing = body(k:e - 1)
    end function land

    function claim() result(check_id)
        !! Claims the next check of a landing as the runner, waiting until one
        !! is queued; sets `ref` to what it checks. A verification of main it
        !! claims meanwhile is held and never reported.
        character(len=:), allocatable :: check_id
        integer :: k, s, e
        check_id = ''
        do k = 1, 600
            call http('POST', '/api/v1/checks/claim', '{}', runner)
            s = index(body, '"id":')
            if (s > 0) then
                s = s + 5
                e = s
                do while (index('0123456789', body(e:e)) > 0)
                    e = e + 1
                end do
                check_id = body(s:e - 1)
                s = index(body, '"ref":"') + 7
                ref = body(s:s + index(body(s:), '"') - 2)
                s = index(body, '"kind":"') + 8
                kind = body(s:s + index(body(s:), '"') - 2)
                if (kind == 'check') return
                cycle
            end if
            call proc_sleep_ms(100)
        end do
        call check(.false., 'a check is queued for the runner: '//body)
    end function claim

    subroutine pass(check_id)
        character(len=*), intent(in) :: check_id
        integer :: k
        ! A server that has just started may not answer yet.
        do k = 1, 300
            call http('POST', '/api/v1/checks/'//check_id//'/result', &
                '{"passed":true,"log":"ok"}', runner)
            if (code /= 0) exit
            call proc_sleep_ms(100)
        end do
        call check(code == 200, 'the check passes: '//body)
    end subroutine pass

    function rev(r) result(commit)
        character(len=*), intent(in) :: r
        character(len=:), allocatable :: commit
        call run('git --git-dir '//data//'/repos/'//repo//'.git rev-parse --verify --quiet '//r)
        commit = trim_nl(out)
    end function rev

    logical function has_ref(r)
        character(len=*), intent(in) :: r
        has_ref = len(rev(r)) > 0
    end function has_ref

    function files(r) result(list)
        !! The files of the commit at the ref `r` of the repository.
        character(len=*), intent(in) :: r
        character(len=:), allocatable :: list
        call run('git --git-dir '//data//'/repos/'//repo//'.git ls-tree --name-only '//r// &
            ' | tr "\n" " "')
        list = ' '//out
    end function files

    function state(landing) result(s)
        character(len=*), intent(in) :: landing
        character(len=:), allocatable :: s
        integer :: k
        call http('GET', '/api/v1/jobs/'//landing, '', admin)
        s = ''
        k = index(body, '"state":"')
        if (k > 0) s = body(k + 9:k + 7 + index(body(k + 9:), '"'))
    end function state

    subroutine wait_state(landing, want)
        character(len=*), intent(in) :: landing, want
        integer :: k
        do k = 1, 600
            if (state(landing) == want) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//landing//' is '//want//': '//body)
    end subroutine wait_state

    integer(int64) function to_int(text)
        character(len=*), intent(in) :: text
        integer :: ios
        to_int = 0
        read (text, *, iostat=ios) to_int
    end function to_int

    function itoa(v) result(s)
        integer, intent(in) :: v
        character(len=:), allocatable :: s
        character(len=16) :: t
        write (t, '(i0)') v
        s = trim(t)
    end function itoa

    function trim_nl(text) result(s)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        integer :: e
        e = index(text, new_line('a'))
        if (e == 0) e = len(text) + 1
        s = trim(text(1:e - 1))
    end function trim_nl

    subroutine http(method, path, payload, token)
        character(len=*), intent(in) :: method, path, payload, token
        character(len=16) :: c
        character(len=:), allocatable :: send
        integer :: ios
        send = ''
        if (len(payload) > 0) send = ' -d '''//payload//''''
        call execute_command_line('curl -s -o '//data//'/body -w "%{http_code}" -X '//method// &
            ' -H "Authorization: Bearer '//token//'" -H "Content-Type: application/json"'// &
            send//' '//server//path//' > '//data//'/code', exitstat=stat)
        body = read_all(data//'/body')
        c = read_all(data//'/code')
        code = 0
        read (c, *, iostat=ios) code
    end subroutine http

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: unit, ios, n
        text = ''
        open (newunit=unit, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=unit, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (unit, iostat=ios) text
        close (unit)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            call execute_command_line('touch '//data//'/go 2>/dev/null')
            error stop 1
        end if
    end subroutine check

end program test_land_resume
