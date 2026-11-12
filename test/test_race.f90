program test_race
    !! Racing (doc/backends.md "Racing") through the runner API, with the test
    !! as the own linux runner and as the backend processes. Recorded checks
    !! put eta(R, github-linux) at 80 s and eta(R, linux) at 100 s, less than
    !! 25 % apart with the own runner idle: a GitHub claim gets a twin of the
    !! check (same ref) and the check stays queued for the own runner. The
    !! twin's pass decides a verification and a landing, and the own runner's
    !! next log upload and its result are refused; the own runner's failure
    !! decides as well, and the twin is then withdrawn for its backend. A
    !! run without verdict on either side decides nothing, and after the
    !! twin's the check waits for its home class as if queued again. A GitLab
    !! class never races, estimates 25 % apart are not raced, and a check
    !! whose twin runs is not taken by another backend.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec, db_prepare, stmt_finalize, stmt_int, stmt_step, &
        stmt_t, SQLITE_ROW
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_race_data'
    character(len=*), parameter :: work = 'build/test_race_work'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=*), parameter :: T = "strftime('%Y-%m-%dT%H:%M:%SZ', 'now'"
    type(db_t) :: db
    character(len=:), allocatable :: admin, own, gh, gl, server, url, body, out, ref, h, c, w
    character(len=16) :: num
    integer :: lfd, stat, sup, i, code

    sup = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', own, stat)
    call store_token_create(db, 1_int64, 'gh-1', 'runner', gh, stat)
    call store_token_create(db, 1_int64, 'gl-1', 'runner', gl, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call sql("UPDATE repos SET public = 1, policy_backends = 'github,gitlab' WHERE name = 'demo';")
    ! run(demo, linux) 100 s; run 80 s and no wait on github-linux and gitlab-x.
    call record('linux', 'NULL', 100, 2, 5)
    call record('linux', "'github-linux'", 80, 2, 5)
    call record('linux', "'gitlab-x'", 80, 2, 5)
    call sql("INSERT INTO checks (repo, ref, state, created_at, started_at, class, ran_class)"// &
        " VALUES ('demo', 'w', 'cancelled', "//T//", '-10 minutes'), "//T//", '-10 minutes'),"// &
        " 'linux', 'github-linux'), ('demo', 'w', 'cancelled', "//T//", '-10 minutes'), "//T// &
        ", '-10 minutes'), 'linux', 'gitlab-x');")
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 150
        call http('GET', '/api/v1/version', '', admin)
        if (code == 200) exit
        call proc_sleep_ms(100)
    end do
    url = 'http://x:'//admin//'@127.0.0.1:'//trim(num)//'/demo.git'

    ! main, from a first landing the own runner checks; its verification races.
    call run('git init -q -b main '//work//' && mkdir '//work//'/.fortyard && printf "true\n" > '// &
        work//'/.fortyard/verify && printf "one\n" > '//work//'/README && '// &
        git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m Start && '// &
        git//'-C '//work//' push -q '//url//' HEAD:start')
    call check(stat == 0, 'a branch is pushed: '//out)
    h = land('start')
    call finish(claim(own, 'linux'), own, '{"passed":true,"log":"ok"}', 200)
    call wait_state(h, 'landed')
    w = claim(gh, 'github-linux')
    call check(rows(ref) == 2, 'the verification is raced: a twin of the same ref')
    c = claim(own, 'linux')
    call check(c /= w .and. rows(ref) == 2, 'the own runner gets the check itself')
    call finish(w, gh, '{"passed":true,"log":"twin passed"}', 200)
    call wait_state('2', 'passed')
    call check(index(body, 'twin passed') > 0, 'the verifier takes the twin''s verdict: '//body)
    call finish(c, own, '{"passed":true,"log":"late"}', 409)
    ! Later verifications go to a class no one serves.
    call sql("UPDATE repos SET policy_verify_on = 'spare' WHERE name = 'demo';")
    call run(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '//work// &
        ' checkout -q -B base FETCH_HEAD')
    call check(stat == 0, 'main is fetched: '//out)

    ! The backend twin passes first; the own runner's run is withdrawn.
    h = land(branch('a', 'A'))
    w = claim(gh, 'github-linux')
    c = claim(own, 'linux')
    call check(c /= w .and. rows(ref) == 2, 'a landing check is raced')
    call http('POST', '/api/v1/checks/'//c//'/log?offset=0', 'line', own)
    call check(code == 200, 'the own runner sends its log while both run: '//body)
    call finish(w, gh, '{"passed":true,"log":"twin passed"}', 200)
    call wait_state(h, 'landed')
    call http('POST', '/api/v1/checks/'//c//'/log?offset=4', 'more', own)
    call check(code == 409 .and. index(body, '"length"') == 0, &
        'the own runner''s next log upload is refused as for a withdrawn check: '//body)
    call finish(c, own, '{"passed":false,"log":"late"}', 409)

    ! The own runner fails first, which decides; the twin is withdrawn.
    h = land(branch('b', 'B'))
    w = claim(gh, 'github-linux')
    c = claim(own, 'linux')
    call check(c /= w .and. rows(ref) == 2, 'raced again')
    call finish(c, own, '{"passed":false,"log":"own failed"}', 200)
    call wait_state(h, 'failed')
    call http('GET', '/api/v1/checks/'//w//'/publish', '', gh)
    call check(index(body, '"state":"cancelled"') > 0, 'the backend sees its twin withdrawn: '//body)
    call finish(w, gh, '{"passed":true,"log":"late"}', 409)

    ! No verdict on one side decides nothing: the own runner's error, then
    ! the twin's.
    h = land(branch('c', 'C'))
    w = claim(gh, 'github-linux')
    c = claim(own, 'linux')
    call finish(c, own, '{"error":"runner","log":"lost"}', 200)
    call check(index(body, '"state":"queued"') > 0, 'the own run is queued again: '//body)
    call check(state(h) == 'running', 'and decides nothing')
    call finish(w, gh, '{"passed":true,"log":"twin passed"}', 200)
    call wait_state(h, 'landed')
    call http('POST', '/api/v1/checks/claim', '{}', own)
    call check(code == 204, 'the decided check is no longer queued: '//body)
    h = land(branch('d', 'D'))
    w = claim(gh, 'github-linux')
    c = claim(own, 'linux')
    call finish(w, gh, '{"error":"runner","log":"no job"}', 200)
    call check(index(body, '"state":"cancelled"') > 0, 'the twin''s error ends the twin: '//body)
    call check(state(h) == 'running', 'and decides nothing')
    call finish(c, own, '{"passed":true,"log":"own passed"}', 200)
    call wait_state(h, 'landed')
    ! The twin's error after the check waited an hour: it waits again from then.
    h = land(branch('g', 'G'))
    w = claim(gh, 'github-linux')
    call sql("UPDATE checks SET created_at = "//T//", '-2 hours'), queued_at = "//T// &
        ", '-2 hours') WHERE ref = '"//ref//"' AND state = 'queued';")
    call finish(w, gh, '{"error":"runner","log":"no job"}', 200)
    call check(count("ref = '"//ref//"' AND state = 'queued' AND queued_at >= "//T// &
        ", '-10 minutes')") == 1, 'the raced check is queued again by the twin''s error')
    c = claim(own, 'linux')
    call finish(c, own, '{"passed":true,"log":"own passed"}', 200)
    call wait_state(h, 'landed')

    ! GitLab never races: it takes the check itself.
    h = land(branch('e', 'E'))
    c = claim(gl, 'gitlab-x')
    call check(rows(ref) == 1, 'no twin for a GitLab class')
    call finish(c, gl, '{"passed":true,"log":"gitlab passed"}', 200)
    call wait_state(h, 'landed')

    ! eta(R, github-linux) 75 s against 100 s: 25 % apart, not raced.
    call record('linux', "'github-linux'", 75, 1, 10)
    h = land(branch('f', 'F'))
    c = claim(gh, 'github-linux')
    call check(rows(ref) == 1, 'no twin when the estimates differ by 25 %')
    call finish(c, gh, '{"passed":true,"log":"github passed"}', 200)
    call wait_state(h, 'landed')

    ! A twin whose row exists before its check names it: the check is not
    ! taken by another backend claim meanwhile.
    h = land(branch('k', 'K'))
    do i = 1, 600
        if (count("class = 'linux' AND state = 'queued'") == 1) exit
        call proc_sleep_ms(100)
    end do
    call sql("INSERT INTO checks (repo, ref, created_at, kind, class, state, runner, ran_class,"// &
        " twin) SELECT repo, ref, created_at, kind, class, 'running', 'gh-2', 'github-linux',"// &
        " id FROM checks WHERE class = 'linux' AND state = 'queued';")
    call http('POST', '/api/v1/checks/claim', '{"class":"github-linux"}', gh)
    call check(code == 204, 'a check whose twin runs is not taken: '//body)
    call sql("UPDATE checks SET state = 'cancelled' WHERE runner = 'gh-2';")
    c = claim(gh, 'github-linux')
    call finish(c, gh, '{"passed":true,"log":"github passed"}', 200)
    call wait_state(h, 'landed')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    sup = 0
    call db_close(db)
    call execute_command_line('rm -rf '//data//' '//work, exitstat=stat)
    print '(a)', 'race: all checks passed'

contains

    subroutine record(class, ran, seconds, days, n)
        !! `n` passed checks of demo, each `seconds` long, `days` ago.
        character(len=*), intent(in) :: class, ran
        integer, intent(in) :: seconds, days, n
        character(len=:), allocatable :: at
        character(len=16) :: s, d
        integer :: k

        write (s, '(i0)') seconds
        write (d, '(i0)') days
        at = T//", '-"//trim(d)//" days'"
        do k = 1, n
            call sql('INSERT INTO checks (repo, ref, state, created_at, claimed_at, started_at,'// &
                " finished_at, class, ran_class) VALUES ('demo', 'h', 'passed', "//at//'), '// &
                at//'), '//at//'), '//at//", '+"//trim(s)//" seconds'), '"//class//"', "//ran//');')
        end do
    end subroutine record

    subroutine sql(text)
        character(len=*), intent(in) :: text
        integer :: s
        call db_exec(db, text, s)
        call check(s == 0, 'sql: '//text)
    end subroutine sql

    integer function count(where)
        !! The checks that match `where`.
        character(len=*), intent(in) :: where
        type(stmt_t) :: st
        integer :: s
        count = -1
        call db_prepare(db, 'SELECT count(*) FROM checks WHERE '//where//';', st, s)
        if (stmt_step(st) == SQLITE_ROW) count = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function count

    integer function rows(r)
        !! The checks of the ref `r`.
        character(len=*), intent(in) :: r
        type(stmt_t) :: st
        integer :: s
        rows = -1
        call db_prepare(db, "SELECT count(*) FROM checks WHERE ref = '"//r//"';", st, s)
        if (stmt_step(st) == SQLITE_ROW) rows = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function rows

    function branch(name, file) result(n)
        !! Pushes the branch `name` from main, adding `file`.
        character(len=*), intent(in) :: name, file
        character(len=:), allocatable :: n
        call run(git//'-C '//work//' checkout -q -b '//name//' base && printf "x\n" > '//work// &
            '/'//file//' && '//git//'-C '//work//' add '//file//' && '//git//'-C '//work// &
            ' commit -q -m "Add '//file//'" && '//git//'-C '//work//' push -q '//url//' '//name)
        call check(stat == 0, 'pushed '//name//': '//out)
        n = name
    end function branch

    function land(name) result(landing)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: landing
        integer :: k, e
        call http('POST', '/api/v1/repos/demo/land', '{"branch":"'//name//'"}', admin)
        call check(code == 202, 'landing queued: '//body)
        k = index(body, '"landing":') + 10
        e = k
        do while (index('0123456789', body(e:e)) > 0)
            e = e + 1
        end do
        landing = body(k:e - 1)
    end function land

    function claim(token, class) result(check_id)
        !! Claims a check for `class` with `token`, waiting until one is
        !! given; sets `ref` to what it checks.
        character(len=*), intent(in) :: token, class
        character(len=:), allocatable :: check_id
        integer :: k, s, e
        check_id = ''
        do k = 1, 600
            call http('POST', '/api/v1/checks/claim', '{"class":"'//class//'"}', token)
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
                return
            end if
            call proc_sleep_ms(100)
        end do
        call check(.false., 'a check is given to '//class//': '//body)
    end function claim

    subroutine finish(check_id, token, result, want)
        !! Reports `result` for the check; the server answers `want`.
        character(len=*), intent(in) :: check_id, token, result
        integer, intent(in) :: want
        call http('POST', '/api/v1/checks/'//check_id//'/result', result, token)
        call check(code == want, 'check '//check_id//' reports '//result//': '//body)
    end subroutine finish

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
        call check(.false., 'job '//landing//' is '//want//': '//body)
    end subroutine wait_state

    subroutine http(method, path, payload, token)
        character(len=*), intent(in) :: method, path, payload, token
        character(len=:), allocatable :: send, cc
        integer :: ios
        send = ''
        if (len(payload) > 0) send = ' -d '''//payload//''''
        call execute_command_line('curl -s -o '//data//'/body -w "%{http_code}" -X '//method// &
            ' -H "Authorization: Bearer '//token//'" -H "Content-Type: application/json"'// &
            send//' '''//server//path//''' > '//data//'/code', exitstat=stat)
        body = read_all(data//'/body')
        cc = read_all(data//'/code')
        code = 0
        read (cc, *, iostat=ios) code
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
            error stop 1
        end if
    end subroutine check

end program test_race
