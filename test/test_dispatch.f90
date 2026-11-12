program test_dispatch
    !! Dispatch to backends (doc/backends.md "Dispatch") on recorded check
    !! rows: run(R, C), speed(C) and wait(C) as the document defines them; a
    !! backend process is given a check only when its estimate is not worse
    !! than the home class's, only of a public repository whose policy names
    !! the backend, only below its concurrency limit and within its time
    !! limit, and only for a class it stands in for; own runners claim as
    !! before and record the class they ran on and the start.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fy_dispatch, only: &
        DISPATCH_NEVER, &
        dispatch_candidates, &
        dispatch_run, &
        dispatch_speed, &
        dispatch_wait
    use fy_policy, only: policy_from_json, policy_get, policy_set, policy_t
    use fy_sqlite, only: db_close, db_exec, db_prepare, db_t, stmt_finalize, stmt_int, &
        stmt_step, stmt_t, stmt_text, SQLITE_ROW
    use fy_store, only: store_bootstrap, store_check_add, store_check_claim, store_check_error, &
        store_check_finish, store_check_take, store_open, store_repo_add, store_repo_set_public
    implicit none

    character(len=*), parameter :: path = 'build/test_dispatch.db'
    character(len=*), parameter :: T = "strftime('%Y-%m-%dT%H:%M:%SZ', 'now'"
    character(len=5), parameter :: REPOS(7) = [character(len=5) :: 'fx', 'ref1', 'priv', &
        'pubnb', 'new', 'two', 'priv2']
    !! Not host names.
    !! Not host names, or ones the egress list must not open: TU Graz,
    !! internal and special-use names, a public suffix.
    character(len=24), parameter :: HOSTS(11) = [character(len=24) :: '10.0.0.1', 'localhost', &
        '-a.example.org', 'a..example.org', 'A.example.org', 'x.org/path', 'tugraz.at', &
        'git.tugraz.at', 'foo.local', 'metadata.google.internal', 'co.uk']
    type(db_t) :: db
    type(policy_t) :: p
    character(len=:), allocatable :: admin, repo, ref, error, text
    integer(int64) :: id, x, y, mac, win, own, aff
    integer(int64), allocatable :: ids1(:), ids2(:)
    integer :: stat, k, n_org, n_win
    logical :: found, refused

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call store_bootstrap(db, admin, stat)
    do k = 1, size(REPOS)
        call store_repo_add(db, trim(REPOS(k)), '', stat, mode='primary')
    end do
    call sql("UPDATE repos SET public = 1, policy_backends = 'github,gitlab' WHERE name = 'fx';"// &
        "UPDATE repos SET public = 1 WHERE name = 'pubnb';"// &
        "UPDATE repos SET policy_backends = 'github' WHERE name = 'priv2';"// &
        "UPDATE repos SET policy_check_timeout = 30 WHERE name = 'new';")

    ! run(R, C): the median of the last 10 verdicts; older and verdict-less
    ! checks do not count.
    call record('fx', 'linux', '', 1000, 2, 3)
    call record('fx', 'linux', '', 100, 10, 2)
    call sql("INSERT INTO checks (repo, ref, state, created_at, claimed_at, finished_at, class)"// &
        " VALUES ('fx', 'x', 'timeout', "//T//", '-1 day'), "//T//", '-1 day'), "//T// &
        ", '-1 day', '+9000 seconds'), 'linux');")
    call check(near(dispatch_run(db, 'fx', 'linux', k), 100.0_real64) .and. k == 10, &
        'run(R, linux) is the median of the last 10 verdicts')
    ! No repository with 3 checks on both: speed 1.5, and run * speed * 1.5.
    call check(near(dispatch_speed(db, 'github-linux'), 1.5_real64), 'speed without history is 1.5')
    call check(near(dispatch_run(db, 'fx', 'github-linux', k), 225.0_real64) .and. k == 0, &
        'run(R, C) without history on C is run(R, linux) * speed(C) * 1.5')
    call record('ref1', 'linux', '', 100, 3, 2)
    call record('ref1', 'linux', 'github-linux', 50, 3, 2)
    call check(near(dispatch_speed(db, 'github-linux'), 0.5_real64), &
        'speed(C) is the median ratio over repositories with 3 checks on both')
    call check(near(dispatch_run(db, 'fx', 'github-linux'), 75.0_real64), &
        'and scales the estimate')
    call check(near(dispatch_run(db, 'ref1', 'github-linux', k), 50.0_real64) .and. k == 3, &
        'a check standing in for linux counts on the class it ran on')
    call check(near(dispatch_run(db, 'new', 'linux'), 1800.0_real64), &
        'with no history at all: the check time limit')
    call record('two', 'linux', '', 40, 1, 2)
    call record('two', 'linux', '', 60, 1, 2)
    call check(near(dispatch_run(db, 'two', 'linux'), 75.0_real64), &
        'fewer than 3 on linux: their median * 1.5')
    call check(near(dispatch_run(db, 'two', 'github-linux'), 75*0.5*1.5_real64), &
        'and run(R, C) scales that run(R, linux)')
    call record('new', 'linux', 'github-linux', 100, 1, 2)
    call check(near(dispatch_run(db, 'new', 'github-linux'), 150.0_real64), &
        'with no check on linux, its own 1 or 2 * 1.5')

    ! wait(C) of a backend: the median wait in the last hour, else 60 s or 120 s.
    call check(near(dispatch_wait(db, 'github-linux', 0_int64), 60.0_real64) .and. &
        near(dispatch_wait(db, 'gitlab-tug', 0_int64), 120.0_real64), 'backend wait defaults')
    do k = 1, 3
        call sql("INSERT INTO checks (repo, ref, state, created_at, started_at, class, ran_class)"// &
            " VALUES ('ref1', 'w', 'running', "//T//", '-"//itoa(30 + 10*k)//" minutes'), "//T// &
            ", '-30 minutes'), 'linux', 'gitlab-old');")
    end do
    call check(near(dispatch_wait(db, 'gitlab-old', 0_int64), 1200.0_real64), &
        'backend wait is the median wait of its checks')
    call sql("DELETE FROM checks WHERE ref = 'w';")

    ! wait(C) of own runners: none seen is never; a free one is no wait.
    call check(dispatch_wait(db, 'linux', 0_int64) >= DISPATCH_NEVER, 'no runner: no wait estimate')
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(.not. found, 'an idle runner finds nothing')
    call check(near(dispatch_wait(db, 'linux', 0_int64), 0.0_real64), 'an idle runner: no wait')
    ! An idle runner asks every 10 minutes and still counts in between.
    call sql("UPDATE runners SET seen_at = "//T//", '-12 minutes') WHERE name = 'r1';")
    call check(near(dispatch_wait(db, 'linux', 0_int64), 0.0_real64), &
        'a runner seen 12 minutes ago counts')

    ! r1 runs a long check of another repository.
    call store_check_add(db, 'pubnb', 'busy', id, stat)
    call store_check_claim(db, 'r1', own, repo, ref, found)
    call check(found .and. own == id, 'the own runner claims as before')
    call sql_text("SELECT ran_class || ' ' || (started_at = claimed_at) FROM checks WHERE id = "// &
        itoa64(own)//';', text)
    call check(text == 'linux 1', 'an own claim records its class and starts the clock: '//text)

    ! A queued check of fx with nothing ahead: home 0 + 100 beats 60 + 75.
    call store_check_add(db, 'fx', 'x', x, stat)
    call check(near(dispatch_wait(db, 'linux', x), 0.0_real64), 'nothing ahead of it')
    call store_check_claim(db, 'gh', id, repo, ref, found, class='github-linux')
    call check(.not. found, 'a backend does not take a check its home class runs sooner')
    ! Three checks of ref1 ahead of it (claim order: by id).
    call sql("UPDATE checks SET id = id + 100 WHERE id = "//itoa64(x)//';')
    x = x + 100
    do k = 1, 3
        call store_check_add(db, 'ref1', 'ahead', id, stat)
    end do
    call sql("UPDATE checks SET id = id - 1000 WHERE ref = 'ahead';")
    call check(near(dispatch_wait(db, 'linux', x), 300.0_real64), &
        'wait(C) sums the run times of the checks given out before it: '// &
        itoa(nint(dispatch_wait(db, 'linux', x))))
    call store_check_claim(db, 'gh', id, repo, ref, found, class='github-linux', max_running=1)
    call check(found .and. id == x .and. repo == 'fx', 'a backend takes it when not worse')
    call sql_text("SELECT ran_class || ' ' || class || ' ' || (started_at IS NOT NULL) FROM"// &
        ' checks WHERE id = '//itoa64(x)//';', text)
    call check(text == 'github-linux linux 1', 'it records the class it ran on: '//text)

    ! The concurrency limit: one running on github-linux, max_running 1.
    call store_check_add(db, 'fx', 'y', y, stat)
    call store_check_claim(db, 'gh', id, repo, ref, found, class='github-linux', max_running=1)
    call check(.not. found, 'not beyond max_running')
    call store_check_claim(db, 'gh', id, repo, ref, found, class='github-linux', max_running=2)
    call check(found .and. id == y, 'below max_running it takes the next')

    ! Private repositories, and public ones whose policy names no backend.
    call store_check_add(db, 'priv', 'p', id, stat)
    call store_check_add(db, 'pubnb', 'q', id, stat)
    call sql("UPDATE checks SET id = id - 2000 WHERE ref IN ('p', 'q');")
    call store_check_claim(db, 'gh2', id, repo, ref, found, class='github-linux', max_running=9)
    call check(.not. found .or. (repo /= 'priv' .and. repo /= 'pubnb'), &
        'no check of a private repository or one without backends')
    ! A private repository whose policy still names a backend: fy repo public
    ! off racing a policy update leaves one.
    call store_check_add(db, 'priv2', 'p2', id, stat)
    call sql("UPDATE checks SET id = id - 1500 WHERE ref = 'p2';")
    call store_check_claim(db, 'gh2', id, repo, ref, found, class='github-linux', max_running=9)
    call check(.not. found, 'no check of a private repository even when its policy names one')
    call sql("UPDATE repos SET public = 1 WHERE name = 'priv2';")
    call store_check_claim(db, 'gh2', id, repo, ref, found, class='github-linux', max_running=9)
    call check(found .and. ref == 'p2', 'the same check of a public one is taken')
    call sql("UPDATE checks SET state = 'cancelled' WHERE ref = 'p2';")
    ! A policy naming only gitlab keeps GitHub away.
    call store_check_add(db, 'fx', 'z', id, stat)
    call sql("UPDATE repos SET policy_backends = 'gitlab' WHERE name = 'fx';")
    call store_check_claim(db, 'gh2', id, repo, ref, found, class='github-linux', max_running=9)
    call check(.not. found, 'backends=gitlab keeps GitHub away')

    ! Standing in: GitLab for linux within its time limit (1.5 * 225 s).
    call store_check_claim(db, 'gl', id, repo, ref, found, class='gitlab-tug', max_minutes=5)
    call check(.not. found, 'not when 1.5 * run(R, backend) exceeds its time limit')
    call store_check_claim(db, 'gl', id, repo, ref, found, class='gitlab-tug', max_minutes=6)
    call check(found .and. ref == 'z', 'a GitLab class stands in for linux')
    call sql("UPDATE repos SET policy_backends = 'github,gitlab' WHERE name = 'fx';")
    ! macOS: only github-macos-arm64, and with no macOS runner at all.
    call store_check_add(db, 'fx', 'mac', mac, stat, class='macos-arm64')
    call store_check_add(db, 'fx', 'lin', id, stat)
    call store_check_claim(db, 'ghw', id, repo, ref, found, class='github-windows')
    call check(.not. found, 'github-windows stands in for nothing')
    call store_check_add(db, 'fx', 'win', win, stat, class='github-windows')
    ! The policy asked for this class: the job's time limit bounds the check,
    ! not the estimate, which may rest on no history at all.
    call store_check_claim(db, 'ghw', id, repo, ref, found, class='github-windows', max_minutes=5)
    call check(found .and. id == win, 'github-windows takes checks of its own class, whatever its limit')
    call store_check_claim(db, 'ghm', id, repo, ref, found, class='github-macos-arm64')
    call check(found .and. id == mac, 'github-macos-arm64 stands in for macos-arm64 only')
    call store_check_claim(db, 'ghm', id, repo, ref, found, class='github-macos-arm64')
    call check(.not. found, 'and takes no linux check')
    call store_check_claim(db, 'm1', id, repo, ref, found, class='macos-arm64')
    call check(.not. found, 'the macOS check is gone from its home queue')

    ! The own runner still takes what the backends leave, and its result
    ! may carry when it started, kept between creation and now.
    call store_check_finish(db, own, 'r1', 'passed', '', found)
    call store_check_claim(db, 'r1', id, repo, ref, found)
    ! The private repository's check, queued first, that no backend took.
    call check(found .and. ref == 'p', 'own runners keep the claim order: '//ref)
    call store_check_finish(db, x, 'gh', 'passed', '', found, '2000-01-01T00:00:00Z')
    call sql_text('SELECT started_at = created_at FROM checks WHERE id = '//itoa64(x)//';', text)
    call check(found .and. text == '1', 'a start before the creation is the creation')
    call store_check_finish(db, y, 'gh', 'failed', '', found, '2999-01-01T00:00:00+01:00')
    call sql_text('SELECT started_at <= finished_at FROM checks WHERE id = '//itoa64(y)//';', text)
    call check(found .and. text == '1', 'a start in the future is now')

    ! Two backend claims that chose the same check: only one takes it.
    call sql("UPDATE checks SET state = 'passed' WHERE state = 'queued';")
    call store_check_add(db, 'fx', 'race', id, stat, class='github-windows')
    call dispatch_candidates(db, 'github-windows', 360, ids1)
    call dispatch_candidates(db, 'github-windows', 360, ids2)
    call check(any(ids1 == id) .and. any(ids2 == id), 'both choose it')
    call store_check_take(db, 'race1', 'github-windows', id, 50, found, x, repo, ref)
    call check(found .and. x == id, 'the first takes it')
    call store_check_take(db, 'race2', 'github-windows', id, 50, found, x, repo, ref)
    call check(.not. found, 'the second finds it taken')

    ! A check given back is queued again without the class and start of the
    ! attempt, which are no wait of that backend.
    call store_check_error(db, id, '', .false., text, found, runner='race1')
    call sql_text("SELECT state || coalesce(ran_class, '-') || coalesce(started_at, '-') FROM"// &
        ' checks WHERE id = '//itoa64(id)//';', text)
    call check(found .and. text == 'queued--', 'a requeued check forgets its attempt: '//text)

    ! GitHub limits the jobs of the organisation: the largest limit a GitHub
    ! process sent bounds the checks running on all GitHub classes.
    call sql("UPDATE runners SET seen_at = '2000-01-01T00:00:00Z' WHERE name <> 'ghw';")
    call sql_text("SELECT count(*) FROM checks WHERE state = 'running' AND ran_class LIKE"// &
        " 'github-%';", text)
    read (text, *) n_org
    call sql_text("SELECT count(*) FROM checks WHERE state = 'running' AND ran_class ="// &
        " 'github-windows';", text)
    read (text, *) n_win
    call check(n_win < n_org, 'other GitHub classes run checks')
    call store_check_claim(db, 'ghw', id, repo, ref, found, class='github-windows', &
        max_running=n_org)
    call check(.not. found, 'not beyond the organisation''s limit')
    call store_check_claim(db, 'ghw', id, repo, ref, found, class='github-windows', &
        max_running=n_org + 1)
    call check(found .and. ref == 'race', 'below it the check is taken')

    ! A backend that served the repository last holds no check back from an
    ! idle own runner.
    call sql("UPDATE checks SET state = 'cancelled' WHERE state = 'running';")
    call store_check_add(db, 'fx', 'aff1', aff, stat)
    call store_check_claim(db, 'gh', id, repo, ref, found, class='github-linux', max_running=5)
    call check(found .and. id == aff, 'the backend takes it')
    call store_check_finish(db, aff, 'gh', 'passed', '', found)
    call store_check_add(db, 'fx', 'aff2', aff, stat)
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(found .and. id == aff, 'the idle own runner takes the next at once')

    ! Policy: backends need a public repository; egress takes host names.
    call policy_get(db, 'priv', p)
    call policy_from_json(p, '{"backends":"github"}', error)
    call check(len(error) > 0, 'a private repository may name no backend')
    call policy_get(db, 'pubnb', p)
    call policy_from_json(p, '{"backends":"github,gitlab","egress":"dl.example.org,x.io"}', error)
    call check(len(error) == 0 .and. p%backends == 'github,gitlab' .and. &
        p%egress == 'dl.example.org,x.io', 'a public one may: '//error)
    call policy_set(p, 'backends', 'gitlab,', error)
    call check(len(error) > 0, 'backends lists only github and gitlab')
    call policy_set(p, 'backends', 'circleci', error)
    call check(len(error) > 0, 'no other backend')
    do k = 1, size(HOSTS)
        call policy_set(p, 'egress', trim(HOSTS(k)), error)
        call check(len(error) > 0, 'egress refuses '//trim(HOSTS(k)))
    end do
    call policy_set(p, 'egress', '', error)
    call check(len(error) == 0 .and. p%egress == '', 'egress may be empty')
    call store_repo_set_public(db, 'fx', .false., found, refused)
    call check(found .and. refused, 'a repository whose policy names backends stays public')
    call store_repo_set_public(db, 'pubnb', .false., found, refused)
    call check(found .and. .not. refused, 'one without becomes private')
    call store_repo_set_public(db, 'nosuch', .true., found, refused)
    call check(.not. found, 'no such repository')

    call db_close(db)
    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')

contains

    subroutine record(repo, class, ran, seconds, n, days)
        !! `n` checks of `repo` with a verdict, each `seconds` long, `days` ago.
        character(len=*), intent(in) :: repo, class, ran
        integer, intent(in) :: seconds, n, days
        character(len=:), allocatable :: at, r
        integer :: i

        at = T//", '-"//itoa(days)//" days'"
        r = 'NULL'
        if (len(ran) > 0) r = "'"//ran//"'"
        do i = 1, n
            call sql('INSERT INTO checks (repo, ref, state, created_at, claimed_at, started_at,'// &
                " finished_at, class, ran_class) VALUES ('"//repo//"', 'h', 'passed', "//at// &
                '), '//at//'), '//at//'), '//at//", '+"//itoa(seconds)//" seconds'), '"// &
                class//"', "//r//');')
        end do
    end subroutine record

    subroutine sql(text)
        character(len=*), intent(in) :: text
        integer :: s
        call db_exec(db, text, s)
        call check(s == 0, 'sql: '//text)
    end subroutine sql

    subroutine sql_text(query, value)
        character(len=*), intent(in) :: query
        character(len=:), allocatable, intent(out) :: value
        type(stmt_t) :: st
        integer :: s
        value = ''
        call db_prepare(db, query, st, s)
        if (stmt_step(st) == SQLITE_ROW) value = stmt_text(st, 0)
        call stmt_finalize(st)
    end subroutine sql_text

    logical function near(a, b)
        real(real64), intent(in) :: a, b
        near = abs(a - b) < 1.0e-6_real64*max(1.0_real64, abs(b))
    end function near

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

    function itoa64(k) result(s)
        integer(int64), intent(in) :: k
        character(len=:), allocatable :: s
        character(len=24) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa64

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_dispatch
