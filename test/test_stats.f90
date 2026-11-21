program test_stats
    !! Queue numbers (fy_stats) from seeded rows: the percentiles are those of
    !! the sorted durations, a measure without rows is no member of the
    !! answer, the window and the repository select the rows, both routes
    !! answer the same, fy prints them, and 10 000 landings over 30 days are
    !! answered within the budget of doc/inspection.md.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_cli, only: cli_stats
    use fortweb_http, only: http_parse_head, http_request_t
    use fy_server, only: response_t, route
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_stats, only: stats_days_ok, stats_json
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: path = 'build/test_stats.db'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    !! Seconds a request for the numbers may take (doc/inspection.md, budgets).
    real, parameter :: BUDGET = 0.050
    character(len=*), parameter :: AT = "strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-' || "
    type(db_t) :: db
    type(response_t) :: res
    character(len=:), allocatable :: admin, a, text
    character(len=16) :: ms
    integer(int64) :: t0, t1, rate
    real :: best
    integer :: stat, i

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call store_repo_add(db, 'other', '', stat, mode='primary')
    call store_repo_add(db, 'empty', '', stat, mode='primary')
    call check(stat == 0, 'a store with three repositories')

    call check(stats_days_ok(1) .and. stats_days_ok(7) .and. stats_days_ok(30) .and. &
        .not. stats_days_ok(0) .and. .not. stats_days_ok(2), 'the windows are 1, 7 and 30 days')

    a = stats_json(db, 'empty', 7)
    call check(index(a, '{"repo":"empty","days":7,"since":"') == 1 .and. &
        index(a, 'Z","landed":0}') == len(a) - 13, &
        'without rows the answer has the number of landings and no other measure: '//a)
    call check(cli_stats(a) == 'Queue numbers · empty · 7 days'//new_line('a')// &
        'No landings in this window.'//new_line('a'), 'fy says that nothing landed: '//cli_stats(a))

    ! demo, each row with the seconds since it was queued and since it
    ! finished: change 1 fails and lands with its second entry, changes 2 to
    ! 4 and a branch without a change land an hour ago, change 5 three days
    ! ago and change 6 twenty days ago.
    call landing('demo', 'landing', 'failed', 1, 7300, 7200)
    call landing('demo', 'landing', 'landed', 1, 3700, 3600)
    call landing('demo', 'landing', 'landed', 2, 3610, 3600)
    call landing('demo', 'landing', 'landed', 3, 3620, 3600)
    call landing('demo', 'landing', 'landed', 4, 3640, 3600)
    call landing('demo', 'landing', 'landed', 0, 3630, 3600)
    call landing('demo', 'landing', 'landed', 5, 3*86400 + 500, 3*86400)
    call landing('demo', 'landing', 'landed', 6, 20*86400 + 900, 20*86400)
    call landing('demo', 'verify', 'passed', 0, 3700, 3600)
    call landing('demo', 'verify', 'failed', 0, 3700, 3600)
    call landing('demo', 'verify', 'cancelled', 0, 3700, 3600)
    call landing('other', 'landing', 'landed', 7, 3607, 3600)
    ! A landing that still runs has not finished.
    call db_exec(db, "INSERT INTO landings (repo, branch, account, state, created_at) VALUES"// &
        " ('demo', 'b', 'agent', 'running', "//AT//"60 || ' seconds'));", stat)
    call check(stat == 0, 'landings are seeded')
    ! Checks wait 1, 2 and 9 s for a runner and run 30, 40 and 50 s; the
    ! check of a verification and a waiting one do not count.
    call checked('check', 'passed', 3700, 3699, 3669)
    call checked('check', 'passed', 3700, 3698, 3658)
    call checked('check', 'failed', 3700, 3691, 3641)
    call checked('verify', 'passed', 3700, 3600, 3000)
    call db_exec(db, "INSERT INTO checks (repo, ref, state, created_at) VALUES ('demo', 'r',"// &
        " 'queued', "//AT//"60 || ' seconds'));", stat)
    call check(stat == 0, 'checks are seeded')
    ! Five landed changes, one of them flagged; an open one does not count.
    call db_exec(db, 'INSERT INTO changes (repo, number, change_key, branch, title, description,'// &
        ' author, state, created_at, updated_at, flagged)'// &
        " SELECT 'demo', value, 'k' || value, 'b' || value, 't', '', 'agent',"// &
        " CASE WHEN value = 6 THEN 'open' ELSE 'landed' END, 'x', 'x', value IN (3, 6)"// &
        ' FROM json_each(''[1,2,3,4,5,6]'');', stat)
    call check(stat == 0, 'changes are seeded')

    a = stats_json(db, 'demo', 7)
    call check(has(a, '"landed":6,'), 'landings of the window are counted: '//a)
    call check(has(a, '"land_to_main_p50_seconds":30,"land_to_main_p95_seconds":500,'), &
        'the time to main is the median and the 95th percentile of the sorted durations: '//a)
    call check(has(a, '"landings_per_hour":0.04,"landings_per_day":0.9,'), &
        'landings per hour and per day are those of the window: '//a)
    call check(has(a, '"failed":1,"failed_share":0.143,'), &
        'the failed share is of the entries that finished: '//a)
    call check(has(a, '"first_entry_share":0.800,'), &
        'four of the five changes landed with their first entry: '//a)
    call check(has(a, '"checks":3,"check_wait_p50_seconds":2,"check_wait_p95_seconds":9,'// &
        '"check_run_p50_seconds":40,"check_run_p95_seconds":50,"checks_per_landed_change":0.50,'), &
        'wait and run time of the checks of landings, and checks per landed change: '//a)
    call check(has(a, '"verifications":2,"verifications_failed":1,'// &
        '"failed_verifications_per_100_landings":16.7,'), &
        'failed verifications per 100 landings: '//a)
    call check(has(a, '"flagged_undecided":1}'), 'landed changes with a flag are counted: '//a)

    a = stats_json(db, 'demo', 1)
    call check(has(a, '"landed":5,') .and. has(a, '"land_to_main_p50_seconds":30,'// &
        '"land_to_main_p95_seconds":100,') .and. has(a, '"first_entry_share":0.750,'), &
        'one day leaves out the landing of three days ago: '//a)
    a = stats_json(db, 'demo', 30)
    call check(has(a, '"landed":7,') .and. has(a, '"land_to_main_p95_seconds":900,'), &
        'thirty days take in the landing of twenty days ago: '//a)
    a = stats_json(db, 'other', 7)
    call check(has(a, '"landed":1,') .and. has(a, '"land_to_main_p50_seconds":7,') .and. &
        has(a, '"failed":0,"failed_share":0.000,"first_entry_share":1.000}') .and. &
        .not. has(a, 'checks') .and. .not. has(a, 'verifications') .and. &
        .not. has(a, 'flagged'), 'a repository has its own rows, a zero where rows say so and no measure without rows: '//a)
    a = stats_json(db, '', 7)
    call check(index(a, '{"repo":"","days":7,') == 1 .and. has(a, '"landed":7,') .and. &
        has(a, '"land_to_main_p50_seconds":30,'), 'the instance has the rows of all: '//a)

    text = cli_stats(stats_json(db, 'demo', 7))
    call check(index(text, 'Queue numbers · demo · 7 days'//new_line('a')) == 1 .and. &
        has(text, ' landed          6   0.04 per hour, 0.9 per day'//new_line('a')) .and. &
        has(text, ' land to main    p50 30 s   p95 8 min 20 s'//new_line('a')) .and. &
        has(text, ' first entry     80 % landed with their first entry'//new_line('a')) .and. &
        has(text, ' failed entries  1   14 % of the finished entries'//new_line('a')) .and. &
        has(text, ' checks          wait p50 2 s  p95 9 s   run p50 40 s  p95 50 s   0.50 checks'// &
        ' per landed change'//new_line('a')) .and. &
        has(text, ' main            1 of 2 verifications failed, 16.7 per 100 landings'// &
        new_line('a')) .and. has(text, ' review          1 flagged undecided'//new_line('a')), &
        'fy prints one line per measure: '//text)

    ! The routes.
    call get('/api/v1/stats?repo=demo', admin)
    call check(res%code == 200 .and. index(res%body, '{"repo":"demo","days":7,') == 1 .and. &
        has(res%body, '"land_to_main_p95_seconds":500,'), &
        'GET /stats answers the numbers of a repository over 7 days: '//res%body)
    call get('/api/v1/stats?days=1', admin)
    call check(res%code == 200 .and. index(res%body, '{"repo":"","days":1,') == 1 .and. &
        has(res%body, '"landed":6,'), 'GET /stats answers for the instance: '//res%body)
    call get('/api/v1/repos/demo/board?stats=1&days=30', admin)
    call check(res%code == 200 .and. index(res%body, '{"repo":"demo","days":30,') == 1 .and. &
        has(res%body, '"landed":7,') .and. has(res%body, '"land_to_main_p95_seconds":900,'), &
        'GET /repos/R/board?stats=1 answers the same numbers: '//res%body)
    call get('/api/v1/stats?days=5', admin)
    call check(res%code == 400, 'another window is refused')
    call get('/api/v1/stats?days=7x', admin)
    call check(res%code == 400, 'a window that is no number is refused')
    call get('/api/v1/stats?repo=nowhere', admin)
    call check(res%code == 404, 'a repository that is not there has no numbers')
    call get('/api/v1/repos/nowhere/board?stats=1', admin)
    call check(res%code == 404, 'nor has its board')
    call get('/api/v1/stats', 'fy_none')
    call check(res%code == 401, 'the numbers need a token')

    ! 10 000 entries over 29 days, one in seven failed and landed again, with
    ! their checks and a verification for every tenth.
    call db_exec(db, 'BEGIN;'// &
        'CREATE TEMP TABLE n AS WITH RECURSIVE s(i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM s'// &
        ' WHERE i < 10000) SELECT i FROM s;'// &
        'INSERT INTO landings (repo, branch, account, state, created_at, finished_at, change,'// &
        " kind) SELECT 'other', 'b' || i, 'agent', CASE WHEN i % 7 = 0 THEN 'failed' ELSE"// &
        " 'landed' END, "//AT//"((10001 - i) * 250 + 20 + i % 90) || ' seconds'), "//AT// &
        "((10001 - i) * 250) || ' seconds'), 100 + i - (i % 7 = 1), 'landing' FROM n;"// &
        'INSERT INTO landings (repo, branch, account, state, created_at, finished_at, kind)'// &
        " SELECT 'other', 'main', 'agent', CASE WHEN i % 50 = 0 THEN 'failed' ELSE 'passed' END,"// &
        ' '//AT//"(i * 250 + 300) || ' seconds'), "//AT//"(i * 250) || ' seconds'), 'verify'"// &
        ' FROM n WHERE i % 10 = 0;'// &
        'INSERT INTO checks (repo, ref, state, created_at, claimed_at, finished_at)'// &
        " SELECT 'other', 'r' || i, CASE WHEN i % 7 = 0 THEN 'failed' ELSE 'passed' END, "// &
        AT//"(i * 250 + 18 + i % 90) || ' seconds'), "//AT//"(i * 250 + 15 + i % 80) ||"// &
        " ' seconds'), "//AT//"(i * 250) || ' seconds') FROM n;"// &
        'COMMIT;', stat)
    call check(stat == 0, '10 000 landings are seeded')
    call system_clock(count_rate=rate)
    best = huge(best)
    do i = 1, 5
        call system_clock(t0)
        call get('/api/v1/stats?days=30', admin)
        call system_clock(t1)
        best = min(best, real(t1 - t0)/real(rate))
    end do
    write (ms, '(f0.1)') 1000*best
    call check(res%code == 200 .and. has(res%body, '"landed":8580,') .and. &
        has(res%body, '"failed":1429,') .and. has(res%body, '"checks":10003,'), &
        'the numbers over 10 000 landings: '//res%body)
    call check(best < BUDGET, 'GET /stats over 30 days with 10 000 landings answers within'// &
        ' 50 ms: took '//trim(ms)//' ms')
    print '(a)', 'stats: GET /stats?days=30 over 10 000 landings took '//trim(ms)//' ms'

    call db_close(db)
    print '(a)', 'stats: all checks passed'

contains

    subroutine landing(repo, kind, state, change, queued, finished)
        !! A finished job that was queued and finished so many seconds ago.
        character(len=*), intent(in) :: repo, kind, state
        integer, intent(in) :: change, queued, finished

        call db_exec(db, 'INSERT INTO landings (repo, branch, account, state, kind, change,'// &
            " created_at, finished_at) VALUES ('"//repo//"', 'b', 'agent', '"//state//"', '"// &
            kind//"', nullif("//itoa(change)//', 0), '//AT//itoa(queued)//" || ' seconds'), "// &
            AT//itoa(finished)//" || ' seconds'));", stat)
        call check(stat == 0, 'a landing is seeded')
    end subroutine landing

    subroutine checked(kind, state, queued, claimed, finished)
        character(len=*), intent(in) :: kind, state
        integer, intent(in) :: queued, claimed, finished

        call db_exec(db, 'INSERT INTO checks (repo, ref, state, kind, created_at, claimed_at,'// &
            " finished_at) VALUES ('demo', 'r', '"//state//"', '"//kind//"', "//AT// &
            itoa(queued)//" || ' seconds'), "//AT//itoa(claimed)//" || ' seconds'), "//AT// &
            itoa(finished)//" || ' seconds'));", stat)
        call check(stat == 0, 'a check is seeded')
    end subroutine checked

    subroutine get(target, token)
        !! The answer of the server to GET `target` with a bearer token.
        character(len=*), intent(in) :: target, token
        type(http_request_t) :: req
        integer :: status

        call http_parse_head('GET '//target//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//CRLF, req, status)
        call route(req, db, res)
    end subroutine get

    logical function has(s, part)
        character(len=*), intent(in) :: s, part
        has = index(s, part) > 0
    end function has

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_stats
