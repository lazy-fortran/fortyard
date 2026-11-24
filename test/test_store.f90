program test_store
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: &
        SCHEMA_VERSION, &
        check_t, &
        landing_t, &
        store_bootstrap, &
        store_check_add, &
        store_check_claim, &
        store_check_error, &
        store_check_finish, &
        store_check_get, &
        store_check_release, &
        store_job_checks, &
        store_jobs, &
        store_open, &
        store_schema_version, &
        store_snapshot, &
        store_token_create, &
        store_token_lookup, &
        token_identity_t
    use fy_proc, only: proc_exit, proc_fork, proc_wait
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len=*), parameter :: path = 'build/test_store.db'
    type(db_t) :: db
    type(token_identity_t) :: who
    character(len=:), allocatable :: admin, second, again
    integer(int64) :: night_two
    integer :: stat, released
    integer(int64) :: id
    character(len=:), allocatable :: repo, ref
    logical :: found
    type(landing_t), allocatable :: jobs(:)
    type(check_t), allocatable :: job_checks(:)
    integer(int64) :: t0, t1, rate
    integer :: k

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm'//' '//path//'.snap')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens and migrates')
    call check(store_schema_version(db) == SCHEMA_VERSION, 'schema at current version')

    call store_bootstrap(db, admin, stat)
    call check(stat == 0 .and. len(admin) == 67 .and. admin(1:3) == 'fy_', 'bootstrap token')
    call store_bootstrap(db, again, stat)
    call check(stat /= 0 .and. len(again) == 0, 'bootstrap succeeds only once')

    call store_token_lookup(db, admin, who)
    call check(who%found .and. who%account == 'admin' .and. who%scopes == 'admin', &
        'bootstrap token identifies the administrator')

    call store_token_create(db, who%account_id, 'laptop', who%scopes, second, stat)
    call check(stat == 0 .and. second /= admin, 'second token created and distinct')
    call store_token_lookup(db, second, who)
    call check(who%found .and. who%token_name == 'laptop', 'second token valid')

    call store_token_lookup(db, admin(1:66)//'x', who)
    call check(.not. who%found, 'altered token rejected')
    call store_token_lookup(db, '', who)
    call check(.not. who%found, 'empty token rejected')

    call db_exec(db, "UPDATE tokens SET revoked = 1 WHERE name = 'laptop';", stat)
    call store_token_lookup(db, second, who)
    call check(.not. who%found, 'revoked token rejected')

    call db_exec(db, "UPDATE tokens SET expires_at = '2000-01-01T00:00:00Z'"// &
        " WHERE name = 'bootstrap';", stat)
    call store_token_lookup(db, admin, who)
    call check(.not. who%found, 'expired token rejected')

    ! A snapshot taken while the store is open is a complete database.
    call store_snapshot(db, path//'.snap', stat)
    call check(stat == 0, 'snapshot written')
    call db_close(db)
    call store_open(path, db, stat)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION, &
        'reopening an existing store is a no-op migration')
    call db_close(db)
    call store_open(path//'.snap', db, stat)
    call store_token_lookup(db, second, who)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION .and. &
        .not. who%found, 'the snapshot holds the database as it was')
    call db_close(db)

    ! A check goes to the runner that checked its repository last; another
    ! runner takes it only after it has waited.
    call store_open(path, db, stat)
    call store_check_add(db, 'demo', 'refs/a', id, stat)
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(found .and. repo == 'demo', 'the first check of a repository goes to any runner')
    call store_check_finish(db, id, 'r1', 'passed', '', found)
    call store_check_add(db, 'demo', 'refs/b', id, stat)
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(.not. found, 'another runner leaves a fresh check to the warm one')
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(found .and. ref == 'refs/b', 'the warm runner takes it')
    call store_check_finish(db, id, 'r1', 'passed', '', found)
    call store_check_add(db, 'demo', 'refs/c', id, stat)
    call db_exec(db, "UPDATE checks SET created_at = '2000-01-01T00:00:00Z' WHERE ref = 'refs/c';", &
        stat)
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(found .and. ref == 'refs/c', 'another runner takes a check that has waited')
    ! A runner that restarts hands back the check it held; it runs again.
    call store_check_release(db, 'r1', released)
    call check(released == 0, 'a runner holding nothing hands nothing back')
    call store_check_release(db, 'r2', released)
    call check(released == 1, 'a restarted runner hands back its running check')
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(found .and. ref == 'refs/c', 'the handed back check is taken again')
    call store_check_finish(db, id, 'r2', 'passed', '', found)
    ! A check runs only on a runner of its class; a runner naming none is linux.
    call store_check_add(db, 'demo', 'refs/m', id, stat, kind='verify', class='macos-arm64')
    call db_exec(db, "UPDATE checks SET created_at = '2000-01-01T00:00:00Z' WHERE ref = 'refs/m';", &
        stat)
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(.not. found, 'a linux runner leaves a check of another class')
    call store_check_claim(db, 'm1', id, repo, ref, found, class='macos-arm64')
    call check(found .and. ref == 'refs/m', 'a runner of its class takes it')
    call store_check_finish(db, id, 'm1', 'passed', '', found)
    call store_check_add(db, 'demo', 'refs/l', id, stat)
    call store_check_claim(db, 'm1', id, repo, ref, found, class='macos-arm64')
    call check(.not. found, 'a macos runner leaves a linux check')
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(found .and. ref == 'refs/l', 'the runner that last checked the repository in'// &
        ' its class is the warm one, not the macos runner that checked it since')
    call store_check_finish(db, id, 'r2', 'passed', '', found)
    ! At most FY_NIGHTLY_SLOTS (default 2) nightly checks run on a class at
    ! once; landing checks still find a runner.
    do k = 1, 3
        call store_check_add(db, 'demo', 'refs/n'//achar(48 + k), id, stat, kind='nightly', &
            class='night')
    end do
    call db_exec(db, "UPDATE checks SET created_at = '2000-01-01T00:00:00Z' WHERE class ="// &
        " 'night';", stat)
    call store_check_claim(db, 'n1', id, repo, ref, found, class='night')
    call check(found .and. ref == 'refs/n1', 'a first nightly check starts')
    call store_check_claim(db, 'n2', night_two, repo, ref, found, class='night')
    call check(found .and. ref == 'refs/n2', 'a second nightly check starts')
    call store_check_claim(db, 'n3', id, repo, ref, found, class='night')
    call check(.not. found, 'a third nightly check waits while two run')
    call store_check_add(db, 'demo', 'refs/nl', id, stat, class='night')
    call store_check_claim(db, 'n3', id, repo, ref, found, class='night')
    call check(found .and. ref == 'refs/nl', 'a landing check runs beside them')
    call store_check_finish(db, night_two, 'n2', 'passed', '', found)
    call store_check_claim(db, 'n4', id, repo, ref, found, class='night')
    call check(found .and. ref == 'refs/n3', 'the third starts when a nightly check ended')

    ! The jobs list looks up, for each of its rows, the entries of the row's
    ! change and its checks: with 6000 landings and their logs that stays a
    ! matter of milliseconds (without an index on either it takes a second).
    call db_exec(db, "WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n"// &
        " WHERE i < 6000) INSERT INTO landings (repo, branch, account, state, created_at,"// &
        " log, change, kind) SELECT 'demo', 'b' || i, 'admin', 'landed', '2026-01-01T00:00:00Z',"// &
        " hex(zeroblob(2000)), (i + 1) / 2, 'landing' FROM n;"// &
        " INSERT INTO checks (repo, ref, state, log, created_at) SELECT 'demo',"// &
        " 'refs/fortyard/checks/' || id, 'passed', log, created_at FROM landings;", stat)
    call check(stat == 0, 'landings and checks are seeded')
    call system_clock(t0, rate)
    call store_jobs(db, '', jobs, stat)
    do k = 1, size(jobs)
        call store_job_checks(db, jobs(k), job_checks)
        call check(size(job_checks) == 1 .and. jobs(k)%entries == 2, 'a job has its check and entries')
    end do
    call system_clock(t1)
    call check(size(jobs) == 40 .and. real(t1 - t0) / real(rate) < 0.2, &
        'the jobs list with its checks is fast on a database with history')
    call db_close(db)

    call fair_share()
    call errors_and_timeouts()
    call concurrent_claims()
    call claim_cost()

contains

    subroutine fresh(file)
        character(len=*), intent(in) :: file
        call execute_command_line('rm -f '//file//' '//file//'-wal '//file//'-shm')
        call store_open(file, db, stat)
        call check(stat == 0, 'a fresh store opens: '//file)
    end subroutine fresh

    subroutine fair_share()
        !! Two runners and three repositories: while r1 runs a long check of
        !! a, the free runner r2 takes b's check, then c's, before a's second,
        !! although a's was queued first; a verification waits for them all.
        integer(int64) :: long, mine
        character(len=:), allocatable :: kind

        call fresh('build/test_store_fair.db')
        call store_check_add(db, 'a', 'refs/a1', long, stat)
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call check(found .and. id == long, 'r1 takes a''s long check')
        call store_check_add(db, 'd', 'refs/v', id, stat, kind='verify')
        call store_check_add(db, 'a', 'refs/a2', id, stat)
        call store_check_add(db, 'b', 'refs/b1', id, stat)
        call store_check_add(db, 'c', 'refs/c1', id, stat)
        call store_check_claim(db, 'r2', mine, repo, ref, found)
        call check(found .and. ref == 'refs/b1', 'a repository holding no runner goes first: '//ref)
        call store_check_finish(db, mine, 'r2', 'passed', '', found)
        call store_check_claim(db, 'r2', mine, repo, ref, found)
        call check(found .and. ref == 'refs/c1', 'then the next one holding none: '//ref)
        call store_check_finish(db, mine, 'r2', 'passed', '', found)
        call store_check_claim(db, 'r2', mine, repo, ref, found)
        call check(found .and. ref == 'refs/a2', 'then a''s second, its warm runner being busy: '// &
            ref)
        call store_check_finish(db, mine, 'r2', 'passed', '', found)
        call store_check_claim(db, 'r2', mine, repo, ref, found, kind)
        call check(found .and. kind == 'verify', 'the verification comes last')
        ! Equal holdings: the oldest check goes first.
        call store_check_add(db, 'b', 'refs/b2', id, stat)
        call store_check_add(db, 'c', 'refs/c2', id, stat)
        call store_check_finish(db, long, 'r1', 'passed', '', found)
        call store_check_claim(db, 'r3', id, repo, ref, found)
        call check(found .and. ref == 'refs/b2', 'between equals the oldest: '//ref)
        call db_close(db)
    end subroutine fair_share

    subroutine errors_and_timeouts()
        !! A runner error queues the check again; so does its first timeout,
        !! and the second ends it in the state timeout. The log keeps each.
        !! Only the runner that holds a check reports it, and one is lost only
        !! past twice the limit of its claim.
        character(len=:), allocatable :: state, log
        type(landing_t) :: job
        type(check_t), allocatable :: attempts(:)
        integer(int64) :: first
        integer :: age, limit

        call fresh('build/test_store_errors.db')
        call store_check_add(db, 'a', 'refs/x', first, stat)
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call store_check_error(db, id, 'runner lost', .false., state, found)
        call check(found .and. state == 'queued', 'a runner error queues the check again')
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call check(found .and. id == first, 'and it is taken again')
        call store_check_error(db, id, 'first timeout', .true., state, found)
        call check(found .and. state == 'queued', 'a first timeout queues it again')
        call store_check_error(db, id, 'not running', .true., state, found)
        call check(.not. found, 'only a running check takes an error')
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call store_check_error(db, id, 'second timeout', .true., state, found)
        call check(found .and. state == 'timeout', 'the second timeout ends it')
        call store_check_get(db, id, state, log, age)
        call check(state == 'timeout' .and. index(log, 'runner lost') > 0 .and. &
            index(log, 'first timeout') > 0 .and. index(log, 'second timeout') > 0, &
            'the log keeps every error: '//log)
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call check(.not. found, 'a check ended by its time limit is not taken again')
        ! A queued again check counts its age from then, not from its creation.
        call store_check_add(db, 'a', 'refs/y', first, stat)
        call db_exec(db, "UPDATE checks SET created_at = '2000-01-01T00:00:00Z' WHERE ref ="// &
            " 'refs/y';", stat)
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call store_check_error(db, id, 'runner lost', .false., state, found)
        call store_check_get(db, id, state, log, age)
        call check(state == 'queued' .and. age < 600, 'a check queued again is young again')
        ! Only the runner that holds a check reports it.
        call store_check_claim(db, 'r2', id, repo, ref, found, limit=limit)
        call check(found .and. limit == 3600, 'a claim is given the limit of its repository')
        call store_check_finish(db, id, 'r1', 'passed', 'late', found)
        call check(.not. found, 'another runner''s result is refused')
        call store_check_error(db, id, 'late', .true., state, found, runner='r1')
        call check(.not. found, 'and so is another runner''s error')
        ! A runner is lost only past twice the limit of its claim and the grace.
        call store_check_error(db, id, 'lost', .false., state, found, lost_grace=300)
        call check(.not. found, 'a runner within its limit is not lost')
        call db_exec(db, "UPDATE checks SET claimed_at = strftime('%Y-%m-%dT%H:%M:%SZ', 'now',"// &
            " '-7400 seconds') WHERE ref = 'refs/y';", stat)
        call store_check_error(db, id, 'lost', .false., state, found, lost_grace=300)
        call check(.not. found, 'nor one that may still run the check a second time')
        call db_exec(db, "UPDATE checks SET claimed_at = strftime('%Y-%m-%dT%H:%M:%SZ', 'now',"// &
            " '-7600 seconds') WHERE ref = 'refs/y';", stat)
        call store_check_error(db, id, 'lost', .false., state, found, lost_grace=300)
        call check(found .and. state == 'queued', 'a runner past that is lost')
        call store_check_claim(db, 'r1', id, repo, ref, found)
        call store_check_finish(db, id, 'r2', 'passed', 'late', found)
        call check(.not. found, 'the lost runner''s late result is refused')
        call store_check_finish(db, id, 'r1', 'passed', 'ok', found)
        call check(found, 'the result of the runner that holds it counts')
        call store_check_add(db, 'a', 'refs/v', first, stat, kind='verify')
        call store_check_claim(db, 'r1', id, repo, ref, found, limit=limit)
        call check(found .and. limit == 21600, 'a verification is given six hours')
        call store_check_add(db, 'a', 'refs/fortyard/verify/1', first, stat, kind='nightly')
        call store_check_claim(db, 'r2', id, repo, ref, found)
        call check(found .and. id == first, 'the nightly is claimed')
        call db_exec(db, "UPDATE checks SET created_at = '2026-01-01T00:00:00Z',"// &
            " queued_at = '2026-01-01T06:00:00Z', claimed_at = '2026-01-01T06:00:07Z'"// &
            " WHERE ref = 'refs/fortyard/verify/1';", stat)
        call check(stat == 0, 'the first attempt predates the latest queue entry')
        call store_check_error(db, id, 'nightly timeout', .true., state, found)
        call check(found .and. state == 'timeout', 'nightly ends on its first timeout')
        job%id = 1
        job%repo = 'a'
        call store_job_checks(db, job, attempts)
        call check(size(attempts) == 1, 'the job retains its ended check')
        call check(attempts(1)%wait == 7, 'waiting is measured from the latest queue entry')
        call store_check_claim(db, 'r2', id, repo, ref, found)
        call check(.not. found, 'a nightly timeout is not claimed again')
        call db_close(db)
    end subroutine errors_and_timeouts

    subroutine concurrent_claims()
        !! Runner processes claiming at once never take the same check.
        integer, parameter :: RUNNERS = 6, CHECKS = 300
        character(len=*), parameter :: file = 'build/test_store_race.db'
        integer :: pids(RUNNERS), k, u, ios, n, taken(CHECKS)
        integer(int64) :: got
        character(len=8) :: name

        call fresh(file)
        do k = 1, CHECKS
            write (name, '(i0)') mod(k, 7)
            call store_check_add(db, 'repo'//trim(name), 'refs/r', id, stat)
        end do
        call db_close(db)
        do k = 1, RUNNERS
            pids(k) = proc_fork()
            if (pids(k) == 0) then
                write (name, '(a,i0)') 'r', k
                call store_open(file, db, stat)
                open (newunit=u, file=file//'.'//trim(name), status='replace', action='write')
                do
                    call store_check_claim(db, trim(name), got, repo, ref, found)
                    if (.not. found) exit
                    write (u, '(i0)') got
                end do
                close (u)
                call db_close(db)
                call proc_exit(0)
            end if
        end do
        do k = 1, RUNNERS
            call proc_wait(pids(k), stat)
            call check(stat == 0, 'a claiming process ends cleanly')
        end do
        taken = 0
        do k = 1, RUNNERS
            write (name, '(a,i0)') 'r', k
            open (newunit=u, file=file//'.'//trim(name), status='old', action='read')
            do
                read (u, *, iostat=ios) n
                if (ios /= 0) exit
                taken(n) = taken(n) + 1
            end do
            close (u, status='delete')
        end do
        call check(all(taken == 1), 'each of the checks was taken exactly once')
    end subroutine concurrent_claims

    subroutine claim_cost()
        !! The claim with 10 000 checks in the table: 9 000 finished, 1 000
        !! queued across 50 repositories, 8 running. Prints the time per claim.
        integer(int64) :: t0, t1, rate
        integer :: k
        character(len=8) :: name

        call fresh('build/test_store_cost.db')
        call db_exec(db, 'BEGIN;', stat)
        do k = 1, 10000
            write (name, '(i0)') mod(k, 50)
            call store_check_add(db, 'repo'//trim(name), 'refs/r', id, stat)
        end do
        call db_exec(db, "UPDATE checks SET state = 'passed', runner = 'r' || (id % 8),"// &
            " created_at = '2000-01-01T00:00:00Z' WHERE id <= 9000;"// &
            "UPDATE checks SET state = 'running', runner = 'r' || id WHERE id > 9000 AND"// &
            ' id <= 9008;', stat)
        call db_exec(db, 'COMMIT;', stat)
        call system_clock(t0, rate)
        do k = 1, 100
            write (name, '(a,i0)') 'q', k
            call store_check_claim(db, trim(name), id, repo, ref, found)
            call check(found, 'a claim among 10 000 checks finds one')
        end do
        call system_clock(t1)
        write (*, '(a,f8.3,a)') 'claim with 10 000 checks: ', &
            1000.0*real(t1 - t0)/real(rate)/100.0, ' ms per claim'
        call db_close(db)
    end subroutine claim_cost

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_store
