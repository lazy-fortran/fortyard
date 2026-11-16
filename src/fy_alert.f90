module fy_alert
    !! Alerts (doc/inspection.md, "Alerts"): conditions with a beginning and an
    !! end, computed on request from the rows of the store and the mirrors of
    !! the repositories. Nothing is stored and no process watches: an alert
    !! fires while its condition holds and is gone once its cause has ended.
    !! The readiness of /readyz (doc/inspection.md, "Monitoring") is here too,
    !! as it reads the same store.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fx_json_build, only: &
        json_escape_string
    use fy_migrations, only: &
        migration_t, &
        migrations_registry
    use fy_proc, only: &
        proc_run
    use fy_repo, only: &
        repo_name_ok
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_prepare, &
        db_t, &
        stmt_bind_int, &
        stmt_bind_text, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t, &
        stmt_text
    use fy_store, only: &
        SCHEMA_VERSION
    implicit none
    private

    public :: &
        alert_delivery_json, &
        alert_json, &
        alert_list, &
        alert_ready, &
        alert_self_repo, &
        alert_t

    type :: alert_t
        !! The rule that fires, the repository it concerns ('' for the
        !! instance), when its cause began (UTC, ISO 8601) and what it is.
        character(len=:), allocatable :: rule, repo, since, detail
    end type alert_t

    !! What the instance serves against main of the repository it follows.
    type :: delivery_t
        character(len=:), allocatable :: repo, head, since
        character(len=:), allocatable :: rel_version, rel_commit, rel_state, rel_created
        !! Commits of main the instance does not serve; -1 when not known.
        integer :: behind = -1
        !! Seconds since the oldest of them was committed.
        integer(int64) :: age = 0
    end type delivery_t

    !! Minutes after which each rule fires.
    integer, parameter :: DEPLOY_MINUTES = 15, STALL_MINUTES = 15, LOST_MINUTES = 2, &
        MIRROR_MINUTES = 10
    !! The newest finished verifications of a repository that are looked at,
    !! and how far back along main they are placed.
    integer, parameter :: VERIFY_ROWS = 50, MAIN_COMMITS = 500
    character(len=*), parameter :: STAMP = "'%Y-%m-%dT%H:%M:%SZ'"

contains

    function alert_self_repo() result(name)
        !! The repository the instance is built from and updates itself from:
        !! FY_SELF_REPO, as self-update.sh names it, else fortyard.
        character(len=:), allocatable :: name
        integer :: n, stat

        name = 'fortyard'
        call get_environment_variable('FY_SELF_REPO', length=n, status=stat)
        if (stat /= 0 .or. n == 0) return
        allocate (character(len=n) :: name)
        call get_environment_variable('FY_SELF_REPO', name)
        if (.not. repo_name_ok(name)) name = 'fortyard'
    end function alert_self_repo

    subroutine alert_list(db, repos_dir, serving, self_repo, alerts)
        !! The alerts that fire now, in the order of the rules in the design.
        !! `serving` is the commit the instance runs (FY_COMMIT), `self_repo`
        !! the repository it follows; the mirrors are `repos_dir`/NAME.git.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repos_dir, serving, self_repo
        type(alert_t), allocatable, intent(out) :: alerts(:)

        allocate (alerts(0))
        call main_broken(db, repos_dir, alerts)
        call queue_stalled(db, alerts)
        call runner_lost(db, alerts)
        call deploy_behind(db, repos_dir, serving, self_repo, alerts)
        call mirror_failing(db, alerts)
    end subroutine alert_list

    function alert_json(alerts) result(json)
        !! [{"rule","repo","since","detail"}], repo null for the instance.
        type(alert_t), intent(in) :: alerts(:)
        character(len=:), allocatable :: json
        integer :: k

        json = '['
        do k = 1, size(alerts)
            if (k > 1) json = json//','
            json = json//'{"rule":"'//alerts(k)%rule//'","repo":'//text_or_null(alerts(k)%repo)// &
                ',"since":'//text_or_null(alerts(k)%since)//',"detail":"'// &
                json_escape_string(alerts(k)%detail)//'"}'
        end do
        json = json//']'
    end function alert_json

    function alert_delivery_json(db, repos_dir, serving, version, self_repo) result(json)
        !! What the instance serves against main of the repository it follows:
        !! {"repo","version","commit","main","behind","since","release"}, with
        !! null for what is not known (no such repository, a build without a
        !! commit) and release the newest of the repository or null.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repos_dir, serving, version, self_repo
        character(len=:), allocatable :: json
        type(delivery_t) :: d
        character(len=16) :: num

        call delivery(db, repos_dir, serving, self_repo, d)
        json = '{"repo":'//text_or_null(d%repo)//',"version":"'//json_escape_string(version)// &
            '","commit":"'//json_escape_string(serving)//'","main":'//text_or_null(d%head)// &
            ',"behind":'
        if (d%behind >= 0) then
            write (num, '(i0)') d%behind
            json = json//trim(num)
        else
            json = json//'null'
        end if
        json = json//',"since":'//text_or_null(d%since)//',"release":'
        if (len(d%rel_version) > 0) then
            json = json//'{"version":"'//json_escape_string(d%rel_version)//'","commit":"'// &
                d%rel_commit//'","state":"'//d%rel_state//'","created":"'//d%rel_created//'"}}'
        else
            json = json//'null}'
        end if
    end function alert_delivery_json

    logical function alert_ready(db, reason) result(ready)
        !! Whether the store can be read and has the schema of this release:
        !! the numbered migrations up to SCHEMA_VERSION and every named one of
        !! the registry. `reason` says what fails.
        type(db_t), intent(in) :: db
        character(len=:), allocatable, intent(out) :: reason
        type(migration_t), allocatable :: list(:)
        type(stmt_t) :: st
        integer(int64) :: version
        integer :: stat, k
        character(len=16) :: have, need

        ready = .false.
        reason = 'the database cannot be read'
        call db_prepare(db, 'SELECT coalesce(max(version), 0) FROM schema_version;', st, stat)
        if (stat /= SQLITE_OK) then
            call stmt_finalize(st)
            ! A store without the table was never migrated.
            call db_prepare(db, 'SELECT count(*) FROM sqlite_master;', st, stat)
            if (stat == SQLITE_OK) then
                if (stmt_step(st) == SQLITE_ROW) reason = 'the schema is not migrated'
            end if
            call stmt_finalize(st)
            return
        end if
        version = -1
        if (stmt_step(st) == SQLITE_ROW) version = stmt_int(st, 0)
        call stmt_finalize(st)
        if (version < 0) return
        if (version < SCHEMA_VERSION) then
            write (have, '(i0)') version
            write (need, '(i0)') SCHEMA_VERSION
            reason = 'the schema is at version '//trim(have)//', this release needs '//trim(need)
            return
        end if
        call migrations_registry(list)
        do k = 1, size(list)
            call db_prepare(db, 'SELECT count(*) FROM migrations WHERE name = ?1;', st, stat)
            if (stat == SQLITE_OK) call stmt_bind_text(st, 1, list(k)%name, stat)
            if (stat == SQLITE_OK) then
                if (stmt_step(st) /= SQLITE_ROW) stat = 1
            end if
            if (stat == SQLITE_OK) then
                if (stmt_int(st, 0) == 0) stat = 1
            end if
            call stmt_finalize(st)
            if (stat /= SQLITE_OK) then
                reason = 'migration "'//list(k)%name//'" is not applied'
                return
            end if
        end do
        reason = ''
        ready = .true.
    end function alert_ready

    ! ---- the rules ------------------------------------------------------------

    subroutine main_broken(db, repos_dir, alerts)
        !! The newest finished verification of a repository failed. Newest is
        !! by the place of its commit on main, as bisection verifies older
        !! commits after the newest one; without the mirror, by its number.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repos_dir
        type(alert_t), allocatable, intent(inout) :: alerts(:)
        type(stmt_t) :: st
        character(len=64), allocatable :: repo(:), state(:), commit(:)
        character(len=20), allocatable :: finished(:)
        integer(int64), allocatable :: id(:)
        character(len=16) :: num, rows
        integer :: stat, n, first, last

        write (rows, '(i0)') VERIFY_ROWS
        allocate (repo(0), state(0), commit(0), finished(0), id(0))
        call db_prepare(db, "SELECT repo, id, coalesce(commit_id, ''), state,"// &
            " coalesce(finished_at, '') FROM (SELECT repo, id, commit_id, state, finished_at,"// &
            ' row_number() OVER (PARTITION BY repo ORDER BY id DESC) AS k FROM landings'// &
            " WHERE kind = 'verify' AND state IN ('passed', 'failed')) WHERE k <= "//trim(rows)// &
            ' ORDER BY repo, id DESC;', st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            repo = [repo, [character(len=64) :: stmt_text(st, 0)]]
            id = [id, stmt_int(st, 1)]
            commit = [commit, [character(len=64) :: stmt_text(st, 2)]]
            state = [state, [character(len=64) :: stmt_text(st, 3)]]
            finished = [finished, [character(len=20) :: stmt_text(st, 4)]]
        end do
        call stmt_finalize(st)
        n = size(repo)
        first = 1
        do while (first <= n)
            last = first
            do while (last < n)
                if (repo(last + 1) /= repo(first)) exit
                last = last + 1
            end do
            call decide(first, last)
            first = last + 1
        end do

    contains

        subroutine decide(a, b)
            integer, intent(in) :: a, b
            character(len=:), allocatable :: out
            integer :: k, best, pos, best_pos, status
            character(len=16) :: count

            best = a
            ! Only a mix of outcomes needs the order along main.
            if (any(state(a:b) /= state(a))) then
                write (count, '(i0)') MAIN_COMMITS
                call proc_run([character(len=1024) :: 'git', '--git-dir', &
                    repos_dir//'/'//trim(repo(a))//'.git', 'rev-list', '--first-parent', &
                    '-n', count, 'refs/heads/main', '--'], out, status, 64 * MAIN_COMMITS)
                if (status == 0) then
                    best_pos = huge(best_pos)
                    do k = a, b
                        if (len_trim(commit(k)) /= 40) cycle
                        pos = index(out, trim(commit(k)))
                        if (pos > 0 .and. pos < best_pos) then
                            best_pos = pos
                            best = k
                        end if
                    end do
                end if
            end if
            if (state(best) /= 'failed') return
            write (num, '(i0)') id(best)
            call add(alerts, 'main-broken', trim(repo(a)), trim(finished(best)), &
                'verification '//trim(num)//' of main '//short(trim(commit(best)))//' failed')
        end subroutine decide

    end subroutine main_broken

    subroutine queue_stalled(db, alerts)
        !! An entry has been queued for STALL_MINUTES while no check of its
        !! repository runs: none on a runner, and no landing whose check the
        !! lander runs itself (one without a check row).
        type(db_t), intent(in) :: db
        type(alert_t), allocatable, intent(inout) :: alerts(:)
        type(stmt_t) :: st
        character(len=16) :: minutes
        integer :: stat

        write (minutes, '(i0)') STALL_MINUTES
        call db_prepare(db, 'SELECT l.repo, count(*), min(l.id), min(l.created_at),'// &
            " CAST((julianday('now') - julianday(min(l.created_at))) * 1440 AS INTEGER)"// &
            " FROM landings l WHERE l.kind = 'landing' AND l.state = 'queued' AND"// &
            ' l.created_at <= strftime('//STAMP//", 'now', '-"//trim(minutes)//" minutes')"// &
            ' AND NOT EXISTS (SELECT 1 FROM checks c WHERE c.repo = l.repo AND'// &
            " c.state = 'running') AND NOT EXISTS (SELECT 1 FROM landings r WHERE"// &
            " r.repo = l.repo AND r.kind = 'landing' AND r.state = 'running' AND"// &
            ' r.check_id IS NULL) GROUP BY l.repo ORDER BY l.repo;', st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            call add(alerts, 'queue-stalled', stmt_text(st, 0), stmt_text(st, 3), &
                entries(stmt_int(st, 1))//' waiting over '//trim(minutes)// &
                ' min, the oldest (job '//itoa(stmt_int(st, 2))//') for '// &
                itoa(stmt_int(st, 4))//' min; no check of '//stmt_text(st, 0)//' runs')
        end do
        call stmt_finalize(st)

    contains

        function entries(n) result(s)
            integer(int64), intent(in) :: n
            character(len=:), allocatable :: s
            if (n == 1) then
                s = '1 entry'
            else
                s = itoa(n)//' entries'
            end if
        end function entries

    end subroutine queue_stalled

    subroutine runner_lost(db, alerts)
        !! Checks of a runner class have waited for LOST_MINUTES and no runner
        !! of that class claimed or reported a check in that time, nor holds
        !! one now. Runners leave their name and times on the checks they take.
        type(db_t), intent(in) :: db
        type(alert_t), allocatable, intent(inout) :: alerts(:)
        type(stmt_t) :: st
        character(len=:), allocatable :: limit, seen, detail
        character(len=16) :: minutes
        integer :: stat

        write (minutes, '(i0)') LOST_MINUTES
        limit = 'strftime('//STAMP//", 'now', '-"//trim(minutes)//" minutes')"
        call db_prepare(db, 'SELECT cls, n, oldest, seen,'// &
            " CAST((julianday('now') - julianday(oldest)) * 1440 AS INTEGER) FROM"// &
            ' (SELECT q.class AS cls, count(*) AS n, min(coalesce(q.queued_at, q.created_at))'// &
            " AS oldest, coalesce((SELECT max(max(coalesce(s.claimed_at, ''),"// &
            " coalesce(s.finished_at, ''))) FROM checks s WHERE s.class = q.class AND"// &
            " s.runner IS NOT NULL), '') AS seen FROM checks q WHERE q.state = 'queued'"// &
            ' GROUP BY q.class) WHERE oldest <= '//limit//' AND seen < '//limit// &
            ' AND NOT EXISTS (SELECT 1 FROM checks r WHERE r.class = cls AND'// &
            " r.state = 'running')"// &
            ' ORDER BY cls;', st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            seen = stmt_text(st, 3)
            detail = itoa(stmt_int(st, 1))//' checks of class '//stmt_text(st, 0)// &
                ' queued, the oldest for '//itoa(stmt_int(st, 4))//' min; '
            if (stmt_int(st, 1) == 1) detail = '1 check of class '//stmt_text(st, 0)// &
                ' queued for '//itoa(stmt_int(st, 4))//' min; '
            if (len(seen) > 0) then
                detail = detail//'no runner of the class seen since '//seen
            else
                detail = detail//'no runner of the class was ever seen'
                seen = stmt_text(st, 2)
            end if
            call add(alerts, 'runner-lost', '', seen, detail)
        end do
        call stmt_finalize(st)
    end subroutine runner_lost

    subroutine deploy_behind(db, repos_dir, serving, self_repo, alerts)
        !! main of the repository the instance follows has had a commit for
        !! DEPLOY_MINUTES that the instance does not serve. A build that failed
        !! is not recorded on the instance (the builder keeps it), and a
        !! refused archive leaves its release open: both leave main ahead, and
        !! the detail names the newest release.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repos_dir, serving, self_repo
        type(alert_t), allocatable, intent(inout) :: alerts(:)
        type(delivery_t) :: d
        character(len=:), allocatable :: detail

        call delivery(db, repos_dir, serving, self_repo, d)
        if (d%behind <= 0 .or. d%age < 60 * DEPLOY_MINUTES) return
        detail = 'main '//short(d%head)//' is '//itoa(int(d%behind, int64))//' commits'
        if (d%behind == 1) detail = 'main '//short(d%head)//' is 1 commit'
        detail = detail//' ahead of the served '//short(serving)
        if (len(d%rel_version) == 0) then
            detail = detail//'; no release yet'
        else if (d%rel_commit /= d%head) then
            detail = detail//'; no release of '//short(d%head)//', the newest is '// &
                d%rel_version//' of '//short(d%rel_commit)
        else if (d%rel_state == 'ready') then
            detail = detail//'; release '//d%rel_version//' is ready and not installed'
        else
            detail = detail//'; release '//d%rel_version//' is '//d%rel_state// &
                ', its archive was not accepted'
        end if
        call add(alerts, 'deploy-behind', d%repo, d%since, detail)
    end subroutine deploy_behind

    subroutine mirror_failing(db, alerts)
        !! A mirror's fetches fail and the last one that succeeded (or, before
        !! any did, its registration) is older than MIRROR_MINUTES.
        type(db_t), intent(in) :: db
        type(alert_t), allocatable, intent(inout) :: alerts(:)
        type(stmt_t) :: st
        character(len=16) :: minutes
        integer :: stat

        write (minutes, '(i0)') MIRROR_MINUTES
        call db_prepare(db, 'SELECT name, coalesce(fetch_ok_at, created_at), fetch_status,'// &
            " fetch_ok_at IS NULL FROM repos WHERE mode = 'mirror' AND fetch_status IS NOT NULL"// &
            ' AND fetch_status <> 0 AND coalesce(fetch_ok_at, created_at) <= strftime('// &
            STAMP//", 'now', '-"//trim(minutes)//" minutes') ORDER BY name;", st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            if (stmt_int(st, 3) /= 0) then
                call add(alerts, 'mirror-failing', stmt_text(st, 0), stmt_text(st, 1), &
                    'no fetch has succeeded; the last failed with status '//itoa(stmt_int(st, 2)))
            else
                call add(alerts, 'mirror-failing', stmt_text(st, 0), stmt_text(st, 1), &
                    'fetches fail since the last success; the last failed with status '// &
                    itoa(stmt_int(st, 2)))
            end if
        end do
        call stmt_finalize(st)
    end subroutine mirror_failing

    ! ---- helpers --------------------------------------------------------------

    subroutine delivery(db, repos_dir, serving, self_repo, d)
        !! main of `self_repo` against the commit `serving`: the commits of
        !! main it lacks, since when the oldest of them is there (its commit
        !! time; a landing commits as it lands) and the newest release.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repos_dir, serving, self_repo
        type(delivery_t), intent(out) :: d
        character(len=:), allocatable :: git_dir, out
        type(stmt_t) :: st
        integer(int64) :: oldest
        integer :: stat, status, k, ios

        d%repo = ''
        d%head = ''
        d%since = ''
        d%rel_version = ''
        d%rel_commit = ''
        d%rel_state = ''
        d%rel_created = ''
        if (.not. repo_name_ok(self_repo)) return
        call db_prepare(db, 'SELECT count(*) FROM repos WHERE name = ?1;', st, stat)
        if (stat == SQLITE_OK) call stmt_bind_text(st, 1, self_repo, stat)
        if (stat == SQLITE_OK) then
            if (stmt_step(st) == SQLITE_ROW) then
                if (stmt_int(st, 0) > 0) d%repo = self_repo
            end if
        end if
        call stmt_finalize(st)
        if (len(d%repo) == 0) return

        call db_prepare(db, 'SELECT version, commit_id, state, created_at FROM releases'// &
            ' WHERE repo = ?1 ORDER BY id DESC LIMIT 1;', st, stat)
        if (stat == SQLITE_OK) call stmt_bind_text(st, 1, self_repo, stat)
        if (stat == SQLITE_OK) then
            if (stmt_step(st) == SQLITE_ROW) then
                d%rel_version = stmt_text(st, 0)
                d%rel_commit = stmt_text(st, 1)
                d%rel_state = stmt_text(st, 2)
                d%rel_created = stmt_text(st, 3)
            end if
        end if
        call stmt_finalize(st)

        git_dir = repos_dir//'/'//self_repo//'.git'
        call git([character(len=64) :: 'rev-parse', '--verify', '-q', 'refs/heads/main'], &
            out, status)
        if (status /= 0 .or. len(out) < 40) return
        d%head = out(1:40)
        ! A build without its commit (a development build) cannot be compared.
        if (len(serving) < 7 .or. len(serving) > 40 .or. &
            verify(serving, '0123456789abcdef') /= 0) return
        if (d%head(1:len(serving)) == serving) then
            d%behind = 0
            return
        end if
        call git([character(len=64) :: 'log', '--format=%ct', serving//'..refs/heads/main', &
            '--'], out, status)
        ! The served commit is not in this history: all of main is new to it.
        if (status /= 0) call git([character(len=64) :: 'log', '-1', '--format=%ct', &
            'refs/heads/main', '--'], out, status)
        if (status /= 0) return
        d%behind = 0
        oldest = 0
        k = 1
        do while (k <= len(out))
            stat = index(out(k:), new_line('a'))
            if (stat == 0) stat = len(out) - k + 2
            if (stat > 1) then
                read (out(k:k + stat - 2), *, iostat=ios) oldest
                if (ios == 0) d%behind = d%behind + 1
            end if
            k = k + stat
        end do
        if (d%behind == 0) return
        call db_prepare(db, 'SELECT strftime('//STAMP//", ?1, 'unixepoch'),"// &
            " CAST(strftime('%s', 'now') AS INTEGER) - ?1;", st, stat)
        if (stat == SQLITE_OK) call stmt_bind_int(st, 1, oldest, stat)
        if (stat == SQLITE_OK) then
            if (stmt_step(st) == SQLITE_ROW) then
                d%since = stmt_text(st, 0)
                d%age = stmt_int(st, 1)
            end if
        end if
        call stmt_finalize(st)

    contains

        subroutine git(args, output, code)
            character(len=*), intent(in) :: args(:)
            character(len=:), allocatable, intent(out) :: output
            integer, intent(out) :: code
            character(len=1024) :: argv(size(args) + 3)

            argv(1) = 'git'
            argv(2) = '--git-dir'
            argv(3) = git_dir
            argv(4:) = args
            call proc_run(argv, output, code, 1048576)
        end subroutine git

    end subroutine delivery

    subroutine add(alerts, rule, repo, since, detail)
        type(alert_t), allocatable, intent(inout) :: alerts(:)
        character(len=*), intent(in) :: rule, repo, since, detail
        type(alert_t), allocatable :: grown(:)
        integer :: k

        allocate (grown(size(alerts) + 1))
        do k = 1, size(alerts)
            call move_alloc(alerts(k)%rule, grown(k)%rule)
            call move_alloc(alerts(k)%repo, grown(k)%repo)
            call move_alloc(alerts(k)%since, grown(k)%since)
            call move_alloc(alerts(k)%detail, grown(k)%detail)
        end do
        k = size(grown)
        grown(k)%rule = rule
        grown(k)%repo = repo
        grown(k)%since = since
        grown(k)%detail = detail
        call move_alloc(grown, alerts)
    end subroutine add

    function short(commit) result(s)
        character(len=*), intent(in) :: commit
        character(len=:), allocatable :: s
        s = commit(1:min(7, len(commit)))
    end function short

    function itoa(v) result(s)
        integer(int64), intent(in) :: v
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, '(i0)') v
        s = trim(buf)
    end function itoa

    function text_or_null(s) result(json)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: json
        if (len(s) == 0) then
            json = 'null'
        else
            json = '"'//json_escape_string(s)//'"'
        end if
    end function text_or_null

end module fy_alert
