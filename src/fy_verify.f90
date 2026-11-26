module fy_verify
    !! Eventual correctness (doc/testing-at-scale.md): landings run the
    !! affected tests, and verifications of main run everything afterwards.
    !! After main moves, a runner verifies its newest commit with the
    !! repository's full suite (.fortyard/verify, else every fpm test run
    !! again), which covers all commits since the last verification. When a
    !! verification fails after an earlier one passed, the commits of main
    !! between them are bisected (first parents, about log2(n) more runs), and
    !! the first failing one is reported once: an issue with the evidence, and a
    !! comment on the change that landed it. The issue is closed when main
    !! passes again. Verifications are jobs of kind "verify"; runners take them
    !! after landing checks.
    !!
    !! A class of verify_on that ends in @nightly (doc/backends.md "Nightly
    !! classes") has jobs of its own, which record the class: the first pass of
    !! a night's window (01:00 to 06:00 server time) records main, which is
    !! verified on that class alone when it has no result there yet, and a main
    !! that moves later that night waits for the next night; a failure is
    !! bisected within that window or the next ones, and reported and closed by
    !! issues of that class. Its open issues are closed once verify_on no
    !! longer names it @nightly. The other classes are verified together after
    !! every move of main, as before.
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64
    use fy_change, only: &
        change_comment, &
        change_get, &
        change_t
    use fy_events, only: &
        events_forget, &
        events_notify, &
        events_wait, &
        events_watch_open
    use fy_issue, only: &
        issue_comment, &
        issue_open, &
        issue_set_state
    use fy_live, only: &
        live_outcome, &
        live_remove
    use fy_mail, only: &
        mail_issue
    use fy_policy, only: &
        policy_get, &
        policy_t
    use fy_proc, only: &
        proc_exit, &
        proc_run
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_exec, &
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
        repo_info_t, &
        store_check_add, &
        store_check_get, &
        store_open, &
        store_repo_list
    implicit none
    private

    public :: &
        verifier_main, &
        verify_request

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    !! Commits of main looked back for the last passing verification.
    integer, parameter :: DEPTH = 200
    integer, parameter :: LOG_TAIL = 6000
    character(len=*), parameter :: ACCOUNT = 'fortyard'
    character(len=*), parameter :: UNCONFIGURED = &
        'Main has neither .fortyard/verify nor fpm.toml, so nothing verifies it.'
    !! The reports of track ?3: CULPRIT@CLASS for a nightly class, else CULPRIT.
    character(len=*), parameter :: OF_TRACK = "(CASE WHEN ?3 = '' THEN instr(culprit, '@') = 0"// &
        " ELSE substr(culprit, -1 - length(?3)) = '@' || ?3 END)"

contains

    subroutine verifier_main(db_path, repos_dir)
        !! The verifier process: one verification at a time in each repository
        !! and nightly class, the repositories side by side; it sleeps until a
        !! job changes, and looks again each minute while checks run, else
        !! after ten minutes, which bounds how late a night window is seen.
        character(len=*), intent(in) :: db_path, repos_dir
        type(db_t) :: db
        type(repo_info_t), allocatable :: repos(:)
        character(len=1024) :: url
        character(len=:), allocatable :: base
        integer :: stat, i, n, watch
        logical :: busy, changed, pending, waits

        ! Links in commits and issues are read by people, so they use the public
        ! address when the server listens on an internal one.
        call get_environment_variable('FY_PUBLIC_URL', url, n, stat)
        if (stat /= 0 .or. n == 0) call get_environment_variable('FY_URL', url, n, stat)
        base = ''
        if (stat == 0) base = trim(url)
        call store_open(db_path, db, stat)
        if (stat /= 0) call proc_exit(1)
        watch = events_watch_open()
        call resume(db, repos_dir)
        do
            busy = .false.
            pending = .false.
            call store_repo_list(db, repos, stat)
            do i = 1, size(repos)
                if (repos(i)%mode /= 'primary') cycle
                if (step(db, repos(i)%name, repos_dir//'/'//trim(repos(i)%name)//'.git', base, &
                    waits)) busy = .true.
                if (waits) pending = .true.
            end do
            if (busy) cycle
            if (pending) then
                changed = events_wait(watch, 'checks', 60000)
            else
                changed = events_wait(watch, 'jobs', 600000)
            end if
        end do
    end subroutine verifier_main

    subroutine resume(db, repos_dir)
        !! After a restart (a new release), verifications whose runner check
        !! still exists go on (step settles them); the others are cancelled,
        !! as they neither passed nor failed.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repos_dir
        type(stmt_t) :: st
        integer(int64), allocatable :: jobs(:), checks(:)
        character(len=64), allocatable :: repos(:), commits(:)
        integer :: stat, k

        allocate (jobs(0), checks(0), repos(0), commits(0))
        call db_prepare(db, "SELECT l.id, coalesce((SELECT max(c.id) FROM checks c WHERE c.ref ="// &
            " 'refs/fortyard/verify/' || l.id), 0), l.repo, l.commit_id FROM landings l"// &
            " WHERE l.kind = 'verify' AND l.state = 'running';", st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            jobs = [jobs, stmt_int(st, 0)]
            checks = [checks, stmt_int(st, 1)]
            repos = [repos, [character(len=64) :: stmt_text(st, 2)]]
            commits = [commits, [character(len=64) :: stmt_text(st, 3)]]
        end do
        call stmt_finalize(st)
        do k = 1, size(jobs)
            if (checks(k) == 0) call exec(db, "UPDATE landings SET state = 'cancelled',"// &
                ' finished_at = '//NOW//' WHERE id = ?1 AND ?2 = ?2;', jobs(k), '')
        end do
        ! A verifier stopped after queuing a check but before telling the
        ! runners would leave it waiting until they look again on their own.
        if (any(checks > 0)) call events_notify('checks')
    end subroutine resume

    integer(int64) function verify_request(db, repo, git_dir) result(id)
        !! Asks for a verification of main's newest commit even when it was
        !! verified (an audit, or a suspected flaky failure); the job's id, or 0
        !! when main does not exist. A request already waiting is reused.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir
        character(len=:), allocatable :: head
        type(stmt_t) :: st
        integer :: stat

        id = 0
        head = rev(git_dir, 'refs/heads/main')
        if (len(head) == 0) return
        id = queued_request(db, repo)
        if (id > 0) return
        call db_prepare(db, "INSERT INTO landings (repo, branch, account, created_at, kind,"// &
            " commit_id) VALUES (?1, 'main', ?2, "//NOW//", 'verify', ?3) RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, ACCOUNT, stat)
        call stmt_bind_text(st, 3, head, stat)
        if (stmt_step(st) == SQLITE_ROW) id = stmt_int(st, 0)
        call stmt_finalize(st)
        call events_notify('jobs')
    end function verify_request

    logical function verifiable(git_dir, nightly) result(able)
        !! Whether main exists and has .fortyard/verify or fpm.toml, without
        !! which a runner has no verification to run; for a `nightly` track
        !! also .fortyard/nightly, which the nightly run takes first.
        character(len=*), intent(in) :: git_dir
        logical, intent(in) :: nightly
        character(len=:), allocatable :: out
        integer :: status

        status = 1
        if (nightly) call git(git_dir, [character(len=256) :: 'cat-file', '-e', &
            'refs/heads/main:.fortyard/nightly'], out, status)
        if (status /= 0) call git(git_dir, [character(len=256) :: 'cat-file', '-e', &
            'refs/heads/main:.fortyard/verify'], out, status)
        if (status /= 0) call git(git_dir, [character(len=256) :: 'cat-file', '-e', &
            'refs/heads/main:fpm.toml'], out, status)
        able = status == 0
    end function verifiable

    logical function step(db, repo, git_dir, base, waits) result(ran)
        !! For each track of `repo` (each nightly class, then the classes
        !! without schedule): settles its running verification once its checks
        !! ended, else starts the one it needs next, if any; `waits` while one
        !! runs. Nightly tracks come first, so a pass that settles the other
        !! track has looked at them with the same main.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, base
        logical, intent(out) :: waits
        character(len=:), allocatable :: head, target, out, commit, regular, night, from
        character(len=32), allocatable :: tracks(:)
        integer(int64) :: request, running
        integer :: status, t
        type(policy_t) :: p
        logical :: checked, able, able_night

        ran = .false.
        waits = .false.
        checked = .false.
        able = .false.
        call policy_get(db, repo, p)
        call tracks_of(db, repo, p%verify_on, tracks, regular)
        call close_dropped(db, repo, tracks)
        do t = 1, size(tracks)
            call running_job(db, repo, trim(tracks(t)), running, commit)
            if (running > 0) then
                if (settle(db, repo, git_dir, commit, running)) then
                    ran = .true.
                else
                    waits = .true.
                end if
                cycle
            end if
            night = ''
            request = 0
            if (len_trim(tracks(t)) > 0) then
                night = night_of(clock())
                if (len(night) == 0) cycle
            else
                request = queued_request(db, repo)
                if (len(regular) == 0) then
                    ! Every class has a schedule: a request has nothing to run on.
                    if (request > 0) call exec(db, "UPDATE landings SET state = 'cancelled',"// &
                        ' finished_at = '//NOW//", log = ?2 WHERE id = ?1;", request, &
                        'Every runner class of verify_on is verified at night only.')
                    cycle
                end if
            end if
            if (.not. checked) then
                checked = .true.
                head = rev(git_dir, 'refs/heads/main')
                able = verifiable(git_dir, .false.)
                able_night = verifiable(git_dir, .true.)
            end if
            if (.not. merge(able_night, able, len(night) > 0)) then
                ! A request left queued here would wait for ever.
                if (request > 0) call exec(db, "UPDATE landings SET state = 'cancelled',"// &
                    ' finished_at = '//NOW//", log = ?2 WHERE id = ?1;", request, UNCONFIGURED)
                cycle
            end if
            if (request > 0) then
                target = head
            else if (len(night) > 0) then
                ! Main is looked at once a night: the track goes on from the main
                ! of the night's first pass even when main moved since.
                from = night_head(db, repo, trim(tracks(t)), night, head)
                target = next_target(db, repo, git_dir, from, base, trim(tracks(t)))
            else
                target = next_target(db, repo, git_dir, head, base, '')
            end if
            if (len(target) == 0) cycle
            if (len_trim(tracks(t)) > 0) then
                call run(db, repo, git_dir, target, request, trim(tracks(t)), trim(tracks(t)))
            else
                call run(db, repo, git_dir, target, request, '', regular)
            end if
            ran = .true.
            waits = .true.
        end do
    end function step

    subroutine tracks_of(db, repo, verify_on, tracks, regular)
        !! The nightly classes of `verify_on`, and those of a running job that
        !! the policy no longer names, then '' for the classes without schedule,
        !! which `regular` lists with commas ('' when there are none).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, verify_on
        character(len=32), allocatable, intent(out) :: tracks(:)
        character(len=:), allocatable, intent(out) :: regular
        character(len=:), allocatable :: on, one
        type(stmt_t) :: st
        integer :: k, stat

        allocate (tracks(0))
        regular = ''
        on = verify_on//','
        do while (len(on) > 1)
            k = index(on, ',')
            one = on(1:k - 1)
            on = on(k + 1:)
            if (len(one) == 0) cycle
            if (len(one) > 8) then
                if (one(len(one) - 7:) == '@nightly') then
                    tracks = [tracks, [character(len=32) :: one(1:len(one) - 8)]]
                    cycle
                end if
            end if
            if (len(regular) > 0) regular = regular//','
            regular = regular//one
        end do
        call db_prepare(db, "SELECT DISTINCT verify_class FROM landings WHERE kind = 'verify'"// &
            " AND repo = ?1 AND state = 'running' AND verify_class <> '';", st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, repo, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                one = stmt_text(st, 0)
                if (.not. any(tracks == one)) tracks = [tracks, [character(len=32) :: one]]
            end do
            call stmt_finalize(st)
        end if
        tracks = [tracks, [character(len=32) :: '']]
    end subroutine tracks_of

    function night_head(db, repo, track, night, head) result(commit)
        !! The main `track` saw in the first pass of `night`, recorded as
        !! main's `head` by that pass.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, track, night, head
        character(len=:), allocatable :: commit
        type(stmt_t) :: st
        integer :: stat

        commit = head
        call db_prepare(db, 'INSERT INTO verify_nights (repo, class, night, main)'// &
            ' VALUES (?1, ?2, ?3, ?4) ON CONFLICT DO UPDATE SET main = main RETURNING main;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, track, stat)
        call stmt_bind_text(st, 3, night, stat)
        call stmt_bind_text(st, 4, head, stat)
        if (stmt_step(st) == SQLITE_ROW) commit = stmt_text(st, 0)
        call stmt_finalize(st)
    end function night_head

    subroutine close_dropped(db, repo, tracks)
        !! Closes the open reports of a nightly class that is no longer one of
        !! `tracks`, as nothing would verify it again.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        character(len=32), intent(in) :: tracks(:)
        character(len=32), allocatable :: gone(:)
        character(len=:), allocatable :: one
        type(stmt_t) :: st
        integer :: stat, k

        allocate (gone(0))
        call db_prepare(db, "SELECT DISTINCT substr(culprit, instr(culprit, '@') + 1)"// &
            " FROM verify_reports WHERE repo = ?1 AND state = 'open'"// &
            " AND instr(culprit, '@') > 0;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            one = stmt_text(st, 0)
            if (.not. any(tracks == one)) gone = [gone, [character(len=32) :: one]]
        end do
        call stmt_finalize(st)
        do k = 1, size(gone)
            call close_reports(db, repo, '', trim(gone(k)), 'The runner class '//trim(gone(k))// &
                ' is no longer verified at night (verify_on), so this report is closed'// &
                ' without a verdict.')
        end do
    end subroutine close_dropped

    function clock() result(t)
        !! The server time as YYYY-MM-DDTHH:MM. FY_VERIFY_NOW, in that form,
        !! sets it for tests.
        character(len=:), allocatable :: t
        character(len=64) :: v
        character(len=8) :: d
        character(len=10) :: tm
        integer :: n, stat

        call get_environment_variable('FY_VERIFY_NOW', v, n, stat)
        if (stat == 0 .and. n >= 16) then
            t = v(1:16)
            return
        end if
        call date_and_time(date=d, time=tm)
        t = d(1:4)//'-'//d(5:6)//'-'//d(7:8)//'T'//tm(1:2)//':'//tm(3:4)
    end function clock

    pure function night_of(t) result(night)
        !! The date of `t` (YYYY-MM-DDTHH:MM) when it lies in that night's
        !! window, 01:00 to 06:00, else ''.
        character(len=*), intent(in) :: t
        character(len=:), allocatable :: night

        night = ''
        if (len(t) < 13) return
        if (t(12:13) >= '01' .and. t(12:13) < '06' .and. verify(t(12:13), '0123456789') == 0) &
            night = t(1:10)
    end function night_of

    function next_target(db, repo, git_dir, head, base, track) result(target)
        !! The commit to verify next in `track`: `head` when it has no result;
        !! a commit between the last pass and the first failure while that
        !! range holds more than one commit; else nothing, after reporting the
        !! culprit or closing the reports when main passes.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, head, base, track
        character(len=:), allocatable :: target, chain, state
        character(len=40), allocatable :: commits(:)
        integer :: status, n, a, lo, hi, i
        type(policy_t) :: policy

        target = ''
        state = result_of(db, repo, head, track)
        if (len(state) == 0) then
            target = head
            return
        end if
        if (state == 'passed') then
            call close_reports(db, repo, head, track)
            return
        end if
        call policy_get(db, repo, policy)
        if (.not. policy%bisect) then
            ! Reported once, without further runs and without a culprit.
            call report(db, repo, git_dir, 'unknown', '', base, track, bisect=.false.)
            return
        end if
        call git(git_dir, [character(len=256) :: 'rev-list', '--first-parent', '--max-count=200', &
            head], chain, status)
        if (status /= 0) return
        call split(chain, commits, n)
        ! commits(1) is head; the newest one that passed is the known good end.
        a = 0
        do i = 2, n
            if (result_of(db, repo, commits(i), track) == 'passed') then
                a = i
                exit
            end if
        end do
        if (a == 0) then
            call report(db, repo, git_dir, 'unknown', '', base, track)
            return
        end if
        ! From oldest to newest between them: commits(a - 1) ... commits(1).
        hi = 0
        do i = a - 1, 1, -1
            if (result_of(db, repo, commits(i), track) == 'failed') then
                hi = i
                exit
            end if
        end do
        lo = a
        do i = a - 1, hi + 1, -1
            if (result_of(db, repo, commits(i), track) == 'passed') lo = i
        end do
        if (lo == hi + 1) then
            call report(db, repo, git_dir, trim(commits(hi)), trim(commits(lo)), base, track)
            return
        end if
        target = trim(commits((lo + hi + 1) / 2))
    end function next_target

    subroutine run(db, repo, git_dir, commit, request, track, classes)
        !! Verifies `commit` on a runner of each of `classes`, as job `request`
        !! or a new one of `track` (a nightly class, or ''); with the policy's
        !! verify_shards N > 1 as N checks per class, shards 1/N to N/N.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, commit, track, classes
        integer(int64), intent(in) :: request
        type(stmt_t) :: st
        integer(int64) :: id, check
        character(len=:), allocatable :: ref, out, on, shard
        character(len=24) :: num
        type(policy_t) :: pol
        integer :: stat, status, k, part

        id = request
        if (id > 0) then
            call exec(db, "UPDATE landings SET state = 'running', started_at = "//NOW// &
                ', commit_id = ?2 WHERE id = ?1;', id, commit)
        else
            call db_prepare(db, "INSERT INTO landings (repo, branch, account, created_at, kind,"// &
                " commit_id, state, started_at, verify_class) VALUES (?1, 'main', ?2,"// &
                " "//NOW//", 'verify', ?3, 'running', "//NOW//", ?4)"// &
                ' RETURNING id;', st, stat)
            if (stat /= SQLITE_OK) return
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_text(st, 2, ACCOUNT, stat)
            call stmt_bind_text(st, 3, commit, stat)
            call stmt_bind_text(st, 4, track, stat)
            if (stmt_step(st) == SQLITE_ROW) id = stmt_int(st, 0)
            call stmt_finalize(st)
        end if
        if (id == 0) return
        call events_notify('jobs')
        write (num, '(i0)') id
        ref = 'refs/fortyard/verify/'//trim(num)
        call git(git_dir, [character(len=256) :: 'update-ref', ref, commit], out, status)
        call policy_get(db, repo, pol)
        on = classes//','
        do while (len(on) > 1)
            k = index(on, ',')
            do part = 1, pol%verify_shards
                shard = ''
                if (pol%verify_shards > 1) shard = itoa(part)//'/'//itoa(pol%verify_shards)
                call store_check_add(db, repo, ref, check, stat, kind=trim(merge('nightly', &
                    'verify ', len(track) > 0)), class=on(1:k - 1), shard=shard)
            end do
            on = on(k + 1:)
        end do
        call events_notify('checks')
        ! The page of the job shows the check while it waits for a runner.
        call events_notify('jobs')
    end subroutine run

    logical function settle(db, repo, git_dir, commit, id) result(done)
        !! Records the outcome of verification job `id` once its runner checks,
        !! one per runner class, ended: passed when all passed, else the first
        !! that did not pass, with its class named when there are several.
        !! Not `done` while a check still waits or runs.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, commit
        integer(int64), intent(in) :: id
        character(len=:), allocatable :: state, log, out, one, text
        integer(int64), allocatable :: checks(:)
        character(len=32), allocatable :: classes(:), shards(:)
        character(len=24) :: num
        integer :: age, status, k, j
        logical :: waiting

        write (num, '(i0)') id
        call checks_of(db, 'refs/fortyard/verify/'//trim(num), checks, classes, shards)
        done = .false.
        state = ''
        log = ''
        if (size(checks) == 0) then
            state = 'failed'
            log = 'No check was queued.'
        end if
        if (size(checks) > 0) then
            ! The result waits for every class; it is the first that did not
            ! pass, else passed.
            state = 'passed'
            log = ''
            k = 0
            waiting = .false.
            do j = 1, size(checks)
                call store_check_get(db, checks(j), one, text, age)
                if (one == 'queued' .or. one == 'running') then
                    if ((one == 'queued' .and. age > 21600) .or. &
                        (one == 'running' .and. age > 28800)) then
                        one = 'lost'
                    else
                        waiting = .true.
                        cycle
                    end if
                end if
                if (k > 0 .or. one == 'passed') then
                    if (k == 0) log = text
                    cycle
                end if
                k = j
                if (one == 'timeout') then
                    text = live_outcome(text, 'Reason: timeout. The verification reached its'// &
                        ' time limit and was ended by the runner.')
                else if (one == 'lost') then
                    text = 'No runner reported a result.'
                end if
                state = 'failed'
                log = text
                if (len_trim(shards(j)) > 0) then
                    log = live_outcome(log, 'In shard '//trim(shards(j))//' on the runner class '// &
                        trim(classes(j))//'.')
                else if (size(checks) > 1) then
                    log = live_outcome(log, 'On the runner class '//trim(classes(j))//'.')
                end if
            end do
            if (waiting) return
        end if
        call git(git_dir, [character(len=256) :: 'update-ref', '-d', &
            'refs/fortyard/verify/'//trim(num)], out, status)
        call exec(db, 'UPDATE landings SET state = ?2, log = ?3, finished_at = '//NOW// &
            ' WHERE id = ?1;', id, state, log)
        ! The job keeps its stored log only.
        do j = 1, size(checks)
            write (num, '(i0)') checks(j)
            call live_remove('check-'//trim(num))
        end do
        write (num, '(i0)') id
        call events_forget('log-'//trim(num))
        call events_notify('jobs')
        write (error_unit, '(a)') 'fy: verification '//trim(num)//' of '//repo//' at '// &
            commit(1:min(12, len(commit)))//' '//state
        flush (error_unit)
        done = .true.
    end function settle

    subroutine checks_of(db, ref, ids, classes, shards)
        !! The runner checks queued for `ref`, one per class and shard, oldest
        !! first; a twin is read with the check it raced (store_check_get).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: ref
        integer(int64), allocatable, intent(out) :: ids(:)
        character(len=32), allocatable, intent(out) :: classes(:)
        character(len=32), allocatable, intent(out), optional :: shards(:)
        character(len=32), allocatable :: parts(:)
        type(stmt_t) :: st
        integer :: stat

        allocate (ids(0), classes(0), parts(0))
        if (present(shards)) allocate (shards(0))
        call db_prepare(db, 'SELECT id, class, shard FROM checks WHERE ref = ?1 AND'// &
            ' coalesce(twin, id) >= id ORDER BY id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, ref, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            ids = [ids, stmt_int(st, 0)]
            classes = [classes, [character(len=32) :: stmt_text(st, 1)]]
            parts = [parts, [character(len=32) :: stmt_text(st, 2)]]
        end do
        call stmt_finalize(st)
        if (present(shards)) shards = parts
    end subroutine checks_of

    subroutine report(db, repo, git_dir, culprit, good, base, track, bisect)
        !! Reports the first failing commit of `track` once: an issue, and a
        !! comment on the change that landed it. `culprit` 'unknown': main fails
        !! and no earlier verification passed, or the policy of the repository
        !! asks for no bisection (`bisect` false). A nightly class keys its
        !! reports as CULPRIT@CLASS, so each class reports a commit once.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, culprit, good, base, track
        logical, intent(in), optional :: bisect
        character(len=:), allocatable :: subject, title, body, log, link, key, on
        type(change_t) :: c
        type(stmt_t) :: st
        integer :: number, change, status, stat
        logical :: found

        key = culprit
        on = ''
        if (len(track) > 0) then
            key = culprit//'@'//track
            on = ' on '//track
        end if
        if (reported(db, repo, key)) return
        if (culprit == 'unknown') then
            title = 'The verification of main'//on//' fails'
            body = 'The verification of main'//on//' (.fortyard/verify, or every test run '// &
                'again) fails, and no earlier verification of main passed, so no change can '// &
                'be blamed. See the recent verify jobs of '//repo//'.'
            if (present(bisect)) then
                if (.not. bisect) body = 'The verification of main'//on//' (.fortyard/verify, '// &
                    'or every test run again) fails. The policy of the repository sets '// &
                    'bisect=off, so the commits since the last passing verification are not '// &
                    'bisected and no change is blamed. See the recent verify jobs of '//repo//'.'
            end if
            change = 0
        else
            call git(git_dir, [character(len=256) :: 'log', '-1', '--format=%s', culprit], subject, &
                status)
            subject = trim_nl(subject)
            change = number_in(subject)
            log = log_of(db, repo, culprit, track)
            title = 'Verification'//on//' fails since '//culprit(1:10)//' '//subject
            if (len(title) > 200) title = title(1:200)
            body = 'The verification of main'//on//' passes at '//good(1:10)//' and fails at '// &
                culprit(1:10)//', the next commit on main:'//new_line('a')//new_line('a')// &
                '    '//subject//new_line('a')//new_line('a')
            if (change > 0) body = body//'It landed change #'//itoa(change)//'. '
            body = body//'The commits of main between the last passing and the first failing '// &
                'verification were bisected.'
            if (len(track) > 0) body = body//' The runner class '//track//' is verified at '// &
                'night only (verify_on '//track//'@nightly).'
            body = body//new_line('a')//new_line('a')// &
                'End of the failing log:'//new_line('a')//new_line('a')//'```'//new_line('a')// &
                tail(log, LOG_TAIL)//new_line('a')//'```'
        end if
        if (len(track) > 0 .and. culprit == 'unknown') body = body//' The runner class '// &
            track//' is verified at night only (verify_on '//track//'@nightly).'
        number = issue_open(db, repo, git_dir, title, body, ACCOUNT)
        if (number == 0) return
        call mail_issue(repo, number, title, body)
        call db_prepare(db, 'INSERT INTO verify_reports (repo, culprit, issue, created_at)'// &
            ' VALUES (?1, ?2, ?3, '//NOW//');', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_text(st, 2, key, stat)
            call stmt_bind_int(st, 3, int(number, int64), stat)
            stat = stmt_step(st)
            call stmt_finalize(st)
        end if
        if (change > 0) then
            call change_get(db, repo, change, c, found)
            link = 'issue #'//itoa(number)
            if (len(base) > 0) link = link//' ('//base//'/'//repo//'/-/issues/'//itoa(number)//')'
            if (found) call change_comment(db, c%id, ACCOUNT, 'The verification of main'//on// &
                ' fails since this change landed: '//link//'.')
        end if
        write (error_unit, '(a)') 'fy: verification of '//repo//on//' reported as issue #'// &
            itoa(number)
        flush (error_unit)
    end subroutine report

    subroutine close_reports(db, repo, head, track, why)
        !! Main passes in `track` at `head`: its open regression reports are
        !! closed, with the comment `why` instead when given.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, head, track
        character(len=*), intent(in), optional :: why
        type(stmt_t) :: st
        integer, allocatable :: issues(:)
        character(len=:), allocatable :: on
        integer :: stat, k

        on = ''
        if (len(track) > 0) on = ' on '//track
        allocate (issues(0))
        call db_prepare(db, "SELECT issue FROM verify_reports WHERE repo = ?1 AND state = 'open'"// &
            ' AND '//OF_TRACK//';', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 3, track, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            issues = [issues, int(stmt_int(st, 0))]
        end do
        call stmt_finalize(st)
        do k = 1, size(issues)
            if (present(why)) then
                call issue_comment(db, repo, issues(k), ACCOUNT, why)
                call mail_issue(repo, issues(k), 'issue closed'//on, why)
            else
                call issue_comment(db, repo, issues(k), ACCOUNT, 'The verification of main'// &
                    on//' passes again at '//head(1:10)//'.')
                call mail_issue(repo, issues(k), 'main passes again'//on, &
                    'The verification of main'//on//' passes again at '//head(1:10)//'.')
            end if
            call issue_set_state(db, repo, issues(k), 'closed')
        end do
        ! A failure without a culprit may be reported again later.
        call exec(db, "DELETE FROM verify_reports WHERE repo = ?2 AND culprit IN ('unknown',"// &
            " 'unknown@' || ?3) AND "//OF_TRACK//' AND ?1 = ?1;', 0_int64, repo, track)
        call exec(db, "UPDATE verify_reports SET state = 'closed' WHERE repo = ?2 AND"// &
            " state = 'open' AND "//OF_TRACK//' AND ?1 = ?1;', 0_int64, repo, track)
    end subroutine close_reports

    logical function reported(db, repo, key)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, key
        type(stmt_t) :: st
        integer :: stat
        reported = .false.
        call db_prepare(db, 'SELECT 1 FROM verify_reports WHERE repo = ?1 AND culprit = ?2;', st, &
            stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, key, stat)
        reported = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end function reported

    function result_of(db, repo, commit, track) result(state)
        !! "passed" or "failed" from the latest verification of `commit` in
        !! `track`, or ''.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, commit, track
        character(len=:), allocatable :: state
        type(stmt_t) :: st
        integer :: stat
        state = ''
        call db_prepare(db, "SELECT state FROM landings WHERE kind = 'verify' AND repo = ?1"// &
            " AND commit_id = ?2 AND verify_class = ?3 AND state IN ('passed', 'failed')"// &
            ' ORDER BY id DESC LIMIT 1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, commit, stat)
        call stmt_bind_text(st, 3, track, stat)
        if (stmt_step(st) == SQLITE_ROW) state = stmt_text(st, 0)
        call stmt_finalize(st)
    end function result_of

    function log_of(db, repo, commit, track) result(log)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, commit, track
        character(len=:), allocatable :: log
        type(stmt_t) :: st
        integer :: stat
        log = ''
        call db_prepare(db, "SELECT coalesce(log, '') FROM landings WHERE kind = 'verify' AND"// &
            " repo = ?1 AND commit_id = ?2 AND verify_class = ?3 AND state = 'failed'"// &
            ' ORDER BY id DESC LIMIT 1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, commit, stat)
        call stmt_bind_text(st, 3, track, stat)
        if (stmt_step(st) == SQLITE_ROW) log = stmt_text(st, 0)
        call stmt_finalize(st)
    end function log_of

    integer(int64) function queued_request(db, repo) result(id)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(stmt_t) :: st
        integer :: stat
        id = 0
        call db_prepare(db, "SELECT id FROM landings WHERE kind = 'verify' AND repo = ?1"// &
            " AND state = 'queued' ORDER BY id LIMIT 1;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        if (stmt_step(st) == SQLITE_ROW) id = stmt_int(st, 0)
        call stmt_finalize(st)
    end function queued_request

    subroutine running_job(db, repo, track, id, commit)
        !! The running verification of `repo` in `track` and its commit; id 0
        !! if none.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, track
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: commit
        type(stmt_t) :: st
        integer :: stat
        id = 0
        commit = ''
        call db_prepare(db, "SELECT id, coalesce(commit_id, '') FROM landings WHERE kind = 'verify'"// &
            " AND repo = ?1 AND state = 'running' AND verify_class = ?2 ORDER BY id LIMIT 1;", st, &
            stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, track, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            commit = stmt_text(st, 1)
        end if
        call stmt_finalize(st)
    end subroutine running_job

    subroutine exec(db, sql, id, a, b)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: sql, a
        integer(int64), intent(in) :: id
        character(len=*), intent(in), optional :: b
        type(stmt_t) :: st
        integer :: stat
        call db_prepare(db, sql, st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, a, stat)
        if (present(b)) call stmt_bind_text(st, 3, b, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine exec

    function rev(git_dir, name) result(sha)
        character(len=*), intent(in) :: git_dir, name
        character(len=:), allocatable :: sha
        integer :: status
        call git(git_dir, [character(len=256) :: 'rev-parse', '--verify', '--quiet', name], sha, &
            status)
        sha = trim_nl(sha)
        if (status /= 0) sha = ''
    end function rev

    subroutine git(git_dir, args, out, status)
        character(len=*), intent(in) :: git_dir
        character(len=*), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: status
        call proc_run([character(len=1024) :: 'git', '--git-dir', git_dir, args], out, status, &
            1048576)
    end subroutine git

    subroutine split(text, lines, n)
        character(len=*), intent(in) :: text
        character(len=40), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: n
        integer :: pos, e
        allocate (lines(DEPTH))
        n = 0
        pos = 1
        do while (pos <= len(text) .and. n < DEPTH)
            e = index(text(pos:), new_line('a'))
            if (e == 0) e = len(text) - pos + 2
            if (e > 1) then
                n = n + 1
                lines(n) = text(pos:pos + e - 2)
            end if
            pos = pos + e
        end do
    end subroutine split

    integer function number_in(subject) result(n)
        !! N of a subject ending in "(#N)", else 0.
        character(len=*), intent(in) :: subject
        integer :: s, ios
        n = 0
        if (len(subject) < 4) return
        if (subject(len(subject):) /= ')') return
        s = index(subject, '(#', back=.true.)
        if (s == 0 .or. s + 2 > len(subject) - 1) return
        if (verify(subject(s + 2:len(subject) - 1), '0123456789') /= 0) return
        read (subject(s + 2:len(subject) - 1), *, iostat=ios) n
        if (ios /= 0) n = 0
    end function number_in

    function tail(s, n) result(t)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: t
        t = s(max(1, len(s) - n + 1):)
    end function tail

    function trim_nl(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: n
        n = len(s)
        do while (n > 0)
            if (s(n:n) /= new_line('a') .and. s(n:n) /= ' ') exit
            n = n - 1
        end do
        out = s(1:n)
    end function trim_nl

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_verify
