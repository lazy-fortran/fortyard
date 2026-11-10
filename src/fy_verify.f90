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
    use, intrinsic :: iso_fortran_env, only: int64, error_unit
    use fy_sqlite, only: db_t, stmt_t, db_prepare, db_exec, stmt_bind_text, stmt_bind_int, &
        stmt_step, stmt_text, stmt_int, stmt_finalize, SQLITE_OK, SQLITE_ROW
    use fy_store, only: store_open, store_repo_list, repo_info_t, store_check_add, store_check_get
    use fy_events, only: events_watch_open, events_wait, events_notify
    use fy_proc, only: proc_run, proc_exit
    use fy_issue, only: issue_open, issue_comment, issue_set_state
    use fy_change, only: change_t, change_get, change_comment
    implicit none
    private

    public :: verifier_main, verify_request

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    !! Commits of main looked back for the last passing verification.
    integer, parameter :: DEPTH = 200
    integer, parameter :: LOG_TAIL = 6000
    character(len=*), parameter :: ACCOUNT = 'fortyard'

contains

    subroutine verifier_main(db_path, repos_dir)
        !! The verifier process: one verification at a time, of whichever
        !! repository needs one; it sleeps until a job changes.
        character(len=*), intent(in) :: db_path, repos_dir
        type(db_t) :: db
        type(repo_info_t), allocatable :: repos(:)
        character(len=1024) :: url
        character(len=:), allocatable :: base
        integer :: stat, i, n, watch
        logical :: busy, changed

        call get_environment_variable('FY_URL', url, n, stat)
        base = ''
        if (stat == 0) base = trim(url)
        call store_open(db_path, db, stat)
        if (stat /= 0) call proc_exit(1)
        watch = events_watch_open()
        ! A verification interrupted by a restart neither passed nor failed.
        call db_exec(db, "UPDATE landings SET state = 'cancelled', finished_at = "//NOW// &
            " WHERE kind = 'verify' AND state = 'running';", stat)
        do
            busy = .false.
            call store_repo_list(db, repos, stat)
            do i = 1, size(repos)
                if (repos(i)%mode /= 'primary') cycle
                if (step(db, repos(i)%name, repos_dir//'/'//trim(repos(i)%name)//'.git', base)) &
                    busy = .true.
            end do
            if (.not. busy) changed = events_wait(watch, 'jobs', 600000)
        end do
    end subroutine verifier_main

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

    logical function step(db, repo, git_dir, base) result(ran)
        !! Runs the verification `repo` needs next, if any.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, base
        character(len=:), allocatable :: head, target, out
        integer(int64) :: request
        integer :: status

        ran = .false.
        head = rev(git_dir, 'refs/heads/main')
        if (len(head) == 0) return
        call git(git_dir, [character(len=256) :: 'cat-file', '-e', 'refs/heads/main:.fortyard/verify'], &
            out, status)
        if (status /= 0) call git(git_dir, [character(len=256) :: 'cat-file', '-e', &
            'refs/heads/main:fpm.toml'], out, status)
        if (status /= 0) return
        request = queued_request(db, repo)
        if (request > 0) then
            target = head
        else
            target = next_target(db, repo, git_dir, head, base)
        end if
        if (len(target) == 0) return
        call run(db, repo, git_dir, target, request)
        ran = .true.
    end function step

    function next_target(db, repo, git_dir, head, base) result(target)
        !! The commit to verify next: main's head when it has no result; a
        !! commit between the last pass and the first failure while that range
        !! holds more than one commit; else nothing, after reporting the
        !! culprit or closing the reports when main passes.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, head, base
        character(len=:), allocatable :: target, chain, state
        character(len=40), allocatable :: commits(:)
        integer :: status, n, a, lo, hi, i

        target = ''
        state = result_of(db, repo, head)
        if (len(state) == 0) then
            target = head
            return
        end if
        if (state == 'passed') then
            call close_reports(db, repo, head)
            return
        end if
        call git(git_dir, [character(len=256) :: 'rev-list', '--first-parent', '--max-count=200', &
            head], chain, status)
        if (status /= 0) return
        call split(chain, commits, n)
        ! commits(1) is head; the newest one that passed is the known good end.
        a = 0
        do i = 2, n
            if (result_of(db, repo, commits(i)) == 'passed') then
                a = i
                exit
            end if
        end do
        if (a == 0) then
            call report(db, repo, git_dir, 'unknown', '', base)
            return
        end if
        ! From oldest to newest between them: commits(a - 1) ... commits(1).
        hi = 0
        do i = a - 1, 1, -1
            if (result_of(db, repo, commits(i)) == 'failed') then
                hi = i
                exit
            end if
        end do
        lo = a
        do i = a - 1, hi + 1, -1
            if (result_of(db, repo, commits(i)) == 'passed') lo = i
        end do
        if (lo == hi + 1) then
            call report(db, repo, git_dir, trim(commits(hi)), trim(commits(lo)), base)
            return
        end if
        target = trim(commits((lo + hi + 1) / 2))
    end function next_target

    subroutine run(db, repo, git_dir, commit, request)
        !! Verifies `commit` on a runner, as job `request` or a new one.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, commit
        integer(int64), intent(in) :: request
        type(stmt_t) :: st
        integer(int64) :: id, check
        character(len=:), allocatable :: ref, state, log, out
        character(len=24) :: num
        integer :: stat, age, fd, status
        logical :: changed

        id = request
        if (id > 0) then
            call exec(db, "UPDATE landings SET state = 'running', started_at = "//NOW// &
                ', commit_id = ?2 WHERE id = ?1;', id, commit)
        else
            call db_prepare(db, "INSERT INTO landings (repo, branch, account, created_at, kind,"// &
                " commit_id, state, started_at) VALUES (?1, 'main', ?2, "//NOW//", 'verify', ?3,"// &
                " 'running', "//NOW//") RETURNING id;", st, stat)
            if (stat /= SQLITE_OK) return
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_text(st, 2, ACCOUNT, stat)
            call stmt_bind_text(st, 3, commit, stat)
            if (stmt_step(st) == SQLITE_ROW) id = stmt_int(st, 0)
            call stmt_finalize(st)
        end if
        if (id == 0) return
        call events_notify('jobs')
        write (num, '(i0)') id
        ref = 'refs/fortyard/verify/'//trim(num)
        call git(git_dir, [character(len=256) :: 'update-ref', ref, commit], out, status)
        fd = events_watch_open()
        call store_check_add(db, repo, ref, check, stat, kind='verify')
        call events_notify('checks')
        state = ''
        log = ''
        do
            call store_check_get(db, check, state, log, age)
            if (state == 'passed' .or. state == 'failed') exit
            if ((state == 'queued' .and. age > 21600) .or. (state == 'running' .and. age > 28800)) &
                then
                state = 'failed'
                log = 'No runner reported a result.'
                exit
            end if
            changed = events_wait(fd, 'checks', 60000)
        end do
        call git(git_dir, [character(len=256) :: 'update-ref', '-d', ref], out, status)
        call exec(db, 'UPDATE landings SET state = ?2, log = ?3, finished_at = '//NOW// &
            ' WHERE id = ?1;', id, state, log)
        call events_notify('jobs')
        write (error_unit, '(a)') 'fy: verification '//trim(num)//' of '//repo//' at '// &
            commit(1:min(12, len(commit)))//' '//state
        flush (error_unit)
    end subroutine run

    subroutine report(db, repo, git_dir, culprit, good, base)
        !! Reports the first failing commit once: an issue, and a comment on
        !! the change that landed it. `culprit` 'unknown': main fails and no
        !! earlier verification passed.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, culprit, good, base
        character(len=:), allocatable :: subject, title, body, log, link
        type(change_t) :: c
        type(stmt_t) :: st
        integer :: number, change, status, stat
        logical :: found

        if (reported(db, repo, culprit)) return
        if (culprit == 'unknown') then
            title = 'The verification of main fails'
            body = 'The verification of main (.fortyard/verify, or every test run again) fails, '// &
                'and no earlier verification of main passed, so no change can be blamed. See '// &
                'the recent verify jobs of '//repo//'.'
            change = 0
        else
            call git(git_dir, [character(len=256) :: 'log', '-1', '--format=%s', culprit], subject, &
                status)
            subject = trim_nl(subject)
            change = number_in(subject)
            log = log_of(db, repo, culprit)
            title = 'Verification fails since '//culprit(1:10)//' '//subject
            if (len(title) > 200) title = title(1:200)
            body = 'The verification of main passes at '//good(1:10)//' and fails at '// &
                culprit(1:10)//', the next commit on main:'//new_line('a')//new_line('a')// &
                '    '//subject//new_line('a')//new_line('a')
            if (change > 0) body = body//'It landed change #'//itoa(change)//'. '
            body = body//'The commits of main between the last passing and the first failing '// &
                'verification were bisected.'//new_line('a')//new_line('a')// &
                'End of the failing log:'//new_line('a')//new_line('a')//'```'//new_line('a')// &
                tail(log, LOG_TAIL)//new_line('a')//'```'
        end if
        number = issue_open(db, repo, git_dir, title, body, ACCOUNT)
        if (number == 0) return
        call db_prepare(db, 'INSERT INTO verify_reports (repo, culprit, issue, created_at)'// &
            ' VALUES (?1, ?2, ?3, '//NOW//');', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_text(st, 2, culprit, stat)
            call stmt_bind_int(st, 3, int(number, int64), stat)
            stat = stmt_step(st)
            call stmt_finalize(st)
        end if
        if (change > 0) then
            call change_get(db, repo, change, c, found)
            link = 'issue #'//itoa(number)
            if (len(base) > 0) link = link//' ('//base//'/'//repo//'/-/issues/'//itoa(number)//')'
            if (found) call change_comment(db, c%id, ACCOUNT, 'The verification of main fails '// &
                'since this change landed: '//link//'.')
        end if
        write (error_unit, '(a)') 'fy: verification of '//repo//' reported as issue #'//itoa(number)
        flush (error_unit)
    end subroutine report

    subroutine close_reports(db, repo, head)
        !! Main passes: the open regression reports are closed.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, head
        type(stmt_t) :: st
        integer, allocatable :: issues(:)
        integer :: stat, k

        allocate (issues(0))
        call db_prepare(db, "SELECT issue FROM verify_reports WHERE repo = ?1 AND state = 'open';", &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            issues = [issues, int(stmt_int(st, 0))]
        end do
        call stmt_finalize(st)
        do k = 1, size(issues)
            call issue_comment(db, repo, issues(k), ACCOUNT, 'The verification of main passes '// &
                'again at '//head(1:10)//'.')
            call issue_set_state(db, repo, issues(k), 'closed')
        end do
        ! A failure without a culprit may be reported again later.
        call exec(db, "DELETE FROM verify_reports WHERE repo = ?2 AND culprit = 'unknown'"// &
            ' AND ?1 = ?1;', 0_int64, repo)
        call exec(db, "UPDATE verify_reports SET state = 'closed' WHERE repo = ?2 AND"// &
            " state = 'open' AND ?1 = ?1;", 0_int64, repo)
    end subroutine close_reports

    logical function reported(db, repo, culprit)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, culprit
        type(stmt_t) :: st
        integer :: stat
        reported = .false.
        call db_prepare(db, 'SELECT 1 FROM verify_reports WHERE repo = ?1 AND culprit = ?2;', st, &
            stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, culprit, stat)
        reported = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end function reported

    function result_of(db, repo, commit) result(state)
        !! "passed" or "failed" from the latest verification of `commit`, or ''.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, commit
        character(len=:), allocatable :: state
        type(stmt_t) :: st
        integer :: stat
        state = ''
        call db_prepare(db, "SELECT state FROM landings WHERE kind = 'verify' AND repo = ?1"// &
            " AND commit_id = ?2 AND state IN ('passed', 'failed') ORDER BY id DESC LIMIT 1;", &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, commit, stat)
        if (stmt_step(st) == SQLITE_ROW) state = stmt_text(st, 0)
        call stmt_finalize(st)
    end function result_of

    function log_of(db, repo, commit) result(log)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, commit
        character(len=:), allocatable :: log
        type(stmt_t) :: st
        integer :: stat
        log = ''
        call db_prepare(db, "SELECT coalesce(log, '') FROM landings WHERE kind = 'verify' AND"// &
            " repo = ?1 AND commit_id = ?2 AND state = 'failed' ORDER BY id DESC LIMIT 1;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, commit, stat)
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
