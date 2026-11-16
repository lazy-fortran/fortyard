module fy_change
    !! Changes, fortyard's pull requests (doc/workflow.md, "Changes"). Pushing a
    !! branch other than main and those under wip/ opens a change or adds a version to it;
    !! a change is matched by the Change-Id trailer of its tip, else by its
    !! branch. Every version's head is kept under refs/fortyard/changes/N/V, so
    !! its diff stays readable after the branch is gone. Issues and changes
    !! share one number sequence per repository with the "(#N)" of main.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        json_extract_string
    use fy_audit, only: &
        audit_append
    use fy_events, only: &
        events_notify
    use fy_http, only: &
        http_query_param
    use fy_proc, only: &
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
    implicit none
    private

    public :: &
        change_api, &
        change_by_branch, &
        change_by_id, &
        change_comment, &
        change_get, &
        change_landed, &
        change_last_version, &
        change_list_json, &
        change_message, &
        change_next_number, &
        change_review_next, &
        change_review_set, &
        change_branch_tips, &
        change_sync, &
        change_t

    type :: change_t
        integer(int64) :: id = 0
        integer :: number = 0
        character(len=:), allocatable :: repo, key, branch, title, description, author, state, &
            created, updated, commit, review, flag_reason, reviewed_head
        !! The token that pushed the newest version; empty when not recorded.
        character(len=:), allocatable :: session
        integer :: versions = 0
        logical :: flagged = .false.
    end type change_t

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    integer, parameter :: OUT_BYTES = 1048576
    character(len=*), parameter :: COLUMNS = 'id, number, repo, change_key, branch, title,'// &
        " description, author, state, created_at, updated_at, coalesce(commit_id, ''),"// &
        ' (SELECT count(*) FROM change_versions v WHERE v.change = changes.id),'// &
        " coalesce(review_state, ''), flagged, coalesce(flag_reason, ''),"// &
        " coalesce(reviewed_head, ''), coalesce((SELECT v.session FROM change_versions v"// &
        " WHERE v.change = changes.id ORDER BY v.version DESC LIMIT 1), '')"
    !! The head of the newest version of a row of changes.
    character(len=*), parameter :: NEWEST = '(SELECT v.head FROM change_versions v'// &
        ' WHERE v.change = changes.id ORDER BY v.version DESC LIMIT 1)'
    !! The entries of a row of changes: their number, and the state and the
    !! job of the newest one.
    character(len=*), parameter :: ENTRY_COUNT = '(SELECT count(*) FROM landings l'// &
        " WHERE l.change = changes.id AND l.kind = 'landing')"
    character(len=*), parameter :: ENTRY_STATE = "coalesce((SELECT l.state FROM landings l"// &
        " WHERE l.change = changes.id AND l.kind = 'landing' ORDER BY l.id DESC LIMIT 1), '')"
    character(len=*), parameter :: ENTRY_ID = 'coalesce((SELECT max(l.id) FROM landings l'// &
        " WHERE l.change = changes.id AND l.kind = 'landing'), 0)"
    !! A review that read a version older than the newest one (doc/inspection.md,
    !! "States and words"); one with no head recorded is not known to be stale.
    character(len=*), parameter :: STALE_REVIEW = "(coalesce(reviewed_head, '') <> '' AND"// &
        " reviewed_head <> coalesce("//NEWEST//", ''))"

contains

    function change_branch_tips(git_dir) result(refs)
        !! The branches of `git_dir` with their tips, one "name id" a line.
        character(len=*), intent(in) :: git_dir
        character(len=:), allocatable :: refs
        integer :: status

        call git(git_dir, [character(len=256) :: 'for-each-ref', '--format=%(refname:short) %(objectname)', &
            'refs/heads'], refs, status)
        if (status /= 0) refs = ''
    end function change_branch_tips

    subroutine change_sync(db, repo, git_dir, account, session, before)
        !! After a push to `repo` by the token named `session`: opens a change
        !! for each new branch and adds a version to the change of each branch
        !! that moved. With `before`, the branch tips from change_branch_tips
        !! taken before the push, only the branches the push created or moved
        !! count; a repository that came from a mirror has many old branches
        !! nobody asked to review.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, account
        character(len=*), intent(in), optional :: session, before
        character(len=:), allocatable :: refs, line, branch, tip, key, main, base
        type(change_t) :: c
        character(len=:), allocatable :: pushed_by
        logical :: found, has_main
        integer :: at, eol, sp, status

        pushed_by = ''
        if (present(session)) pushed_by = session
        call git(git_dir, [character(len=256) :: 'for-each-ref', '--format=%(refname:short) %(objectname)', &
            'refs/heads'], refs, status)
        if (status /= 0) return
        call git(git_dir, [character(len=256) :: 'rev-parse', '--verify', '--quiet', 'refs/heads/main'], &
            main, status)
        has_main = status == 0
        main = trim_nl(main)
        at = 1
        do while (at <= len(refs))
            eol = index(refs(at:), new_line('a'))
            if (eol == 0) eol = len(refs) - at + 2
            line = refs(at:at + eol - 2)
            at = at + eol
            sp = index(line, ' ')
            if (sp < 2) cycle
            if (present(before)) then
                if (index(new_line('a')//before//new_line('a'), new_line('a')//line//new_line('a')) > 0) &
                    cycle
            end if
            branch = line(1:sp - 1)
            tip = line(sp + 1:)
            if (branch == 'main' .or. index(branch, 'wip/') == 1) cycle
            base = ''
            if (has_main) then
                ! A branch already contained in main has nothing to review.
                call git(git_dir, [character(len=256) :: 'merge-base', '--is-ancestor', tip, main], &
                    line, status)
                if (status == 0) cycle
                call git(git_dir, [character(len=256) :: 'merge-base', main, tip], base, status)
                base = trim_nl(base)
            end if
            call git(git_dir, [character(len=256) :: 'log', '-1', &
                '--format=%(trailers:key=Change-Id,valueonly,separator=)', tip], key, status)
            key = trim_nl(key)
            if (len(key) == 0) key = 'branch:'//branch
            call find_open(db, repo, key, branch, c, found)
            ! A branch left at a version of a change (landed, closed or another
            ! branch's) is no new work.
            if (.not. found .and. known_head(db, repo, tip)) cycle
            if (.not. found) then
                call open_change(db, repo, git_dir, key, branch, tip, base, account, c, found)
                if (.not. found) cycle
            else if (has_version(db, c%id, tip)) then
                ! This version is known, also when an older branch still has it.
                cycle
            else
                call exec_bind(db, 'UPDATE changes SET branch = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1;', c%id, branch)
                call redescribe(db, git_dir, c, tip, base)
            end if
            call add_version(db, repo, git_dir, c, tip, base, account, pushed_by)
        end do
    end subroutine change_sync

    subroutine describe(git_dir, tip, base, title, description, ok)
        !! The title and description of a version: those of the branch's one
        !! commit, or the first one's subject and the list of all of them; merge
        !! commits count only as a merge onto main.
        character(len=*), intent(in) :: git_dir, tip, base
        character(len=:), allocatable, intent(out) :: title, description
        logical, intent(out) :: ok
        character(len=:), allocatable :: range, subjects, body
        integer :: status, n

        ok = .false.
        title = ''
        description = ''
        range = tip
        if (len(base) > 0) range = base//'..'//tip
        ! A merge onto main (such as of an upstream) lands as itself and is
        ! described by its own message; main merged into a branch is not.
        call git(git_dir, [character(len=256) :: 'rev-parse', '--verify', '--quiet', tip//'^2'], &
            body, status)
        if (status == 0 .and. len(base) > 0) call git(git_dir, [character(len=256) :: &
            'merge-base', '--is-ancestor', tip//'^1', base], body, status)
        if (status == 0) then
            subjects = ''
            n = 0
        else
            call git(git_dir, [character(len=256) :: 'log', '--no-merges', '--reverse', &
                '--format=%s', range], subjects, status)
            if (status /= 0) return
            subjects = trim_nl(subjects)
            n = count_lines(subjects)
        end if
        if (n == 0) then
            call git(git_dir, [character(len=256) :: 'log', '-1', '--format=%s', tip], title, status)
            title = trim_nl(title)
            call git(git_dir, [character(len=256) :: 'log', '-1', '--format=%b', tip], body, status)
            description = strip_trailers(trim_nl(body))
        else if (n == 1) then
            title = first_line(subjects)
            call git(git_dir, [character(len=256) :: 'log', '--no-merges', '-1', '--format=%b', &
                range], body, status)
            description = strip_trailers(trim_nl(body))
        else
            title = first_line(subjects)
            description = 'Commits:'//new_line('a')//bullets(subjects)
        end if
        ok = .true.
    end subroutine describe

    subroutine redescribe(db, git_dir, c, tip, base)
        !! A new version of `c` takes the title and description of its own
        !! commits, except where a person edited them.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: git_dir, tip, base
        type(change_t), intent(in) :: c
        character(len=:), allocatable :: title, description
        type(stmt_t) :: st
        integer :: stat
        logical :: ok

        call describe(git_dir, tip, base, title, description, ok)
        if (.not. ok .or. len(title) == 0) return
        call db_prepare(db, 'UPDATE changes SET'// &
            ' title = CASE WHEN title_edited <> 0 THEN title ELSE ?2 END,'// &
            ' description = CASE WHEN description_edited <> 0 THEN description ELSE ?3 END'// &
            ' WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, c%id, stat)
        call stmt_bind_text(st, 2, title, stat)
        call stmt_bind_text(st, 3, description, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine redescribe

    subroutine open_change(db, repo, git_dir, key, branch, tip, base, account, c, ok)
        !! A new change, described by its commits (describe).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, key, branch, tip, base, account
        type(change_t), intent(out) :: c
        logical, intent(out) :: ok
        character(len=:), allocatable :: title, description
        type(stmt_t) :: st
        integer :: stat

        call describe(git_dir, tip, base, title, description, ok)
        if (.not. ok) return
        ok = .false.
        call db_prepare(db,'INSERT INTO changes (repo, number, change_key, branch, title,'// &
            ' description, author, created_at, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, '// &
            NOW//', '//NOW//') RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        ! Number and row in one write transaction: no other worker reads the
        ! same number in between.
        call db_exec(db, 'BEGIN IMMEDIATE;', stat)
        c%number = change_next_number(db, repo, git_dir)
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_int(st, 2, int(c%number, int64), stat)
        call stmt_bind_text(st, 3, key, stat)
        call stmt_bind_text(st, 4, branch, stat)
        call stmt_bind_text(st, 5, title, stat)
        call stmt_bind_text(st, 6, description, stat)
        call stmt_bind_text(st, 7, account, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            c%id = stmt_int(st, 0)
            ok = .true.
        end if
        call stmt_finalize(st)
        if (ok) then
            call db_exec(db, 'COMMIT;', stat)
        else
            call db_exec(db, 'ROLLBACK;', stat)
        end if
    end subroutine open_change

    subroutine add_version(db, repo, git_dir, c, tip, base, account, session)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, tip, base, account, session
        type(change_t), intent(in) :: c
        type(stmt_t) :: st
        character(len=:), allocatable :: out
        character(len=16) :: num, ver
        integer :: stat, version, status

        version = 0
        call db_prepare(db, 'SELECT coalesce(max(version), 0) FROM change_versions'// &
            ' WHERE change = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, c%id, stat)
        if (stmt_step(st) == SQLITE_ROW) version = int(stmt_int(st, 0)) + 1
        call stmt_finalize(st)
        call db_prepare(db, 'INSERT INTO change_versions (change, version, head, base, pusher,'// &
            ' created_at, session) VALUES (?1, ?2, ?3, ?4, ?5, '//NOW//", nullif(?6, ''));", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, c%id, stat)
        call stmt_bind_int(st, 2, int(version, int64), stat)
        call stmt_bind_text(st, 3, tip, stat)
        call stmt_bind_text(st, 4, base, stat)
        call stmt_bind_text(st, 5, account, stat)
        call stmt_bind_text(st, 6, session, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
        write (num, '(i0)') c%number
        write (ver, '(i0)') version
        ! Keeps the version's commits after its branch is gone.
        call git(git_dir, [character(len=256) :: 'update-ref', &
            'refs/fortyard/changes/'//trim(num)//'/'//trim(ver), tip], out, status)
        if (review_first(db, repo)) then
            ! A review holds for the version it read; the agent reviews this one.
            if (version > 1) call exec_bind(db, 'UPDATE changes SET review_state = NULL,'// &
                ' flagged = 0, flag_reason = NULL WHERE id = ?1 AND state = ?2;', c%id, 'open')
            call events_notify('jobs')
        end if
    end subroutine add_version

    logical function review_first(db, repo)
        !! The policy of `repo` asks for a review before landing (fy_policy).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(stmt_t) :: st
        integer :: stat

        review_first = .false.
        call db_prepare(db, "SELECT 1 FROM repos WHERE name = ?1 AND policy_review = 'before';", &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        review_first = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end function review_first

    integer function change_next_number(db, repo, git_dir) result(m)
        !! One more than the highest number of the repository's changes and
        !! issues and of the "(#N)" ending a subject on main. A repository with
        !! a set counter (`fy repo numbers`) takes that instead and advances it.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir
        type(stmt_t) :: st
        character(len=:), allocatable :: subjects
        integer :: stat, status, pos, e, s, v, ios

        m = 0
        call db_prepare(db, 'SELECT coalesce(number_next, 0) FROM repos WHERE name = ?1;', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, repo, stat)
            if (stmt_step(st) == SQLITE_ROW) m = int(stmt_int(st, 0))
            call stmt_finalize(st)
        end if
        if (m > 0) then
            do while (taken(m))
                m = m + 1
            end do
            call db_prepare(db, 'UPDATE repos SET number_next = ?2 WHERE name = ?1;', st, stat)
            if (stat == SQLITE_OK) then
                call stmt_bind_text(st, 1, repo, stat)
                call stmt_bind_int(st, 2, int(m + 1, int64), stat)
                stat = stmt_step(st)
                call stmt_finalize(st)
            end if
            return
        end if
        m = 1
        call db_prepare(db, 'SELECT max(coalesce((SELECT max(number) FROM changes WHERE repo = ?1),'// &
            ' 0), coalesce((SELECT max(number) FROM issues WHERE repo = ?1), 0));', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, repo, stat)
            if (stmt_step(st) == SQLITE_ROW) m = int(stmt_int(st, 0)) + 1
            call stmt_finalize(st)
        end if
        call git(git_dir, [character(len=256) :: 'log', '--format=%s', 'refs/heads/main'], &
            subjects, status)
        if (status /= 0) return
        pos = 1
        do while (pos <= len(subjects))
            e = index(subjects(pos:), new_line('a'))
            if (e == 0) e = len(subjects) - pos + 2
            e = pos + e - 2
            if (e - pos >= 3) then
                if (subjects(e:e) == ')') then
                    s = index(subjects(pos:e), '(#', back=.true.)
                    if (s > 0) then
                        s = pos + s + 1
                        if (e - 1 >= s .and. verify(subjects(s:e - 1), '0123456789') == 0) then
                            read (subjects(s:e - 1), *, iostat=ios) v
                            if (ios == 0) m = max(m, v + 1)
                        end if
                    end if
                end if
            end if
            pos = e + 2
        end do

    contains

        logical function taken(n)
            integer, intent(in) :: n

            taken = .false.
            call db_prepare(db, 'SELECT 1 FROM changes WHERE repo = ?1 AND number = ?2 UNION ALL'// &
                ' SELECT 1 FROM issues WHERE repo = ?1 AND number = ?2;', st, stat)
            if (stat /= SQLITE_OK) return
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_int(st, 2, int(n, int64), stat)
            taken = stmt_step(st) == SQLITE_ROW
            call stmt_finalize(st)
        end function taken
    end function change_next_number

    subroutine change_get(db, repo, number, c, found)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        integer, intent(in) :: number
        type(change_t), intent(out) :: c
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'SELECT '//COLUMNS//' FROM changes WHERE repo = ?1 AND number = ?2;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_int(st, 2, int(number, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            call read_row(st, c)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine change_get

    subroutine change_by_id(db, id, c, found)
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        type(change_t), intent(out) :: c
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'SELECT '//COLUMNS//' FROM changes WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            call read_row(st, c)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine change_by_id

    subroutine change_by_branch(db, repo, branch, c, found)
        !! The open change of `branch`.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, branch
        type(change_t), intent(out) :: c
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'SELECT '//COLUMNS//" FROM changes WHERE repo = ?1 AND branch = ?2"// &
            " AND state = 'open' ORDER BY id DESC LIMIT 1;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, branch, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            call read_row(st, c)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine change_by_branch

    subroutine change_landed(db, id, commit)
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: commit
        call exec_bind(db, "UPDATE changes SET state = 'landed', commit_id = ?2, updated_at = "// &
            NOW//' WHERE id = ?1;', id, commit)
    end subroutine change_landed

    subroutine change_review_set(db, id, state, flagged, reason, head)
        !! Records a review of the change `id`: its state, and whether and why
        !! it is flagged for a person. The review holds for the version with
        !! the head `head`, which is the one it read; without `head` (or with
        !! an empty one) for the newest version as of this statement.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: state, reason
        logical, intent(in) :: flagged
        character(len=*), intent(in), optional :: head
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, 'UPDATE changes SET review_state = ?2, flagged = ?3, flag_reason = ?4,'// &
            " reviewed_head = coalesce(nullif(?5, ''), "//NEWEST//'), updated_at = '//NOW// &
            ' WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_int(st, 3, int(merge(1, 0, flagged), int64), stat)
        call stmt_bind_text(st, 4, reason, stat)
        if (present(head)) then
            call stmt_bind_text(st, 5, head, stat)
        else
            call stmt_bind_text(st, 5, '', stat)
        end if
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine change_review_set

    subroutine change_review_next(db, c, found)
        !! The oldest change waiting for the review agent: one whose review was
        !! requested, a landed one not reviewed yet, or, in a repository whose
        !! policy asks for a review before landing, an open one that is not
        !! reviewed yet or whose review by the agent read an older version (one
        !! pushed while the agent was reading).
        type(db_t), intent(in) :: db
        type(change_t), intent(out) :: c
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'SELECT '//COLUMNS//" FROM changes WHERE review_state = 'requested'"// &
            " OR (state = 'landed' AND review_state IS NULL)"// &
            " OR (state = 'open' AND (review_state IS NULL OR (review_state IN ('ai-reviewed',"// &
            " 'failed') AND coalesce(reviewed_head, '') <> coalesce("//NEWEST//", '')))"// &
            " AND repo IN (SELECT name FROM repos WHERE policy_review = 'before'))"// &
            ' ORDER BY id LIMIT 1;', st, stat)
        if (stat /= SQLITE_OK) return
        if (stmt_step(st) == SQLITE_ROW) then
            call read_row(st, c)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine change_review_next

    subroutine change_last_version(db, id, head, base)
        !! Head and base commit of the newest version of the change `id`.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: head, base
        type(stmt_t) :: st
        integer :: stat

        head = ''
        base = ''
        call db_prepare(db, 'SELECT head, base FROM change_versions WHERE change = ?1'// &
            ' ORDER BY version DESC LIMIT 1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            head = stmt_text(st, 0)
            base = stmt_text(st, 1)
        end if
        call stmt_finalize(st)
    end subroutine change_last_version

    function change_message(c, url) result(message)
        !! The landing commit's message: title (#N), description and trailers.
        type(change_t), intent(in) :: c
        character(len=*), intent(in) :: url
        character(len=:), allocatable :: message
        character(len=16) :: num

        write (num, '(i0)') c%number
        message = c%title//' (#'//trim(num)//')'
        if (len(c%description) > 0) message = message//new_line('a')//new_line('a')//c%description
        message = message//new_line('a')
        if (len(url) > 0) message = message//new_line('a')//'Change: '//url
        if (index(c%key, 'branch:') /= 1) message = message//new_line('a')//'Change-Id: '//c%key
    end function change_message

    subroutine change_api(db, method, repo, git_dir, rest, query, body, account, admin, code, &
            answer, ctype, land_branch, land)
        !! /api/v1/repos/REPO/changes[/N[/diff|/comments|/land]]; `rest` is the
        !! part after "changes". `land_branch` is set to the change's branch
        !! when the caller is to queue its landing.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: method, repo, git_dir, rest, query, body, account
        logical, intent(in) :: admin
        !! Whether the caller may queue landings; as `admin` when absent.
        logical, intent(in), optional :: land
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: answer, ctype, land_branch
        type(change_t) :: c
        character(len=:), allocatable :: sub, value
        integer :: number, slash, ios
        logical :: found, may_land

        may_land = admin
        if (present(land)) may_land = land
        code = 200
        ctype = 'application/json'
        land_branch = ''
        if (len(rest) == 0) then
            if (method /= 'GET') then
                call refuse(405, 'GET only')
                return
            end if
            answer = change_list_json(db, repo, query, code)
            return
        end if
        if (rest(1:1) /= '/') then
            call refuse(404, 'not found')
            return
        end if
        slash = index(rest(2:), '/')
        if (slash == 0) then
            value = rest(2:)
            sub = ''
        else
            value = rest(2:slash)
            sub = rest(slash + 2:)
        end if
        number = 0
        if (len(value) > 0 .and. len(value) < 9 .and. verify(value, '0123456789') == 0) &
            read (value, *, iostat=ios) number
        call change_get(db, repo, number, c, found)
        if (.not. found) then
            call refuse(404, 'no such change')
            return
        end if
        select case (sub)
        case ('')
            if (method == 'GET') then
                answer = change_json(db, c)
            else if (method == 'POST') then
                call edit()
            else
                call refuse(405, 'GET or POST')
            end if
        case ('diff')
            call diff()
        case ('comments')
            if (method /= 'POST') then
                call refuse(405, 'POST')
                return
            end if
            call json_extract_string(body, 'body', value, found)
            if (.not. found .or. len(value) == 0) then
                call refuse(400, 'body must be {"body":"..."}')
                return
            end if
            call comment_add(db, c%id, account, value)
            code = 201
            answer = '{"change":'//itoa(c%number)//'}'
        case ('review', 'reviewed')
            ! review: asks the review agent for a (new) review; reviewed: a
            ! person has reviewed the change, which also clears its flag.
            if (method /= 'POST') then
                call refuse(405, 'POST')
                return
            end if
            if (.not. admin) then
                call refuse(403, 'review states need the admin scope')
                return
            end if
            if (sub == 'review') then
                call change_review_set(db, c%id, 'requested', c%flagged, c%flag_reason)
                call events_notify('jobs')
            else
                call change_review_set(db, c%id, 'human-reviewed', .false., '')
                call comment_add(db, c%id, account, 'Reviewed.')
            end if
            answer = '{"change":'//itoa(c%number)//'}'
        case ('land')
            if (method /= 'POST') then
                call refuse(405, 'POST')
                return
            end if
            if (.not. may_land) then
                call refuse(403, 'landing needs the land or admin scope')
                return
            end if
            if (c%state /= 'open') then
                call refuse(409, 'the change is '//c%state)
                return
            end if
            land_branch = c%branch
            code = 202
            answer = ''
        case default
            call refuse(404, 'not found')
        end select

    contains

        subroutine refuse(status, message)
            integer, intent(in) :: status
            character(len=*), intent(in) :: message
            code = status
            answer = '{"error":"'//json_escape_string(message)//'"}'
        end subroutine refuse

        subroutine edit()
            !! {"title","description","state":"open"|"closed"}, each optional.
            character(len=:), allocatable :: s
            logical :: ok

            if (account /= c%author .and. .not. admin) then
                call refuse(403, 'only the author or an administrator edits a change')
                return
            end if
            call json_extract_string(body, 'title', s, ok)
            if (ok) then
                if (len(s) == 0 .or. index(s, new_line('a')) > 0) then
                    call refuse(400, 'a title is one non-empty line')
                    return
                end if
                ! An edited title is no longer taken from the commits.
                call exec_bind(db, 'UPDATE changes SET title = ?2, title_edited = 1,'// &
                    ' updated_at = '//NOW//' WHERE id = ?1;', c%id, s)
            end if
            call json_extract_string(body, 'description', s, ok)
            if (ok) call exec_bind(db, 'UPDATE changes SET description = ?2,'// &
                ' description_edited = 1, updated_at = '//NOW//' WHERE id = ?1;', c%id, s)
            call json_extract_string(body, 'state', s, ok)
            if (ok) then
                if ((s /= 'open' .and. s /= 'closed') .or. c%state == 'landed') then
                    call refuse(400, 'state is open or closed, and a landed change stays landed')
                    return
                end if
                call exec_bind(db, 'UPDATE changes SET state = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1;', c%id, s)
                if (s == 'closed' .and. c%state /= s) then
                    call audit_append(db, account, 'change.closed', repo//'#'//itoa(c%number), &
                        c%title)
                else if (c%state /= s) then
                    call audit_append(db, account, 'change.reopened', &
                        repo//'#'//itoa(c%number), c%title)
                end if
            end if
            ! Not c%number: the change is reset on entry.
            call change_get(db, repo, number, c, ok)
            answer = change_json(db, c)
        end subroutine edit

        subroutine diff()
            !! The version's diff against its base on main, or with ?from=W the
            !! difference between versions W and V (the interdiff).
            character(len=:), allocatable :: head, base, from, out
            integer :: v, w, status

            v = c%versions
            value = query_value(query, 'version')
            if (len(value) > 0) read (value, *, iostat=ios) v
            call version_refs(db, c%id, v, head, base)
            if (len(head) == 0) then
                call refuse(404, 'no such version')
                return
            end if
            value = query_value(query, 'from')
            if (len(value) > 0) then
                read (value, *, iostat=ios) w
                call version_refs(db, c%id, w, from, base)
                if (ios /= 0 .or. len(from) == 0) then
                    call refuse(404, 'no such version')
                    return
                end if
                call git(git_dir, [character(len=256) :: 'diff', from, head], out, status)
            else if (len(base) > 0) then
                call git(git_dir, [character(len=256) :: 'diff', base, head], out, status)
            else
                call git(git_dir, [character(len=256) :: 'show', '--format=', head], out, status)
            end if
            ctype = 'text/plain; charset=utf-8'
            answer = out
        end subroutine diff

    end subroutine change_api

    function change_list_json(db, repo, query, code) result(json)
        !! The changes of `repo`, or of every repository when it is empty,
        !! newest activity first, as the query asks (doc/inspection.md,
        !! "Changes"): state (open without it, landed, closed, all), review
        !! (flagged, unreviewed), attention (needs-author), q (text in the
        !! title, branch, author or session), limit (100, up to 200) and before (the
        !! "next" of the page before; empty after the oldest change).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, query
        integer, intent(out) :: code
        character(len=:), allocatable :: json
        type(stmt_t) :: st
        type(change_t) :: c
        character(len=:), allocatable :: sql, state, review, attention, q, before, text, at, &
            newest, next
        integer(int64) :: before_id, entry
        integer :: stat, n, limit, ios, entries
        logical :: stale

        code = 200
        state = http_query_param(query, 'state')
        review = http_query_param(query, 'review')
        attention = http_query_param(query, 'attention')
        q = http_query_param(query, 'q')
        before = http_query_param(query, 'before')
        if (len(state) == 0) state = 'open'
        limit = 100
        ios = 0
        text = http_query_param(query, 'limit')
        if (len(text) > 0) then
            ios = 1
            if (len(text) < 9 .and. verify(text, '0123456789') == 0) read (text, *, iostat=ios) limit
            if (limit < 1 .or. limit > 200) ios = 1
        end if
        before_id = 0
        at = ''
        if (len(before) > 0 .and. ios == 0) then
            ! A key is the 14 digits of a time of activity and the row's id.
            ios = 1
            if (len(before) > 15 .and. len(before) < 34) then
                if (verify(before(1:14), '0123456789') == 0 .and. before(15:15) == '-' .and. &
                    verify(before(16:), '0123456789') == 0) read (before(16:), *, iostat=ios) before_id
            end if
            if (ios == 0) at = before(1:4)//'-'//before(5:6)//'-'//before(7:8)//'T'// &
                before(9:10)//':'//before(11:12)//':'//before(13:14)//'Z'
        end if
        if (ios /= 0) then
            code = 400
            json = '{"error":"limit is 1 to 200 and before is the next of an earlier answer"}'
            return
        end if

        sql = 'SELECT '//COLUMNS//', '//ENTRY_COUNT//', '//ENTRY_STATE//', '//ENTRY_ID//', '//STALE_REVIEW// &
            " FROM changes WHERE (?1 = '' OR repo = ?1)"
        select case (state)
        case ('all')
        case ('flagged')
            ! The spelling before `review=flagged`, kept for its callers.
            sql = sql//' AND flagged <> 0'
        case default
            sql = sql//' AND state = ?2'
        end select
        select case (review)
        case ('')
        case ('flagged')
            sql = sql//' AND flagged <> 0'
        case ('unreviewed')
            ! A stale review left the newest version unreviewed.
            sql = sql//" AND (coalesce(review_state, '') NOT IN ('ai-reviewed', 'human-reviewed')"// &
                ' OR '//STALE_REVIEW//')'
        case default
            sql = sql//' AND 0'
        end select
        select case (attention)
        case ('')
        case ('needs-author')
            sql = sql//" AND state = 'open' AND "//ENTRY_STATE//" = 'failed'"
        case default
            ! Also "reverted": no revert is recorded yet.
            sql = sql//' AND 0'
        end select
        if (len(q) > 0) sql = sql//' AND (instr(lower(title), lower(?3)) > 0 OR'// &
            ' instr(lower(branch), lower(?3)) > 0 OR instr(lower(author), lower(?3)) > 0 OR'// &
            ' EXISTS (SELECT 1 FROM change_versions v WHERE v.change = changes.id'// &
            ' AND instr(lower(v.session), lower(?3)) > 0))'
        if (len(at) > 0) sql = sql//' AND (updated_at < ?4 OR (updated_at = ?4 AND id < ?5))'
        sql = sql//' ORDER BY updated_at DESC, id DESC LIMIT ?6;'

        json = '{"changes":['
        next = ''
        call db_prepare(db, sql, st, stat)
        if (stat /= SQLITE_OK) then
            json = json//'],"next":""}'
            return
        end if
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_text(st, 3, q, stat)
        call stmt_bind_text(st, 4, at, stat)
        call stmt_bind_int(st, 5, before_id, stat)
        ! One more than asked for tells whether older ones follow.
        call stmt_bind_int(st, 6, int(limit + 1, int64), stat)
        n = 0
        do while (stmt_step(st) == SQLITE_ROW)
            if (n == limit) then
                next = list_key(c)
                exit
            end if
            call read_row(st, c)
            entries = int(stmt_int(st, 18))
            newest = stmt_text(st, 19)
            entry = stmt_int(st, 20)
            stale = stmt_int(st, 21) /= 0 .and. (c%review == 'ai-reviewed' .or. &
                c%review == 'human-reviewed')
            if (n > 0) json = json//','
            text = summary_json(c)
            json = json//text(1:len(text) - 1)//',"repo":"'//json_escape_string(c%repo)// &
                '","status":"'//status_word(c%state, newest)//'","entries":'//itoa(entries)// &
                ',"entry":'
            if (entry > 0) then
                json = json//itoa(int(entry))
            else
                json = json//'null'
            end if
            ! Why an entry failed is not recorded yet.
            json = json//',"reason":null,"review_stale":'//trim(merge('true ', 'false', stale))//'}'
            n = n + 1
        end do
        call stmt_finalize(st)
        json = json//'],"next":"'//next//'"}'
    end function change_list_json

    function status_word(state, entry_state) result(word)
        !! What a change is shown as (doc/inspection.md, "States and words"):
        !! an open one by its newest entry, `draft` without a live one.
        character(len=*), intent(in) :: state, entry_state
        character(len=:), allocatable :: word

        word = state
        if (state /= 'open') return
        select case (entry_state)
        case ('queued', 'running')
            word = entry_state
        case ('failed')
            word = 'needs author'
        case default
            word = 'draft'
        end select
    end function status_word

    function list_key(c) result(key)
        !! The place of `c` in the list by activity, for `before`.
        type(change_t), intent(in) :: c
        character(len=:), allocatable :: key
        character(len=24) :: num
        integer :: k

        key = ''
        do k = 1, len(c%updated)
            if (index('0123456789', c%updated(k:k)) > 0) key = key//c%updated(k:k)
        end do
        write (num, '(i0)') c%id
        key = key//'-'//trim(num)
    end function list_key

    function summary_json(c) result(json)
        type(change_t), intent(in) :: c
        character(len=:), allocatable :: json
        json = '{"number":'//itoa(c%number)//',"title":"'//json_escape_string(c%title)// &
            '","state":"'//c%state//'","branch":"'//json_escape_string(c%branch)// &
            '","author":"'//json_escape_string(c%author)//'","versions":'//itoa(c%versions)// &
            ',"created":"'//c%created//'","updated":"'//c%updated//'","commit":"'//c%commit// &
            '","session":'//text_or_null(c%session)
        if (index(c%key, 'branch:') /= 1) json = json//',"change_id":"'// &
            json_escape_string(c%key)//'"'
        json = json//',"review":"'//c%review//'","flagged":'//merge('true ', 'false', c%flagged)
        json = trim(json)//',"flag_reason":"'//json_escape_string(c%flag_reason)//'"}'
    end function summary_json

    function change_json(db, c) result(json)
        !! The change with its versions, comments and landings.
        type(db_t), intent(in) :: db
        type(change_t), intent(in) :: c
        character(len=:), allocatable :: json
        type(stmt_t) :: st
        integer :: stat, n

        json = summary_json(c)
        json = json(1:len(json) - 1)//',"description":"'//json_escape_string(c%description)//'"'
        ! Whether a person edited them; otherwise each version recomputes them.
        call db_prepare(db, 'SELECT title_edited, description_edited FROM changes WHERE id = ?1;', &
            st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, c%id, stat)
            if (stmt_step(st) == SQLITE_ROW) json = json//',"title_edited":'// &
                trim(merge('true ', 'false', stmt_int(st, 0) /= 0))//',"description_edited":'// &
                trim(merge('true ', 'false', stmt_int(st, 1) /= 0))
            call stmt_finalize(st)
        end if
        json = json//',"versions_list":['
        call db_prepare(db, 'SELECT version, head, base, coalesce(pusher, ''''), created_at,'// &
            ' coalesce(session, '''') FROM change_versions WHERE change = ?1 ORDER BY version;', st, stat)
        n = 0
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, c%id, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                if (n > 0) json = json//','
                json = json//'{"version":'//itoa(int(stmt_int(st, 0)))//',"head":"'// &
                    stmt_text(st, 1)//'","base":"'//stmt_text(st, 2)//'","pusher":"'// &
                    json_escape_string(stmt_text(st, 3))//'","created":"'//stmt_text(st, 4)// &
                    '","session":'//text_or_null(stmt_text(st, 5))//'}'
                n = n + 1
            end do
            call stmt_finalize(st)
        end if
        json = json//'],"comments":['
        call db_prepare(db, 'SELECT account, body, created_at FROM change_comments'// &
            ' WHERE change = ?1 ORDER BY id;', st, stat)
        n = 0
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, c%id, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                if (n > 0) json = json//','
                json = json//'{"account":"'//json_escape_string(stmt_text(st, 0))//'","body":"'// &
                    json_escape_string(stmt_text(st, 1))//'","created":"'//stmt_text(st, 2)//'"}'
                n = n + 1
            end do
            call stmt_finalize(st)
        end if
        json = json//'],"landings":['
        call db_prepare(db, "SELECT id, state, coalesce(commit_id, ''), coalesce(session, '')"// &
            ' FROM landings WHERE change = ?1 ORDER BY id;', st, stat)
        n = 0
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, c%id, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                if (n > 0) json = json//','
                json = json//'{"landing":'//itoa(int(stmt_int(st, 0)))//',"state":"'// &
                    stmt_text(st, 1)//'","commit":"'//stmt_text(st, 2)//'","session":'// &
                    text_or_null(stmt_text(st, 3))//'}'
                n = n + 1
            end do
            call stmt_finalize(st)
        end if
        json = json//']}'
    end function change_json

    subroutine change_comment(db, change, account, text)
        !! A comment on a change (by its id), as from a person or fortyard.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: change
        character(len=*), intent(in) :: account, text
        call comment_add(db, change, account, text)
    end subroutine change_comment

    subroutine comment_add(db, change, account, text)
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: change
        character(len=*), intent(in) :: account, text
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, 'INSERT INTO change_comments (change, account, body, created_at)'// &
            ' VALUES (?1, ?2, ?3, '//NOW//');', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, change, stat)
        call stmt_bind_text(st, 2, account, stat)
        call stmt_bind_text(st, 3, text, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine comment_add

    subroutine version_refs(db, change, version, head, base)
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: change
        integer, intent(in) :: version
        character(len=:), allocatable, intent(out) :: head, base
        type(stmt_t) :: st
        integer :: stat

        head = ''
        base = ''
        call db_prepare(db, 'SELECT head, base FROM change_versions WHERE change = ?1'// &
            ' AND version = ?2;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, change, stat)
        call stmt_bind_int(st, 2, int(version, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            head = stmt_text(st, 0)
            base = stmt_text(st, 1)
        end if
        call stmt_finalize(st)
    end subroutine version_refs

    subroutine find_open(db, repo, key, branch, c, found)
        !! The open change with this key, else the open one of this branch.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, key, branch
        type(change_t), intent(out) :: c
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'SELECT '//COLUMNS//" FROM changes WHERE repo = ?1 AND state = 'open'"// &
            ' AND (change_key = ?2 OR branch = ?3) ORDER BY change_key = ?2 DESC, id DESC'// &
            ' LIMIT 1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, key, stat)
        call stmt_bind_text(st, 3, branch, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            call read_row(st, c)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine find_open

    logical function known_head(db, repo, head)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, head
        type(stmt_t) :: st
        integer :: stat

        known_head = .false.
        call db_prepare(db, 'SELECT 1 FROM change_versions v JOIN changes c ON c.id = v.change'// &
            ' WHERE c.repo = ?1 AND v.head = ?2 LIMIT 1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, head, stat)
        known_head = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end function known_head

    logical function has_version(db, change, head)
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: change
        character(len=*), intent(in) :: head
        type(stmt_t) :: st
        integer :: stat

        has_version = .false.
        call db_prepare(db, 'SELECT 1 FROM change_versions WHERE change = ?1 AND head = ?2;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, change, stat)
        call stmt_bind_text(st, 2, head, stat)
        has_version = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end function has_version

    subroutine read_row(st, c)
        type(stmt_t), intent(in) :: st
        type(change_t), intent(out) :: c
        c%id = stmt_int(st, 0)
        c%number = int(stmt_int(st, 1))
        c%repo = stmt_text(st, 2)
        c%key = stmt_text(st, 3)
        c%branch = stmt_text(st, 4)
        c%title = stmt_text(st, 5)
        c%description = stmt_text(st, 6)
        c%author = stmt_text(st, 7)
        c%state = stmt_text(st, 8)
        c%created = stmt_text(st, 9)
        c%updated = stmt_text(st, 10)
        c%commit = stmt_text(st, 11)
        c%versions = int(stmt_int(st, 12))
        c%review = stmt_text(st, 13)
        c%flagged = stmt_int(st, 14) /= 0
        c%flag_reason = stmt_text(st, 15)
        c%reviewed_head = stmt_text(st, 16)
        c%session = stmt_text(st, 17)
    end subroutine read_row

    function text_or_null(s) result(json)
        !! A JSON string, or null for a member that was not recorded.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: json
        if (len(s) == 0) then
            json = 'null'
        else
            json = '"'//json_escape_string(s)//'"'
        end if
    end function text_or_null

    subroutine exec_bind(db, sql, id, text)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: sql, text
        integer(int64), intent(in) :: id
        type(stmt_t) :: st
        integer :: stat
        call db_prepare(db, sql, st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, text, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine exec_bind

    subroutine git(git_dir, args, out, status)
        character(len=*), intent(in) :: git_dir
        character(len=*), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: status
        call proc_run([character(len=1024) :: 'git', '--git-dir', git_dir, args], out, status, &
            OUT_BYTES)
    end subroutine git

    function query_value(query, name) result(value)
        character(len=*), intent(in) :: query, name
        character(len=:), allocatable :: value
        integer :: at, e
        value = ''
        at = index('&'//query, '&'//name//'=')
        if (at == 0) return
        value = query(at + len(name) + 1:)
        e = index(value, '&')
        if (e > 0) value = value(1:e - 1)
    end function query_value

    function strip_trailers(body) result(out)
        !! The body without its trailing Change-Id line.
        character(len=*), intent(in) :: body
        character(len=:), allocatable :: out
        integer :: k
        out = body
        k = index(out, 'Change-Id: ', back=.true.)
        if (k == 1) then
            out = ''
        else if (k > 1) then
            if (out(k - 1:k - 1) == new_line('a') .and. index(out(k:), new_line('a')) == 0) &
                out = trim_nl(out(1:k - 1))
        end if
    end function strip_trailers

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

    function first_line(s) result(l)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: l
        integer :: k
        k = index(s, new_line('a'))
        if (k == 0) then
            l = s
        else
            l = s(1:k - 1)
        end if
    end function first_line

    integer function count_lines(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k
        n = 0
        if (len(s) == 0) return
        n = 1
        do k = 1, len(s)
            if (s(k:k) == new_line('a')) n = n + 1
        end do
    end function count_lines

    function bullets(s) result(b)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: b
        integer :: pos, k
        b = ''
        pos = 1
        do while (pos <= len(s))
            k = index(s(pos:), new_line('a'))
            if (k == 0) k = len(s) - pos + 2
            if (k > 1) b = b//'- '//s(pos:pos + k - 2)//new_line('a')
            pos = pos + k
        end do
        b = trim_nl(b)
    end function bullets

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_change
