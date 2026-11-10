module fy_change
    !! Changes, fortyard's pull requests (doc/workflow.md, "Changes"). Pushing a
    !! branch other than main and those under wip/ opens a change or adds a version to it;
    !! a change is matched by the Change-Id trailer of its tip, else by its
    !! branch. Every version's head is kept under refs/fortyard/changes/N/V, so
    !! its diff stays readable after the branch is gone. Issues and changes
    !! share one number sequence per repository with the "(#N)" of main.
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string
    use fy_proc, only: proc_run
    use fy_sqlite, only: db_t, stmt_t, db_prepare, stmt_bind_text, stmt_bind_int, stmt_step, &
        stmt_text, stmt_int, stmt_finalize, SQLITE_OK, SQLITE_ROW
    implicit none
    private

    public :: change_t, change_sync, change_get, change_by_id, change_by_branch, change_landed, &
        change_next_number, change_api, change_message

    type :: change_t
        integer(int64) :: id = 0
        integer :: number = 0
        character(len=:), allocatable :: repo, key, branch, title, description, author, state, &
            created, updated, commit
        integer :: versions = 0
    end type change_t

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    integer, parameter :: OUT_BYTES = 1048576
    character(len=*), parameter :: COLUMNS = 'id, number, repo, change_key, branch, title,'// &
        " description, author, state, created_at, updated_at, coalesce(commit_id, ''),"// &
        ' (SELECT count(*) FROM change_versions v WHERE v.change = changes.id)'

contains

    subroutine change_sync(db, repo, git_dir, account)
        !! After a push to `repo`: opens a change for each new branch and adds a
        !! version to the change of each branch that moved.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, account
        character(len=:), allocatable :: refs, line, branch, tip, key, main, base
        type(change_t) :: c
        logical :: found, has_main
        integer :: at, eol, sp, status

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
            end if
            call add_version(db, git_dir, c, tip, base, account)
        end do
    end subroutine change_sync

    subroutine open_change(db, repo, git_dir, key, branch, tip, base, account, c, ok)
        !! A new change: its title and description come from the branch's one
        !! commit, or from the first one and the list of all of them.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, key, branch, tip, base, account
        type(change_t), intent(out) :: c
        logical, intent(out) :: ok
        character(len=:), allocatable :: range, subjects, title, description, body
        type(stmt_t) :: st
        integer :: status, stat, n

        ok = .false.
        range = tip
        if (len(base) > 0) range = base//'..'//tip
        call git(git_dir, [character(len=256) :: 'log', '--reverse', '--format=%s', range], &
            subjects, status)
        if (status /= 0) return
        subjects = trim_nl(subjects)
        n = count_lines(subjects)
        title = first_line(subjects)
        ! A merge (such as of an upstream) is described by its own message.
        call git(git_dir, [character(len=256) :: 'rev-parse', '--verify', '--quiet', tip//'^2'], &
            body, status)
        if (status == 0) then
            call git(git_dir, [character(len=256) :: 'log', '-1', '--format=%s', tip], title, status)
            title = trim_nl(title)
            n = 1
        end if
        if (n <= 1) then
            call git(git_dir, [character(len=256) :: 'log', '-1', '--format=%b', tip], body, status)
            description = strip_trailers(trim_nl(body))
        else
            description = 'Commits:'//new_line('a')//bullets(subjects)
        end if
        call db_prepare(db, 'INSERT INTO changes (repo, number, change_key, branch, title,'// &
            ' description, author, created_at, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, '// &
            NOW//', '//NOW//') RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
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
    end subroutine open_change

    subroutine add_version(db, git_dir, c, tip, base, account)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: git_dir, tip, base, account
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
            ' created_at) VALUES (?1, ?2, ?3, ?4, ?5, '//NOW//');', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, c%id, stat)
        call stmt_bind_int(st, 2, int(version, int64), stat)
        call stmt_bind_text(st, 3, tip, stat)
        call stmt_bind_text(st, 4, base, stat)
        call stmt_bind_text(st, 5, account, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
        write (num, '(i0)') c%number
        write (ver, '(i0)') version
        ! Keeps the version's commits after its branch is gone.
        call git(git_dir, [character(len=256) :: 'update-ref', &
            'refs/fortyard/changes/'//trim(num)//'/'//trim(ver), tip], out, status)
    end subroutine add_version

    integer function change_next_number(db, repo, git_dir) result(m)
        !! One more than the highest number of the repository's changes and
        !! issues and of the "(#N)" ending a subject on main.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir
        type(stmt_t) :: st
        character(len=:), allocatable :: subjects
        integer :: stat, status, pos, e, s, v, ios

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
            answer, ctype, land_branch)
        !! /api/v1/repos/REPO/changes[/N[/diff|/comments|/land]]; `rest` is the
        !! part after "changes". `land_branch` is set to the change's branch
        !! when the caller is to queue its landing.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: method, repo, git_dir, rest, query, body, account
        logical, intent(in) :: admin
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: answer, ctype, land_branch
        type(change_t) :: c
        character(len=:), allocatable :: sub, value
        integer :: number, slash, ios
        logical :: found

        code = 200
        ctype = 'application/json'
        land_branch = ''
        if (len(rest) == 0) then
            if (method /= 'GET') then
                call refuse(405, 'GET only')
                return
            end if
            answer = list_json(db, repo, query_value(query, 'state'))
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
        case ('land')
            if (method /= 'POST') then
                call refuse(405, 'POST')
                return
            end if
            if (.not. admin) then
                call refuse(403, 'landing needs the admin scope')
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
                call exec_bind(db, 'UPDATE changes SET title = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1;', c%id, s)
            end if
            call json_extract_string(body, 'description', s, ok)
            if (ok) call exec_bind(db, 'UPDATE changes SET description = ?2, updated_at = '// &
                NOW//' WHERE id = ?1;', c%id, s)
            call json_extract_string(body, 'state', s, ok)
            if (ok) then
                if ((s /= 'open' .and. s /= 'closed') .or. c%state == 'landed') then
                    call refuse(400, 'state is open or closed, and a landed change stays landed')
                    return
                end if
                call exec_bind(db, 'UPDATE changes SET state = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1;', c%id, s)
            end if
            call change_get(db, repo, c%number, c, ok)
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

    function list_json(db, repo, state) result(json)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, state
        character(len=:), allocatable :: json
        type(stmt_t) :: st
        type(change_t) :: c
        integer :: stat, n

        json = '{"changes":['
        if (state == 'all') then
            call db_prepare(db, 'SELECT '//COLUMNS//' FROM changes WHERE repo = ?1 AND ?2 = ?2'// &
                ' ORDER BY number DESC LIMIT 100;', st, stat)
        else
            call db_prepare(db, 'SELECT '//COLUMNS//' FROM changes WHERE repo = ?1 AND state = ?2'// &
                ' ORDER BY number DESC LIMIT 100;', st, stat)
        end if
        if (stat /= SQLITE_OK) then
            json = json//']}'
            return
        end if
        call stmt_bind_text(st, 1, repo, stat)
        if (len(state) == 0) then
            call stmt_bind_text(st, 2, 'open', stat)
        else
            call stmt_bind_text(st, 2, state, stat)
        end if
        n = 0
        do while (stmt_step(st) == SQLITE_ROW)
            call read_row(st, c)
            if (n > 0) json = json//','
            json = json//summary_json(c)
            n = n + 1
        end do
        call stmt_finalize(st)
        json = json//']}'
    end function list_json

    function summary_json(c) result(json)
        type(change_t), intent(in) :: c
        character(len=:), allocatable :: json
        json = '{"number":'//itoa(c%number)//',"title":"'//json_escape_string(c%title)// &
            '","state":"'//c%state//'","branch":"'//json_escape_string(c%branch)// &
            '","author":"'//json_escape_string(c%author)//'","versions":'//itoa(c%versions)// &
            ',"created":"'//c%created//'","updated":"'//c%updated//'","commit":"'//c%commit//'"'
        if (index(c%key, 'branch:') /= 1) json = json//',"change_id":"'// &
            json_escape_string(c%key)//'"'
        json = json//'}'
    end function summary_json

    function change_json(db, c) result(json)
        !! The change with its versions, comments and landings.
        type(db_t), intent(in) :: db
        type(change_t), intent(in) :: c
        character(len=:), allocatable :: json
        type(stmt_t) :: st
        integer :: stat, n

        json = summary_json(c)
        json = json(1:len(json) - 1)//',"description":"'//json_escape_string(c%description)// &
            '","versions_list":['
        call db_prepare(db, 'SELECT version, head, base, coalesce(pusher, ''''), created_at'// &
            ' FROM change_versions WHERE change = ?1 ORDER BY version;', st, stat)
        n = 0
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, c%id, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                if (n > 0) json = json//','
                json = json//'{"version":'//itoa(int(stmt_int(st, 0)))//',"head":"'// &
                    stmt_text(st, 1)//'","base":"'//stmt_text(st, 2)//'","pusher":"'// &
                    json_escape_string(stmt_text(st, 3))//'","created":"'//stmt_text(st, 4)//'"}'
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
        call db_prepare(db, "SELECT id, state, coalesce(commit_id, '') FROM landings"// &
            ' WHERE change = ?1 ORDER BY id;', st, stat)
        n = 0
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, c%id, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                if (n > 0) json = json//','
                json = json//'{"landing":'//itoa(int(stmt_int(st, 0)))//',"state":"'// &
                    stmt_text(st, 1)//'","commit":"'//stmt_text(st, 2)//'"}'
                n = n + 1
            end do
            call stmt_finalize(st)
        end if
        json = json//']}'
    end function change_json

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
    end subroutine read_row

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
