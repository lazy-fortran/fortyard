module fy_issue
    !! Issues of a repository (doc/workflow.md): a title, a Markdown body,
    !! comments, and open or closed. They share one number sequence with the
    !! repository's changes (fy_change).
    !!   GET  /api/v1/repos/REPO/issues[?state=open|closed|all]
    !!   POST /api/v1/repos/REPO/issues {"title","body"}
    !!   GET  /api/v1/repos/REPO/issues/N
    !!   POST /api/v1/repos/REPO/issues/N {"title","body","state"}
    !!   POST /api/v1/repos/REPO/issues/N/comments {"body"}
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string
    use fy_change, only: change_next_number
    use fy_sqlite, only: db_t, stmt_t, db_prepare, stmt_bind_text, stmt_bind_int, stmt_step, &
        stmt_text, stmt_int, stmt_finalize, SQLITE_OK, SQLITE_ROW, SQLITE_DONE
    implicit none
    private

    public :: issue_api, issue_open, issue_comment, issue_set_state

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    character(len=*), parameter :: COLUMNS = 'id, number, title, body, author, state,'// &
        ' created_at, updated_at, (SELECT count(*) FROM issue_comments c WHERE c.issue = issues.id)'

    type :: issue_t
        integer(int64) :: id = 0
        integer :: number = 0, comments = 0
        character(len=:), allocatable :: title, body, author, state, created, updated
    end type issue_t

contains

    subroutine issue_api(db, method, repo, git_dir, rest, query, body, account, admin, code, answer)
        !! `rest` is the part of the path after "issues".
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: method, repo, git_dir, rest, query, body, account
        logical, intent(in) :: admin
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: answer
        type(issue_t) :: it
        character(len=:), allocatable :: value, sub, title, text
        integer :: number, slash, ios
        logical :: found, has_title, has_text

        code = 200
        if (len(rest) == 0) then
            if (method == 'GET') then
                answer = list_json(db, repo, query_value(query, 'state'))
            else if (method == 'POST') then
                call json_extract_string(body, 'title', title, has_title)
                call json_extract_string(body, 'body', text, has_text)
                if (.not. has_title .or. .not. title_ok(title)) then
                    call refuse(400, 'body must be {"title":"...","body":"..."} with a one-line title')
                    return
                end if
                if (.not. has_text) text = ''
                call create(title, text)
            else
                call refuse(405, 'GET or POST')
            end if
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
        call issue_get(db, repo, number, it, found)
        if (.not. found) then
            call refuse(404, 'no such issue')
            return
        end if
        select case (sub)
        case ('')
            if (method == 'GET') then
                answer = issue_json(db, it)
            else if (method == 'POST') then
                call edit()
            else
                call refuse(405, 'GET or POST')
            end if
        case ('comments')
            if (method /= 'POST') then
                call refuse(405, 'POST')
                return
            end if
            call json_extract_string(body, 'body', text, found)
            if (.not. found .or. len(text) == 0) then
                call refuse(400, 'body must be {"body":"..."}')
                return
            end if
            call exec_text3(db, 'INSERT INTO issue_comments (issue, account, body, created_at)'// &
                ' VALUES (?1, ?2, ?3, '//NOW//');', it%id, account, text)
            call exec_text3(db, 'UPDATE issues SET updated_at = '//NOW//' WHERE id = ?1'// &
                ' AND ?2 = ?2 AND ?3 = ?3;', it%id, '', '')
            code = 201
            answer = '{"issue":'//itoa(it%number)//'}'
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

        subroutine create(title, text)
            character(len=*), intent(in) :: title, text
            type(stmt_t) :: st
            integer :: stat, n

            n = change_next_number(db, repo, git_dir)
            call db_prepare(db, 'INSERT INTO issues (repo, number, title, body, author,'// &
                ' created_at, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, '//NOW//', '//NOW//');', &
                st, stat)
            if (stat /= SQLITE_OK) then
                call refuse(500, 'issue not stored')
                return
            end if
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_int(st, 2, int(n, int64), stat)
            call stmt_bind_text(st, 3, title, stat)
            call stmt_bind_text(st, 4, text, stat)
            call stmt_bind_text(st, 5, account, stat)
            stat = stmt_step(st)
            call stmt_finalize(st)
            code = 201
            answer = '{"issue":'//itoa(n)//'}'
        end subroutine create

        subroutine edit()
            !! {"title","body","state":"open"|"closed"}, each optional; the
            !! author and administrators edit, anyone who writes may close.
            character(len=:), allocatable :: s
            logical :: ok

            call json_extract_string(body, 'title', s, ok)
            if (ok) then
                if (.not. title_ok(s)) then
                    call refuse(400, 'a title is one non-empty line')
                    return
                end if
                if (account /= it%author .and. .not. admin) then
                    call refuse(403, 'only the author or an administrator edits an issue')
                    return
                end if
                call exec_text3(db, 'UPDATE issues SET title = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1 AND ?3 = ?3;', it%id, s, '')
            end if
            call json_extract_string(body, 'body', s, ok)
            if (ok) then
                if (account /= it%author .and. .not. admin) then
                    call refuse(403, 'only the author or an administrator edits an issue')
                    return
                end if
                call exec_text3(db, 'UPDATE issues SET body = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1 AND ?3 = ?3;', it%id, s, '')
            end if
            call json_extract_string(body, 'state', s, ok)
            if (ok) then
                if (s /= 'open' .and. s /= 'closed') then
                    call refuse(400, 'state is open or closed')
                    return
                end if
                call exec_text3(db, 'UPDATE issues SET state = ?2, updated_at = '//NOW// &
                    ' WHERE id = ?1 AND ?3 = ?3;', it%id, s, '')
            end if
            call issue_get(db, repo, it%number, it, ok)
            answer = issue_json(db, it)
        end subroutine edit

    end subroutine issue_api

    integer function issue_open(db, repo, git_dir, title, text, account) result(number)
        !! Opens an issue and returns its number, or 0.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, title, text, account
        type(stmt_t) :: st
        integer :: stat

        number = change_next_number(db, repo, git_dir)
        call db_prepare(db, 'INSERT INTO issues (repo, number, title, body, author,'// &
            ' created_at, updated_at) VALUES (?1, ?2, ?3, ?4, ?5, '//NOW//', '//NOW//');', &
            st, stat)
        if (stat /= SQLITE_OK) then
            number = 0
            return
        end if
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_int(st, 2, int(number, int64), stat)
        call stmt_bind_text(st, 3, title, stat)
        call stmt_bind_text(st, 4, text, stat)
        call stmt_bind_text(st, 5, account, stat)
        if (stmt_step(st) /= SQLITE_DONE) number = 0
        call stmt_finalize(st)
    end function issue_open

    subroutine issue_comment(db, repo, number, account, text)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, account, text
        integer, intent(in) :: number
        type(issue_t) :: it
        logical :: found

        call issue_get(db, repo, number, it, found)
        if (.not. found) return
        call exec_text3(db, 'INSERT INTO issue_comments (issue, account, body, created_at)'// &
            ' VALUES (?1, ?2, ?3, '//NOW//');', it%id, account, text)
    end subroutine issue_comment

    subroutine issue_set_state(db, repo, number, state)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, state
        integer, intent(in) :: number
        type(issue_t) :: it
        logical :: found

        call issue_get(db, repo, number, it, found)
        if (.not. found) return
        call exec_text3(db, 'UPDATE issues SET state = ?2, updated_at = '//NOW// &
            ' WHERE id = ?1 AND ?3 = ?3;', it%id, state, '')
    end subroutine issue_set_state

    subroutine issue_get(db, repo, number, it, found)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        integer, intent(in) :: number
        type(issue_t), intent(out) :: it
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'SELECT '//COLUMNS//' FROM issues WHERE repo = ?1 AND number = ?2;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_int(st, 2, int(number, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            call read_row(st, it)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine issue_get

    function list_json(db, repo, state) result(json)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, state
        character(len=:), allocatable :: json
        type(stmt_t) :: st
        type(issue_t) :: it
        integer :: stat, n

        json = '{"issues":['
        call db_prepare(db, 'SELECT '//COLUMNS//' FROM issues WHERE repo = ?1 AND'// &
            " (?2 = 'all' OR state = ?2) ORDER BY number DESC LIMIT 200;", st, stat)
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
            call read_row(st, it)
            if (n > 0) json = json//','
            json = json//summary_json(it)
            n = n + 1
        end do
        call stmt_finalize(st)
        json = json//']}'
    end function list_json

    function summary_json(it) result(json)
        type(issue_t), intent(in) :: it
        character(len=:), allocatable :: json
        json = '{"number":'//itoa(it%number)//',"title":"'//json_escape_string(it%title)// &
            '","state":"'//it%state//'","author":"'//json_escape_string(it%author)// &
            '","comments":'//itoa(it%comments)//',"created":"'//it%created// &
            '","updated":"'//it%updated//'"}'
    end function summary_json

    function issue_json(db, it) result(json)
        type(db_t), intent(in) :: db
        type(issue_t), intent(in) :: it
        character(len=:), allocatable :: json
        type(stmt_t) :: st
        integer :: stat, n

        json = summary_json(it)
        json = json(1:len(json) - 1)//',"body":"'//json_escape_string(it%body)//'","comments_list":['
        call db_prepare(db, 'SELECT account, body, created_at FROM issue_comments'// &
            ' WHERE issue = ?1 ORDER BY id;', st, stat)
        n = 0
        if (stat == SQLITE_OK) then
            call stmt_bind_int(st, 1, it%id, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                if (n > 0) json = json//','
                json = json//'{"account":"'//json_escape_string(stmt_text(st, 0))//'","body":"'// &
                    json_escape_string(stmt_text(st, 1))//'","created":"'//stmt_text(st, 2)//'"}'
                n = n + 1
            end do
            call stmt_finalize(st)
        end if
        json = json//']}'
    end function issue_json

    subroutine read_row(st, it)
        type(stmt_t), intent(in) :: st
        type(issue_t), intent(out) :: it
        it%id = stmt_int(st, 0)
        it%number = int(stmt_int(st, 1))
        it%title = stmt_text(st, 2)
        it%body = stmt_text(st, 3)
        it%author = stmt_text(st, 4)
        it%state = stmt_text(st, 5)
        it%created = stmt_text(st, 6)
        it%updated = stmt_text(st, 7)
        it%comments = int(stmt_int(st, 8))
    end subroutine read_row

    subroutine exec_text3(db, sql, id, a, b)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: sql, a, b
        integer(int64), intent(in) :: id
        type(stmt_t) :: st
        integer :: stat
        call db_prepare(db, sql, st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, a, stat)
        call stmt_bind_text(st, 3, b, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine exec_text3

    pure logical function title_ok(s)
        character(len=*), intent(in) :: s
        title_ok = len_trim(s) > 0 .and. len(s) <= 300 .and. index(s, new_line('a')) == 0
    end function title_ok

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

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_issue
