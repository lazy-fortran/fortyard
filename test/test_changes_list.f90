program test_changes_list
    !! The list of changes (doc/inspection.md, "Changes"): newest activity
    !! first; the open ones without a state, as before; the filters state,
    !! review, attention and q; the words a change is shown as; older rows by
    !! the key of the answer; and the changes of every repository.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_change, only: change_list_json, change_review_set
    use fy_sqlite, only: db_close, db_exec, db_t
    use fy_store, only: store_landing_add, store_landing_finish, store_open, store_repo_add
    implicit none

    character(len=*), parameter :: path = 'build/test_changes_list.db'
    type(db_t) :: db
    character(len=:), allocatable :: a, key
    integer(int64) :: entry
    real :: t0, t1
    integer :: stat, code, k

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call store_repo_add(db, 'other', '', stat, mode='primary')

    ! Numbers in the order opened, activity in another: #3 moved last.
    call change(1, 'demo', 'First landed', 'one', 'agent-1', 'landed', '10:01', "'none'", 0, '')
    call change(2, 'demo', 'Guard the bounds', 'bounds', 'agent-9', 'landed', '10:05', &
        "'ai-reviewed'", 1, 'security-relevant')
    call change(3, 'demo', 'Web image', 'web-image-lld', 'agent-4', 'open', '10:09', 'NULL', 0, '')
    call change(4, 'demo', 'Dropped idea', 'idea', 'agent-4', 'closed', '10:03', 'NULL', 0, '')
    call change(5, 'demo', 'Waits in the queue', 'queue', 'agent-2', 'open', '10:07', &
        "'requested'", 0, '')
    call change(6, 'demo', 'Never landed', 'draft', 'agent-2', 'open', '10:02', "'human-reviewed'", &
        0, '')
    call change(1, 'other', 'Elsewhere', 'else', 'agent-7', 'open', '10:08', 'NULL', 0, '')
    call db_exec(db, "UPDATE changes SET commit_id = '3a96647aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'"// &
        " WHERE number = 2;", stat)
    ! #3 failed twice, #5 is queued; a verification of main is no entry.
    call landing(3, 'failed', 'landing')
    call landing(3, 'failed', 'landing')
    call landing(5, 'failed', 'landing')
    call landing(5, 'queued', 'landing')
    call landing(6, 'failed', 'verify')

    a = change_list_json(db, 'demo', '', code)
    call check(code == 200 .and. numbers(a) == '3 5 6', &
        'without a state the open changes, newest activity first: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all', code)
    call check(numbers(a) == '3 5 2 4 6 1', 'state=all lists every state by activity: '//numbers(a))
    call check(index(a, '"next":""') > 0, 'a complete list has no next key: '//a)
    a = change_list_json(db, 'demo', 'state=landed', code)
    call check(numbers(a) == '2 1', 'state=landed: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=closed', code)
    call check(numbers(a) == '4', 'state=closed: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=flagged', code)
    call check(numbers(a) == '2', 'the earlier state=flagged still answers: '//numbers(a))

    a = change_list_json(db, 'demo', 'state=all&review=flagged', code)
    call check(numbers(a) == '2', 'review=flagged: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all&review=unreviewed', code)
    call check(numbers(a) == '3 5 4 1', &
        'review=unreviewed leaves out the AI- and human-reviewed: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=landed&review=unreviewed', code)
    call check(numbers(a) == '1', 'filters combine: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all&attention=needs-author', code)
    call check(numbers(a) == '3', 'attention=needs-author: the newest entry failed: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all&attention=reverted', code)
    call check(code == 200 .and. numbers(a) == '', 'no revert is recorded: '//a)
    a = change_list_json(db, 'demo', 'state=all&review=nonsense', code)
    call check(code == 200 .and. numbers(a) == '', 'a review this server does not know: '//a)

    a = change_list_json(db, 'demo', 'state=all&q=BOUNDS', code)
    call check(numbers(a) == '2', 'q finds text in a title, whatever its case: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all&q=web-image', code)
    call check(numbers(a) == '3', 'q finds a branch: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all&q=agent-4', code)
    call check(numbers(a) == '3 4', 'q finds an author: '//numbers(a))
    call db_exec(db, 'INSERT INTO change_versions (change, version, head, base, created_at,'// &
        " session) VALUES (6, 1, 'h', 'b', '2026-11-11T10:02:00Z', 'lane-Draft');", stat)
    a = change_list_json(db, 'demo', 'state=all&q=LANE-draft', code)
    call check(numbers(a) == '6' .and. index(a, '"session":"lane-Draft"') > 0, &
        'q finds the session that pushed a version, and the row names it: '//a)
    a = change_list_json(db, 'demo', 'state=all&q=100%25', code)
    call check(numbers(a) == '', 'q is text, not a pattern: '//numbers(a))

    ! #6 was approved at its first version; a second one came after.
    call db_exec(db, "UPDATE changes SET reviewed_head = 'h' WHERE id = 6;", stat)
    a = change_list_json(db, 'demo', 'state=all&review=unreviewed', code)
    call check(numbers(a) == '3 5 4 1' .and. index(row(a, 6), '"review_stale":true') == 0, &
        'a review of the newest version is not stale: '//a)
    call db_exec(db, 'INSERT INTO change_versions (change, version, head, base, created_at)'// &
        " VALUES (6, 2, 'h2', 'b', '2026-11-11T10:02:00Z');", stat)
    a = change_list_json(db, 'demo', 'state=all&review=unreviewed', code)
    call check(numbers(a) == '3 5 4 6 1' .and. index(row(a, 6), &
        '"review":"human-reviewed"') > 0 .and. index(row(a, 6), '"review_stale":true}') > 0, &
        'a review of an older version is stale, and the newest is unreviewed: '//a)
    call check(index(row(a, 3), '"review_stale":false}') > 0, &
        'a change without a review has none that is stale: '//row(a, 3))
    call db_exec(db, 'DELETE FROM change_versions WHERE change = 6 AND version = 2;', stat)

    ! The words of a row.
    a = change_list_json(db, 'demo', 'state=all', code)
    call check(index(row(a, 3), '"status":"needs author","entries":2,"entry":2,"reason":null') > 0, &
        'an open change whose newest entry failed needs its author: '//row(a, 3))
    call check(index(row(a, 5), '"status":"queued","entries":2,"entry":4') > 0, &
        'an open change shows its live entry: '//row(a, 5))
    call check(index(row(a, 6), '"status":"draft","entries":0,"entry":null') > 0, &
        'an open change without an entry is a draft: '//row(a, 6))
    call check(index(row(a, 2), '"status":"landed"') > 0 .and. index(row(a, 2), &
        '"commit":"3a96647') > 0 .and. index(row(a, 2), '"flag_reason":"security-relevant"') > 0 &
        .and. index(row(a, 2), '"repo":"demo"') > 0, 'a landed change: '//row(a, 2))
    call check(index(row(a, 4), '"status":"closed"') > 0 .and. index(row(a, 4), &
        '"state":"closed"') > 0, 'a closed change, with the state as before: '//row(a, 4))

    ! Older rows by key: three pages of two, none twice.
    a = change_list_json(db, 'demo', 'state=all&limit=2', code)
    key = next_of(a)
    call check(numbers(a) == '3 5' .and. len(key) > 0, 'the first page: '//a)
    a = change_list_json(db, 'demo', 'state=all&limit=2&before='//key, code)
    call check(numbers(a) == '2 4', 'the page before the key: '//numbers(a))
    a = change_list_json(db, 'demo', 'state=all&limit=2&before='//next_of(a), code)
    call check(numbers(a) == '6 1' .and. next_of(a) == '', 'the last page ends the list: '//a)
    ! Two changes of one second are told apart by the key.
    call db_exec(db, "UPDATE changes SET updated_at = '2026-11-11T10:09:00Z' WHERE repo = 'demo'"// &
        ' AND number IN (3, 5);', stat)
    a = change_list_json(db, 'demo', 'state=all&limit=1', code)
    k = index(numbers(a), ' ')
    call check(k == 0 .and. numbers(change_list_json(db, 'demo', 'state=all&limit=1&before='// &
        next_of(a), code)) == '3', 'changes of the same second are not skipped: '//numbers(a))
    a = change_list_json(db, 'demo', 'before=yesterday', code)
    call check(code == 400, 'a key that is none is refused: '//a)
    a = change_list_json(db, 'demo', 'limit=0', code)
    call check(code == 400, 'a limit of 0 is refused: '//a)

    ! Every repository.
    a = change_list_json(db, '', 'state=all&limit=3', code)
    call check(index(a, '"title":"Elsewhere"') > 0 .and. index(a, '"repo":"other"') > 0 .and. &
        index(a, '"repo":"demo"') > 0, 'without a repository the changes of all: '//a)

    ! An entry and a review are activity of their change (ids 8 and 9).
    call change(2, 'other', 'Quiet', 'quiet', 'agent-7', 'open', '10:02', 'NULL', 0, '')
    call change(3, 'other', 'Still', 'still', 'agent-7', 'open', '10:01', 'NULL', 0, '')
    call db_exec(db, "UPDATE changes SET updated_at = replace(updated_at, '2026', '2020')"// &
        " WHERE repo = 'other';", stat)
    a = change_list_json(db, 'other', '', code)
    call check(numbers(a) == '1 2 3', 'three changes at rest: '//numbers(a))
    call store_landing_add(db, 'other', 'still', 'admin', entry, stat, change=9_int64)
    a = change_list_json(db, 'other', '', code)
    call check(stat == 0 .and. numbers(a) == '3 1 2', &
        'a change that is queued comes first: '//numbers(a))
    call db_exec(db, "UPDATE changes SET updated_at = '2020-11-11T10:00:00Z' WHERE id = 9;", stat)
    call store_landing_finish(db, entry, 'failed', '', 'no')
    a = change_list_json(db, 'other', '', code)
    call check(numbers(a) == '3 1 2' .and. index(row(a, 3), '"status":"needs author"') > 0, &
        'a change whose entry failed comes first: '//a)
    call db_exec(db, "UPDATE changes SET updated_at = '2020-11-11T10:00:00Z' WHERE id = 9;", stat)
    call change_review_set(db, 8_int64, 'ai-reviewed', .true., 'risky')
    a = change_list_json(db, 'other', '', code)
    call check(numbers(a) == '2 1 3' .and. index(row(a, 2), '"flag_reason":"risky"') > 0, &
        'a change that was flagged comes first: '//a)

    ! A store with history, of the size of the budget (doc/inspection.md,
    ! "Budgets"): the entries of a row are found by an index, not by reading
    ! every landing with its log for each row (tens of seconds without it).
    ! Processor time, not the clock, so a busy machine does not decide.
    call db_exec(db, 'BEGIN;'// &
        'WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n WHERE i < 1000)'// &
        ' INSERT INTO changes (repo, number, change_key, branch, title, description, author,'// &
        " state, created_at, updated_at) SELECT 'big', i, 'branch:b' || i, 'b' || i,"// &
        " 'Change ' || i, '', 'agent-1', 'landed', '2020-01-01T00:00:00Z',"// &
        " '2020-01-01T00:00:00Z' FROM n;"// &
        'WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n WHERE i < 3000)'// &
        ' INSERT INTO landings (repo, branch, account, state, created_at, change, kind, log)'// &
        " SELECT 'big', 'b', 'admin', 'landed', '2020-01-01T00:00:00Z',"// &
        " (SELECT max(id) FROM changes WHERE repo = 'big') - i % 1000,"// &
        " CASE WHEN i <= 2000 THEN 'landing' ELSE 'verify' END, hex(randomblob(4000)) FROM n;"// &
        'COMMIT;', stat)
    call check(stat == 0, 'a thousand changes with three thousand logged jobs are stored')
    call cpu_time(t0)
    a = change_list_json(db, 'big', 'state=all', code)
    call cpu_time(t1)
    call check(index(a, '"number":1000,') > 0 .and. index(a, '"status":"landed","entries":2,') > 0, &
        'they are listed: '//a(1:400))
    call check(t1 - t0 < 0.1, 'and that takes less than 100 ms of processor time')

    call db_close(db)
    print '(a)', 'changes list: all checks passed'

contains

    subroutine change(number, repo, title, branch, author, state, time, review, flagged, reason)
        integer, intent(in) :: number, flagged
        character(len=*), intent(in) :: repo, title, branch, author, state, time, review, reason
        character(len=8) :: n

        write (n, '(i0)') number
        call db_exec(db, 'INSERT INTO changes (repo, number, change_key, branch, title,'// &
            ' description, author, state, created_at, updated_at, review_state, flagged,'// &
            " flag_reason) VALUES ('"//repo//"', "//trim(n)//", 'branch:"//branch//"', '"// &
            branch//"', '"//title//"', '', '"//author//"', '"//state//"', '2026-11-11T09:00:00Z',"// &
            " '2026-11-11T"//time//":00Z', "//review//', '//achar(48 + flagged)//", '"//reason// &
            "');", stat)
        call check(stat == 0, 'a change is stored: '//title)
    end subroutine change

    subroutine landing(number, state, kind)
        !! A job of the change `number` of demo.
        integer, intent(in) :: number
        character(len=*), intent(in) :: state, kind
        character(len=8) :: n

        write (n, '(i0)') number
        call db_exec(db, 'INSERT INTO landings (repo, branch, account, state, created_at,'// &
            " change, kind) VALUES ('demo', 'b', 'admin', '"//state//"', '2026-11-11T10:00:00Z',"// &
            " (SELECT id FROM changes WHERE repo = 'demo' AND number = "//trim(n)//"), '"// &
            kind//"');", stat)
        call check(stat == 0, 'a job is stored')
    end subroutine landing

    function numbers(json) result(list)
        !! The numbers of the rows of an answer, in its order: "3 5 6".
        character(len=*), intent(in) :: json
        character(len=:), allocatable :: list
        integer :: at, e

        list = ''
        at = 1
        do
            e = index(json(at:), '{"number":')
            if (e == 0) exit
            at = at + e + 9
            e = verify(json(at:), '0123456789')
            if (len(list) > 0) list = list//' '
            list = list//json(at:at + e - 2)
        end do
    end function numbers

    function row(json, number) result(text)
        !! The row of the change `number` in an answer.
        character(len=*), intent(in) :: json
        integer, intent(in) :: number
        character(len=:), allocatable :: text
        integer :: at, e

        text = ''
        at = index(json, '{"number":'//achar(48 + number)//',')
        if (at == 0) return
        e = index(json(at:), '}')
        text = json(at:at + e - 1)
    end function row

    function next_of(json) result(key)
        character(len=*), intent(in) :: json
        character(len=:), allocatable :: key
        integer :: at

        key = ''
        at = index(json, '"next":"')
        if (at == 0) return
        key = json(at + 8:)
        key = key(1:index(key, '"') - 1)
    end function next_of

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_changes_list
