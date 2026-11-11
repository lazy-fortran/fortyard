module fy_audit
    !! The audit log (doc/workflow.md, "Audit"): security-relevant events in
    !! an append-only table. Every row carries the hash of the row before it
    !! and the SHA-256 of that hash with its own content, so a row that was
    !! altered or removed later breaks the chain from there on.
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fx_hash, only: sha256_string
    use fx_json_build, only: json_escape_string
    use fy_sqlite, only: db_t, stmt_t, db_exec, db_prepare, stmt_bind_text, stmt_bind_int, &
        stmt_step, stmt_text, stmt_int, stmt_finalize, SQLITE_OK, SQLITE_ROW, SQLITE_DONE
    implicit none
    private

    public :: audit_t, audit_append, audit_list, audit_verify, audit_json

    !! What the first row carries as the hash of its predecessor.
    character(len=64), parameter, public :: AUDIT_GENESIS = repeat('0', 64)

    type :: audit_t
        integer(int64) :: id = 0
        character(len=:), allocatable :: at, actor, event, subject, detail, prev, hash
    end type audit_t

contains

    subroutine audit_append(db, actor, event, subject, detail, stat)
        !! Appends the event `event` of `actor` on `subject`. Must not be
        !! called inside a transaction: reading the newest row and adding the
        !! next one are one transaction, so concurrent processes keep one chain.
        !! The row continues the recorded newest entry (audit_head), not the
        !! last row present, so rows cut off the end leave a gap in the numbers.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: actor, event, subject, detail
        integer, intent(out), optional :: stat
        type(stmt_t) :: st
        type(audit_t) :: e
        integer :: rc, ignored

        call db_exec(db, 'BEGIN IMMEDIATE;', rc)
        if (rc == SQLITE_OK) then
            e%id = 1
            e%prev = AUDIT_GENESIS
            e%at = ''
            call db_prepare(db, "SELECT strftime('%Y-%m-%dT%H:%M:%SZ','now'),"// &
                " coalesce((SELECT id FROM audit_head), 0) + 1,"// &
                " coalesce((SELECT hash FROM audit_head), '"//AUDIT_GENESIS//"');", st, rc)
            if (rc == SQLITE_OK) then
                rc = 1
                if (stmt_step(st) == SQLITE_ROW) then
                    e%at = stmt_text(st, 0)
                    e%id = stmt_int(st, 1)
                    e%prev = stmt_text(st, 2)
                    rc = SQLITE_OK
                end if
                call stmt_finalize(st)
            end if
            if (rc == SQLITE_OK) then
                e%actor = stored(actor)
                e%event = stored(event)
                e%subject = stored(subject)
                e%detail = stored(detail)
                e%hash = row_hash(e)
                call db_prepare(db, 'INSERT INTO audit (id, at, actor, event, subject, detail,'// &
                    ' prev, hash) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8);', st, rc)
            end if
            if (rc == SQLITE_OK) then
                call stmt_bind_int(st, 1, e%id, rc)
                call stmt_bind_text(st, 2, e%at, rc)
                call stmt_bind_text(st, 3, e%actor, rc)
                call stmt_bind_text(st, 4, e%event, rc)
                call stmt_bind_text(st, 5, e%subject, rc)
                call stmt_bind_text(st, 6, e%detail, rc)
                call stmt_bind_text(st, 7, e%prev, rc)
                call stmt_bind_text(st, 8, e%hash, rc)
                rc = SQLITE_OK
                if (stmt_step(st) /= SQLITE_DONE) rc = 1
                call stmt_finalize(st)
            end if
            if (rc == SQLITE_OK) call db_prepare(db, 'INSERT OR REPLACE INTO audit_head (one, id, hash)'// &
                ' VALUES (1, ?1, ?2);', st, rc)
            if (rc == SQLITE_OK) then
                call stmt_bind_int(st, 1, e%id, rc)
                call stmt_bind_text(st, 2, e%hash, rc)
                rc = SQLITE_OK
                if (stmt_step(st) /= SQLITE_DONE) rc = 1
                call stmt_finalize(st)
            end if
            if (rc == SQLITE_OK) then
                call db_exec(db, 'COMMIT;', rc)
            else
                call db_exec(db, 'ROLLBACK;', ignored)
            end if
        end if
        if (rc /= SQLITE_OK) then
            write (error_unit, '(a)') 'fy: audit entry not written: '//event//' '//subject
            flush (error_unit)
        end if
        if (present(stat)) stat = rc
    end subroutine audit_append

    subroutine audit_list(db, before, limit, rows, next)
        !! At most `limit` entries older than entry `before` (0: the newest),
        !! newest first; `next` is the `before` of the following page, or 0.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: before
        integer, intent(in) :: limit
        type(audit_t), allocatable, intent(out) :: rows(:)
        integer(int64), intent(out) :: next
        type(stmt_t) :: st
        type(audit_t) :: e
        integer :: stat

        allocate (rows(0))
        next = 0
        call db_prepare(db, 'SELECT id, at, actor, event, subject, detail, prev, hash FROM audit'// &
            ' WHERE (?1 = 0 OR id < ?1) ORDER BY id DESC LIMIT ?2;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, before, stat)
        ! One more than asked for tells whether another page follows.
        call stmt_bind_int(st, 2, int(limit, int64) + 1, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            if (size(rows) == limit) then
                next = rows(limit)%id
                exit
            end if
            call read_row(st, e)
            rows = [rows, e]
        end do
        call stmt_finalize(st)
    end subroutine audit_list

    subroutine audit_verify(db, count, broken, reason, head)
        !! Recomputes the chain from its first row. `count` rows were read;
        !! `broken` is the id of the first row that does not fit, or 0, with
        !! the `reason`; `head` is the hash of the last row that fits, the
        !! value to compare with one kept outside the instance. Rows cut off
        !! the end leave an intact, shorter chain, which the recorded newest
        !! entry (audit_head) gives away.
        type(db_t), intent(in) :: db
        integer(int64), intent(out) :: count, broken
        character(len=:), allocatable, intent(out) :: reason, head
        type(stmt_t) :: st
        type(audit_t) :: e
        integer(int64) :: expected, newest
        integer :: stat

        count = 0
        broken = 0
        reason = ''
        head = AUDIT_GENESIS
        expected = 1
        call db_prepare(db, 'SELECT id, at, actor, event, subject, detail, prev, hash FROM audit'// &
            ' ORDER BY id;', st, stat)
        if (stat /= SQLITE_OK) then
            reason = 'the audit log cannot be read'
            broken = -1
            return
        end if
        do while (stmt_step(st) == SQLITE_ROW)
            call read_row(st, e)
            count = count + 1
            if (e%id /= expected) then
                reason = 'entries before it are missing'
            else if (e%prev /= head) then
                reason = 'it does not continue the entry before it'
            else if (e%hash /= row_hash(e)) then
                reason = 'its content was altered'
            end if
            if (len(reason) > 0) then
                broken = e%id
                exit
            end if
            head = e%hash
            expected = e%id + 1
        end do
        call stmt_finalize(st)
        if (broken /= 0) return
        call db_prepare(db, "SELECT coalesce((SELECT id FROM audit_head), 0),"// &
            " coalesce((SELECT hash FROM audit_head), '"//AUDIT_GENESIS//"');", st, stat)
        if (stat /= SQLITE_OK) then
            reason = 'the audit log cannot be read'
            broken = -1
            return
        end if
        if (stmt_step(st) == SQLITE_ROW) then
            newest = stmt_int(st, 0)
            if (newest >= expected) then
                reason = 'it and the entries after it are missing'
                broken = expected
            else if (newest /= expected - 1 .or. stmt_text(st, 1) /= head) then
                reason = 'it is not the newest entry on record'
                broken = max(expected - 1, 1_int64)
            end if
        end if
        call stmt_finalize(st)
    end subroutine audit_verify

    function audit_json(e) result(s)
        type(audit_t), intent(in) :: e
        character(len=:), allocatable :: s

        s = '{"id":'//itoa(e%id)//',"at":"'//json_escape_string(e%at)//'","actor":"'// &
            json_escape_string(e%actor)//'","event":"'//json_escape_string(e%event)// &
            '","subject":"'//json_escape_string(e%subject)//'","detail":"'// &
            json_escape_string(e%detail)//'","prev":"'//json_escape_string(e%prev)// &
            '","hash":"'//json_escape_string(e%hash)//'"}'
    end function audit_json

    function row_hash(e) result(hex)
        !! Every field goes in with its length, so no two rows share an input.
        type(audit_t), intent(in) :: e
        character(len=64) :: hex

        hex = sha256_string(e%prev//itoa(e%id)//':'//field(e%at)//field(e%actor)// &
            field(e%event)//field(e%subject)//field(e%detail))
    end function row_hash

    function stored(s) result(t)
        !! A NUL byte is written out: text columns are read back only up to
        !! the first one, and the hash must cover what a reader gets.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i

        t = ''
        do i = 1, len(s)
            if (s(i:i) == achar(0)) then
                t = t//'\0'
            else
                t = t//s(i:i)
            end if
        end do
    end function stored

    function field(s) result(f)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: f

        f = itoa(int(len(s), int64))//':'//s
    end function field

    subroutine read_row(st, e)
        type(stmt_t), intent(in) :: st
        type(audit_t), intent(out) :: e

        e%id = stmt_int(st, 0)
        e%at = stmt_text(st, 1)
        e%actor = stmt_text(st, 2)
        e%event = stmt_text(st, 3)
        e%subject = stmt_text(st, 4)
        e%detail = stmt_text(st, 5)
        e%prev = stmt_text(st, 6)
        e%hash = stmt_text(st, 7)
    end subroutine read_row

    function itoa(k) result(s)
        integer(int64), intent(in) :: k
        character(len=:), allocatable :: s
        character(len=24) :: b

        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_audit
