module fy_sqlite
    !! Minimal SQLite binding: open, exec, prepared statements with text and
    !! integer parameters.
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: db_t, stmt_t
    public :: db_open, db_close, db_exec, db_errmsg, db_deny_transaction
    public :: db_prepare, stmt_bind_text, stmt_bind_int, stmt_step
    public :: stmt_text, stmt_int, stmt_finalize

    integer, parameter, public :: SQLITE_OK = 0, SQLITE_BUSY = 5, SQLITE_AUTH = 23, &
        SQLITE_ROW = 100, SQLITE_DONE = 101
    integer(c_int), parameter :: OPEN_READWRITE = 2, OPEN_CREATE = 4
    ! SQLITE_TRANSIENT: SQLite copies bound text before the call returns.
    integer(c_intptr_t), parameter :: TRANSIENT = -1_c_intptr_t

    type :: db_t
        type(c_ptr) :: handle = c_null_ptr
    end type db_t

    type :: stmt_t
        type(c_ptr) :: handle = c_null_ptr
    end type stmt_t

    interface
        function sqlite3_open_v2(filename, db, flags, vfs) bind(C) result(rc)
            import :: c_char, c_ptr, c_int
            character(kind=c_char), intent(in) :: filename(*)
            type(c_ptr), intent(out) :: db
            integer(c_int), value :: flags
            type(c_ptr), value :: vfs
            integer(c_int) :: rc
        end function sqlite3_open_v2

        function sqlite3_close_v2(db) bind(C) result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int) :: rc
        end function sqlite3_close_v2

        function sqlite3_exec(db, sql, callback, arg, errmsg) bind(C) result(rc)
            import :: c_ptr, c_char, c_funptr, c_int
            type(c_ptr), value :: db
            character(kind=c_char), intent(in) :: sql(*)
            type(c_funptr), value :: callback
            type(c_ptr), value :: arg, errmsg
            integer(c_int) :: rc
        end function sqlite3_exec

        function sqlite3_busy_handler(db, handler, arg) bind(C) result(rc)
            import :: c_ptr, c_funptr, c_int
            type(c_ptr), value :: db
            type(c_funptr), value :: handler
            type(c_ptr), value :: arg
            integer(c_int) :: rc
        end function sqlite3_busy_handler

        function sqlite3_set_authorizer(db, auth, arg) bind(C) result(rc)
            import :: c_ptr, c_funptr, c_int
            type(c_ptr), value :: db, arg
            type(c_funptr), value :: auth
            integer(c_int) :: rc
        end function sqlite3_set_authorizer

        function c_usleep(usec) bind(C, name='usleep') result(rc)
            import :: c_int
            integer(c_int), value :: usec
            integer(c_int) :: rc
        end function c_usleep

        function sqlite3_errmsg(db) bind(C) result(msg)
            import :: c_ptr
            type(c_ptr), value :: db
            type(c_ptr) :: msg
        end function sqlite3_errmsg

        function sqlite3_prepare_v2(db, sql, nbyte, stmt, tail) bind(C) result(rc)
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: db
            character(kind=c_char), intent(in) :: sql(*)
            integer(c_int), value :: nbyte
            type(c_ptr), intent(out) :: stmt
            type(c_ptr), value :: tail
            integer(c_int) :: rc
        end function sqlite3_prepare_v2

        function sqlite3_bind_text(stmt, idx, text, n, destructor) bind(C) result(rc)
            import :: c_ptr, c_int, c_char, c_intptr_t
            type(c_ptr), value :: stmt
            integer(c_int), value :: idx
            character(kind=c_char), intent(in) :: text(*)
            integer(c_int), value :: n
            integer(c_intptr_t), value :: destructor
            integer(c_int) :: rc
        end function sqlite3_bind_text

        function sqlite3_bind_int64(stmt, idx, val) bind(C) result(rc)
            import :: c_ptr, c_int, c_int64_t
            type(c_ptr), value :: stmt
            integer(c_int), value :: idx
            integer(c_int64_t), value :: val
            integer(c_int) :: rc
        end function sqlite3_bind_int64

        function sqlite3_step(stmt) bind(C) result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int) :: rc
        end function sqlite3_step

        function sqlite3_column_text(stmt, col) bind(C) result(p)
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int), value :: col
            type(c_ptr) :: p
        end function sqlite3_column_text

        function sqlite3_column_int64(stmt, col) bind(C) result(v)
            import :: c_ptr, c_int, c_int64_t
            type(c_ptr), value :: stmt
            integer(c_int), value :: col
            integer(c_int64_t) :: v
        end function sqlite3_column_int64

        function sqlite3_finalize(stmt) bind(C) result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: stmt
            integer(c_int) :: rc
        end function sqlite3_finalize

        function c_strlen(s) bind(C, name='strlen') result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: s
            integer(c_size_t) :: n
        end function c_strlen
    end interface

contains

    subroutine db_open(path, db, stat)
        character(len=*), intent(in) :: path
        type(db_t), intent(out) :: db
        integer, intent(out) :: stat

        stat = sqlite3_open_v2(path//c_null_char, db%handle, &
            ior(OPEN_READWRITE, OPEN_CREATE), c_null_ptr)
        if (stat /= SQLITE_OK) return
        stat = sqlite3_busy_handler(db%handle, c_funloc(busy_wait), c_null_ptr)
    end subroutine db_open

    function busy_wait(arg, tries) bind(C) result(again)
        !! Asks for a locked database every millisecond for about five
        !! seconds. SQLite's own timeout ends up asking every 100 ms, and a
        !! process that waits like that loses the lock again and again to
        !! one that writes without pause, until its time is up.
        type(c_ptr), value :: arg
        integer(c_int), value :: tries
        integer(c_int) :: again, rc

        again = 0
        if (tries >= 5000) return
        rc = c_usleep(1000_c_int)
        again = 1
    end function busy_wait

    subroutine db_deny_transaction(db, deny)
        !! While `deny` is set, SQL on this connection that begins or ends a
        !! transaction fails with SQLITE_AUTH before it runs, so SQL from
        !! elsewhere cannot end the transaction it is run in.
        type(db_t), intent(in) :: db
        logical, intent(in) :: deny
        integer(c_int) :: rc

        if (deny) then
            rc = sqlite3_set_authorizer(db%handle, c_funloc(no_transaction), c_null_ptr)
        else
            rc = sqlite3_set_authorizer(db%handle, c_null_funptr, c_null_ptr)
        end if
    end subroutine db_deny_transaction

    function no_transaction(arg, action, a, b, c, d) bind(C) result(rc)
        type(c_ptr), value :: arg, a, b, c, d
        integer(c_int), value :: action
        integer(c_int) :: rc
        ! SQLITE_TRANSACTION: BEGIN, COMMIT, END and ROLLBACK.
        integer(c_int), parameter :: TRANSACTION = 22, DENY = 1

        rc = merge(DENY, 0_c_int, action == TRANSACTION)
    end function no_transaction

    subroutine db_close(db)
        type(db_t), intent(inout) :: db
        integer(c_int) :: rc

        if (c_associated(db%handle)) rc = sqlite3_close_v2(db%handle)
        db%handle = c_null_ptr
    end subroutine db_close

    subroutine db_exec(db, sql, stat)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: sql
        integer, intent(out) :: stat

        stat = sqlite3_exec(db%handle, sql//c_null_char, c_null_funptr, c_null_ptr, &
            c_null_ptr)
    end subroutine db_exec

    function db_errmsg(db) result(msg)
        type(db_t), intent(in) :: db
        character(len=:), allocatable :: msg

        msg = c_string(sqlite3_errmsg(db%handle))
    end function db_errmsg

    subroutine db_prepare(db, sql, stmt, stat)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: sql
        type(stmt_t), intent(out) :: stmt
        integer, intent(out) :: stat

        stat = sqlite3_prepare_v2(db%handle, sql//c_null_char, -1_c_int, stmt%handle, &
            c_null_ptr)
    end subroutine db_prepare

    subroutine stmt_bind_text(stmt, idx, text, stat)
        type(stmt_t), intent(in) :: stmt
        integer, intent(in) :: idx
        character(len=*), intent(in) :: text
        integer, intent(out) :: stat
        character(kind=c_char), parameter :: empty(1) = c_null_char

        if (len(text) == 0) then
            stat = sqlite3_bind_text(stmt%handle, int(idx, c_int), empty, 0_c_int, &
                TRANSIENT)
        else
            stat = sqlite3_bind_text(stmt%handle, int(idx, c_int), text, &
                int(len(text), c_int), TRANSIENT)
        end if
    end subroutine stmt_bind_text

    subroutine stmt_bind_int(stmt, idx, val, stat)
        type(stmt_t), intent(in) :: stmt
        integer, intent(in) :: idx
        integer(int64), intent(in) :: val
        integer, intent(out) :: stat

        stat = sqlite3_bind_int64(stmt%handle, int(idx, c_int), int(val, c_int64_t))
    end subroutine stmt_bind_int

    function stmt_step(stmt) result(rc)
        type(stmt_t), intent(in) :: stmt
        integer :: rc

        rc = sqlite3_step(stmt%handle)
    end function stmt_step

    function stmt_text(stmt, col) result(text)
        !! Column text, 0-based column index; NULL gives an empty string.
        type(stmt_t), intent(in) :: stmt
        integer, intent(in) :: col
        character(len=:), allocatable :: text

        text = c_string(sqlite3_column_text(stmt%handle, int(col, c_int)))
    end function stmt_text

    function stmt_int(stmt, col) result(val)
        type(stmt_t), intent(in) :: stmt
        integer, intent(in) :: col
        integer(int64) :: val

        val = int(sqlite3_column_int64(stmt%handle, int(col, c_int)), int64)
    end function stmt_int

    subroutine stmt_finalize(stmt)
        type(stmt_t), intent(inout) :: stmt
        integer(c_int) :: rc

        if (c_associated(stmt%handle)) rc = sqlite3_finalize(stmt%handle)
        stmt%handle = c_null_ptr
    end subroutine stmt_finalize

    function c_string(p) result(s)
        type(c_ptr), intent(in) :: p
        character(len=:), allocatable :: s
        character(kind=c_char), pointer :: chars(:)
        integer :: n, i

        s = ''
        if (.not. c_associated(p)) return
        n = int(c_strlen(p))
        if (n == 0) return
        call c_f_pointer(p, chars, [n])
        deallocate (s)
        allocate (character(len=n) :: s)
        do i = 1, n
            s(i:i) = chars(i)
        end do
    end function c_string

end module fy_sqlite
