program test_numbers
    !! Issues opened at the same moment by several processes, each with its own
    !! database connection as the server's workers have, all get stored and get
    !! different numbers.
    use fy_issue, only: issue_open
    use fy_proc, only: proc_fork, proc_exit, proc_wait
    use fy_sqlite, only: &
        SQLITE_ROW, &
        db_close, &
        db_prepare, &
        db_t, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t
    use fy_store, only: store_open, store_repo_add
    implicit none

    character(len=*), parameter :: path = 'build/test_numbers.db'
    integer, parameter :: procs = 4, each = 40
    type(db_t) :: db
    type(stmt_t) :: st
    integer :: stat, pid(procs), i, k, status, rows, distinct

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)

    do i = 1, procs
        pid(i) = proc_fork()
        if (pid(i) == 0) then
            call store_open(path, db, stat)
            status = 0
            do k = 1, each
                if (issue_open(db, 'demo', 'build/no-such.git', 'title', 'text', 'admin') == 0) &
                    status = 1
            end do
            call db_close(db)
            call proc_exit(status)
        end if
    end do
    do i = 1, procs
        call proc_wait(pid(i), status)
        call check(status == 0, 'every issue is stored')
    end do

    call store_open(path, db, stat)
    call db_prepare(db, 'SELECT count(*), count(DISTINCT number) FROM issues;', st, stat)
    call check(stmt_step(st) == SQLITE_ROW, 'issues counted')
    rows = int(stmt_int(st, 0))
    distinct = int(stmt_int(st, 1))
    call stmt_finalize(st)
    call db_close(db)
    call check(rows == procs*each .and. distinct == rows, 'numbers are distinct')

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_numbers
