program test_policy_limit
    !! unreviewed_limit holds against landings queued at the same moment by
    !! several processes, each with its own database connection as the
    !! server's workers have: no repository gets more than its limit lets in.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_policy, only: policy_t, policy_get, policy_put, policy_queue
    use fy_proc, only: proc_fork, proc_exit, proc_wait, proc_sleep_ms
    use fy_sqlite, only: db_t, stmt_t, db_close, db_exec, db_prepare, stmt_step, stmt_int, &
        stmt_finalize, SQLITE_ROW
    use fy_store, only: store_open, store_repo_add
    implicit none

    character(len=*), parameter :: path = 'build/test_policy_limit.db'
    character(len=*), parameter :: start = 'build/test_policy_limit.start'
    integer, parameter :: procs = 4, repos = 40
    type(db_t) :: db
    type(stmt_t) :: st
    type(policy_t) :: p
    character(len=:), allocatable :: refusal
    character(len=16) :: name, num
    integer(int64) :: id, landing
    integer :: stat, pid(procs), i, k, r, status, most, landings
    logical :: found

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm '//start)
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens')
    ! Each repository lets one unreviewed change in and has an open one for each process.
    do r = 1, repos
        write (name, '(a,i0)') 'r', r
        call store_repo_add(db, trim(name), '', stat, mode='primary')
        call policy_get(db, trim(name), p)
        p%unreviewed_limit = 1
        call policy_put(db, trim(name), p, found)
        do k = 1, procs
            write (num, '(i0)') k
            call db_exec(db, 'INSERT INTO changes (repo, number, change_key, branch, title,'// &
                " description, author, created_at, updated_at) VALUES ('"//trim(name)//"', "// &
                trim(num)//", 'k"//trim(num)//"', 'b"//trim(num)//"', 'T', '', 'admin', 'x', 'x');", &
                stat)
            call check(stat == 0, 'a change is written')
        end do
    end do
    call db_close(db)

    do i = 1, procs
        pid(i) = proc_fork()
        if (pid(i) == 0) then
            call store_open(path, db, stat)
            ! All start together.
            do
                inquire (file=start, exist=found)
                if (found) exit
            end do
            status = 0
            do r = 1, repos
                write (name, '(a,i0)') 'r', r
                ! The process's own change of the repository: ids count up
                ! through the repositories.
                id = int((r - 1)*procs + i, int64)
                call policy_queue(db, trim(name), 'b', 'admin', id, landing, refusal, stat)
                if (stat /= 0) status = 1
            end do
            call db_close(db)
            call proc_exit(status)
        end if
    end do
    call proc_sleep_ms(300)
    call execute_command_line('touch '//start)
    do i = 1, procs
        call proc_wait(pid(i), status)
        call check(status == 0, 'every request is answered')
    end do

    call store_open(path, db, stat)
    call db_prepare(db, 'SELECT max(n), sum(n) FROM (SELECT count(*) AS n FROM landings'// &
        ' GROUP BY repo);', st, stat)
    call check(stmt_step(st) == SQLITE_ROW, 'landings counted')
    most = int(stmt_int(st, 0))
    landings = int(stmt_int(st, 1))
    call stmt_finalize(st)
    call db_close(db)
    call check(most == 1, 'no repository takes more than its limit')
    call check(landings == repos, 'every repository takes a landing')

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_policy_limit
