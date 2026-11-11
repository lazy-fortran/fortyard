program test_sqlite_threads
    !! Text columns read by two threads at once, as the server's threads read
    !! them, each on its own connection.
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_close, &
        db_open, &
        db_prepare, &
        db_t, &
        stmt_finalize, &
        stmt_step, &
        stmt_t, &
        stmt_text
    implicit none

    integer, parameter :: ROUNDS = 200000
    integer :: wrong_empty, wrong_long

    wrong_empty = 0
    wrong_long = 0
    !$omp parallel sections num_threads(2) default(shared)
    !$omp section
    call read_empty(wrong_empty)
    !$omp section
    call read_long(wrong_long)
    !$omp end parallel sections
    call check(wrong_empty == 0, 'an empty column stays empty while another thread reads')
    call check(wrong_long == 0, 'a long column keeps its text while another thread reads')

contains

    ! One routine per thread on purpose: gfortran keeps the length of a
    ! deferred-length function result in a static variable per call, so two
    ! threads in one routine would disturb each other here in the test.

    subroutine read_empty(wrong)
        integer, intent(out) :: wrong
        type(db_t) :: db
        type(stmt_t) :: st
        character(len=:), allocatable :: got
        integer :: k, stat

        wrong = 0
        call db_open(':memory:', db, stat)
        if (stat /= SQLITE_OK) wrong = 1
        do k = 1, ROUNDS
            call db_prepare(db, "SELECT '';", st, stat)
            if (stmt_step(st) /= SQLITE_ROW) wrong = wrong + 1
            got = stmt_text(st, 0)
            if (len(got) /= 0) wrong = wrong + 1
            call stmt_finalize(st)
        end do
        call db_close(db)
    end subroutine read_empty

    subroutine read_long(wrong)
        integer, intent(out) :: wrong
        character(len=*), parameter :: WORDS = 'a text of some length, as a title or a path has it'
        type(db_t) :: db
        type(stmt_t) :: st
        character(len=:), allocatable :: got
        integer :: k, stat

        wrong = 0
        call db_open(':memory:', db, stat)
        if (stat /= SQLITE_OK) wrong = 1
        do k = 1, ROUNDS
            call db_prepare(db, "SELECT '"//WORDS//"';", st, stat)
            if (stmt_step(st) /= SQLITE_ROW) wrong = wrong + 1
            got = stmt_text(st, 0)
            if (len(got) /= len(WORDS)) then
                wrong = wrong + 1
            else if (got /= WORDS) then
                wrong = wrong + 1
            end if
            call stmt_finalize(st)
        end do
        call db_close(db)
    end subroutine read_long

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_sqlite_threads
