module fy_stats
    !! Queue numbers (doc/inspection.md, "Analytics"; doc/parallel-development.md,
    !! "What is measured"): how fast and how reliably changes land, read on
    !! request from the timestamps of landings, checks and changes. Nothing is
    !! rolled up: a percentile is taken from the sorted durations of the window.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fx_json_build, only: &
        json_escape_string
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_prepare, &
        db_t, &
        stmt_bind_text, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t, &
        stmt_text
    implicit none
    private

    public :: &
        stats_days_ok, &
        stats_json

    !! Every query takes the repository as ?1 ('' for all of them) and the
    !! start of the window as ?2.
    character(len=*), parameter :: OF_REPO = " (?1 = '' OR repo = ?1)"
    character(len=*), parameter :: IN_WINDOW = OF_REPO//' AND finished_at >= ?2'
    character(len=*), parameter :: CHECKS_DONE = " FROM checks WHERE kind = 'check' AND"// &
        " state IN ('passed', 'failed') AND claimed_at IS NOT NULL AND"//IN_WINDOW//';'

contains

    pure logical function stats_days_ok(days)
        !! The windows the numbers are given for.
        integer, intent(in) :: days

        stats_days_ok = days == 1 .or. days == 7 .or. days == 30
    end function stats_days_ok

    function stats_json(db, repo, days) result(json)
        !! The numbers of repository `repo`, or of the instance for '', over
        !! the last `days` days. A measure is a member only when rows carry
        !! it: no landings give no time to main, not one of zero seconds.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        integer, intent(in) :: days
        character(len=:), allocatable :: json, since
        integer(int64), allocatable :: land(:), waits(:), runs(:), v(:)
        integer(int64) :: landed, failed, checks
        type(stmt_t) :: st
        character(len=16) :: window
        integer :: stat

        write (window, '(a,i0,a)') '-', days, ' days'
        since = ''
        call db_prepare(db, "SELECT strftime('%Y-%m-%dT%H:%M:%SZ', 'now', ?1);", st, stat)
        if (stat == SQLITE_OK) call stmt_bind_text(st, 1, trim(window), stat)
        if (stat == SQLITE_OK) then
            if (stmt_step(st) == SQLITE_ROW) since = stmt_text(st, 0)
        end if
        call stmt_finalize(st)

        json = '{"repo":"'//json_escape_string(repo)//'","days":'//int_text(int(days, int64))// &
            ',"since":"'//since//'"'

        ! An entry of the queue is a landing row; its time runs from fy land
        ! (the row is written) to main (it is finished as landed).
        land = ints('SELECT'//seconds('created_at', 'finished_at')//' FROM landings'// &
            " WHERE kind = 'landing' AND state = 'landed' AND"//IN_WINDOW//';', 1)
        call sort(land)
        landed = size(land)
        call add('landed', landed)
        if (landed > 0) then
            json = json//',"landings_per_hour":'//real_text(real(landed)/(24*days), 2)// &
                ',"landings_per_day":'//real_text(real(landed)/days, 1)
            call add('land_to_main_p50_seconds', percentile(land, 50))
            call add('land_to_main_p95_seconds', percentile(land, 95))
        end if

        v = counts("SELECT count(*) FROM landings WHERE kind = 'landing' AND state = 'failed'"// &
            ' AND'//IN_WINDOW//';', 1)
        failed = v(1)
        if (landed + failed > 0) then
            call add('failed', failed)
            json = json//',"failed_share":'//real_text(real(failed)/(landed + failed), 3)
        end if

        ! A change lands once, with its last entry, so it landed with its
        ! first one when it has no entry that did not land. Those are few,
        ! and read once for all landings.
        v = counts('SELECT count(*), coalesce(sum(change NOT IN (SELECT change FROM landings'// &
            " WHERE kind = 'landing' AND state <> 'landed' AND change IS NOT NULL)), 0)"// &
            " FROM landings WHERE kind = 'landing' AND state = 'landed' AND"// &
            ' change IS NOT NULL AND'//IN_WINDOW//';', 2)
        if (v(1) > 0) json = json//',"first_entry_share":'//real_text(real(v(2))/v(1), 3)

        ! The checks of landings; a check that finished was claimed by a
        ! runner. Those of verifications show under main.
        v = ints('SELECT'//seconds('created_at', 'claimed_at')//','// &
            seconds('claimed_at', 'finished_at')//CHECKS_DONE, 2)
        waits = v(1::2)
        runs = v(2::2)
        call sort(waits)
        call sort(runs)
        checks = size(runs)
        if (checks > 0) then
            call add('checks', checks)
            call add('check_wait_p50_seconds', percentile(waits, 50))
            call add('check_wait_p95_seconds', percentile(waits, 95))
            call add('check_run_p50_seconds', percentile(runs, 50))
            call add('check_run_p95_seconds', percentile(runs, 95))
            if (landed > 0) json = json//',"checks_per_landed_change":'// &
                real_text(real(checks)/landed, 2)
        end if

        v = counts("SELECT count(*), coalesce(sum(state = 'failed'), 0) FROM landings"// &
            " WHERE kind = 'verify' AND state IN ('passed', 'failed') AND"//IN_WINDOW//';', 2)
        if (v(1) > 0) then
            call add('verifications', v(1))
            call add('verifications_failed', v(2))
            if (landed > 0) json = json//',"failed_verifications_per_100_landings":'// &
                real_text(100*real(v(2))/landed, 1)
        end if

        ! A flag waits for a person however old it is, so these are all the
        ! landed changes, not those of the window.
        v = counts('SELECT count(*), coalesce(sum(flagged <> 0), 0) FROM changes'// &
            " WHERE state = 'landed' AND"//OF_REPO//' AND ?2 = ?2;', 2)
        if (v(1) > 0) call add('flagged_undecided', v(2))
        json = json//'}'

    contains

        function ints(sql, columns) result(values)
            !! The integers a query answers, row after row.
            character(len=*), intent(in) :: sql
            integer, intent(in) :: columns
            integer(int64), allocatable :: values(:), grown(:)
            integer :: n, k

            allocate (values(256))
            n = 0
            call db_prepare(db, sql, st, stat)
            if (stat == SQLITE_OK) call stmt_bind_text(st, 1, repo, stat)
            if (stat == SQLITE_OK) call stmt_bind_text(st, 2, since, stat)
            if (stat == SQLITE_OK) then
                do while (stmt_step(st) == SQLITE_ROW)
                    if (n + columns > size(values)) then
                        allocate (grown(2*size(values)))
                        grown(1:n) = values(1:n)
                        call move_alloc(grown, values)
                    end if
                    do k = 1, columns
                        values(n + k) = stmt_int(st, k - 1)
                    end do
                    n = n + columns
                end do
            end if
            call stmt_finalize(st)
            values = values(1:n)
        end function ints

        function counts(sql, columns) result(values)
            !! The one row of an aggregate; zeros when it could not be read.
            character(len=*), intent(in) :: sql
            integer, intent(in) :: columns
            integer(int64), allocatable :: values(:)

            values = ints(sql, columns)
            if (size(values) /= columns) then
                deallocate (values)
                allocate (values(columns), source=0_int64)
            end if
        end function counts

        subroutine add(name, value)
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: value

            json = json//',"'//name//'":'//int_text(value)
        end subroutine add

    end function stats_json

    pure function seconds(from, to) result(sql)
        !! SQL for the whole seconds between two timestamp columns. julianday
        !! reads a timestamp in a third of the time strftime('%s') takes.
        character(len=*), intent(in) :: from, to
        character(len=:), allocatable :: sql

        sql = ' CAST(round((julianday('//to//') - julianday('//from//')) * 86400) AS INTEGER)'
    end function seconds

    pure subroutine sort(a)
        !! Ascending, in place (Shell sort). SQLite's ORDER BY costs more
        !! than the query it sorts.
        integer(int64), intent(inout) :: a(:)
        integer(int64) :: x
        integer :: gap, i, j

        gap = size(a)/2
        do while (gap > 0)
            do i = gap + 1, size(a)
                x = a(i)
                j = i
                do while (j > gap)
                    if (a(j - gap) <= x) exit
                    a(j) = a(j - gap)
                    j = j - gap
                end do
                a(j) = x
            end do
            gap = gap/2
        end do
    end subroutine sort

    pure function percentile(sorted, p) result(value)
        !! The smallest of the sorted durations that at least `p` percent of
        !! them do not exceed (nearest rank), so it is one that occurred.
        integer(int64), intent(in) :: sorted(:)
        integer, intent(in) :: p
        integer(int64) :: value

        value = sorted(max(1, (size(sorted)*p + 99)/100))
    end function percentile

    pure function int_text(value) result(s)
        integer(int64), intent(in) :: value
        character(len=:), allocatable :: s
        character(len=24) :: b

        write (b, '(i0)') value
        s = trim(b)
    end function int_text

    pure function real_text(value, decimals) result(s)
        !! A JSON number with `decimals` places.
        real, intent(in) :: value
        integer, intent(in) :: decimals
        character(len=:), allocatable :: s
        character(len=32) :: b
        character(len=8) :: fmt

        write (fmt, '(a,i0,a)') '(f0.', decimals, ')'
        write (b, fmt) value
        s = trim(b)
        ! f0 leaves out the zero before the point, which JSON requires.
        if (s(1:1) == '.') s = '0'//s
    end function real_text

end module fy_stats
