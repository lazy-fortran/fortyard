module fy_dispatch
    !! Where a check runs (doc/backends.md "Dispatch"): the estimate
    !! eta(R, C) = wait(C) + run(R, C) of repository R on runner class C,
    !! computed from the rows of `checks` alone, and the queued checks a
    !! backend process may take. A check's duration is finished_at -
    !! started_at, its wait started_at - created_at; rows from before
    !! started_at and ran_class count with claimed_at and their home class.
    use, intrinsic :: iso_fortran_env, only: &
        int64, &
        real64
    use fx_json_build, only: &
        json_escape_string
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
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
        dispatch_backend, &
        dispatch_candidates, &
        dispatch_home, &
        dispatch_home_free, &
        dispatch_json, &
        dispatch_max_minutes, &
        dispatch_max_running, &
        dispatch_run, &
        dispatch_speed, &
        dispatch_wait

    !! No runner of the class, or none free soon.
    real(real64), parameter, public :: DISPATCH_NEVER = huge(1.0_real64)

    !! The class a check ran on.
    character(len=*), parameter :: RAN = 'coalesce(ran_class, class)'
    character(len=*), parameter :: VERDICT = "state IN ('passed', 'failed')"
    character(len=*), parameter :: SECONDS = "CAST(strftime('%s', finished_at) -"// &
        " strftime('%s', coalesce(started_at, claimed_at)) AS INTEGER)"
    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    !! A runner counts for wait(C) when it asked for a check this recently:
    !! longer than the 10 minutes an idle runner waits between its claims.
    character(len=*), parameter, public :: DISPATCH_SEEN = &
        "strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-900 seconds')"
    !! Candidates looked at per claim of a backend.
    integer, parameter :: CANDIDATES = 50

contains

    pure logical function dispatch_backend(class)
        !! A class served by a backend process rather than by own runners.
        character(len=*), intent(in) :: class

        dispatch_backend = index(class, 'github-') == 1 .or. index(class, 'gitlab-') == 1
    end function dispatch_backend

    pure function dispatch_home(class) result(home)
        !! The home class a backend class stands in for; '' for none.
        character(len=*), intent(in) :: class
        character(len=:), allocatable :: home

        home = ''
        if (class == 'github-linux' .or. index(class, 'gitlab-') == 1) home = 'linux'
        if (class == 'github-macos-arm64') home = 'macos-arm64'
    end function dispatch_home

    pure integer function dispatch_max_minutes(class) result(m)
        !! The time limit of a backend process that sends none.
        character(len=*), intent(in) :: class

        m = 360
        if (index(class, 'gitlab-') == 1) m = 15
    end function dispatch_max_minutes

    pure integer function dispatch_max_running(class) result(m)
        !! The concurrency limit of a backend process that sends none: GitHub's
        !! Free plan (20 jobs, 5 of them on macOS), one job on GitLab.
        character(len=*), intent(in) :: class

        m = 20
        if (class == 'github-macos-arm64') m = 5
        if (index(class, 'gitlab-') == 1) m = 1
    end function dispatch_max_running

    recursive function dispatch_run(db, repo, class, n) result(s)
        !! run(R, C) in seconds: the median duration of the last 10 checks of
        !! `repo` on `class` that ended with a verdict. With fewer than 3,
        !! run(R, linux) * speed(C) * 1.5, run(R, linux) by this same rule
        !! (speed(linux) = 1: the median of its 1 or 2 checks * 1.5). With no
        !! check on linux, the median of the checks on `class` * 1.5, and with
        !! none either, the repository's check time limit. `n`: the checks on
        !! `class` it rests on (fewer than 3: an estimate).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, class
        integer, intent(out), optional :: n
        real(real64) :: s
        real(real64), allocatable :: d(:), lin(:)

        call recent(db, repo, class, d)
        if (present(n)) n = size(d)
        if (size(d) >= 3) then
            s = median(d)
            return
        end if
        if (class /= 'linux') then
            call recent(db, repo, 'linux', lin)
            if (size(lin) > 0) then
                s = dispatch_run(db, repo, 'linux')*dispatch_speed(db, class)*1.5_real64
                return
            end if
        end if
        if (size(d) > 0) then
            s = median(d)*1.5_real64
        else
            s = time_limit(db, repo)
        end if
    end function dispatch_run

    function dispatch_speed(db, class) result(s)
        !! speed(C): the median, over repositories with at least 3 checks on
        !! both, of run(R, C) / run(R, linux); 1.5 when no repository has those.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: class
        real(real64) :: s
        real(real64), allocatable :: ratios(:), d(:), lin(:)
        character(len=:), allocatable :: repo
        type(stmt_t) :: st
        integer :: stat

        s = 1
        if (class == 'linux') return
        allocate (ratios(0))
        call db_prepare(db, 'SELECT repo FROM checks WHERE '//RAN//' = ?1 AND '//VERDICT// &
            ' GROUP BY repo HAVING count(*) >= 3;', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, class, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                repo = stmt_text(st, 0)
                call recent(db, repo, class, d)
                call recent(db, repo, 'linux', lin)
                if (size(d) < 3 .or. size(lin) < 3) cycle
                if (median(lin) <= 0) cycle
                ratios = [ratios, median(d)/median(lin)]
            end do
            call stmt_finalize(st)
        end if
        s = 1.5_real64
        if (size(ratios) > 0) s = median(ratios)
    end function dispatch_speed

    function dispatch_wait(db, class, check) result(w)
        !! wait(C) in seconds for the queued check `check` (0: one queued now).
        !! A backend class: the median wait of its checks that started in the
        !! last hour, else 60 s (GitHub) or 120 s (GitLab). Own runners: the
        !! run times of the queued checks of the class that the claim order
        !! gives out before `check`, divided by the runners of the class seen
        !! in the last 15 minutes or running a check; zero when one of them is
        !! free, DISPATCH_NEVER when there is none.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: class
        integer(int64), intent(in) :: check
        real(real64) :: w
        real(real64), allocatable :: d(:)
        character(len=64), allocatable :: repos(:)
        real(real64), allocatable :: runs(:)
        character(len=:), allocatable :: repo
        type(stmt_t) :: st
        integer :: stat, runners, k
        logical :: free

        w = 0
        if (dispatch_backend(class)) then
            allocate (d(0))
            call db_prepare(db, "SELECT CAST(strftime('%s', started_at) - strftime('%s',"// &
                " created_at) AS INTEGER) FROM checks WHERE ran_class = ?1 AND started_at >="// &
                " strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-3600 seconds');", st, stat)
            if (stat == SQLITE_OK) then
                call stmt_bind_text(st, 1, class, stat)
                do while (stmt_step(st) == SQLITE_ROW)
                    d = [d, real(max(0_int64, stmt_int(st, 0)), real64)]
                end do
                call stmt_finalize(st)
            end if
            if (size(d) > 0) then
                w = median(d)
            else if (index(class, 'gitlab-') == 1) then
                w = 120
            else
                w = 60
            end if
            return
        end if
        runners = 0
        free = .false.
        call db_prepare(db, "SELECT (SELECT count(*) FROM checks b WHERE b.state = 'running'"// &
            ' AND b.runner = r.name) FROM runners r WHERE r.class = ?1 AND (r.seen_at >= '// &
            DISPATCH_SEEN//" OR EXISTS (SELECT 1 FROM checks b WHERE b.state = 'running' AND"// &
            ' b.runner = r.name));', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, class, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                runners = runners + 1
                if (stmt_int(st, 0) == 0) free = .true.
            end do
            call stmt_finalize(st)
        end if
        if (runners == 0) then
            w = DISPATCH_NEVER
            return
        end if
        if (free) return
        allocate (repos(0), runs(0))
        ! The claim order of store_check_claim.
        call db_prepare(db, "SELECT c.id, c.repo FROM checks c WHERE c.state = 'queued' AND"// &
            " c.class = ?1 ORDER BY c.kind IN ('verify', 'nightly'), (SELECT count(*) FROM checks r WHERE"// &
            " r.state = 'running' AND r.repo = c.repo AND coalesce(r.ran_class, r.class) ="// &
            " c.class), c.id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, class, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            if (stmt_int(st, 0) == check) exit
            repo = stmt_text(st, 1)
            k = findloc(repos, repo, 1)
            if (k == 0) then
                repos = [character(len=64) :: repos, repo]
                runs = [runs, dispatch_run(db, repo, class)]
                k = size(repos)
            end if
            w = w + runs(k)
        end do
        call stmt_finalize(st)
        w = w/runners
    end function dispatch_wait

    subroutine dispatch_candidates(db, class, max_minutes, ids, race)
        !! The queued checks a backend process of `class` may take, in the
        !! claim order: those of its own class, and those of the home class it
        !! stands in for where 1.5 * run(R, class) fits `max_minutes` and
        !! eta(R, class) <= eta(R, home). Only of public repositories whose policy
        !! `backends` names the backend, and none whose twin still runs.
        !! `race(k)`: the backend is given a twin of check `ids(k)` and the
        !! home class keeps the check (doc/backends.md "Racing"), because the
        !! two estimates differ by less than 25 % of the larger, the home
        !! class has capacity (dispatch_home_free), the check has no twin yet
        !! and the backend is not GitLab's. Without `race` nothing is raced.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: class
        integer, intent(in) :: max_minutes
        integer(int64), allocatable, intent(out) :: ids(:)
        logical, allocatable, intent(out), optional :: race(:)
        character(len=:), allocatable :: home, family, repo, of
        real(real64) :: wait_b, run_b, eta_b, eta_h
        logical :: twinned, free, asked, raced
        type(stmt_t) :: st
        integer :: stat

        allocate (ids(0))
        if (present(race)) allocate (race(0))
        if (.not. dispatch_backend(class)) return
        home = dispatch_home(class)
        family = class(1:6)
        wait_b = -1
        free = .false.
        asked = .false.
        call db_prepare(db, 'SELECT c.id, c.repo, c.class, c.twin IS NOT NULL FROM checks c'// &
            " JOIN repos p ON p.name = c.repo WHERE c.state = 'queued' AND (c.class = ?1 OR"// &
            " c.class = ?2) AND p.public = 1 AND instr(',' || p.policy_backends || ',', ',' ||"// &
            " ?3 || ',') > 0 AND NOT EXISTS (SELECT 1 FROM checks t WHERE t.id = c.twin AND"// &
            " t.state IN ('queued', 'running'))"// &
            " ORDER BY c.kind IN ('verify', 'nightly'), (SELECT count(*) FROM checks r WHERE r.state ="// &
            " 'running' AND r.repo = c.repo AND coalesce(r.ran_class, r.class) = c.class), c.id"// &
            " LIMIT ?4;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, class, stat)
        call stmt_bind_text(st, 2, home, stat)
        call stmt_bind_text(st, 3, family, stat)
        call stmt_bind_int(st, 4, int(CANDIDATES, int64), stat)
        do while (stmt_step(st) == SQLITE_ROW)
            repo = stmt_text(st, 1)
            of = stmt_text(st, 2)
            twinned = stmt_int(st, 3) /= 0
            raced = .false.
            ! A check of the backend's own class was asked for by the policy; the
            ! job's time limit bounds it. Standing in, it must fit that limit.
            if (of /= class) then
                run_b = dispatch_run(db, repo, class)
                if (1.5_real64*run_b > 60.0_real64*max_minutes) cycle
                if (wait_b < 0) wait_b = dispatch_wait(db, class, 0_int64)
                eta_b = wait_b + run_b
                eta_h = eta_home(stmt_int(st, 0))
                if (present(race) .and. family /= 'gitlab' .and. .not. twinned .and. &
                    eta_h < DISPATCH_NEVER) then
                    if (abs(eta_b - eta_h) < 0.25_real64*max(eta_b, eta_h)) then
                        if (.not. asked) free = dispatch_home_free(db, home)
                        asked = .true.
                        raced = free
                    end if
                end if
                if (.not. raced .and. .not. eta_b <= eta_h) cycle
            end if
            ids = [ids, stmt_int(st, 0)]
            if (present(race)) race = [race, raced]
        end do
        call stmt_finalize(st)

    contains

        function eta_home(id) result(e)
            integer(int64), intent(in) :: id
            real(real64) :: e

            e = dispatch_wait(db, home, id)
            if (e < DISPATCH_NEVER) e = e + dispatch_run(db, repo, home)
        end function eta_home

    end subroutine dispatch_candidates

    function dispatch_home_free(db, home) result(free)
        !! Whether an own runner of class `home` has capacity for one more
        !! check: one seen lately is idle, or the check it runs has run for
        !! at least three quarters of its run(R, home), so it is about to be.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: home
        logical :: free
        type(stmt_t) :: st
        integer :: stat

        free = .false.
        call db_prepare(db, "SELECT coalesce(b.repo, ''), b.id IS NULL, coalesce(CAST(strftime("// &
            "'%s', 'now') - strftime('%s', coalesce(b.started_at, b.claimed_at)) AS INTEGER), 0)"// &
            " FROM runners r LEFT JOIN checks b ON b.state = 'running' AND b.runner = r.name"// &
            ' WHERE r.class = ?1 AND (r.seen_at >= '//DISPATCH_SEEN//' OR b.id IS NOT NULL);', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, home, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            if (stmt_int(st, 1) /= 0) then
                free = .true.
            else
                free = real(stmt_int(st, 2), real64) >= &
                    0.75_real64*dispatch_run(db, stmt_text(st, 0), home)
            end if
            if (free) exit
        end do
        call stmt_finalize(st)
    end function dispatch_home_free

    function dispatch_json(db) result(json)
        !! GET /api/v1/backends: "classes", one per runner class with its
        !! runners or backend processes seen in the last 15 minutes (or running
        !! a check), its running and queued checks, its concurrency limit and
        !! time limit (backends; null for own runners) and the median wait of
        !! its checks that started in the last hour (null: none); "estimates",
        !! run(R, C) in seconds per class and repository with the checks it
        !! rests on (fewer than 3: an estimate). A backend lists the
        !! repositories that may use it too.
        type(db_t), intent(in) :: db
        character(len=:), allocatable :: json, classes, estimates, names, class
        character(len=64), allocatable :: all(:)
        type(stmt_t) :: st
        integer :: stat, k, n, running, queued, limit, minutes
        real(real64) :: w

        allocate (all(0))
        call db_prepare(db, 'SELECT class FROM runners UNION SELECT class FROM checks WHERE'// &
            " state IN ('queued', 'running') UNION SELECT "//RAN//' FROM checks WHERE '// &
            VERDICT//' OR state = ''running'' ORDER BY 1;', st, stat)
        if (stat == SQLITE_OK) then
            do while (stmt_step(st) == SQLITE_ROW)
                all = [character(len=64) :: all, stmt_text(st, 0)]
            end do
            call stmt_finalize(st)
        end if
        classes = ''
        estimates = ''
        do k = 1, size(all)
            class = trim(all(k))
            names = ''
            limit = 0
            minutes = 0
            call db_prepare(db, 'SELECT name, coalesce(max_running, 0), coalesce(max_minutes,'// &
                ' 0) FROM runners r WHERE class = ?1 AND (seen_at >= '//DISPATCH_SEEN//' OR EXISTS'// &
                " (SELECT 1 FROM checks b WHERE b.state = 'running' AND b.runner = r.name))"// &
                ' ORDER BY name;', st, stat)
            if (stat == SQLITE_OK) then
                call stmt_bind_text(st, 1, class, stat)
                do while (stmt_step(st) == SQLITE_ROW)
                    if (len(names) > 0) names = names//', '
                    names = names//stmt_text(st, 0)
                    limit = max(limit, int(stmt_int(st, 1)))
                    minutes = max(minutes, int(stmt_int(st, 2)))
                end do
                call stmt_finalize(st)
            end if
            running = count_of("state = 'running' AND "//RAN//' = ?1', class)
            queued = count_of("state = 'queued' AND class = ?1", class)
            w = median_wait(class)
            if (len(classes) > 0) classes = classes//','
            classes = classes//'{"class":"'//json_escape_string(class)//'","backend":'// &
                trim(merge('true ', 'false', dispatch_backend(class)))//',"runners":"'// &
                json_escape_string(names)//'","running":'//itoa(running)//',"queued":'// &
                itoa(queued)//',"max_running":'//num_or_null(limit)//',"max_minutes":'// &
                num_or_null(minutes)//',"wait":'
            if (w < 0) then
                classes = classes//'null}'
            else
                classes = classes//itoa(nint(w))//'}'
            end if
            call add_estimates(class)
        end do
        json = '{"classes":['//classes//'],"estimates":['//estimates//']}'

    contains

        integer function count_of(where, value) result(c)
            character(len=*), intent(in) :: where, value
            type(stmt_t) :: q
            integer :: s

            c = 0
            call db_prepare(db, 'SELECT count(*) FROM checks WHERE '//where//';', q, s)
            if (s /= SQLITE_OK) return
            call stmt_bind_text(q, 1, value, s)
            if (stmt_step(q) == SQLITE_ROW) c = int(stmt_int(q, 0))
            call stmt_finalize(q)
        end function count_of

        function median_wait(of) result(m)
            character(len=*), intent(in) :: of
            real(real64) :: m
            real(real64), allocatable :: d(:)
            type(stmt_t) :: q
            integer :: s

            m = -1
            allocate (d(0))
            call db_prepare(db, "SELECT CAST(strftime('%s', coalesce(started_at, claimed_at)) -"// &
                " strftime('%s', created_at) AS INTEGER) FROM checks WHERE "//RAN//' = ?1 AND'// &
                ' coalesce(started_at, claimed_at) >='// &
                " strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-3600 seconds');", q, s)
            if (s /= SQLITE_OK) return
            call stmt_bind_text(q, 1, of, s)
            do while (stmt_step(q) == SQLITE_ROW)
                d = [d, real(max(0_int64, stmt_int(q, 0)), real64)]
            end do
            call stmt_finalize(q)
            if (size(d) > 0) m = median(d)
        end function median_wait

        subroutine add_estimates(of)
            character(len=*), intent(in) :: of
            type(stmt_t) :: q
            integer :: s
            character(len=:), allocatable :: repo, sql
            real(real64) :: r

            sql = 'SELECT DISTINCT repo FROM checks WHERE '//RAN//' = ?1 AND '//VERDICT
            if (dispatch_backend(of)) sql = sql//" UNION SELECT name FROM repos WHERE public = 1"// &
                " AND instr(',' || policy_backends || ',', ',' || ?2 || ',') > 0"
            call db_prepare(db, sql//' ORDER BY 1;', q, s)
            if (s /= SQLITE_OK) return
            call stmt_bind_text(q, 1, of, s)
            if (dispatch_backend(of)) call stmt_bind_text(q, 2, of(1:6), s)
            do while (stmt_step(q) == SQLITE_ROW)
                repo = stmt_text(q, 0)
                r = dispatch_run(db, repo, of, n)
                if (len(estimates) > 0) estimates = estimates//','
                estimates = estimates//'{"class":"'//json_escape_string(of)//'","repo":"'// &
                    json_escape_string(repo)//'","run":'//itoa(nint(r))//',"checks":'// &
                    itoa(n)//'}'
            end do
            call stmt_finalize(q)
        end subroutine add_estimates

        function num_or_null(v) result(s)
            integer, intent(in) :: v
            character(len=:), allocatable :: s

            s = 'null'
            if (v > 0) s = itoa(v)
        end function num_or_null

    end function dispatch_json

    subroutine recent(db, repo, class, d)
        !! Durations in seconds of the last 10 checks of `repo` on `class`
        !! that ended with a verdict, newest first.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, class
        real(real64), allocatable, intent(out) :: d(:)
        type(stmt_t) :: st
        integer :: stat

        allocate (d(0))
        call db_prepare(db, 'SELECT '//SECONDS//' FROM checks WHERE repo = ?1 AND '//RAN// &
            ' = ?2 AND '//VERDICT//' AND finished_at IS NOT NULL AND coalesce(started_at,'// &
            ' claimed_at) IS NOT NULL ORDER BY finished_at DESC, id DESC LIMIT 10;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, class, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            d = [d, real(max(0_int64, stmt_int(st, 0)), real64)]
        end do
        call stmt_finalize(st)
    end subroutine recent

    function time_limit(db, repo) result(s)
        !! The repository's check time limit in seconds; an hour without one.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        real(real64) :: s
        type(stmt_t) :: st
        integer :: stat

        s = 3600
        call db_prepare(db, 'SELECT policy_check_timeout FROM repos WHERE name = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            if (stmt_int(st, 0) > 0) s = 60*real(stmt_int(st, 0), real64)
        end if
        call stmt_finalize(st)
    end function time_limit

    pure function median(x) result(m)
        real(real64), intent(in) :: x(:)
        real(real64) :: m
        real(real64) :: s(size(x)), t
        integer :: i, j, n

        n = size(x)
        m = 0
        if (n == 0) return
        s = x
        do i = 2, n
            t = s(i)
            j = i - 1
            do while (j >= 1)
                if (s(j) <= t) exit
                s(j + 1) = s(j)
                j = j - 1
            end do
            s(j + 1) = t
        end do
        if (mod(n, 2) == 1) then
            m = s((n + 1)/2)
        else
            m = (s(n/2) + s(n/2 + 1))/2
        end if
    end function median

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b

        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_dispatch
