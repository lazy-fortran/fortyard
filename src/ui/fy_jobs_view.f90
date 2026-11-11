module fy_jobs_view
    !! The jobs page: what runs and waits on the instance, in total and per
    !! repository, and the log of what has run: each job with its change, kind,
    !! state, duration, runner and commit, filtered through the address
    !! (?repo=&kind=&state=). A job's own page has its log. The page
    !! stays current through the server's event stream: each event fetches the
    !! jobs again, and a timer advances the durations of running jobs. A
    !! running job shows the last line its check wrote, which follows the
    !! events of the topic logs while the page shows running jobs, at most
    !! every TAILS_EVERY seconds.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fortui
    use fx_json_parse, only: &
        JSON_ARRAY_END, &
        JSON_ARRAY_START, &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_INTEGER, &
        JSON_KEY, &
        JSON_OBJECT_END, &
        JSON_OBJECT_START, &
        JSON_STRING, &
        json_event_t, &
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    use fy_fetch, only: &
        fetch_ready, &
        fetch_start, &
        request_t
    use fy_pages
    use fy_route, only: &
        at, &
        go, &
        query_value, &
        route_href, &
        route_t, &
        url_decode, &
        url_encode
    use fy_stream, only: &
        stream_close, &
        stream_count_of, &
        stream_open
    use fy_time, only: &
        ago, &
        duration, &
        epoch, &
        seconds_between, &
        seconds_since
    implicit none
    private

    public :: &
        jobs_view_compose, &
        jobs_view_hide, &
        jobs_view_receive, &
        jobs_view_show, &
        jobs_view_title

    type :: job_t
        integer :: id = 0
        character(len=:), allocatable :: repo, branch, account, state, queued, started, &
            finished, commit, kind, title, runner, log_tail
        !! The number of its change, and which of the change's entries it is.
        integer :: change = 0, entry = 0, entries = 0
    end type job_t

    type :: count_t
        character(len=:), allocatable :: name
        integer :: n(4) = 0
    end type count_t

    real, parameter :: BODY = 15.0, SMALL = 13.0, HEADING = 17.0, TITLE = 22.0
    character(len=*), parameter :: LABELS(4) = [character(len=13) :: 'Running', 'Waiting', &
        'Landed (24 h)', 'Failed (24 h)']

    !! The filters of the address: the values of kind= and state=, and the
    !! words their links show.
    character(len=*), parameter :: KINDS(3) = [character(len=7) :: '', 'landing', 'verify']
    character(len=*), parameter :: KIND_LABELS(3) = [character(len=13) :: 'all', 'entries', &
        'verifications']
    character(len=*), parameter :: STATES(6) = [character(len=7) :: '', 'queued', 'running', &
        'landed', 'passed', 'failed']
    character(len=*), parameter :: STATE_LABELS(6) = [character(len=7) :: 'all', 'queued', &
        'running', 'landed', 'passed', 'failed']

    type(request_t) :: list_req
    type(job_t), allocatable :: jobs(:)
    type(count_t), allocatable :: counts(:)
    integer :: totals(4) = 0, stream = 0, seen = 0, logs_seen = 0
    character(len=:), allocatable :: state, filter, only_kind, only_state
    !! The server refused the filters of the address: no job matches them.
    logical :: refused = .false.
    logical :: active = .false., loaded = .false., paused = .false.
    !! The last lines of running jobs are asked for at most every TAILS_EVERY
    !! seconds, however many checks write: when the list was last asked for
    !! them, and whether a check wrote since.
    integer, parameter :: TAILS_EVERY = 5
    integer(int64) :: tails_at = 0
    logical :: tails_due = .false.

contains

    subroutine jobs_view_show(r)
        !! Shows the jobs the address `r` asks for: of its repository, or of
        !! all when it names none, of its kind= and in its state=.
        type(route_t), intent(in) :: r
        if (.not. allocated(jobs)) allocate (jobs(0), counts(0))
        filter = r%repo
        only_kind = url_decode(query_value(r%query, 'kind'))
        only_state = url_decode(query_value(r%query, 'state'))
        if (only_state == 'all') only_state = ''
        active = .true.
        paused = .false.
        refused = .false.
        state = ''
        call request()
        if (stream == 0) then
            stream = stream_open('/api/v1/events?topic=jobs,logs', 'jobs,logs')
            seen = 0
            logs_seen = 0
        end if
    end subroutine jobs_view_show

    subroutine jobs_view_hide()
        !! The page is left: its event stream closes.
        active = .false.
        if (stream /= 0) call stream_close(stream)
        stream = 0
    end subroutine jobs_view_hide

    subroutine request()
        character(len=:), allocatable :: path
        path = '/api/v1'//address(filter, only_kind, only_state)
        ! The page lists the jobs of every state unless its address names one.
        if (len(only_state) == 0) then
            if (index(path, '?') > 0) then
                path = path//'&state=all'
            else
                path = path//'?state=all'
            end if
        end if
        call fetch_start(list_req, 'GET', path)
    end subroutine request

    subroutine jobs_view_receive()
        integer :: n

        ! An event of jobs means the jobs changed; one of logs, that a running
        ! check wrote more, which matters while a running job is shown.
        if (active .and. stream /= 0) then
            n = stream_count_of(stream, 'jobs')
            if (n /= seen) then
                seen = n
                call request()
            end if
            n = stream_count_of(stream, 'logs')
            if (n /= logs_seen) then
                logs_seen = n
                if (running()) tails_due = .true.
            end if
            if (tails_due .and. now_seconds() - tails_at >= TAILS_EVERY) then
                tails_due = .false.
                tails_at = now_seconds()
                call request()
            end if
        end if
        if (fetch_ready(list_req)) then
            if (list_req%status == 200) then
                call parse(list_req%body)
                loaded = .true.
                paused = .false.
                refused = .false.
            else if (list_req%status == 400) then
                refused = .true.
                paused = .false.
            else
                paused = .true.
            end if
        end if
    end subroutine jobs_view_receive

    function jobs_view_title() result(s)
        character(len=:), allocatable :: s
        s = 'Jobs'
        if (allocated(filter)) then
            if (len(filter) > 0) s = 'Jobs · '//filter
        end if
    end function jobs_view_title

    subroutine jobs_view_compose(narrow)
        logical, intent(in) :: narrow
        integer :: k

        if (.not. allocated(jobs)) allocate (jobs(0), counts(0))
        if (.not. allocated(filter)) filter = ''
        if (.not. allocated(only_kind)) only_kind = ''
        if (.not. allocated(only_state)) only_state = ''
        call row_begin(10.0, id="jobshead")
        call text(jobs_view_title(), size=TITLE, style=UI_HEADING, id="title")
        if (len(filter) > 0) then
            if (link("all", "All repositories", at(address('', only_kind, only_state)), &
                size=BODY)) call go(address('', only_kind, only_state))
        end if
        call text("", width=UI_FILL)
        if (paused) then
            call text("Updates paused", size=SMALL, tone=UI_TONE_NEGATIVE, id="livebadge")
        else
            call text("Live", size=SMALL, tone=UI_TONE_POSITIVE, id="livebadge")
        end if
        call row_end()
        if (refused) then
            call text("Jobs", size=HEADING, style=UI_HEADING, id="heading-jobs")
            call filters()
            call text("No jobs match.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        if (.not. loaded) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            return
        end if

        ! The four counts, as tiles.
        call row_begin(12.0, id="tiles")
        do k = 1, 4
            call column_begin(2.0, id="card-tile"//ui_str(k))
            call text(ui_str(tile(k)), size=TITLE, style=UI_HEADING, tone=tile_tone(k))
            call text(trim(LABELS(k)), size=SMALL, tone=UI_TONE_MUTED)
            call column_end()
        end do
        call row_end()

        if (len(filter) == 0 .and. size(counts) > 0) then
            call text("By repository", size=HEADING, style=UI_HEADING, id="heading-repos")
            call column_begin(0.0, id="card-jobrepos")
            do k = 1, size(counts)
                call row_begin(12.0, id="row-jr"//ui_str(k))
                if (link("repo", counts(k)%name, at(address(counts(k)%name, only_kind, &
                    only_state)), key=k, size=BODY, width=UI_FILL)) &
                    call go(address(counts(k)%name, only_kind, only_state))
                call text(summary(counts(k)%n), size=SMALL, tone=UI_TONE_MUTED)
                call row_end()
            end do
            call column_end()
        end if

        ! Durations of running and waiting jobs advance once a second.
        if (busy() .or. tails_due) call ui_wake_after(1.0)
        call text("Jobs", size=HEADING, style=UI_HEADING, id="heading-jobs")
        call filters()
        if (size(jobs) == 0) then
            call text("No jobs match.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        call column_begin(0.0, id="card-jobs")
        do k = 1, size(jobs)
            call job_row(k, narrow)
        end do
        call column_end()
    end subroutine jobs_view_compose

    subroutine filters()
        !! The filters as links: each leads to the address with that filter.
        integer :: k
        character(len=:), allocatable :: href

        call row_begin(8.0, id="jobfilters")
        do k = 1, size(KINDS)
            href = address(filter, trim(KINDS(k)), only_state)
            if (link("kind", trim(KIND_LABELS(k)), at(href), key=k, size=SMALL, &
                tone=merge(UI_TONE_STRONG, UI_TONE_LINK, only_kind == KINDS(k)))) call go(href)
        end do
        call text("·", size=SMALL, tone=UI_TONE_MUTED)
        do k = 1, size(STATES)
            href = address(filter, only_kind, trim(STATES(k)))
            if (link("state", trim(STATE_LABELS(k)), at(href), key=k, size=SMALL, &
                tone=merge(UI_TONE_STRONG, UI_TONE_LINK, only_state == STATES(k)))) call go(href)
        end do
        call row_end()
    end subroutine filters

    function address(repo, kind, only) result(path)
        !! The jobs page of `repo` with the filters `kind` and `only` (a
        !! state); an empty one is left out. The API has the same query.
        character(len=*), intent(in) :: repo, kind, only
        character(len=:), allocatable :: path

        path = ''
        if (len(repo) > 0) path = path//'&repo='//url_encode(repo)
        if (len(kind) > 0) path = path//'&kind='//url_encode(kind)
        if (len(only) > 0) path = path//'&state='//url_encode(only)
        if (len(path) > 0) path = '?'//path(2:)
        path = '/jobs'//path
    end function address

    subroutine job_row(k, narrow)
        integer, intent(in) :: k
        logical, intent(in) :: narrow
        character(len=:), allocatable :: what, when_text, href

        associate (j => jobs(k))
            call column_begin(4.0, id="row-j"//ui_str(j%id))
            call row_begin(10.0, id="jobline")
            call text(j%state, size=12.0, id="state", style=UI_HEADING, &
                tone=state_tone(j%state, j%entry == j%entries))
            href = route_href(R_JOB, '', ui_str(j%id))
            if (link("job", "job "//ui_str(j%id), href, key=j%id, size=BODY, style=UI_HEADING)) &
                call go(href)
            call text(kind_word(j%kind), size=SMALL, tone=UI_TONE_MUTED)
            if (j%change > 0) then
                ! The row leads to the change the job served.
                href = route_href(R_CHANGE, j%repo, ui_str(j%change))
                if (link("change", j%repo//' #'//ui_str(j%change)//' '//j%title, href, key=j%id, &
                    size=BODY, width=UI_FILL, ellipsis=.not. narrow)) call go(href)
            else
                call text(j%repo//' · '//j%branch, size=BODY, width=UI_FILL, ellipsis=.not. narrow)
            end if
            call row_end()
            select case (j%state)
            case ('running')
                when_text = 'running for '//duration(seconds_since(j%started))
            case ('queued')
                when_text = 'waiting for '//duration(seconds_since(j%queued))
            case default
                when_text = j%state//' '//ago(j%finished)
                if (len(j%started) > 0) when_text = when_text//' after '// &
                    duration(seconds_between(j%started, j%finished))
            end select
            what = j%account//' · '//when_text
            if (j%entries > 1) what = what//' · entry '//ui_str(j%entry)//' of '//ui_str(j%entries)
            if (len(j%runner) > 0) what = what//' · runner '//j%runner
            call row_begin(8.0, id="jobmeta")
            call text(what, size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL)
            if (len(j%commit) >= 7) then
                if (link("commit", j%commit(1:7), route_href(R_COMMIT, j%repo, j%commit), key=j%id, &
                    size=SMALL, style=UI_CODE)) call go(route_href(R_COMMIT, j%repo, j%commit))
            end if
            call row_end()
            if (j%state == 'running' .and. len(j%log_tail) > 0) call text(j%log_tail, size=SMALL, &
                style=UI_CODE, tone=UI_TONE_MUTED, width=UI_FILL, ellipsis=.true., id="logtail")
            call column_end()
        end associate
    end subroutine job_row

    integer(int64) function now_seconds()
        integer :: v(8)
        call date_and_time(values=v)
        now_seconds = epoch(v(1), v(2), v(3), v(5), v(6), v(7))
    end function now_seconds

    logical function running()
        integer :: k
        running = .false.
        do k = 1, size(jobs)
            if (jobs(k)%state == 'running') running = .true.
        end do
    end function running

    logical function busy()
        integer :: k
        busy = .false.
        do k = 1, size(jobs)
            if (jobs(k)%state == 'running' .or. jobs(k)%state == 'queued') busy = .true.
        end do
    end function busy

    integer function tile(k)
        integer, intent(in) :: k
        tile = totals(k)
        if (len(filter) == 0) return
        tile = 0
        block
            integer :: i
            do i = 1, size(counts)
                if (counts(i)%name == filter) tile = counts(i)%n(k)
            end do
        end block
    end function tile

    integer function tile_tone(k)
        integer, intent(in) :: k
        select case (k)
        case (1); tile_tone = merge(UI_TONE_ACCENT, UI_TONE_NORMAL, tile(1) > 0)
        case (3); tile_tone = UI_TONE_POSITIVE
        case (4); tile_tone = merge(UI_TONE_NEGATIVE, UI_TONE_NORMAL, tile(4) > 0)
        case default; tile_tone = UI_TONE_NORMAL
        end select
    end function tile_tone

    function summary(n) result(s)
        integer, intent(in) :: n(4)
        character(len=:), allocatable :: s
        s = ui_str(n(1))//' running · '//ui_str(n(2))//' waiting · '//ui_str(n(3))// &
            ' landed · '//ui_str(n(4))//' failed'
    end function summary

    function kind_word(k) result(s)
        !! What a job of kind `k` is called.
        character(len=*), intent(in) :: k
        character(len=:), allocatable :: s
        select case (k)
        case ('landing'); s = 'entry'
        case ('verify'); s = 'verification'
        case default; s = k
        end select
    end function kind_word

    integer function state_tone(s, newest)
        !! Settled states are plain, work in motion has the accent, and a
        !! failure is red while nothing newer of its change replaced it.
        character(len=*), intent(in) :: s
        logical, intent(in) :: newest
        select case (s)
        case ('landed', 'passed'); state_tone = UI_TONE_NORMAL
        case ('queued', 'running'); state_tone = UI_TONE_ACCENT
        case ('failed'); state_tone = merge(UI_TONE_NEGATIVE, UI_TONE_MUTED, newest)
        case default; state_tone = UI_TONE_MUTED
        end select
    end function state_tone

    subroutine parse(body)
        !! {"state":..,"repos":[{name,running,queued,landed,failed}],
        !!  "totals":{...},"jobs":[{id,kind,repo,branch,...,change,title,entry,
        !!  entries,runner,checks:[...]}]}; a job's checks are read on its page.
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key, section
        type(job_t) :: j
        type(count_t) :: c
        integer :: depth, slot

        deallocate (jobs, counts)
        allocate (jobs(0), counts(0))
        totals = 0
        key = ''
        section = ''
        depth = 0
        call reset_job()
        c%name = ''
        c%n = 0
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START)
                depth = depth + 1
                if (depth == 2 .and. key == 'totals') section = 'totals'
            case (JSON_ARRAY_START)
                if (depth == 1) section = key
            case (JSON_ARRAY_END)
                if (depth == 1) section = ''
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                if (depth == 1 .and. key == 'state') state = ev%string_val
                if (section == 'repos' .and. key == 'name') c%name = ev%string_val
                if (section == 'jobs' .and. depth == 2) then
                    select case (key)
                    case ('kind'); j%kind = ev%string_val
                    case ('title'); j%title = ev%string_val
                    case ('runner'); j%runner = ev%string_val
                    case ('log_tail'); j%log_tail = ev%string_val
                    case ('repo'); j%repo = ev%string_val
                    case ('branch'); j%branch = ev%string_val
                    case ('account'); j%account = ev%string_val
                    case ('state'); j%state = ev%string_val
                    case ('queued'); j%queued = ev%string_val
                    case ('started'); j%started = ev%string_val
                    case ('finished'); j%finished = ev%string_val
                    case ('commit'); j%commit = ev%string_val
                    end select
                end if
            case (JSON_INTEGER)
                slot = findloc([character(len=7) :: 'running', 'queued', 'landed', 'failed'], &
                    key, 1)
                if (section == 'repos' .and. slot > 0) c%n(slot) = int(ev%int_val)
                if (section == 'totals' .and. slot > 0) totals(slot) = int(ev%int_val)
                if (section == 'jobs' .and. depth == 2) then
                    select case (key)
                    case ('id'); j%id = int(ev%int_val)
                    case ('change'); j%change = int(ev%int_val)
                    case ('entry'); j%entry = int(ev%int_val)
                    case ('entries'); j%entries = int(ev%int_val)
                    end select
                end if
            case (JSON_OBJECT_END)
                if (section == 'repos' .and. len(c%name) > 0) then
                    counts = [counts, c]
                    c%name = ''
                    c%n = 0
                else if (section == 'jobs' .and. depth == 2 .and. j%id > 0) then
                    jobs = [jobs, j]
                    call reset_job()
                else if (section == 'totals') then
                    section = ''
                end if
                depth = depth - 1
            end select
        end do

    contains

        subroutine reset_job()
            j%id = 0
            j%repo = ''
            j%branch = ''
            j%account = ''
            j%state = ''
            j%queued = ''
            j%started = ''
            j%finished = ''
            j%commit = ''
            j%kind = ''
            j%title = ''
            j%runner = ''
            j%log_tail = ''
            j%change = 0
            j%entry = 0
            j%entries = 0
        end subroutine reset_job

    end subroutine parse

end module fy_jobs_view
