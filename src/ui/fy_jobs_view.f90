module fy_jobs_view
    !! The jobs page: what runs and waits on the instance, in total and per
    !! repository, with recent results and the log of a chosen job. The page
    !! stays current through the server's event stream: each event fetches the
    !! jobs again, and a timer advances the durations of running jobs.
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
        json_extract_string, &
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
        route_href, &
        url_encode
    use fy_stream, only: &
        stream_close, &
        stream_count, &
        stream_open
    use fy_time, only: &
        ago, &
        duration, &
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
            finished, commit
    end type job_t

    type :: count_t
        character(len=:), allocatable :: name
        integer :: n(4) = 0
    end type count_t

    real, parameter :: BODY = 15.0, SMALL = 13.0, HEADING = 17.0, TITLE = 22.0
    character(len=*), parameter :: LABELS(4) = [character(len=13) :: 'Running', 'Waiting', &
        'Landed (24 h)', 'Failed (24 h)']

    type(request_t) :: list_req, log_req
    type(job_t), allocatable :: jobs(:)
    type(count_t), allocatable :: counts(:)
    integer :: totals(4) = 0, open_id = 0, stream = 0, seen = 0
    character(len=:), allocatable :: state, filter, open_log
    logical :: active = .false., loaded = .false., paused = .false.

contains

    subroutine jobs_view_show(repo)
        !! Shows the jobs of `repo`, or of all repositories when it is empty.
        character(len=*), intent(in) :: repo
        if (.not. allocated(jobs)) allocate (jobs(0), counts(0))
        filter = repo
        active = .true.
        paused = .false.
        open_id = 0
        open_log = ''
        state = ''
        call request()
        if (stream == 0) then
            stream = stream_open('/api/v1/events?topic=jobs', 'jobs')
            seen = 0
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
        path = '/api/v1/jobs'
        if (len(filter) > 0) path = path//'?repo='//url_encode(filter)
        call fetch_start(list_req, 'GET', path)
    end subroutine request

    subroutine jobs_view_receive()
        logical :: found
        integer :: k, n

        ! Each event of the stream means the jobs changed.
        if (active .and. stream /= 0) then
            n = stream_count(stream)
            if (n /= seen) then
                seen = n
                call request()
            end if
        end if
        if (fetch_ready(list_req)) then
            if (list_req%status == 200) then
                call parse(list_req%body)
                loaded = .true.
                ! A running job's log grows: fetch it again with each change.
                do k = 1, size(jobs)
                    if (jobs(k)%id == open_id) call fetch_start(log_req, 'GET', &
                        '/api/v1/jobs/'//ui_str(open_id))
                end do
            else
                paused = .true.
            end if
        end if
        if (fetch_ready(log_req)) then
            if (log_req%status == 200) then
                call json_extract_string(log_req%body, 'log', open_log, found)
                if (.not. found) open_log = ''
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
        call row_begin(10.0, id="jobshead")
        call text(jobs_view_title(), size=TITLE, style=UI_HEADING, id="title")
        if (len(filter) > 0) then
            if (link("all", "All repositories", at('/jobs'), size=BODY)) call go('/jobs')
        end if
        call text("", width=UI_FILL)
        if (paused) then
            call text("Updates paused", size=SMALL, tone=UI_TONE_NEGATIVE, id="livebadge")
        else
            call text("Live", size=SMALL, tone=UI_TONE_POSITIVE, id="livebadge")
        end if
        call row_end()
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
                if (link("repo", counts(k)%name, at('/jobs?repo='//url_encode(counts(k)%name)), &
                    key=k, size=BODY, width=UI_FILL)) call go('/jobs?repo='// &
                    url_encode(counts(k)%name))
                call text(summary(counts(k)%n), size=SMALL, tone=UI_TONE_MUTED)
                call row_end()
            end do
            call column_end()
        end if

        ! Durations of running and waiting jobs advance once a second.
        if (busy()) call ui_wake_after(1.0)
        call text("Jobs", size=HEADING, style=UI_HEADING, id="heading-jobs")
        if (size(jobs) == 0) then
            call text("No jobs yet.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        call column_begin(0.0, id="card-jobs")
        do k = 1, size(jobs)
            call job_row(k, narrow)
        end do
        call column_end()
    end subroutine jobs_view_compose

    subroutine job_row(k, narrow)
        integer, intent(in) :: k
        logical, intent(in) :: narrow
        character(len=:), allocatable :: what, when_text

        associate (j => jobs(k))
            what = j%repo//' · '//j%branch
            call column_begin(4.0, id="row-j"//ui_str(j%id))
            call row_begin(10.0, id="jobline")
            call text(j%state, size=12.0, id="badge", tone=state_tone(j%state))
            if (link("job", what, at('/jobs?repo='//url_encode(j%repo)), key=j%id, size=BODY, &
                style=UI_HEADING, width=UI_FILL, ellipsis=.not. narrow)) call toggle(j%id)
            if (.not. narrow) call text("#"//ui_str(j%id), size=SMALL, style=UI_CODE, &
                tone=UI_TONE_MUTED)
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
            call row_begin(8.0, id="jobmeta")
            call text(j%account//' · '//when_text, size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL)
            if (len(j%commit) >= 7) then
                if (link("commit", j%commit(1:7), route_href(R_COMMIT, j%repo, j%commit), key=j%id, &
                    size=SMALL, style=UI_CODE)) call go(route_href(R_COMMIT, j%repo, j%commit))
            end if
            call row_end()
            if (open_id == j%id) then
                call column_begin(0.0, id="joblog")
                call log_lines(open_log)
                call column_end()
            end if
            call column_end()
        end associate
    end subroutine job_row

    logical function busy()
        integer :: k
        busy = .false.
        do k = 1, size(jobs)
            if (jobs(k)%state == 'running' .or. jobs(k)%state == 'queued') busy = .true.
        end do
    end function busy

    subroutine toggle(id)
        !! Opens the log of job `id`, or closes it when it is open.
        integer, intent(in) :: id
        if (open_id == id) then
            open_id = 0
            open_log = ''
            return
        end if
        open_id = id
        open_log = 'Loading...'
        call fetch_start(log_req, 'GET', '/api/v1/jobs/'//ui_str(id))
    end subroutine toggle

    subroutine log_lines(log)
        !! The log as code lines; progress bars of builds are left out.
        character(len=*), intent(in) :: log
        integer :: pos, nl, shown
        character(len=:), allocatable :: line

        pos = 1
        shown = 0
        do while (pos <= len(log) .and. shown < 400)
            nl = index(log(pos:), new_line('a'))
            if (nl == 0) then
                line = log(pos:)
                pos = len(log) + 1
            else
                line = log(pos:pos + nl - 2)
                pos = pos + nl
            end if
            if (index(line, '[') > 0 .and. index(line, '%') == len(line)) cycle
            shown = shown + 1
            call text(line, style=UI_CODE, size=12.5, width=UI_FILL)
        end do
    end subroutine log_lines

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

    integer function state_tone(s)
        character(len=*), intent(in) :: s
        select case (s)
        case ('landed'); state_tone = UI_TONE_POSITIVE
        case ('failed'); state_tone = UI_TONE_NEGATIVE
        case ('running'); state_tone = UI_TONE_ACCENT
        case default; state_tone = UI_TONE_MUTED
        end select
    end function state_tone

    subroutine parse(body)
        !! {"state":..,"repos":[{name,running,queued,landed,failed}],
        !!  "totals":{...},"jobs":[{id,kind,repo,branch,...}]}
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
                if (section == 'jobs') then
                    select case (key)
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
                if (section == 'jobs' .and. key == 'id') j%id = int(ev%int_val)
            case (JSON_OBJECT_END)
                if (section == 'repos' .and. len(c%name) > 0) then
                    counts = [counts, c]
                    c%name = ''
                    c%n = 0
                else if (section == 'jobs' .and. j%id > 0) then
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
        end subroutine reset_job

    end subroutine parse

end module fy_jobs_view
