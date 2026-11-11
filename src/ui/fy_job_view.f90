module fy_job_view
    !! The page of one job, /jobs/ID: its change and outcome, the entry that
    !! followed it for the same change, its checks, and the whole stored log
    !! as flat text in which every line has an address (/jobs/ID#L812). The
    !! page shows what GET /api/v1/jobs/ID answers and stays current through
    !! the server's event stream, as the jobs page does. While a check of the
    !! job runs, its log grows at the end as the check writes it (GET
    !! /api/v1/jobs/ID/log?from=N on each event of the topic log-ID), the
    !! view follows the end while the reader is there, and of a long log the
    !! page keeps the first and the last lines.
    use fortui
    use fx_json_parse, only: &
        JSON_ARRAY_END, &
        JSON_ARRAY_START, &
        JSON_BOOL, &
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
    use fy_code_lines, only: &
        right
    use fy_fetch, only: &
        fetch_ready, &
        fetch_start, &
        request_t
    use fy_nav, only: &
        nav_at_end, &
        nav_push, &
        nav_scroll_end
    use fy_pages
    use fy_route, only: &
        at, &
        go, &
        route_href
    use fy_stream, only: &
        stream_close, &
        stream_count_of, &
        stream_open
    use fy_time, only: &
        duration, &
        seconds_between, &
        seconds_since
    implicit none
    private

    public :: &
        job_view_compose, &
        job_view_hide, &
        job_view_missing, &
        job_view_receive, &
        job_view_show, &
        job_view_title

    type :: check_row_t
        integer :: id = 0, attempt = 0, wait = -1, run = -1
        character(len=:), allocatable :: state, runner
    end type check_row_t

    real, parameter :: BODY = 14.0, SMALL = 12.0, HEADING = 16.0, TITLE = 20.0, CODE = 12.0

    type(request_t) :: req, log_req
    type(check_row_t), allocatable :: checks(:)
    !! First and last byte of each line of the log, and its number (0: the
    !! line that stands for lines left out).
    integer, allocatable :: line_from(:), line_to(:), line_no(:)
    character(len=:), allocatable :: job_number, job_kind, repo, branch, account, state, queued, &
        started, finished, commit, change_title, next_state, log_text, stored_log
    integer :: change = 0, entry = 0, entries = 0, next = 0, error_line = 0
    integer :: stream = 0, seen = 0, logs_seen = 0
    !! The job whose event stream is open, and the job the pending log
    !! request is for.
    character(len=:), allocatable :: stream_job, log_job
    logical :: active = .false., loaded = .false., absent = .false., paused = .false.
    !! The live log of a running check: the length the job names, the offset
    !! to ask for next, whether the view follows the end, whether the log grew
    !! in this frame, and whether another request waits for the pending one.
    integer :: live_length = -1, live_next = 0
    !! Names the live log shown ("begun" of its answers), which changes when
    !! the log starts again.
    character(len=:), allocatable :: live_begun
    logical :: live = .false., following = .true., grew = .false., log_again = .false.
    !! Of a live log the page keeps its first lines (head, head_count of them)
    !! and its last (tail, from line tail_first on), so that a check that
    !! writes much does not grow the page; the lines between are counted. The
    !! first line with Error or FAIL is found as the text arrives: `carry` is
    !! the end of the text so far, and carry_line the line it starts on.
    integer, parameter :: HEAD_LINES = 100, TAIL_LINES = 400, HEAD_BYTES = 32768, &
        TAIL_BYTES = 65536
    character(len=:), allocatable :: head, tail, carry
    integer :: head_count = 0, tail_first = 1, carry_line = 1, live_error = 0
    logical :: in_tail = .false.

contains

    subroutine job_view_show(id)
        !! Shows job `id` (its number as in the address).
        character(len=*), intent(in) :: id
        if (.not. allocated(checks)) allocate (checks(0), line_from(0), line_to(0), line_no(0))
        if (.not. allocated(job_number)) job_number = ''
        ! The same job again keeps what is shown until the answer is there.
        if (job_number /= id) then
            loaded = .false.
            live = .false.
            following = .true.
            call live_reset()
        end if
        job_number = id
        active = .true.
        absent = .false.
        paused = .false.
        call fetch_start(req, 'GET', '/api/v1/jobs/'//job_number)
        ! The stream names this job's live log, so that the page hears of
        ! its growth only.
        if (stream /= 0) then
            if (stream_job /= job_number) then
                call stream_close(stream)
                stream = 0
            end if
        end if
        if (stream == 0) then
            stream = stream_open('/api/v1/events?topic=jobs,log-'//job_number, &
                'jobs,log-'//job_number)
            stream_job = job_number
            seen = 0
            logs_seen = 0
        end if
    end subroutine job_view_show

    subroutine job_view_hide()
        !! The page is left: its event stream closes.
        active = .false.
        if (stream /= 0) call stream_close(stream)
        stream = 0
    end subroutine job_view_hide

    subroutine job_view_receive()
        integer :: n

        ! An event of jobs means that a job changed, one of log-N that the
        ! live log of this job grew.
        if (active .and. stream /= 0) then
            n = stream_count_of(stream, 'jobs')
            if (n /= seen) then
                seen = n
                call fetch_start(req, 'GET', '/api/v1/jobs/'//job_number)
            end if
            n = stream_count_of(stream, 'log-'//job_number)
            if (n /= logs_seen) then
                logs_seen = n
                if (loaded .and. state == 'running') call request_log()
            end if
        end if
        if (fetch_ready(req)) then
            if (req%status == 200) then
                call parse(req%body)
                loaded = .true.
                paused = .false.
            else if (req%status == 404) then
                absent = .true.
            else
                paused = .true.
            end if
        end if
        if (fetch_ready(log_req)) then
            if (log_req%status == 200) call apply_log(log_req%body)
            if (log_again) then
                log_again = .false.
                call request_log()
            end if
        end if
    end subroutine job_view_receive

    function job_view_title() result(s)
        character(len=:), allocatable :: s
        s = 'Job'
        if (allocated(job_number)) s = 'Job '//job_number
    end function job_view_title

    logical function job_view_missing()
        !! Whether there is no job of this number.
        job_view_missing = absent
    end function job_view_missing

    subroutine job_view_compose()
        integer :: k
        character(len=:), allocatable :: href, what

        if (.not. allocated(job_number)) job_number = ''
        if (absent) then
            call text("Job not found", size=TITLE, style=UI_HEADING, id="title")
            call text("There is no job "//job_number//".", size=BODY, tone=UI_TONE_MUTED)
            if (link("back", "All jobs", at('/jobs'), size=BODY)) call go('/jobs')
            return
        end if
        if (.not. loaded) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            return
        end if

        ! The header line: the job, its change and its outcome.
        call row_begin(8.0, id="jobhead")
        call text("job "//job_number, size=TITLE, style=UI_HEADING, id="title")
        call text(kind_word(job_kind)//" of", size=BODY, tone=UI_TONE_MUTED)
        if (change > 0) then
            href = route_href(R_CHANGE, repo, ui_str(change))
            if (link("change", repo//' #'//ui_str(change)//' '//change_title, href, size=BODY)) &
                call go(href)
        else
            call text(repo//' · '//branch, size=BODY, id="subject")
        end if
        call text(state, size=BODY, style=UI_HEADING, tone=state_tone(state, entry == entries), &
            id="state")
        if (entries > 1) call text("entry "//ui_str(entry)//" of "//ui_str(entries), size=BODY, &
            id="entry")
        select case (state)
        case ('running')
            what = 'running for '//duration(seconds_since(started))
        case ('queued')
            what = 'waiting for '//duration(seconds_since(queued))
        case default
            what = ''
            if (len(started) > 0) what = 'after '//duration(seconds_between(started, finished))
        end select
        if (len(what) > 0) call text(what, size=BODY, tone=UI_TONE_MUTED, id="took")
        call text("", width=UI_FILL)
        if (paused) then
            call text("Updates paused", size=SMALL, tone=UI_TONE_NEGATIVE, id="livebadge")
        else
            call text("Live", size=SMALL, tone=UI_TONE_POSITIVE, id="livebadge")
        end if
        call row_end()
        ! Durations of a running or waiting job advance once a second.
        if (state == 'running' .or. state == 'queued') call ui_wake_after(1.0)

        if (next > 0) then
            call row_begin(6.0, id="jobnext")
            call text("next entry of this change:", size=BODY, tone=UI_TONE_MUTED)
            href = route_href(R_JOB, '', ui_str(next))
            if (link("next", "job "//ui_str(next)//", "//next_state, href, size=BODY)) &
                call go(href)
            call row_end()
        end if

        call row_begin(8.0, id="jobtimes")
        what = 'queued '//clock(queued)//' by '//account
        if (len(started) > 0) what = what//' · started '//clock(started)
        if (len(finished) > 0) what = what//' · finished '//clock(finished)
        call text(what//' UTC', size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL, id="times")
        if (len(commit) >= 7) then
            href = route_href(R_COMMIT, repo, commit)
            if (link("commit", commit(1:7), href, size=SMALL, style=UI_CODE)) call go(href)
        end if
        call row_end()

        call text("Checks", size=HEADING, style=UI_HEADING, id="heading-checks", anchor="checks")
        if (size(checks) == 0) then
            call text("No check is recorded for this job.", size=BODY, tone=UI_TONE_MUTED, &
                id="nochecks")
        else
            call column_begin(0.0, id="card-checks")
            do k = 1, size(checks)
                call check_row(checks(k))
            end do
            call column_end()
        end if

        call row_begin(8.0, id="loghead")
        call text("Log", size=HEADING, style=UI_HEADING, id="heading-log", anchor="log")
        if (last_line() == 1) then
            call text("1 line", size=SMALL, tone=UI_TONE_MUTED, id="lines")
        else if (last_line() > 1) then
            call text(ui_str(last_line())//" lines", size=SMALL, tone=UI_TONE_MUTED, id="lines")
        end if
        if (error_line > 0) then
            call text("· first line with Error or FAIL:", size=SMALL, tone=UI_TONE_MUTED)
            href = route_href(R_JOB, '', job_number)//'#L'//ui_str(error_line)
            if (link("errorline", ui_str(error_line), href, size=SMALL)) then
                ! The same page: the browser goes to the line and marks it.
                call nav_push('/jobs/'//job_number//'#L'//ui_str(error_line))
            end if
        end if
        if (live) then
            ! Scrolling up stops following; the reader is asked once a frame,
            ! except in the frame in which the log grew.
            if (.not. grew .and. size(line_from) > 0) following = nav_at_end()
            grew = .false.
            call text("· live", size=SMALL, tone=UI_TONE_POSITIVE, id="livelog")
            if (following) then
                call text("· follows the end", size=SMALL, tone=UI_TONE_MUTED, id="following")
            else
                href = route_href(R_JOB, '', job_number)//'#L'//ui_str(last_line())
                if (link("follow", "follow", href, size=SMALL)) then
                    following = .true.
                    call nav_scroll_end()
                end if
            end if
        end if
        call row_end()
        if (size(line_from) == 0) then
            if (live) then
                call text("The log appears here as the check writes it.", size=BODY, &
                    tone=UI_TONE_MUTED, id="nolog")
            else if (state == 'running' .or. state == 'queued') then
                call text("The log is stored when the job ends.", size=BODY, tone=UI_TONE_MUTED, &
                    id="nolog")
            else
                call text("No log is stored for this job.", size=BODY, tone=UI_TONE_MUTED, &
                    id="nolog")
            end if
            return
        end if
        call column_begin(0.0, id="joblog")
        do k = 1, size(line_from)
            call log_line(k)
        end do
        call column_end()
    end subroutine job_view_compose

    subroutine check_row(c)
        type(check_row_t), intent(in) :: c
        character(len=:), allocatable :: took

        call row_begin(12.0, id="row-k"//ui_str(c%id))
        call text("check "//ui_str(c%id), size=BODY, style=UI_HEADING, id="check")
        call text("attempt "//ui_str(c%attempt), size=BODY, tone=UI_TONE_MUTED, id="attempt")
        call text(c%state, size=BODY, tone=state_tone(c%state, .true.), id="checkstate")
        took = ''
        if (c%run >= 0) took = duration(c%run)
        if (c%wait >= 0) then
            if (len(took) > 0) took = took//' after '
            took = took//duration(c%wait)//' wait'
        end if
        if (len(took) > 0) call text(took, size=BODY, tone=UI_TONE_MUTED, id="took")
        if (len(c%runner) > 0) call text("runner "//c%runner, size=BODY, tone=UI_TONE_MUTED, &
            id="runner")
        call row_end()
    end subroutine check_row

    subroutine log_line(k)
        !! The k-th line shown with its number n as a muted run; its address is
        !! #Ln. The line that stands for lines left out has neither.
        integer, intent(in) :: k
        character(len=:), allocatable :: prefix, line
        integer :: tone, n

        n = line_no(k)
        line = log_text(line_from(k):line_to(k))
        if (n == 0) then
            call text(line, style=UI_CODE, size=CODE, width=UI_FILL, tone=UI_TONE_MUTED, &
                id="l"//ui_str(k))
            return
        end if
        prefix = right(ui_str(n), 5)//'  '
        tone = UI_TONE_NORMAL
        if (n == error_line) tone = UI_TONE_NEGATIVE
        call text(prefix//line, style=UI_CODE, size=CODE, width=UI_FILL, id="l"//ui_str(k), &
            ends=[len(prefix), len(prefix) + len(line)], tones=[UI_TONE_MUTED, tone], &
            anchor="L"//ui_str(n))
    end subroutine log_line

    function clock(stamp) result(s)
        !! "2026-11-11 09:41:12" from an ISO 8601 time.
        character(len=*), intent(in) :: stamp
        character(len=:), allocatable :: s
        s = stamp
        if (len(stamp) >= 19) s = stamp(1:10)//' '//stamp(12:19)
    end function clock

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

    subroutine request_log()
        !! Asks for the live log from where the page has it; while a request
        !! is pending, once more when it is answered.
        if (log_req%id > 0) then
            log_again = .true.
            return
        end if
        log_job = job_number
        call fetch_start(log_req, 'GET', '/api/v1/jobs/'//job_number//'/log?from='// &
            ui_str(live_next))
    end subroutine request_log

    subroutine apply_log(body)
        !! {"live":..,"ended":..,"from":..,"next":..,"length":..,"text":..}: the
        !! new text is added at the end, and the view stays at the end if the
        !! reader was there.
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key, add, begun
        integer :: from, upto, length
        logical :: is_live, ended

        ! The answer for a job the page no longer shows.
        if (log_job /= job_number) return
        key = ''
        add = ''
        begun = ''
        from = -1
        upto = 0
        length = 0
        is_live = .false.
        ended = .false.
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_BOOL)
                if (key == 'live') is_live = ev%bool_val
                if (key == 'ended') ended = ev%bool_val
            case (JSON_INTEGER)
                select case (key)
                case ('from'); from = int(ev%int_val)
                case ('next'); upto = int(ev%int_val)
                case ('length'); length = int(ev%int_val)
                end select
            case (JSON_STRING)
                if (key == 'text') add = ev%string_val
                if (key == 'begun') begun = ev%string_val
            end select
        end do
        if (.not. is_live) then
            ! The check has ended: the job's stored log follows with its state.
            if (ended) call fetch_start(req, 'GET', '/api/v1/jobs/'//job_number)
            return
        end if
        if (.not. live) then
            live = .true.
            call live_reset()
        end if
        if (len(live_begun) == 0) live_begun = begun
        if (length < live_next .or. begun /= live_begun) then
            ! The check started again (run again, or another check of the
            ! job): its log starts again.
            call live_reset()
            call show_live()
            call request_log()
            return
        end if
        if (from /= live_next) then
            call request_log()
            return
        end if
        if (len(add) > 0) then
            if (size(line_from) > 0) following = nav_at_end()
            call live_add(add)
            live_next = upto
            call show_live()
            grew = .true.
            if (following) call nav_scroll_end()
        end if
        ! More is there unless the answer stopped short of an incomplete
        ! UTF-8 character at the end, which the next event completes.
        if (upto < length .and. upto > from) call request_log()
    end subroutine apply_log

    subroutine live_reset()
        live_next = 0
        head = ''
        tail = ''
        carry = ''
        head_count = 0
        tail_first = 1
        carry_line = 1
        live_error = 0
        in_tail = .false.
        live_begun = ''
    end subroutine live_reset

    subroutine live_add(add)
        !! Adds text of the live log: the head keeps the first HEAD_LINES lines
        !! (at most HEAD_BYTES), the tail the last TAIL_LINES (at most
        !! TAIL_BYTES), and the first line with Error or FAIL is noted.
        character(len=*), intent(in) :: add
        character(len=:), allocatable :: s
        integer :: at, fail, k, n

        if (live_error == 0) then
            s = carry//add
            at = index(s, 'Error')
            fail = index(s, 'FAIL')
            if (at == 0 .or. (fail > 0 .and. fail < at)) at = fail
            if (at > 0) live_error = carry_line + breaks(s(1:at - 1))
        end if
        s = carry//add
        k = max(1, len(s) - 3)
        carry_line = carry_line + breaks(s(1:k - 1))
        carry = s(k:)
        if (.not. in_tail) then
            head = head//add
            k = after_lines(head, HEAD_LINES)
            if (k == 0 .and. len(head) > HEAD_BYTES) then
                k = index(head(1:HEAD_BYTES), new_line('a'), back=.true.)
                if (k == 0) k = -1
            end if
            if (k /= 0) then
                k = max(k, 0)
                tail = head(k + 1:)
                head = head(1:k)
                head_count = breaks(head)
                tail_first = head_count + 1
                in_tail = .true.
            end if
        else
            tail = tail//add
        end if
        if (.not. in_tail) return
        n = breaks(tail)
        if (len(tail) > 0) then
            if (tail(len(tail):) /= new_line('a')) n = n + 1
        end if
        if (n > TAIL_LINES) then
            k = after_lines(tail, n - TAIL_LINES)
            tail_first = tail_first + n - TAIL_LINES
            tail = tail(k + 1:)
        end if
        if (len(tail) > TAIL_BYTES) then
            k = index(tail(len(tail) - TAIL_BYTES + 1:), new_line('a'))
            if (k > 0 .and. len(tail) - TAIL_BYTES + k < len(tail)) then
                k = len(tail) - TAIL_BYTES + k
                tail_first = tail_first + breaks(tail(1:k))
                tail = tail(k + 1:)
            else
                ! One long line: its end.
                tail = tail(len(tail) - TAIL_BYTES + 1:)
            end if
        end if
    end subroutine live_add

    subroutine show_live()
        !! The lines of the live log the page keeps, with their numbers.
        integer :: k, h
        logical :: gap

        gap = in_tail .and. tail_first > head_count + 1
        log_text = head
        if (gap) log_text = log_text//'... lines '//ui_str(head_count + 1)//' to '// &
            ui_str(tail_first - 1)//' are left out here ...'//new_line('a')
        if (in_tail) log_text = log_text//tail
        call split_lines()
        h = head_count
        if (.not. in_tail) h = size(line_from)
        do k = 1, size(line_no)
            if (k <= h) then
                line_no(k) = k
            else if (gap .and. k == h + 1) then
                line_no(k) = 0
            else
                line_no(k) = tail_first + k - h - 1 - merge(1, 0, gap)
            end if
        end do
        error_line = live_error
    end subroutine show_live

    subroutine show_stored()
        !! The stored log, each line with its own number.
        integer :: k
        log_text = stored_log
        call split_lines()
        do k = 1, size(line_no)
            line_no(k) = k
        end do
    end subroutine show_stored

    integer function last_line()
        !! The number of the last line shown; 0 without lines.
        last_line = 0
        if (size(line_no) > 0) last_line = line_no(size(line_no))
    end function last_line

    pure integer function breaks(s) result(n)
        !! The line breaks in `s`.
        character(len=*), intent(in) :: s
        integer :: pos, k
        n = 0
        pos = 1
        do
            k = index(s(pos:), new_line('a'))
            if (k == 0) return
            n = n + 1
            pos = pos + k
        end do
    end function breaks

    pure integer function after_lines(s, lines) result(at)
        !! The position of the line break that ends line `lines` of `s`; 0
        !! when `s` has fewer complete lines.
        character(len=*), intent(in) :: s
        integer, intent(in) :: lines
        integer :: n, k
        at = 0
        do n = 1, lines
            k = index(s(at + 1:), new_line('a'))
            if (k == 0) then
                at = 0
                return
            end if
            at = at + k
        end do
    end function after_lines

    subroutine split_lines()
        !! Where the lines of the log begin and end; a last line break opens
        !! no further line, and the carriage return of a line that ends in
        !! CR LF is not part of it.
        integer :: pos, nl, n

        n = 0
        pos = 1
        do while (pos <= len(log_text))
            nl = index(log_text(pos:), new_line('a'))
            n = n + 1
            if (nl == 0) exit
            pos = pos + nl
        end do
        deallocate (line_from, line_to, line_no)
        allocate (line_from(n), line_to(n), line_no(n))
        n = 0
        pos = 1
        do while (pos <= len(log_text))
            nl = index(log_text(pos:), new_line('a'))
            n = n + 1
            line_from(n) = pos
            if (nl == 0) then
                line_to(n) = len(log_text)
                exit
            end if
            line_to(n) = pos + nl - 2
            pos = pos + nl
        end do
        do n = 1, size(line_to)
            if (line_to(n) < line_from(n)) cycle
            if (log_text(line_to(n):line_to(n)) == achar(13)) line_to(n) = line_to(n) - 1
        end do
    end subroutine split_lines

    subroutine parse(body)
        !! {"id":..,"kind":..,"repo":..,"change":N|null,"title":..,"entry":..,
        !!  "entries":..,"checks":[{id,attempt,state,runner,wait,run}],
        !!  "next":{"id":..,"state":..}|null,"live_log":{"length":N}|null,
        !!  "error_line":N|null,"log":".."}; the live log replaces the stored one
        !!  while the job runs with one.
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key, section
        type(check_row_t) :: c
        integer :: depth

        deallocate (checks)
        allocate (checks(0))
        job_kind = ''
        repo = ''
        branch = ''
        account = ''
        state = ''
        queued = ''
        started = ''
        finished = ''
        commit = ''
        change_title = ''
        next_state = ''
        stored_log = ''
        live_length = -1
        change = 0
        entry = 0
        entries = 0
        next = 0
        error_line = 0
        key = ''
        section = ''
        depth = 0
        call reset_check()
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                depth = depth + 1
                if (depth == 2) section = key
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                if (depth == 3 .and. section == 'checks' .and. c%id > 0) then
                    checks = [checks, c]
                    call reset_check()
                end if
                depth = depth - 1
                if (depth == 1) section = ''
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                if (depth == 1) then
                    select case (key)
                    case ('kind'); job_kind = ev%string_val
                    case ('repo'); repo = ev%string_val
                    case ('branch'); branch = ev%string_val
                    case ('account'); account = ev%string_val
                    case ('state'); state = ev%string_val
                    case ('queued'); queued = ev%string_val
                    case ('started'); started = ev%string_val
                    case ('finished'); finished = ev%string_val
                    case ('commit'); commit = ev%string_val
                    case ('title'); change_title = ev%string_val
                    case ('log'); stored_log = ev%string_val
                    end select
                else if (depth == 2 .and. section == 'next') then
                    if (key == 'state') next_state = ev%string_val
                else if (depth == 3 .and. section == 'checks') then
                    if (key == 'state') c%state = ev%string_val
                    if (key == 'runner') c%runner = ev%string_val
                end if
            case (JSON_INTEGER)
                if (depth == 1) then
                    select case (key)
                    case ('change'); change = int(ev%int_val)
                    case ('entry'); entry = int(ev%int_val)
                    case ('entries'); entries = int(ev%int_val)
                    case ('error_line'); error_line = int(ev%int_val)
                    end select
                else if (depth == 2 .and. section == 'next') then
                    if (key == 'id') next = int(ev%int_val)
                else if (depth == 2 .and. section == 'live_log') then
                    if (key == 'length') live_length = int(ev%int_val)
                else if (depth == 3 .and. section == 'checks') then
                    select case (key)
                    case ('id'); c%id = int(ev%int_val)
                    case ('attempt'); c%attempt = int(ev%int_val)
                    case ('wait'); c%wait = int(ev%int_val)
                    case ('run'); c%run = int(ev%int_val)
                    end select
                end if
            end select
        end do
        if (state == 'running' .and. live_length >= 0) then
            if (.not. live) then
                live = .true.
                call live_reset()
            end if
            call show_live()
            if (live_next < live_length) call request_log()
        else
            live = .false.
            call show_stored()
        end if

    contains

        subroutine reset_check()
            c%id = 0
            c%attempt = 0
            c%wait = -1
            c%run = -1
            c%state = ''
            c%runner = ''
        end subroutine reset_check

    end subroutine parse

end module fy_job_view
