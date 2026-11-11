module fy_changes_view
    !! The changes of a repository, /REPO/-/changes (doc/inspection.md,
    !! "Changes"): every state, newest activity first, one row per change with
    !! its state and review as words. The filters are in the address,
    !! ?state=&review=&attention=&q=&before=, so a filtered list is a link.
    use fortui
    use fx_json_parse, only: &
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
    use fy_fetch, only: &
        fetch_ready, &
        fetch_start, &
        request_t
    use fy_pages
    use fy_route, only: &
        go, &
        query_value, &
        route_href, &
        route_t, &
        url_decode, &
        url_encode
    use fy_time, only: &
        ago
    implicit none
    private

    public :: &
        changes_view_compose, &
        changes_view_missing, &
        changes_view_receive, &
        changes_view_repo, &
        changes_view_show, &
        changes_view_title

    type :: row_t
        integer :: number = 0, entries = 0
        character(len=:), allocatable :: title, state, status, reason, commit, review, &
            flag_reason, author, updated
        logical :: flagged = .false., stale = .false.
    end type row_t

    real, parameter :: BODY = 14.0, SMALL = 12.0, TITLE = 20.0

    type(request_t) :: list_req
    type(row_t), allocatable :: rows(:)
    character(len=:), allocatable :: repo, f_state, f_review, f_attention, f_q, f_before, &
        q_input, list_of, shown_of, next
    logical :: loaded = .false., missing = .false., bad = .false.

contains

    subroutine changes_view_show(r)
        !! Shows the list that the address of route `r` asks for.
        type(route_t), intent(in) :: r

        if (.not. allocated(rows)) then
            allocate (rows(0))
            repo = ''
            list_of = ''
            shown_of = ''
            next = ''
        end if
        if (repo /= r%repo) then
            deallocate (rows)
            allocate (rows(0))
            loaded = .false.
        end if
        repo = r%repo
        missing = .false.
        bad = .false.
        f_state = query_value(r%query, 'state')
        f_review = query_value(r%query, 'review')
        f_attention = query_value(r%query, 'attention')
        f_q = url_decode(plus_spaces(query_value(r%query, 'q')))
        f_before = query_value(r%query, 'before')
        if (len(f_state) == 0) f_state = 'all'
        q_input = f_q
        ! Asked for again on every visit: a list of states is stale soon, and the
        ! server composes pages for many people. The rows shown stay until the
        ! answer is here, so a filter does not empty the page on the way; an empty
        ! list has none to keep, and its sentence would name the wrong filter.
        if (list_of /= api_list() .and. size(rows) == 0) loaded = .false.
        list_of = api_list()
        call fetch_start(list_req, 'GET', list_of)
    end subroutine changes_view_show

    function plus_spaces(s) result(t)
        !! A form writes a space of a query value as "+".
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: k
        t = s
        do k = 1, len(t)
            if (t(k:k) == '+') t(k:k) = ' '
        end do
    end function plus_spaces

    function api_list() result(path)
        character(len=:), allocatable :: path
        path = '/api/v1/repos/'//url_encode(repo)//'/changes?state='//url_encode(f_state)
        if (len(f_review) > 0) path = path//'&review='//url_encode(f_review)
        if (len(f_attention) > 0) path = path//'&attention='//url_encode(f_attention)
        if (len(f_q) > 0) path = path//'&q='//url_encode(f_q)
        if (len(f_before) > 0) path = path//'&before='//url_encode(f_before)
    end function api_list

    function address(state, review, attention, q, before) result(href)
        !! The address of the list with these filters; `all` and an empty
        !! filter are left out.
        character(len=*), intent(in) :: state, review, attention, q, before
        character(len=:), allocatable :: href, query

        query = ''
        if (len(state) > 0 .and. state /= 'all') query = query//'&state='//url_encode(state)
        if (len(review) > 0) query = query//'&review='//url_encode(review)
        if (len(attention) > 0) query = query//'&attention='//url_encode(attention)
        if (len(q) > 0) query = query//'&q='//url_encode(q)
        if (len(before) > 0) query = query//'&before='//url_encode(before)
        href = route_href(R_CHANGES, repo)
        if (len(query) > 0) href = href//'?'//query(2:)
    end function address

    function changes_view_repo() result(s)
        character(len=:), allocatable :: s
        s = ''
        if (allocated(repo)) s = repo
    end function changes_view_repo

    logical function changes_view_missing()
        changes_view_missing = missing
    end function changes_view_missing

    function changes_view_title() result(s)
        character(len=:), allocatable :: s
        if (.not. allocated(repo)) repo = ''
        s = 'Changes · '//repo
    end function changes_view_title

    subroutine changes_view_receive()
        if (fetch_ready(list_req)) then
            if (list_req%status == 200) then
                call parse_list(list_req%body)
                shown_of = list_of
                loaded = .true.
            else if (list_req%status == 400) then
                ! A `before` that is no key: the page stays, without rows.
                bad = .true.
            else
                missing = .true.
            end if
        end if
    end subroutine changes_view_receive

    subroutine parse_list(body)
        !! {"changes":[{number, title, state, status, entries, reason, commit,
        !! review, flagged, flag_reason, review_stale, author, updated}], "next"}
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key
        type(row_t) :: r
        integer :: depth

        deallocate (rows)
        allocate (rows(0))
        next = ''
        key = ''
        depth = 0
        call reset_row()
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START)
                depth = depth + 1
            case (JSON_OBJECT_END)
                if (depth == 2 .and. r%number > 0) then
                    rows = [rows, r]
                    call reset_row()
                end if
                depth = depth - 1
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                if (depth == 1 .and. key == 'next') next = ev%string_val
                if (depth == 2) then
                    select case (key)
                    case ('author'); r%author = ev%string_val
                    case ('commit'); r%commit = ev%string_val
                    case ('flag_reason'); r%flag_reason = ev%string_val
                    case ('reason'); r%reason = ev%string_val
                    case ('review'); r%review = ev%string_val
                    case ('session')
                        ! The token that pushed the newest version names the
                        ! session, where it was recorded: it says more than the account.
                        r%author = ev%string_val
                    case ('state'); r%state = ev%string_val
                    case ('status'); r%status = ev%string_val
                    case ('title'); r%title = ev%string_val
                    case ('updated'); r%updated = ev%string_val
                    end select
                end if
            case (JSON_INTEGER)
                if (depth == 2) then
                    select case (key)
                    case ('entries'); r%entries = int(ev%int_val)
                    case ('number'); r%number = int(ev%int_val)
                    end select
                end if
            case (JSON_BOOL)
                if (depth == 2 .and. key == 'flagged') r%flagged = ev%bool_val
                if (depth == 2 .and. key == 'review_stale') r%stale = ev%bool_val
            end select
        end do

    contains

        subroutine reset_row()
            r%number = 0
            r%entries = 0
            r%flagged = .false.
            r%stale = .false.
            r%title = ''
            r%state = ''
            r%status = ''
            r%reason = ''
            r%commit = ''
            r%review = ''
            r%flag_reason = ''
            r%author = ''
            r%updated = ''
        end subroutine reset_row

    end subroutine parse_list

    ! ---- composing -----------------------------------------------------------------------

    subroutine changes_view_compose()
        character(len=:), allocatable :: href
        integer :: k

        if (.not. allocated(rows)) return
        if (missing) then
            call text("There is no such page.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        call text("Changes", size=TITLE, style=UI_HEADING, id="title")
        call filters()
        if (bad) then
            call text("This address is no place in the list of changes.", size=BODY, &
                tone=UI_TONE_MUTED, id="bad")
            href = address(f_state, f_review, f_attention, f_q, '')
            if (link("newest", "Newest changes", href, size=BODY)) call go(href)
            return
        end if
        if (.not. loaded) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        if (size(rows) == 0) then
            call text(empty_sentence(), size=BODY, tone=UI_TONE_MUTED, id="empty")
        else
            call column_begin(0.0, id="card-changes")
            do k = 1, size(rows)
                call change_row(rows(k))
            end do
            call column_end()
        end if
        call row_begin(16.0, id="pages")
        if (len(f_before) > 0) then
            href = address(f_state, f_review, f_attention, f_q, '')
            if (link("newest", "Newest changes", href, size=BODY)) call go(href)
        end if
        ! The key belongs to the rows shown, not to a list still on its way.
        if (len(next) > 0 .and. shown_of == list_of) then
            href = address(f_state, f_review, f_attention, f_q, next)
            if (link("older", "Older changes", href, size=BODY)) call go(href)
        end if
        call row_end()
    end subroutine changes_view_compose

    subroutine filters()
        !! Three groups of links and the text field; a link of the second and
        !! third group that is on leads back to the list without it.
        character(len=:), allocatable :: href
        logical :: submitted, changed

        ! A group is a row of its own, so a narrow page breaks between groups.
        call row_begin(16.0, id="filters")
        call row_begin(8.0, id="by-state")
        call state_link("all", "all")
        call dot("d1")
        call state_link("open", "open")
        call dot("d2")
        call state_link("landed", "landed")
        call dot("d3")
        call state_link("closed", "closed")
        call row_end()
        call row_begin(8.0, id="by-review")
        call review_link("flagged")
        call dot("d4")
        call review_link("unreviewed")
        call row_end()
        href = address(f_state, f_review, toggled(f_attention, 'needs-author'), f_q, '')
        if (link("needs-author", "needs author", href, size=BODY, &
            tone=on_tone(f_attention == 'needs-author'))) call go(href)
        call text("", width=UI_FILL, id="g3")
        ! "/" is the key of the filter field of a list.
        if (ui_shortcut("/")) call ui_focus("filter")
        changed = text_input("filter", q_input, placeholder="Title, branch or session", &
            submitted=submitted, width=220.0, size=BODY)
        if (submitted) then
            href = address(f_state, f_review, f_attention, q_input, '')
            call go(href)
        end if
        call row_end()
    end subroutine filters

    subroutine state_link(id, state)
        character(len=*), intent(in) :: id, state
        character(len=:), allocatable :: href
        href = address(state, f_review, f_attention, f_q, '')
        if (link(id, state, href, size=BODY, tone=on_tone(f_state == state))) call go(href)
    end subroutine state_link

    subroutine review_link(review)
        character(len=*), intent(in) :: review
        character(len=:), allocatable :: href
        href = address(f_state, toggled(f_review, review), f_attention, f_q, '')
        if (link(review, review, href, size=BODY, tone=on_tone(f_review == review))) &
            call go(href)
    end subroutine review_link

    subroutine dot(id)
        character(len=*), intent(in) :: id
        call text("·", size=BODY, tone=UI_TONE_MUTED, id=id)
    end subroutine dot

    function toggled(now, value) result(s)
        character(len=*), intent(in) :: now, value
        character(len=:), allocatable :: s
        s = value
        if (now == value) s = ''
    end function toggled

    integer function on_tone(on)
        logical, intent(in) :: on
        on_tone = UI_TONE_NORMAL
        if (on) on_tone = UI_TONE_STRONG
    end function on_tone

    function empty_sentence() result(s)
        !! "No flagged changes in fortyard.": the sentence names the filter.
        character(len=:), allocatable :: s

        s = 'No'
        if (len(f_before) > 0) s = s//' older'
        if (f_state /= 'all') s = s//' '//f_state
        if (len(f_review) > 0) s = s//' '//f_review
        s = s//' changes'
        if (f_attention == 'needs-author') then
            s = s//' that need their author'
        else if (len(f_attention) > 0) then
            s = s//' with attention '//f_attention
        end if
        if (len(f_q) > 0) s = s//' matching "'//f_q//'"'
        s = s//' in '//repo//'.'
        if (s == 'No changes in '//repo//'.') s = s//' Pushing a branch opens one: fy push.'
    end function empty_sentence

    subroutine change_row(r)
        !! One row at every width; the page's styles wrap it on a phone.
        type(row_t), intent(in) :: r
        character(len=:), allocatable :: href, word
        integer :: tone

        href = route_href(R_CHANGE, repo, ui_str(r%number))
        call row_begin(10.0, id="row-i"//ui_str(r%number))
        call text("#"//ui_str(r%number), size=SMALL, style=UI_CODE, tone=UI_TONE_MUTED, id="number")
        if (link("item", r%title, href, size=BODY, width=UI_FILL, ellipsis=.true.)) &
            call go(href)
        call state_word(r, word, tone)
        call text(word, size=SMALL, tone=tone, id="state", look="state")
        call review_word(r, word, tone)
        call text(word, size=SMALL, tone=tone, id="review", look="state")
        call text(r%author, size=SMALL, tone=UI_TONE_MUTED, id="author")
        call text(ago(r%updated), size=SMALL, tone=UI_TONE_MUTED, id="age")
        call row_end()
    end subroutine change_row

    subroutine state_word(r, word, tone)
        !! The state of a change with its reason (doc/inspection.md, "States
        !! and words"); a state this page does not know is shown muted.
        type(row_t), intent(in) :: r
        character(len=:), allocatable, intent(out) :: word
        integer, intent(out) :: tone

        word = r%status
        select case (r%status)
        case ('closed')
            tone = UI_TONE_MUTED
        case ('draft')
            tone = UI_TONE_NORMAL
        case ('landed')
            tone = UI_TONE_NORMAL
            if (len(r%commit) >= 7) word = word//' '//r%commit(1:7)
        case ('needs author')
            tone = UI_TONE_ATTENTION
            if (len(r%reason) > 0) word = word//': '//r%reason
            word = word//' · entry '//ui_str(r%entries)//' of '//ui_str(r%entries)
        case ('queued', 'running')
            tone = UI_TONE_ACCENT
        case default
            tone = UI_TONE_MUTED
            if (len(word) == 0) word = r%state
        end select
    end subroutine state_word

    subroutine review_word(r, word, tone)
        !! The review of a change: a flag first, then where its review stands.
        type(row_t), intent(in) :: r
        character(len=:), allocatable, intent(out) :: word
        integer, intent(out) :: tone

        if (r%flagged) then
            word = 'flagged'
            if (len(r%flag_reason) > 0) word = word//': '//r%flag_reason
            tone = UI_TONE_ATTENTION
            return
        end if
        select case (r%review)
        case ('', 'none')
            word = 'unreviewed'
            tone = UI_TONE_ATTENTION
        case ('ai-reviewed')
            word = 'AI-reviewed'
            tone = UI_TONE_NORMAL
        case ('failed')
            word = 'review failed'
            tone = UI_TONE_NEGATIVE
        case ('human-reviewed')
            ! Who approved is not recorded with the change.
            word = 'approved'
            tone = UI_TONE_NORMAL
        case ('requested')
            word = 'review requested'
            tone = UI_TONE_ACCENT
        case default
            word = r%review
            tone = UI_TONE_MUTED
        end select
        if (r%stale) then
            ! The newest version waits for a review of its own.
            word = word//' an older version'
            tone = UI_TONE_ATTENTION
        end if
    end subroutine review_word

end module fy_changes_view
