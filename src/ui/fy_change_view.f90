module fy_change_view
    !! The pages of a repository's changes and issues: the lists, a change
    !! with its conversation, versions and diff, an issue with its comments,
    !! and the form for a new issue. Addresses (fy_route):
    !!   /REPO/-/changes, /REPO/-/changes/N, /REPO/-/changes/N/files
    !!   /REPO/-/issues, /REPO/-/issues/N, /REPO/-/issues/new
    use fortui
    use fx_json_build, only: &
        json_escape_string
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
        json_extract_int, &
        json_extract_string, &
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    use fy_code_lines, only: &
        diff_compose, &
        diff_load, &
        shown_file_t
    use fy_fetch, only: &
        fetch_ready, &
        fetch_start, &
        request_t
    use fy_markdown, only: &
        md_compose, &
        md_doc_t, &
        md_parse
    use fy_pages
    use fy_route, only: &
        at, &
        go, &
        route_href, &
        route_t, &
        url_encode
    use fy_time, only: &
        ago
    implicit none
    private

    public :: &
        change_view_compose, &
        change_view_missing, &
        change_view_receive, &
        change_view_repo, &
        change_view_show, &
        change_view_title

    type :: row_t
        integer :: number = 0, versions = 0, comments = 0
        character(len=:), allocatable :: title, state, author, branch, updated
    end type row_t

    type :: note_t
        !! A version, comment or landing of the item shown.
        integer :: n = 0
        character(len=:), allocatable :: a, b, c
    end type note_t

    real, parameter :: BODY = 14.0, SMALL = 12.0, HEADING = 16.0, TITLE = 20.0
    real, parameter :: GAP = 8.0, ROW_H = 40.0

    type(request_t) :: list_req, item_req, diff_req, action_req
    type(row_t), allocatable :: rows(:)
    type(note_t), allocatable :: versions(:), comments(:), landings(:)
    type(shown_file_t), allocatable :: diff_files(:)
    type(md_doc_t) :: description
    character(len=:), allocatable :: repo, sub, tab, i_title, i_state, i_author, i_branch, &
        i_created, comment_input, new_title, new_body, message, list_of, item_of
    integer :: kind = R_CHANGES, number = 0, version = 0, from = 0
    logical :: show_all = .false., loaded = .false., item_loaded = .false., missing = .false.
    logical :: diff_cut = .false., has_text = .false.

contains

    subroutine change_view_show(r)
        !! Shows the page of route `r`.
        type(route_t), intent(in) :: r
        character(len=:), allocatable :: rest
        integer :: slash, ios

        if (.not. allocated(rows)) call init()
        kind = r%kind
        repo = r%repo
        rest = r%path
        message = ''
        missing = .false.
        show_all = r%all
        tab = 'conversation'
        number = 0
        if (kind == R_CHANGE .or. kind == R_ISSUE) then
            slash = index(rest, '/')
            if (slash > 0) then
                if (rest(slash + 1:) == 'files') tab = 'files'
                rest = rest(1:slash - 1)
            end if
            if (rest == 'new' .and. kind == R_ISSUE) then
                tab = 'new'
                if (.not. allocated(new_title)) new_title = ''
                if (.not. allocated(new_body)) new_body = ''
                return
            end if
            if (len(rest) == 0 .or. len(rest) > 8 .or. verify(rest, '0123456789') /= 0) then
                missing = .true.
                return
            end if
            read (rest, *, iostat=ios) number
            if (item_of /= api_item()) then
                item_loaded = .false.
                item_of = api_item()
                version = 0
                from = 0
                call fetch_start(item_req, 'GET', item_of)
            end if
            if (tab == 'files' .and. item_loaded) call request_diff()
        else
            if (list_of /= api_list()) then
                loaded = .false.
                list_of = api_list()
                call fetch_start(list_req, 'GET', list_of)
            end if
        end if
    end subroutine change_view_show

    subroutine init()
        allocate (rows(0), versions(0), comments(0), landings(0), diff_files(0))
        list_of = ''
        item_of = ''
        comment_input = ''
        new_title = ''
        new_body = ''
        message = ''
        repo = ''
        tab = 'conversation'
        i_title = ''
        i_state = ''
        i_author = ''
        i_branch = ''
        i_created = ''
    end subroutine init

    function noun() result(s)
        character(len=:), allocatable :: s
        if (kind == R_CHANGES .or. kind == R_CHANGE) then
            s = 'changes'
        else
            s = 'issues'
        end if
    end function noun

    function api_list() result(path)
        character(len=:), allocatable :: path
        path = '/api/v1/repos/'//url_encode(repo)//'/'//noun()
        if (show_all) path = path//'?state=all'
    end function api_list

    function api_item() result(path)
        character(len=:), allocatable :: path
        path = '/api/v1/repos/'//url_encode(repo)//'/'//noun()//'/'//ui_str(number)
    end function api_item

    function page_of(n) result(path)
        !! The application's address of item `n` of this kind.
        integer, intent(in) :: n
        character(len=:), allocatable :: path
        path = '/'//repo//'/-/'//noun()//'/'//ui_str(n)
    end function page_of

    subroutine request_diff()
        character(len=:), allocatable :: q
        q = ''
        if (version > 0) q = '?version='//ui_str(version)
        if (from > 0) then
            if (len(q) == 0) then
                q = '?from='//ui_str(from)
            else
                q = q//'&from='//ui_str(from)
            end if
        end if
        call fetch_start(diff_req, 'GET', item_of//'/diff'//q)
    end subroutine request_diff

    function change_view_repo() result(s)
        character(len=:), allocatable :: s
        s = ''
        if (allocated(repo)) s = repo
    end function change_view_repo

    logical function change_view_missing()
        change_view_missing = missing
    end function change_view_missing

    function change_view_title() result(s)
        character(len=:), allocatable :: s
        if (.not. allocated(repo)) repo = ''
        if (kind == R_CHANGES) then
            s = 'Changes · '//repo
        else if (kind == R_ISSUES) then
            s = 'Issues · '//repo
        else if (tab == 'new') then
            s = 'New issue · '//repo
        else if (item_loaded) then
            s = i_title//' (#'//ui_str(number)//') · '//repo
        else
            s = '#'//ui_str(number)//' · '//repo
        end if
    end function change_view_title

    ! ---- receiving -----------------------------------------------------------------------

    subroutine change_view_receive()
        logical :: found
        integer :: n

        if (fetch_ready(list_req)) then
            if (list_req%status == 200) then
                call parse_list(list_req%body)
                loaded = .true.
            else
                missing = .true.
            end if
        end if
        if (fetch_ready(item_req)) then
            if (item_req%status == 200) then
                call parse_item(item_req%body)
                item_loaded = .true.
                if (tab == 'files') call request_diff()
            else
                missing = .true.
            end if
        end if
        if (fetch_ready(diff_req)) then
            if (diff_req%status == 200) then
                call diff_load(diff_req%body, diff_files, diff_cut)
            else
                message = 'The diff is not available.'
            end if
        end if
        if (fetch_ready(action_req)) then
            if (action_req%status >= 200 .and. action_req%status < 300) then
                call json_extract_int(action_req%body, 'landing', n, found)
                if (found) then
                    message = ''
                    call go('/jobs?repo='//url_encode(repo))
                    return
                end if
                call json_extract_int(action_req%body, 'issue', n, found)
                if (found .and. tab == 'new') then
                    new_title = ''
                    new_body = ''
                    list_of = ''
                    call go(page_of(n))
                    return
                end if
                comment_input = ''
                call fetch_start(item_req, 'GET', item_of)
            else
                call json_extract_string(action_req%body, 'error', message, found)
                if (.not. found) message = 'The server refused this.'
            end if
        end if
    end subroutine change_view_receive

    subroutine parse_list(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key
        type(row_t) :: r
        integer :: depth

        deallocate (rows)
        allocate (rows(0))
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
                if (depth == 2) then
                    select case (key)
                    case ('title'); r%title = ev%string_val
                    case ('state'); r%state = ev%string_val
                    case ('author'); r%author = ev%string_val
                    case ('branch'); r%branch = ev%string_val
                    case ('updated'); r%updated = ev%string_val
                    end select
                end if
            case (JSON_INTEGER)
                if (depth == 2) then
                    select case (key)
                    case ('number'); r%number = int(ev%int_val)
                    case ('versions'); r%versions = int(ev%int_val)
                    case ('comments'); r%comments = int(ev%int_val)
                    end select
                end if
            end select
        end do

    contains

        subroutine reset_row()
            r%number = 0
            r%versions = 0
            r%comments = 0
            r%title = ''
            r%state = ''
            r%author = ''
            r%branch = ''
            r%updated = ''
        end subroutine reset_row

    end subroutine parse_list

    subroutine parse_item(body)
        !! A change {title, state, author, branch, created, description,
        !! versions_list, comments, landings} or an issue {title, state, author,
        !! created, body, comments_list}.
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key, list, text
        type(note_t) :: n
        integer :: depth

        deallocate (versions, comments, landings)
        allocate (versions(0), comments(0), landings(0))
        i_title = ''
        i_state = ''
        i_author = ''
        i_branch = ''
        i_created = ''
        text = ''
        key = ''
        list = ''
        depth = 0
        call reset_note()
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START)
                depth = depth + 1
            case (JSON_ARRAY_START)
                if (depth == 1) list = key
            case (JSON_ARRAY_END)
                if (depth == 1) list = ''
            case (JSON_OBJECT_END)
                if (depth == 2) then
                    select case (list)
                    case ('versions_list'); versions = [versions, n]
                    case ('comments', 'comments_list'); comments = [comments, n]
                    case ('landings'); landings = [landings, n]
                    end select
                    call reset_note()
                end if
                depth = depth - 1
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                if (depth == 1) then
                    select case (key)
                    case ('title'); i_title = ev%string_val
                    case ('state'); i_state = ev%string_val
                    case ('author'); i_author = ev%string_val
                    case ('branch'); i_branch = ev%string_val
                    case ('created'); i_created = ev%string_val
                    case ('description', 'body'); text = ev%string_val
                    end select
                else if (depth == 2) then
                    select case (list)
                    case ('versions_list')
                        if (key == 'head') n%a = ev%string_val
                        if (key == 'pusher') n%b = ev%string_val
                        if (key == 'created') n%c = ev%string_val
                    case ('comments', 'comments_list')
                        if (key == 'account') n%a = ev%string_val
                        if (key == 'body') n%b = ev%string_val
                        if (key == 'created') n%c = ev%string_val
                    case ('landings')
                        if (key == 'state') n%a = ev%string_val
                        if (key == 'commit') n%b = ev%string_val
                    end select
                end if
            case (JSON_INTEGER)
                if (depth == 2 .and. (key == 'version' .or. key == 'landing')) n%n = int(ev%int_val)
            end select
        end do
        has_text = len_trim(text) > 0
        call md_parse(text, description)

    contains

        subroutine reset_note()
            n%n = 0
            n%a = ''
            n%b = ''
            n%c = ''
        end subroutine reset_note

    end subroutine parse_item

    ! ---- composing -----------------------------------------------------------------------

    subroutine change_view_compose(may_land, narrow)
        logical, intent(in) :: may_land, narrow

        if (.not. allocated(rows)) call init()
        if (len(message) > 0) call text(message, size=BODY, tone=UI_TONE_NEGATIVE, id="cvmessage")
        if (missing) then
            call text("There is no such page.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        select case (kind)
        case (R_CHANGES, R_ISSUES)
            call list_view(narrow)
        case default
            if (tab == 'new') then
                call new_issue_view()
            else
                call item_view(may_land, narrow)
            end if
        end select
    end subroutine change_view_compose

    subroutine list_view(narrow)
        logical, intent(in) :: narrow
        character(len=:), allocatable :: base_path, meta
        integer :: k

        base_path = '/'//repo//'/-/'//noun()
        call row_begin(10.0, id="listhead")
        if (kind == R_CHANGES) then
            call text("Changes", size=TITLE, style=UI_HEADING, id="title")
        else
            call text("Issues", size=TITLE, style=UI_HEADING, id="title")
        end if
        if (show_all) then
            if (link("open", "Open only", at(base_path), size=BODY)) call go(base_path)
        else
            if (link("all", "All", at(base_path//'?all'), size=BODY)) call go(base_path//'?all')
        end if
        call text("", width=UI_FILL)
        if (kind == R_ISSUES) then
            if (link("newissue", "New issue", at(base_path//'/new'), size=BODY)) &
                call go(base_path//'/new')
        end if
        call row_end()
        if (.not. loaded) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        if (size(rows) == 0) then
            if (kind == R_CHANGES) then
                call text("No open changes. Pushing a branch opens one: fy push.", size=BODY, &
                    tone=UI_TONE_MUTED)
            else
                call text("No open issues.", size=BODY, tone=UI_TONE_MUTED)
            end if
            return
        end if
        call column_begin(0.0, id="card-items")
        do k = 1, size(rows)
            associate (r => rows(k))
                call column_begin(4.0, id="row-i"//ui_str(r%number))
                call row_begin(10.0, id="itemline")
                call text(r%state, size=12.0, id="badge", tone=state_tone(r%state))
                if (link("item", r%title, at(page_of(r%number)), key=r%number, size=BODY, &
                    style=UI_HEADING, width=UI_FILL, ellipsis=.not. narrow)) &
                    call go(page_of(r%number))
                call text("#"//ui_str(r%number), size=SMALL, style=UI_CODE, tone=UI_TONE_MUTED)
                call row_end()
                if (kind == R_CHANGES) then
                    meta = r%branch//' · '//r%author//' · '//ui_str(r%versions)//' version'
                    if (r%versions /= 1) meta = meta//'s'
                else
                    meta = r%author//' · '//ui_str(r%comments)//' comment'
                    if (r%comments /= 1) meta = meta//'s'
                end if
                call text(meta//' · updated '//ago(r%updated), size=SMALL, tone=UI_TONE_MUTED)
                call column_end()
            end associate
        end do
        call column_end()
    end subroutine list_view

    subroutine item_view(may_land, narrow)
        logical, intent(in) :: may_land, narrow
        character(len=:), allocatable :: base_path
        integer :: k

        if (.not. item_loaded) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        base_path = page_of(number)
        call column_begin(8.0, id="card-itemhead")
        call row_begin(GAP, id="itemtitle")
        call text(i_title, size=TITLE, style=UI_HEADING, width=UI_FILL, id="subject")
        if (.not. narrow) call text("#"//ui_str(number), size=TITLE, tone=UI_TONE_MUTED)
        call row_end()
        call row_begin(GAP, id="itemmeta")
        call text(i_state, size=12.0, id="badge", tone=state_tone(i_state))
        if (kind == R_CHANGE) then
            call text(i_author//' wants to land '//i_branch//' · opened '//ago(i_created), &
                size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL)
        else
            call text(i_author//' opened this '//ago(i_created), size=SMALL, &
                tone=UI_TONE_MUTED, width=UI_FILL)
        end if
        call row_end()
        call column_end()

        if (kind == R_CHANGE) then
            call row_begin(16.0, id="tabs", height=ROW_H)
            if (link("tab-conv", "Conversation", at(base_path), size=BODY, &
                style=merge(UI_HEADING, UI_BODY, tab == 'conversation'))) call go(base_path)
            if (link("tab-files", "Files changed ("//ui_str(size(versions))//" version"// &
                trim(merge('s', ' ', size(versions) /= 1))//")", at(base_path//'/files'), &
                size=BODY, style=merge(UI_HEADING, UI_BODY, tab == 'files'))) &
                call go(base_path//'/files')
            call row_end()
        end if
        if (tab == 'files') then
            call files_view()
            return
        end if

        if (has_text) then
            call column_begin(0.0, id="card-description")
            call md_compose(description)
            call column_end()
        end if

        if (size(landings) > 0) then
            call text("Landings", size=HEADING, style=UI_HEADING, id="heading-landings")
            call column_begin(0.0, id="card-landings")
            do k = 1, size(landings)
                call row_begin(GAP, id="row-l"//ui_str(k))
                call text(landings(k)%a, size=12.0, id="badge", tone=state_tone(landings(k)%a))
                if (link("landing", "landing "//ui_str(landings(k)%n), &
                    at('/jobs?repo='//url_encode(repo)), key=k, size=BODY, width=UI_FILL)) &
                    call go('/jobs?repo='//url_encode(repo))
                if (len(landings(k)%b) >= 7) then
                    if (link("commit", landings(k)%b(1:7), route_href(R_COMMIT, repo, &
                        landings(k)%b), key=k, size=SMALL, style=UI_CODE)) &
                        call go(route_href(R_COMMIT, repo, landings(k)%b))
                end if
                call row_end()
            end do
            call column_end()
        end if

        if (size(comments) > 0) then
            call text("Comments", size=HEADING, style=UI_HEADING, id="heading-comments")
            do k = 1, size(comments)
                call column_begin(4.0, id="card-comment"//ui_str(k))
                call text(comments(k)%a//' · '//ago(comments(k)%c), size=SMALL, &
                    style=UI_HEADING, tone=UI_TONE_MUTED)
                call text(comments(k)%b, size=BODY, width=UI_FILL)
                call column_end()
            end do
        end if

        call row_begin(GAP, id="commentrow")
        if (text_input("comment", comment_input, placeholder="Add a comment", &
            size=BODY, lines=4)) continue
        if (button("send", "Comment", height=ROW_H, size=BODY) .and. &
            len_trim(comment_input) > 0) call fetch_start(action_req, 'POST', item_of// &
            '/comments', '{"body":"'//json_escape_string(trim(comment_input))//'"}')
        call row_end()

        call row_begin(GAP, id="actions")
        if (kind == R_CHANGE .and. i_state == 'open' .and. may_land) then
            if (button("land", "Land", height=ROW_H, size=BODY)) &
                call fetch_start(action_req, 'POST', item_of//'/land', '{}')
        end if
        if (i_state == 'open') then
            if (button("closeitem", "Close", height=ROW_H, size=BODY)) &
                call fetch_start(action_req, 'POST', item_of, '{"state":"closed"}')
        else if (i_state == 'closed') then
            if (button("reopen", "Reopen", height=ROW_H, size=BODY)) &
                call fetch_start(action_req, 'POST', item_of, '{"state":"open"}')
        end if
        call row_end()
    end subroutine item_view

    subroutine files_view()
        !! The diff of a version against main, or between two versions.
        integer :: k

        if (size(versions) == 0) then
            call text("This change has no versions.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        call row_begin(GAP, id="versions")
        call text("Version", size=SMALL, tone=UI_TONE_MUTED)
        do k = 1, size(versions)
            if (button("v", ui_str(versions(k)%n), key=k, height=32.0, size=SMALL, &
                enabled=shown_version() /= versions(k)%n)) then
                version = versions(k)%n
                from = 0
                call request_diff()
            end if
        end do
        call text("", width=UI_FILL)
        if (size(versions) > 1 .and. shown_version() > 1) then
            if (from == 0) then
                if (button("interdiff", "Changes since version "// &
                    ui_str(shown_version() - 1), height=32.0, size=SMALL)) then
                    from = shown_version() - 1
                    call request_diff()
                end if
            else
                if (button("whole", "Whole change", height=32.0, size=SMALL)) then
                    from = 0
                    call request_diff()
                end if
            end if
        end if
        call row_end()
        k = shown_version()
        if (from > 0) then
            call text("Version "//ui_str(k)//" compared with version "//ui_str(from), &
                size=SMALL, tone=UI_TONE_MUTED)
        else
            call text("Version "//ui_str(k)//" against main, pushed by "// &
                versions(k)%b//" "//ago(versions(k)%c), size=SMALL, tone=UI_TONE_MUTED)
        end if
        call diff_compose(diff_files, diff_cut)
    end subroutine files_view

    integer function shown_version()
        shown_version = version
        if (shown_version == 0) shown_version = size(versions)
    end function shown_version

    subroutine new_issue_view()
        logical :: submitted

        call text("New issue", size=TITLE, style=UI_HEADING, id="title")
        if (text_input("newtitle", new_title, placeholder="Title", submitted=submitted, &
            size=BODY)) continue
        if (text_input("newbody", new_body, placeholder="What happened, and what should", &
            size=BODY, lines=8)) continue
        if ((button("create", "Open issue", height=ROW_H, size=BODY) .or. submitted) .and. &
            len_trim(new_title) > 0) call fetch_start(action_req, 'POST', '/api/v1/repos/'// &
            url_encode(repo)//'/issues', '{"title":"'//json_escape_string(trim(new_title))// &
            '","body":"'//json_escape_string(trim(new_body))//'"}')
    end subroutine new_issue_view

    integer function state_tone(s)
        character(len=*), intent(in) :: s
        select case (s)
        case ('open', 'running'); state_tone = UI_TONE_ACCENT
        case ('landed', 'passed'); state_tone = UI_TONE_POSITIVE
        case ('failed'); state_tone = UI_TONE_NEGATIVE
        case default; state_tone = UI_TONE_MUTED
        end select
    end function state_tone

end module fy_change_view
