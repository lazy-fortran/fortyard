program fy_app
    !! The fortyard browser application: sign-in, server status and the
    !! account's access tokens.
    use fortui
    use fortui_web, only: web_backend_t
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string, json_parser_t, json_event_t, &
        json_parser_init, json_parser_next, JSON_KEY, JSON_STRING, JSON_INTEGER, &
        JSON_BOOL, JSON_OBJECT_END, JSON_END_OF_INPUT, JSON_ERROR
    use fy_fetch, only: request_t, fetch_start, fetch_ready
    use fy_repo_view, only: repo_view_open, repo_view_compose, repo_view_receive
    implicit none

    type :: token_row_t
        integer :: id = 0
        character(len=:), allocatable :: name, scopes, created, expires
        logical :: revoked = .false., current = .false.
    end type token_row_t

    type(request_t) :: version_req, whoami_req, tokens_req, action_req
    type(token_row_t), allocatable :: tokens(:)
    character(len=:), allocatable :: version, commit, account, scopes, message
    character(len=:), allocatable :: token_input, new_name, new_token
    character(len=16), parameter :: tab_labels(3) = [character(len=16) :: "Repositories", &
        "Access tokens", "Server"]
    integer :: tab, stat, i
    logical :: signed_in, checked, submitted
    logical :: narrow
    real :: page_w, page_h
    ! Logical text sizes and spacing of the page.
    real, parameter :: TITLE = 22.0, HEADING = 17.0, BODY = 15.0, SMALL = 13.0, CTRL = 15.0
    real, parameter :: GAP = 12.0, SECTION_GAP = 24.0, ROW_H = 36.0
    real, parameter :: STATUS_W = 150.0, DATE_W = 110.0
    character(len=16) :: action

    version = ''
    commit = ''
    account = ''
    scopes = ''
    message = ''
    token_input = ''
    new_name = ''
    new_token = ''
    action = ''
    tab = 1
    signed_in = .false.
    checked = .false.
    allocate (tokens(0))

    call ui_init(web_backend_t(root_id="app"), 1000, 700, "fortyard", stat=stat)
    if (stat /= 0) error stop "fortyard needs a browser page"
    call fetch_start(version_req, 'GET', '/api/v1/version')
    call fetch_start(whoami_req, 'GET', '/api/v1/whoami')

    do while (.not. ui_should_close())
        call ui_frame_begin()
        call receive()
        call ui_window_size(page_w, page_h)
        narrow = page_w < 600.0

        call column_begin(SECTION_GAP)
        call header()
        if (.not. checked) then
            call text("Loading...", size=BODY)
        else if (.not. signed_in) then
            call sign_in_view()
        else
            if (tabs("tabs", tab_labels, tab, width=min(420.0, page_w), size=CTRL)) then
                message = ''
                if (tab == 1) call repo_view_open()
            end if
            select case (tab)
            case (1)
                call repo_view_compose(page_w, narrow, index(scopes, 'admin') > 0)
            case (2)
                call tokens_view()
            case default
                call overview_view()
            end select
        end if
        if (len(message) > 0) call text(message, size=SMALL, style=UI_CAPTION)
        call column_end()
        call ui_present()
    end do
    call ui_shutdown()

contains

    subroutine header()
        real :: used

        call row_begin(GAP, height=44.0)
        call text("fortyard", size=TITLE, style=UI_TITLE)
        if (signed_in) then
            ! Account and sign-out sit at the right edge of the page.
            used = ui_text_width("fortyard", TITLE, UI_TITLE) + &
                ui_text_width(account, SMALL) + button_width("Sign out") + 3.0 * GAP
            call text("", width=max(0.0, page_w - used))
            call text(account, size=SMALL, style=UI_CAPTION)
            if (button("signout", "Sign out", size=CTRL)) then
                call fetch_start(action_req, 'DELETE', '/api/v1/session')
                action = 'signout'
            end if
        end if
        call row_end()
    end subroutine header

    subroutine sign_in_view()
        real :: field_w

        call column_begin(GAP)
        call text("Sign in", size=HEADING, style=UI_HEADING)
        call text("Paste an access token. The browser keeps only a session cookie.", &
            size=BODY, width=page_w, ellipsis=.true.)
        field_w = min(520.0, page_w - button_width("Sign in") - GAP)
        if (narrow) field_w = page_w
        if (.not. narrow) call row_begin(GAP, height=ROW_H)
        if (text_input("token", token_input, placeholder="fy_...", submitted=submitted, &
            width=field_w, size=CTRL)) continue
        if (button("signin", "Sign in", size=CTRL) .or. submitted) then
            if (len_trim(token_input) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/session', &
                    '{"token":"' // json_escape_string(trim(token_input)) // '"}')
                action = 'signin'
            end if
        end if
        if (.not. narrow) call row_end()
        call column_end()
    end subroutine sign_in_view

    subroutine overview_view()
        call column_begin(GAP)
        call text("Server", size=HEADING, style=UI_HEADING)
        call field("Version", version)
        call field("Commit", commit)
        call text("Account", size=HEADING, style=UI_HEADING)
        call field("Name", account)
        call field("Scopes", scopes)
        call column_end()
    end subroutine overview_view

    subroutine field(label, value)
        character(len=*), intent(in) :: label, value

        call row_begin(GAP, height=24.0)
        call text(label, size=SMALL, style=UI_CAPTION, width=90.0)
        call text(value, size=BODY, width=max(0.0, page_w - 90.0 - GAP), ellipsis=.true.)
        call row_end()
    end subroutine field

    subroutine tokens_view()
        real :: name_w, field_w
        integer :: k

        call column_begin(GAP)
        call text("New token", size=HEADING, style=UI_HEADING)
        field_w = min(320.0, page_w - button_width("Create") - GAP)
        call row_begin(GAP, height=ROW_H)
        if (text_input("newname", new_name, placeholder="Name, e.g. laptop", &
            submitted=submitted, width=field_w, size=CTRL)) continue
        if (button("create", "Create", size=CTRL) .or. submitted) then
            if (len_trim(new_name) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/tokens', &
                    '{"name":"' // json_escape_string(trim(new_name)) // '"}')
                action = 'create'
            end if
        end if
        call row_end()
        if (len(new_token) > 0) then
            call text("Copy it now; it is shown only once:", size=SMALL, style=UI_CAPTION)
            call text(new_token, size=BODY, width=page_w, ellipsis=.true., id="newtoken")
        end if
        call column_end()

        call column_begin(4.0)
        call text("Tokens", size=HEADING, style=UI_HEADING)
        if (narrow) then
            name_w = page_w - button_width("Revoke") - GAP
        else
            name_w = max(140.0, page_w - STATUS_W - DATE_W - button_width("Revoke") - &
                3.0 * GAP)
            call row_begin(GAP, height=24.0)
            call text("Name", size=SMALL, style=UI_CAPTION, width=name_w)
            call text("Status", size=SMALL, style=UI_CAPTION, width=STATUS_W)
            call text("Created", size=SMALL, style=UI_CAPTION, width=DATE_W)
            call row_end()
        end if
        do k = 1, size(tokens)
            call row_begin(GAP, id="row" // ui_str(tokens(k)%id), height=ROW_H)
            call text(tokens(k)%name, size=BODY, width=name_w, ellipsis=.true.)
            if (.not. narrow) then
                call text(status_of(tokens(k)), size=SMALL, style=UI_CAPTION, &
                    width=STATUS_W)
                call text(day(tokens(k)%created), size=SMALL, style=UI_CAPTION, &
                    width=DATE_W)
            end if
            if (.not. tokens(k)%revoked .and. .not. tokens(k)%current) then
                if (button("revoke", "Revoke", key=tokens(k)%id, size=CTRL)) then
                    call fetch_start(action_req, 'POST', '/api/v1/tokens/' // &
                        ui_str(tokens(k)%id) // '/revoke', '{}')
                    action = 'revoke'
                end if
            end if
            call row_end()
            if (narrow) call text(status_of(tokens(k)) // ", created " // &
                day(tokens(k)%created), size=SMALL, style=UI_CAPTION)
        end do
        call column_end()
    end subroutine tokens_view

    real function button_width(label)
        !! As fortui sizes a button: its label plus padding, at least 72.
        character(len=*), intent(in) :: label
        button_width = max(72.0, ui_text_width(label, CTRL) + 24.0)
    end function button_width

    function day(stamp) result(s)
        character(len=*), intent(in) :: stamp
        character(len=:), allocatable :: s
        s = stamp(1:min(10, len(stamp)))
    end function day

    function status_of(t) result(s)
        type(token_row_t), intent(in) :: t
        character(len=:), allocatable :: s

        if (t%revoked) then
            s = "revoked"
        else if (t%current) then
            s = "this session"
        else if (len(t%expires) > 0) then
            s = "expires " // day(t%expires)
        else
            s = "active"
        end if
    end function status_of

    subroutine receive()
        !! Applies finished requests to the application state.
        logical :: found

        if (fetch_ready(version_req)) then
            call json_extract_string(version_req%body, 'version', version, found)
            call json_extract_string(version_req%body, 'commit', commit, found)
        end if
        if (fetch_ready(whoami_req)) then
            checked = .true.
            signed_in = whoami_req%status == 200
            if (signed_in) then
                call json_extract_string(whoami_req%body, 'account', account, found)
                call json_extract_string(whoami_req%body, 'scopes', scopes, found)
                call fetch_start(tokens_req, 'GET', '/api/v1/tokens')
                call repo_view_open()
            end if
        end if
        call repo_view_receive()
        if (fetch_ready(tokens_req)) then
            if (tokens_req%status == 200) call parse_tokens(tokens_req%body)
        end if
        if (fetch_ready(action_req)) then
            select case (trim(action))
            case ('signin')
                token_input = ''
                if (action_req%status == 200) then
                    message = ''
                    call fetch_start(whoami_req, 'GET', '/api/v1/whoami')
                else
                    message = "That token was not accepted."
                end if
            case ('signout')
                signed_in = .false.
                new_token = ''
                deallocate (tokens)
                allocate (tokens(0))
            case ('create')
                if (action_req%status == 201) then
                    call json_extract_string(action_req%body, 'token', new_token, found)
                    new_name = ''
                    message = ''
                else
                    message = "The token was not created."
                end if
                call fetch_start(tokens_req, 'GET', '/api/v1/tokens')
            case ('revoke')
                if (action_req%status /= 200) message = "The token was not revoked."
                call fetch_start(tokens_req, 'GET', '/api/v1/tokens')
            end select
            action = ''
        end if
    end subroutine receive

    subroutine parse_tokens(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        type(token_row_t) :: row
        character(len=:), allocatable :: key

        deallocate (tokens)
        allocate (tokens(0))
        call reset_row()
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                select case (key)
                case ('name'); row%name = ev%string_val
                case ('scopes'); row%scopes = ev%string_val
                case ('created'); row%created = ev%string_val
                case ('expires'); row%expires = ev%string_val
                end select
            case (JSON_INTEGER)
                if (key == 'id') row%id = ev%int_val
            case (JSON_BOOL)
                if (key == 'revoked') row%revoked = ev%bool_val
                if (key == 'current') row%current = ev%bool_val
            case (JSON_OBJECT_END)
                if (row%id > 0) tokens = [tokens, row]
                call reset_row()
            end select
        end do

    contains

        subroutine reset_row()
            row%id = 0
            row%name = ''
            row%scopes = ''
            row%created = ''
            row%expires = ''
            row%revoked = .false.
            row%current = .false.
        end subroutine reset_row

    end subroutine parse_tokens

end program fy_app
