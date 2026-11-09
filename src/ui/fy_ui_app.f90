module fy_ui_app
    !! The fortyard application's state and frames: sign-in, repositories,
    !! access tokens and server status. Compiled to WebAssembly for the browser
    !! and natively into the server, which composes the first frame of a page
    !! and sends it as HTML with the state that produced it.
    use fortui
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string, json_parser_t, json_event_t, &
        json_parser_init, json_parser_next, JSON_KEY, JSON_STRING, JSON_INTEGER, &
        JSON_BOOL, JSON_OBJECT_END, JSON_END_OF_INPUT, JSON_ERROR
    use fy_fetch, only: request_t, fetch_start, fetch_ready
    use fy_repo_view, only: repo_view_open, repo_view_compose, repo_view_receive, &
        repo_view_load, repo_view_reset
    implicit none
    private

    public :: app_start, app_compose

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
    integer :: tab = 1
    logical :: signed_in = .false., checked = .false., submitted = .false.
    logical :: narrow = .false.
    real :: page_w = 1000.0, page_h = 800.0
    ! Logical text sizes and spacing of the page.
    real, parameter :: TITLE = 22.0, HEADING = 17.0, BODY = 15.0, SMALL = 13.0, CTRL = 15.0
    real, parameter :: GAP = 12.0, SECTION_GAP = 24.0, ROW_H = 36.0
    real, parameter :: STATUS_W = 150.0, DATE_W = 110.0
    character(len=16) :: action = ''

contains

    subroutine app_start(version_json, whoami_json, repos_json)
        !! Starts from the responses the server embedded in the page (as the API
        !! returns them; whoami empty when nobody is signed in), or, when the page
        !! carries none, asks the server.
        character(len=*), intent(in) :: version_json, whoami_json, repos_json

        ! Everything starts afresh: the server renders pages for different people.
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
        submitted = .false.
        if (allocated(tokens)) deallocate (tokens)
        allocate (tokens(0))
        call repo_view_reset()
        if (len_trim(version_json) == 0) then
            call fetch_start(version_req, 'GET', '/api/v1/version')
            call fetch_start(whoami_req, 'GET', '/api/v1/whoami')
            return
        end if
        call apply_version(version_json)
        if (len_trim(whoami_json) == 0) then
            call apply_whoami('', 401)
        else
            call apply_whoami(whoami_json, 200)
            if (len_trim(repos_json) > 0) call repo_view_load(repos_json)
        end if
    end subroutine app_start

    subroutine app_compose()
        !! One frame.
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
                if (tab == 2) call fetch_start(tokens_req, 'GET', '/api/v1/tokens')
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
    end subroutine app_compose

    subroutine apply_version(body)
        character(len=*), intent(in) :: body
        logical :: found
        call json_extract_string(body, 'version', version, found)
        if (.not. found) version = ''
        call json_extract_string(body, 'commit', commit, found)
        if (.not. found) commit = ''
    end subroutine apply_version

    subroutine apply_whoami(body, status)
        character(len=*), intent(in) :: body
        integer, intent(in) :: status
        logical :: found
        checked = .true.
        signed_in = status == 200
        if (.not. signed_in) return
        call json_extract_string(body, 'account', account, found)
        call json_extract_string(body, 'scopes', scopes, found)
    end subroutine apply_whoami


    subroutine header()
        call row_begin(GAP, height=44.0)
        call text("fortyard", size=TITLE, style=UI_TITLE)
        if (signed_in) then
            ! Account and sign-out sit at the right edge of the page.
            call text("", width=UI_FILL)
            call text(account, size=SMALL, style=UI_CAPTION)
            if (button("signout", "Sign out", size=CTRL)) then
                call fetch_start(action_req, 'DELETE', '/api/v1/session')
                action = 'signout'
            end if
        end if
        call row_end()
    end subroutine header

    subroutine sign_in_view()
        call column_begin(GAP)
        call text("Sign in", size=HEADING, style=UI_HEADING)
        call text("Paste an access token. The browser keeps only a session cookie.", &
            size=BODY)
        call row_begin(GAP, height=ROW_H)
        if (text_input("token", token_input, placeholder="fy_...", submitted=submitted, &
            width=UI_FILL, size=CTRL)) continue
        if (button("signin", "Sign in", size=CTRL) .or. submitted) then
            if (len_trim(token_input) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/session', &
                    '{"token":"' // json_escape_string(trim(token_input)) // '"}')
                action = 'signin'
            end if
        end if
        call row_end()
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
        call text(value, size=BODY, width=UI_FILL, ellipsis=.true.)
        call row_end()
    end subroutine field

    subroutine tokens_view()
        real, parameter :: ACTION_W = 90.0
        integer :: k

        call column_begin(GAP)
        call text("New token", size=HEADING, style=UI_HEADING)
        call row_begin(GAP, height=ROW_H)
        if (text_input("newname", new_name, placeholder="Name, e.g. laptop", &
            submitted=submitted, width=320.0, size=CTRL)) continue
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
            call text(new_token, size=BODY, width=UI_FILL, ellipsis=.true., id="newtoken")
        end if
        call column_end()

        call column_begin(4.0)
        call text("Tokens", size=HEADING, style=UI_HEADING)
        if (.not. narrow) then
            call row_begin(GAP, height=24.0)
            call text("Name", size=SMALL, style=UI_CAPTION, width=UI_FILL)
            call text("Status", size=SMALL, style=UI_CAPTION, width=STATUS_W)
            call text("Created", size=SMALL, style=UI_CAPTION, width=DATE_W)
            call text("", width=ACTION_W)
            call row_end()
        end if
        do k = 1, size(tokens)
            call row_begin(GAP, id="row" // ui_str(tokens(k)%id), height=ROW_H)
            call text(tokens(k)%name, size=BODY, width=UI_FILL, ellipsis=.true.)
            if (.not. narrow) then
                call text(status_of(tokens(k)), size=SMALL, style=UI_CAPTION, &
                    width=STATUS_W)
                call text(day(tokens(k)%created), size=SMALL, style=UI_CAPTION, &
                    width=DATE_W)
            end if
            if (.not. tokens(k)%revoked .and. .not. tokens(k)%current) then
                if (button("revoke", "Revoke", key=tokens(k)%id, size=CTRL, &
                    width=ACTION_W)) then
                    call fetch_start(action_req, 'POST', '/api/v1/tokens/' // &
                        ui_str(tokens(k)%id) // '/revoke', '{}')
                    action = 'revoke'
                end if
            else
                call text("", width=ACTION_W)
            end if
            call row_end()
            if (narrow) call text(status_of(tokens(k)) // ", created " // &
                day(tokens(k)%created), size=SMALL, style=UI_CAPTION)
        end do
        call column_end()
    end subroutine tokens_view

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

        if (fetch_ready(version_req)) call apply_version(version_req%body)
        if (fetch_ready(whoami_req)) then
            call apply_whoami(whoami_req%body, whoami_req%status)
            if (signed_in) then
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

end module fy_ui_app
