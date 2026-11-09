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
    character(len=16), parameter :: tab_labels(2) = [character(len=16) :: "Overview", &
        "Access tokens"]
    integer :: tab, stat, i
    logical :: signed_in, checked, submitted
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

        call row_begin(16.0)
        call text("fortyard", style=UI_TITLE)
        if (signed_in) then
            call text(account, style=UI_CAPTION)
            if (button("signout", "Sign out")) then
                call fetch_start(action_req, 'DELETE', '/api/v1/session')
                action = 'signout'
            end if
        end if
        call row_end()

        if (.not. checked) then
            call text("Loading...")
        else if (.not. signed_in) then
            call sign_in_view()
        else
            if (tabs("tabs", tab_labels, tab)) message = ''
            select case (tab)
            case (1)
                call overview_view()
            case default
                call tokens_view()
            end select
        end if
        if (len(message) > 0) call text(message, style=UI_CAPTION)
        call ui_present()
    end do
    call ui_shutdown()

contains

    subroutine sign_in_view()
        call text("Sign in with an access token. The browser keeps only a session " // &
            "cookie, not the token.")
        call row_begin(8.0)
        if (text_input("token", token_input, placeholder="fy_...", submitted=submitted, &
            width=520.0)) continue
        if (button("signin", "Sign in") .or. submitted) then
            if (len_trim(token_input) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/session', &
                    '{"token":"' // json_escape_string(trim(token_input)) // '"}')
                action = 'signin'
            end if
        end if
        call row_end()
    end subroutine sign_in_view

    subroutine overview_view()
        call text("Server", style=UI_HEADING)
        call text("Version " // version)
        call text("Commit " // commit)
        call text("Account", style=UI_HEADING)
        call text(account // " (" // scopes // ")")
    end subroutine overview_view

    subroutine tokens_view()
        integer :: k

        call text("Create a token", style=UI_HEADING)
        call row_begin(8.0)
        if (text_input("newname", new_name, placeholder="Name, e.g. laptop", &
            submitted=submitted, width=320.0)) continue
        if (button("create", "Create") .or. submitted) then
            if (len_trim(new_name) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/tokens', &
                    '{"name":"' // json_escape_string(trim(new_name)) // '"}')
                action = 'create'
            end if
        end if
        call row_end()
        if (len(new_token) > 0) then
            call text("New token, shown once:", style=UI_CAPTION)
            call text(new_token)
        end if

        call text("Tokens", style=UI_HEADING)
        do k = 1, size(tokens)
            call row_begin(12.0, id="row" // ui_str(tokens(k)%id))
            call text(tokens(k)%name, width=220.0, ellipsis=.true.)
            call text(status_of(tokens(k)), style=UI_CAPTION, width=200.0)
            call text("created " // tokens(k)%created(1:min(10, len(tokens(k)%created))), &
                style=UI_CAPTION, width=180.0)
            if (.not. tokens(k)%revoked .and. .not. tokens(k)%current) then
                if (button("revoke", "Revoke", key=tokens(k)%id)) then
                    call fetch_start(action_req, 'POST', '/api/v1/tokens/' // &
                        ui_str(tokens(k)%id) // '/revoke', '{}')
                    action = 'revoke'
                end if
            end if
            call row_end()
        end do
    end subroutine tokens_view

    function status_of(t) result(s)
        type(token_row_t), intent(in) :: t
        character(len=:), allocatable :: s

        if (t%revoked) then
            s = "revoked"
        else if (t%current) then
            s = "this session"
        else if (len(t%expires) > 0) then
            s = "expires " // t%expires(1:min(10, len(t%expires)))
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
            end if
        end if
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
