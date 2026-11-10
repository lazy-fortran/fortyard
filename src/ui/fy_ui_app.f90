module fy_ui_app
    !! The fortyard application's state and frames: sign-in, the navigation
    !! drawer, repositories, access tokens and server status, each at its own
    !! location (fy_route). Compiled to WebAssembly for the browser and natively
    !! into the server, which composes the first frame of a page with the same
    !! code and sends it as HTML with the responses that produced it.
    use fortui
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string, json_extract_bool, json_parser_t, json_event_t, &
        json_parser_init, json_parser_next, JSON_KEY, JSON_STRING, JSON_INTEGER, &
        JSON_BOOL, JSON_OBJECT_END, JSON_END_OF_INPUT, JSON_ERROR
    use fy_fetch, only: request_t, fetch_start, fetch_ready
    use fy_nav, only: nav_push, nav_popped, nav_title, nav_assign, nav_set_pref, nav_copy
    use fy_route, only: route_t, route_parse, route_href, go, go_take, at, R_REPOS, R_TOKENS, &
        R_SERVER, R_TREE, R_BLOB, R_COMMITS, R_COMMIT, R_MISSING, R_JOBS, R_CHANGES, R_CHANGE, &
        R_ISSUES, R_ISSUE
    use fy_jobs_view, only: jobs_view_show, jobs_view_hide, jobs_view_receive, &
        jobs_view_compose, jobs_view_title
    use fy_change_view, only: change_view_show, change_view_receive, change_view_compose, &
        change_view_title, change_view_repo, change_view_missing
    use fy_repo_view, only: repo_view_reset, repo_view_show, repo_view_list, &
        repo_view_compose, repo_view_list_compose, repo_view_receive, repo_view_title, &
        repo_view_repo, repo_view_missing
    use fy_time, only: ago, day_of, when
    implicit none
    private

    public :: app_start, app_compose, app_title, app_missing

    type :: token_row_t
        integer :: id = 0
        character(len=:), allocatable :: name, scopes, created, expires, kind
        logical :: revoked = .false., current = .false., expired = .false.
    end type token_row_t

    type(request_t) :: version_req, whoami_req, tokens_req, action_req
    type(token_row_t), allocatable :: tokens(:)
    character(len=:), allocatable :: version, commit, built, account, scopes, message
    character(len=:), allocatable :: token_input, new_name, new_token, here, theme
    character(len=16) :: action = ''
    integer :: page = R_REPOS, theme_choice = 1
    logical :: signed_in = .false., checked = .false., submitted = .false.
    !! The server can send people through the institute's sign-in (Keycloak).
    logical :: login_offered = .false.
    logical :: pinned = .false., drawer_open = .false., copied = .false.
    real :: page_w = 1000.0, page_h = 800.0
    ! Text sizes and spacing of the page, in CSS pixels as forges use them.
    real, parameter :: BODY = 14.0, SMALL = 12.0, HEADING = 16.0, TITLE = 20.0
    real, parameter :: GAP = 8.0, ROW_H = 40.0
    !! From this width on a pinned sidebar stands beside the page (as in the CSS).
    real, parameter :: WIDE = 1012.0
    !! Rounds of answers to apply before the first frame: version and whoami,
    !! then a page's requests, then those they start (a README).
    integer, parameter :: SETTLE = 6
    character(len=8), parameter :: THEMES(3) = [character(len=8) :: 'system', 'light', 'dark']

contains

    subroutine app_start(location, sidebar, theme_pref)
        !! Starts at `location` (path and query), with the sidebar preference
        !! ('pinned' or not) and the colour theme ('light', 'dark' or the
        !! system's). Answers already at hand (the server's own, or those it sent
        !! with the page) are applied at once, so server and browser compose the
        !! same first frame.
        character(len=*), intent(in) :: location, sidebar, theme_pref
        integer :: k

        ! Everything starts afresh: the server renders pages for different people.
        version = ''
        commit = ''
        built = ''
        account = ''
        scopes = ''
        message = ''
        token_input = ''
        new_name = ''
        new_token = ''
        action = ''
        here = location
        theme = theme_pref
        theme_choice = 1
        do k = 1, size(THEMES)
            if (theme == trim(THEMES(k))) theme_choice = k
        end do
        pinned = sidebar == 'pinned'
        drawer_open = .false.
        copied = .false.
        signed_in = .false.
        checked = .false.
        submitted = .false.
        call set_page(location)
        if (allocated(tokens)) deallocate (tokens)
        allocate (tokens(0))
        call repo_view_reset()
        call fetch_start(version_req, 'GET', '/api/v1/version')
        call fetch_start(whoami_req, 'GET', '/api/v1/whoami')
        do k = 1, SETTLE
            call receive()
        end do
    end subroutine app_start

    subroutine set_page(location)
        character(len=*), intent(in) :: location
        type(route_t) :: r
        call route_parse(location, r)
        page = r%kind
    end subroutine set_page

    subroutine show(location)
        !! Shows the page at `location`, asking the server only for what is not
        !! already loaded.
        character(len=*), intent(in) :: location
        type(route_t) :: r

        here = location
        call route_parse(location, r)
        page = r%kind
        drawer_open = .false.
        message = ''
        if (page /= R_JOBS) call jobs_view_hide()
        if (.not. signed_in) return
        select case (page)
        case (R_REPOS)
            call repo_view_list()
        case (R_JOBS)
            call jobs_view_show(r%repo)
        case (R_TOKENS)
            copied = .false.
            call fetch_start(tokens_req, 'GET', '/api/v1/tokens')
        case (R_TREE, R_BLOB, R_COMMITS, R_COMMIT)
            call repo_view_show(r)
        case (R_CHANGES, R_CHANGE, R_ISSUES, R_ISSUE)
            call change_view_show(r)
        end select
    end subroutine show

    subroutine follow(href)
        !! A followed link: a page of the application gets its own history
        !! entry; anything else is left to the browser.
        character(len=*), intent(in) :: href
        type(route_t) :: r

        call route_parse(href, r)
        if (r%kind == R_MISSING .and. (index(href, '/api/') == 1 .or. &
            href == '/status' .or. href == '/healthz')) then
            call nav_assign(href)
        else
            call nav_push(href)
            call show(href)
        end if
    end subroutine follow

    function app_title() result(s)
        !! The page's title for the browser's tab and history.
        character(len=:), allocatable :: s
        if (.not. signed_in) then
            s = 'Sign in · fortyard'
            return
        end if
        select case (page)
        case (R_REPOS)
            s = 'Repositories · fortyard'
        case (R_TOKENS)
            s = 'Access tokens · fortyard'
        case (R_SERVER)
            s = 'Server · fortyard'
        case (R_JOBS)
            s = jobs_view_title()//' · fortyard'
        case (R_MISSING)
            s = 'Page not found · fortyard'
        case (R_CHANGES, R_CHANGE, R_ISSUES, R_ISSUE)
            s = change_view_title()//' · fortyard'
        case default
            s = repo_view_title()//' · fortyard'
        end select
    end function app_title

    logical function app_missing()
        !! Whether the page shown does not exist (the server answers 404).
        app_missing = signed_in .and. (page == R_MISSING .or. (in_repo(page) .and. &
            page <= R_COMMIT .and. repo_view_missing()) .or. (page >= R_CHANGES .and. &
            change_view_missing()))
    end function app_missing

    subroutine app_compose()
        !! One frame.
        character(len=:), allocatable :: href

        call receive()
        if (go_take(href)) call follow(href)
        if (nav_popped(href)) call show(href)
        if (drawer_open .and. ui_key_pressed(KEY_ESCAPE)) drawer_open = .false.
        call nav_title(app_title())
        call ui_window_size(page_w, page_h)

        call column_begin(0.0)
        call topbar()
        call row_begin(0.0, id="shell")
        if (signed_in .and. drawer_open) then
            if (button("scrim", "Close menu")) drawer_open = .false.
            call drawer("drawer")
        else if (signed_in .and. pinned) then
            call drawer("sidebar")
        end if
        call column_begin(0.0, id="main")
        call column_begin(16.0)
        if (.not. checked) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
        else if (.not. signed_in) then
            call sign_in_view()
        else
            select case (page)
            case (R_REPOS)
                call repo_view_list_compose(index(scopes, 'admin') > 0)
            case (R_TOKENS)
                call tokens_view()
            case (R_SERVER)
                call server_view()
            case (R_JOBS)
                call jobs_view_compose(page_w < 600.0)
            case (R_MISSING)
                call missing_view()
            case (R_CHANGES, R_CHANGE, R_ISSUES, R_ISSUE)
                call change_view_compose(index(scopes, 'admin') > 0, page_w < 600.0)
            case default
                call repo_view_compose()
            end select
        end if
        if (len(message) > 0) call text(message, size=BODY, tone=UI_TONE_NEGATIVE, id="message")
        call column_end()
        call column_end()
        call row_end()
        call column_end()
    end subroutine app_compose

    ! ---- frame of every page -------------------------------------------------------

    subroutine topbar()
        character(len=:), allocatable :: repo, rest
        integer :: slash

        call row_begin(GAP, id="top", height=64.0)
        if (signed_in) then
            if (button("menu", "Open menu", width=40.0, height=40.0)) then
                if (pinned .and. page_w >= WIDE) then
                    call pin(.false.)
                else
                    drawer_open = .not. drawer_open
                end if
            end if
        end if
        if (link("home", "fortyard", at("/"), size=HEADING, style=UI_HEADING, &
            tone=UI_TONE_NORMAL)) call go("/")
        if (signed_in .and. checked) then
            ! Where the page is: the repository's groups and name, or the page.
            call row_begin(4.0, id="context")
            if (in_repo(page)) then
                repo = page_repo()
                rest = repo
                call text("/", size=HEADING, tone=UI_TONE_MUTED)
                do
                    slash = index(rest, '/')
                    if (slash == 0) exit
                    call text(rest(1:slash - 1), size=HEADING, tone=UI_TONE_MUTED, &
                        id="group"//ui_str(len(rest)))
                    call text("/", size=HEADING, tone=UI_TONE_MUTED)
                    rest = rest(slash + 1:)
                end do
                if (link("repo", rest, route_href(R_TREE, repo), size=HEADING, &
                    style=UI_HEADING, tone=UI_TONE_NORMAL, ellipsis=.true.)) &
                    call go(route_href(R_TREE, repo))
            else if (page /= R_MISSING) then
                call text("/", size=HEADING, tone=UI_TONE_MUTED)
                call text(page_name(page), size=HEADING)
            end if
            call row_end()
        end if
        call text("", width=UI_FILL)
        if (signed_in) call text(account, size=BODY, tone=UI_TONE_MUTED, id="account")
        call row_end()
    end subroutine topbar

    logical function in_repo(kind)
        !! Whether a page of this kind belongs to a repository.
        integer, intent(in) :: kind
        in_repo = (kind >= R_TREE .and. kind <= R_COMMIT) .or. &
            (kind >= R_CHANGES .and. kind <= R_ISSUE)
    end function in_repo

    function page_repo() result(s)
        !! The repository of the page shown.
        character(len=:), allocatable :: s
        if (page >= R_CHANGES) then
            s = change_view_repo()
        else
            s = repo_view_repo()
        end if
    end function page_repo

    function page_name(kind) result(s)
        integer, intent(in) :: kind
        character(len=:), allocatable :: s
        select case (kind)
        case (R_TOKENS); s = "Access tokens"
        case (R_SERVER); s = "Server"
        case default; s = "Repositories"
        end select
    end function page_name

    subroutine drawer(name)
        !! The navigation: an overlay drawer, or pinned beside the page.
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: repo
        call column_begin(2.0, id=name)
        if (name == "drawer") then
            call row_begin(GAP, id="drawerhead", height=48.0)
            if (link("home", "fortyard", at("/"), size=HEADING, style=UI_HEADING, &
                tone=UI_TONE_NORMAL)) call go("/")
            call text("", width=UI_FILL)
            if (button("close", "Close menu", width=40.0, height=40.0)) drawer_open = .false.
            call row_end()
        end if
        if (in_repo(page)) then
            repo = page_repo()
            call text(repo, size=SMALL, style=UI_HEADING, tone=UI_TONE_MUTED, id="section", &
                width=UI_FILL)
            call side_link("code", "Code", route_href(R_TREE, repo), &
                page == R_TREE .or. page == R_BLOB)
            call side_link("history", "History", route_href(R_COMMITS, repo), &
                page == R_COMMITS .or. page == R_COMMIT)
            call side_link("changes", "Changes", route_href(R_CHANGES, repo), &
                page == R_CHANGES .or. page == R_CHANGE)
            call side_link("issues", "Issues", route_href(R_ISSUES, repo), &
                page == R_ISSUES .or. page == R_ISSUE)
            call text("", width=UI_FILL, id="hr1")
        end if
        call side_link("repos", "Repositories", at("/"), page == R_REPOS)
        call side_link("jobs", "Jobs", at("/jobs"), page == R_JOBS)
        call side_link("tokens", "Access tokens", at("/tokens"), page == R_TOKENS)
        call side_link("server", "Server", at("/server"), page == R_SERVER)
        call text("", width=UI_FILL, id="hr2")
        call text("Theme", size=SMALL, style=UI_HEADING, tone=UI_TONE_MUTED, id="section2")
        if (tabs("theme", [character(len=6) :: "System", "Light", "Dark"], theme_choice, &
            size=SMALL)) then
            theme = trim(THEMES(theme_choice))
            call nav_set_pref('fy_theme', theme)
        end if
        if (pinned) then
            if (button("pin", "Unpin sidebar", width=UI_FILL, height=ROW_H, size=BODY)) &
                call pin(.false.)
        else
            if (button("pin", "Keep sidebar open", width=UI_FILL, height=ROW_H, size=BODY)) &
                call pin(.true.)
        end if
        call text("Signed in as "//account, size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL, &
            id="who")
        if (button("signout", "Sign out", width=UI_FILL, height=ROW_H, size=BODY)) call sign_out()
        call column_end()
    end subroutine drawer

    subroutine side_link(id, label, href, selected)
        character(len=*), intent(in) :: id, label, href
        logical, intent(in) :: selected
        if (link(id, label, href, size=BODY, width=UI_FILL, &
            tone=merge(UI_TONE_STRONG, UI_TONE_NORMAL, selected))) then
            drawer_open = .false.
            call go(href)
        end if
    end subroutine side_link

    subroutine pin(on)
        logical, intent(in) :: on
        pinned = on
        drawer_open = .false.
        if (on) then
            call nav_set_pref('fy_sidebar', 'pinned')
        else
            call nav_set_pref('fy_sidebar', '')
        end if
    end subroutine pin

    subroutine sign_out()
        drawer_open = .false.
        call fetch_start(action_req, 'DELETE', '/api/v1/session')
        action = 'signout'
    end subroutine sign_out

    ! ---- pages ----------------------------------------------------------------------

    subroutine sign_in_view()
        call column_begin(12.0, id="card-signin")
        call text("Sign in to fortyard", size=TITLE, style=UI_HEADING)
        if (login_offered) then
            if (button("signin-sso", "Sign in with TU Graz account", width=UI_FILL, &
                height=ROW_H, size=BODY)) call nav_assign("/login")
            call text("Or paste an access token, as programs and the command line do.", &
                size=BODY, tone=UI_TONE_MUTED, width=UI_FILL)
        else
            call text("Paste an access token. The browser keeps only a session cookie.", &
                size=BODY, tone=UI_TONE_MUTED, width=UI_FILL)
        end if
        call text("Access token", size=BODY, style=UI_HEADING)
        if (text_input("token", token_input, placeholder="fy_...", submitted=submitted, &
            width=UI_FILL, size=BODY)) continue
        if (button("signin", "Sign in", width=UI_FILL, height=ROW_H, size=BODY) &
            .or. submitted) then
            if (len_trim(token_input) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/session', &
                    '{"token":"'//json_escape_string(trim(token_input))//'"}')
                action = 'signin'
            end if
        end if
        call text("No token yet? An administrator can create one for you.", size=SMALL, &
            tone=UI_TONE_MUTED, width=UI_FILL)
        call column_end()
    end subroutine sign_in_view

    subroutine missing_view()
        call text("Page not found", size=TITLE, style=UI_HEADING)
        call text("This page does not exist.", size=BODY, tone=UI_TONE_MUTED)
        if (link("back", "Go to the repositories", at("/"), size=BODY)) call go("/")
    end subroutine missing_view

    subroutine server_view()

        call text("Server", size=TITLE, style=UI_HEADING, id="title")
        call column_begin(0.0, id="card-server")
        call field("Version", version, .false.)
        call field("Commit", commit, .true.)
        if (len(built) >= 16) then
            call field("Built", when(built)//' UTC', .false.)
        else
            call field("Built", "unknown", .false.)
        end if
        call column_end()
        call text("Your account", size=HEADING, style=UI_HEADING, id="heading2")
        call column_begin(0.0, id="card-account")
        call field("Account", account, .false.)
        call row_begin(12.0, id="row-scopes", height=ROW_H)
        call text("Scopes", size=BODY, tone=UI_TONE_MUTED, width=120.0)
        call scope_badges(scopes)
        call row_end()
        call column_end()
        call text("Health", size=HEADING, style=UI_HEADING, id="heading3")
        call row_begin(16.0, id="links")
        if (link("status", "Status page", at("/status"), size=BODY)) call go("/status")
        if (link("healthz", "Health check", at("/healthz"), size=BODY)) call go("/healthz")
        if (link("versionapi", "Version (JSON)", at("/api/v1/version"), size=BODY)) &
            call go("/api/v1/version")
        call row_end()
    end subroutine server_view

    subroutine field(label, value, code)
        character(len=*), intent(in) :: label, value
        logical, intent(in) :: code

        call row_begin(12.0, id="row-"//label, height=ROW_H)
        call text(label, size=BODY, tone=UI_TONE_MUTED, width=120.0)
        if (code) then
            call text(value, size=SMALL, style=UI_CODE, width=UI_FILL)
        else
            call text(value, size=BODY, width=UI_FILL)
        end if
        call row_end()
    end subroutine field

    subroutine tokens_view()
        integer :: k, n

        call text("Access tokens", size=TITLE, style=UI_HEADING, id="title")
        call text("Tokens let the command line (fy login) and other programs act as you, "// &
            "with your scopes.", size=BODY, tone=UI_TONE_MUTED, width=UI_FILL, id="intro")
        call row_begin(GAP, id="createform", height=ROW_H)
        if (text_input("newname", new_name, placeholder="Token name, e.g. laptop", &
            submitted=submitted, width=UI_FILL, size=BODY)) continue
        if (button("create", "Create token", height=ROW_H, size=BODY) .or. submitted) then
            if (len_trim(new_name) > 0) then
                call fetch_start(action_req, 'POST', '/api/v1/tokens', &
                    '{"name":"'//json_escape_string(trim(new_name))//'"}')
                action = 'create'
            end if
        end if
        call row_end()
        if (len(new_token) > 0) then
            call column_begin(GAP, id="card-newtoken")
            call text("Copy your new token now. It is shown only once.", size=BODY, &
                style=UI_HEADING, width=UI_FILL)
            call row_begin(GAP, id="tokenrow", height=ROW_H)
            call text(new_token, size=SMALL, style=UI_CODE, width=UI_FILL, id="newtoken")
            if (button("copy", merge("Copied", "Copy  ", copied), height=ROW_H, size=BODY)) then
                call nav_copy(new_token)
                copied = .true.
            end if
            call row_end()
            call column_end()
        end if

        call text("Tokens", size=HEADING, style=UI_HEADING, id="heading-tokens")
        call column_begin(0.0, id="card-tokens")
        n = 0
        do k = 1, size(tokens)
            if (tokens(k)%kind == 'session' .or. tokens(k)%revoked) cycle
            n = n + 1
            call row_begin(12.0, id="row-t"//ui_str(tokens(k)%id), height=64.0)
            call column_begin(4.0)
            call row_begin(GAP)
            call text(tokens(k)%name, size=BODY, style=UI_HEADING)
            call scope_badges(tokens(k)%scopes)
            call row_end()
            call text(token_dates(tokens(k)), size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL)
            call column_end()
            if (tokens(k)%current) then
                call text("used here", size=SMALL, id="here")
            else if (button("revoke", "Revoke", key=tokens(k)%id, height=ROW_H, size=BODY)) then
                call revoke(tokens(k)%id)
            end if
            call row_end()
        end do
        if (n == 0) then
            call row_begin(12.0, id="row-none", height=ROW_H)
            call text("No access tokens.", size=BODY, tone=UI_TONE_MUTED)
            call row_end()
        end if
        call column_end()

        call text("Sessions", size=HEADING, style=UI_HEADING, id="heading-sessions")
        call text("Browsers signed in to your account. Signing a session out ends it there.", &
            size=BODY, tone=UI_TONE_MUTED, width=UI_FILL, id="intro2")
        call column_begin(0.0, id="card-sessions")
        do k = 1, size(tokens)
            if (tokens(k)%kind /= 'session' .or. tokens(k)%revoked .or. tokens(k)%expired) cycle
            call row_begin(12.0, id="row-s"//ui_str(tokens(k)%id), height=64.0)
            call column_begin(4.0)
            call row_begin(GAP)
            call text("Browser session", size=BODY, style=UI_HEADING)
            if (tokens(k)%current) call text("this browser", size=SMALL, id="current")
            call row_end()
            call text("Signed in "//ago(tokens(k)%created)//" · expires "// &
                day_of(tokens(k)%expires), size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL)
            call column_end()
            if (tokens(k)%current) then
                if (button("signouthere", "Sign out", height=ROW_H, size=BODY)) call sign_out()
            else if (button("revoke", "Sign out", key=tokens(k)%id, height=ROW_H, size=BODY)) then
                call revoke(tokens(k)%id)
            end if
            call row_end()
        end do
        call column_end()
    end subroutine tokens_view

    subroutine scope_badges(list)
        character(len=*), intent(in) :: list
        character(len=:), allocatable :: rest
        integer :: k, sp

        rest = trim(adjustl(list))
        k = 0
        do while (len(rest) > 0)
            k = k + 1
            sp = index(rest, ' ')
            if (sp == 0) sp = len(rest) + 1
            call text(rest(1:sp - 1), size=SMALL, id="scope"//ui_str(k))
            if (sp > len(rest)) exit
            rest = trim(adjustl(rest(sp + 1:)))
        end do
    end subroutine scope_badges

    function token_dates(t) result(s)
        type(token_row_t), intent(in) :: t
        character(len=:), allocatable :: s
        s = "Created "//ago(t%created)
        if (t%expired) then
            s = s//" · expired "//day_of(t%expires)
        else if (len(t%expires) > 0) then
            s = s//" · expires "//day_of(t%expires)
        else
            s = s//" · never expires"
        end if
    end function token_dates

    subroutine revoke(id)
        integer, intent(in) :: id
        call fetch_start(action_req, 'POST', '/api/v1/tokens/'//ui_str(id)//'/revoke', '{}')
        action = 'revoke'
    end subroutine revoke

    ! ---- responses ------------------------------------------------------------------

    subroutine receive()
        !! Applies finished requests to the application state.
        logical :: found

        if (fetch_ready(version_req)) call apply_version(version_req%body)
        if (fetch_ready(whoami_req)) then
            call apply_whoami(whoami_req%body, whoami_req%status)
            if (signed_in) call show(here)
        end if
        call repo_view_receive()
        call change_view_receive()
        call jobs_view_receive()
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
                call repo_view_reset()
                call nav_push('/')
                here = '/'
                page = R_REPOS
            case ('create')
                if (action_req%status == 201) then
                    call json_extract_string(action_req%body, 'token', new_token, found)
                    new_name = ''
                    message = ''
                    copied = .false.
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

    subroutine apply_version(body)
        character(len=*), intent(in) :: body
        logical :: found
        call json_extract_string(body, 'version', version, found)
        if (.not. found) version = ''
        call json_extract_string(body, 'commit', commit, found)
        if (.not. found) commit = ''
        call json_extract_string(body, 'built', built, found)
        if (.not. found) built = ''
        call json_extract_bool(body, 'login', login_offered, found)
        if (.not. found) login_offered = .false.
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
                case ('kind'); row%kind = ev%string_val
                end select
            case (JSON_INTEGER)
                if (key == 'id') row%id = ev%int_val
            case (JSON_BOOL)
                if (key == 'revoked') row%revoked = ev%bool_val
                if (key == 'current') row%current = ev%bool_val
                if (key == 'expired') row%expired = ev%bool_val
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
            row%kind = 'token'
            row%revoked = .false.
            row%current = .false.
            row%expired = .false.
        end subroutine reset_row

    end subroutine parse_tokens

end module fy_ui_app
