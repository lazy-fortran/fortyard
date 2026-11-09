module fy_server
    !! Request routing for `fy serve`.
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string
    use fy_build_info, only: FY_VERSION, FY_COMMIT, FY_BUILT
    use fy_http, only: http_request_t, http_read_request, http_send_response, &
        http_bearer_token, http_cookie, http_query_param, http_read_head, http_read_body, &
        http_basic_password
    use fy_git_http, only: git_http_route, git_http_serve
    use fortui, only: ui_init, ui_frame_begin, ui_present, ui_shutdown
    use fortui_backend_headless, only: headless_backend_t
    use fortui_html, only: ui_frame_html
    use fy_ui_app, only: app_start, app_compose, app_title, app_missing
    use fy_fetch, only: fetch_serve, fetch_served_json
    use fy_route, only: route_is_page
    use fy_repo, only: repo_name_ok, repo_ref_ok, repo_path_ok, repo_log_json, repo_tree_json, &
        repo_blob, repo_commit_json, repo_refs_json, repo_clone, repo_fetch, repo_init
    use fy_posix, only: tcp_accept, sock_close, sock_set_timeouts
    use fy_sqlite, only: db_t
    use fy_proc, only: proc_fork, proc_wait_any, proc_exit, proc_pid, &
        proc_die_with_parent, proc_sleep_ms
    use fy_store, only: token_identity_t, token_info_t, store_token_lookup, store_token_create, &
        store_open, store_token_list, store_token_revoke, store_token_revoke_hash, repo_info_t, &
        store_repo_add, store_repo_list, store_repo_fetched, store_repo_set_mode
    use fy_sqlite, only: db_close
    implicit none
    private

    public :: server_loop, serve_connection, route, server_supervise, server_set_web_dir
    public :: server_set_data_dir, server_set_mirror_interval
    public :: response_t

    character(len=*), parameter :: JSON = 'application/json'
    character(len=*), parameter :: SESSION_COOKIE = 'fy_session'
    integer, parameter :: SESSION_DAYS = 14
    character(len=*), parameter :: SESSION_MAX_AGE = '1209600'
    !! Name of the tokens that are browser sessions.
    character(len=*), parameter :: SESSION_NAME = 'browser session'
    !! Preferences the browser keeps in cookies, read for the first frame.
    character(len=*), parameter :: SIDEBAR_COOKIE = 'fy_sidebar', THEME_COOKIE = 'fy_theme'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)

    type :: response_t
        integer :: code = 200
        character(len=:), allocatable :: ctype, body, headers, cache
    end type response_t

    !! Directory of the web application; empty serves the status page at /.
    character(len=:), allocatable :: web_dir
    !! Directory of the repository mirrors.
    character(len=:), allocatable :: repos_dir
    !! Seconds between fetches of a mirror.
    integer :: mirror_interval = 60
    !! Database and cookie of the page being composed (in critical fortui_render).
    type(db_t) :: page_db
    character(len=:), allocatable :: page_cookie

contains

    subroutine server_supervise(lfd, db_path, nworkers, nthreads)
        !! Pre-forked workers share the listening socket, each with its own
        !! database connection. A worker that dies, for example on a Fortran
        !! runtime error, takes only its own requests down and is replaced. Inside a
        !! worker, OpenMP threads run their own accept loops, so a long-lived
        !! connection holds one thread rather than one process.
        integer, intent(in) :: lfd, nworkers, nthreads
        character(len=*), intent(in) :: db_path
        ! Slot nworkers + 1, when there is a data directory, is the process
        ! that mirrors repositories.
        integer :: pids(nworkers + 1)
        integer(int64) :: started(nworkers + 1), now, rate
        integer :: i, pid, status, slots
        integer :: me

        me = proc_pid()
        slots = nworkers
        if (allocated(repos_dir)) slots = nworkers + 1
        do i = 1, slots
            call spawn(i)
        end do
        do
            call proc_wait_any(pid, status)
            if (pid < 0) then
                call proc_sleep_ms(100)
                cycle
            end if
            do i = 1, slots
                if (pids(i) /= pid) cycle
                write (error_unit, '(a,i0,a,i0)') 'fy: worker ', pid, &
                    ' ended with wait status ', status
                call system_clock(now, rate)
                ! A worker that dies within a second of starting is crash-looping.
                if (now - started(i) < rate) call proc_sleep_ms(1000)
                call spawn(i)
            end do
        end do

    contains

        subroutine spawn(slot)
            ! The supervisor never starts an OpenMP team: a child forked from a
            ! process with live OpenMP threads inherits a broken thread pool.
            integer, intent(in) :: slot
            integer(int64) :: rate_

            do
                flush (error_unit)
                flush (output_unit)
                call system_clock(started(slot), rate_)
                pids(slot) = proc_fork()
                if (pids(slot) >= 0) exit
                write (error_unit, '(a)') 'fy: cannot fork a worker; retrying'
                call proc_sleep_ms(1000)
            end do
            if (pids(slot) /= 0) return
            if (.not. proc_die_with_parent(me)) call proc_exit(0)
            if (slot > nworkers) then
                call mirror_main(db_path)
            else
                call worker_main(lfd, db_path, nthreads)
            end if
            call proc_exit(0)
        end subroutine spawn

    end subroutine server_supervise

    subroutine mirror_main(db_path)
        !! Clones repositories that have no mirror yet and fetches the others
        !! every mirror interval; a failed fetch is recorded and retried then.
        character(len=*), intent(in) :: db_path
        type(db_t) :: db
        type(repo_info_t), allocatable :: repos(:)
        character(len=64), allocatable :: names(:)
        integer(int64), allocatable :: last(:)
        integer(int64) :: now, rate
        integer :: stat, i, k, status
        logical :: exists

        allocate (names(0), last(0))
        call execute_command_line('mkdir -p '''//repos_dir//'''', exitstat=stat)
        do
            call store_open(db_path, db, stat)
            if (stat == 0) then
                call store_repo_list(db, repos, stat)
                do i = 1, size(repos)
                    k = findloc(names, repos(i)%name, 1)
                    if (k == 0) then
                        names = [names, [character(len=64) :: repos(i)%name]]
                        call system_clock(now, rate)
                        ! Due at once: fetch in the first round after a start.
                        last = [last, now - int(mirror_interval, int64) * rate]
                        k = size(names)
                    end if
                    call system_clock(now, rate)
                    inquire (file=repo_path(repos(i)%name)//'/HEAD', exist=exists)
                    if (repos(i)%mode == 'primary') then
                        ! Pushed to fortyard, never fetched; created empty once.
                        if (exists) cycle
                        call repo_init(repo_path(repos(i)%name), status)
                    else if (.not. exists) then
                        call repo_clone(repos(i)%upstream, repo_path(repos(i)%name), status)
                    else if (now - last(k) >= int(mirror_interval, int64) * rate) then
                        call repo_fetch(repo_path(repos(i)%name), status)
                    else
                        cycle
                    end if
                    last(k) = now
                    call store_repo_fetched(db, repos(i)%name, status)
                    if (status /= 0) then
                        write (error_unit, '(a,i0)') 'fy: mirroring '//repos(i)%name// &
                            ' failed with status ', status
                        flush (error_unit)
                    end if
                end do
                call db_close(db)
            end if
            call proc_sleep_ms(min(10000, 1000 * mirror_interval))
        end do
    end subroutine mirror_main

    function repo_path(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        path = repos_dir//'/'//trim(name)//'.git'
    end function repo_path

    subroutine server_set_mirror_interval(seconds)
        !! Set once before the mirror process starts.
        integer, intent(in) :: seconds
        mirror_interval = max(1, seconds)
    end subroutine server_set_mirror_interval

    subroutine server_set_data_dir(dir)
        !! Set once before workers start; read-only afterwards.
        character(len=*), intent(in) :: dir
        repos_dir = dir//'/repos'
    end subroutine server_set_data_dir

    subroutine repos_route(req, db, res)
        !! /api/v1/repos and /api/v1/repos/NAME/{refs,log,tree,blob,commits/ID}.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        type(token_identity_t) :: who
        type(repo_info_t), allocatable :: repos(:)
        character(len=:), allocatable :: rest, name, sub, ref, path, upstream
        integer :: slash, i, status, stat
        logical :: found, exists

        if (.not. authenticated(req, db, who, res)) return
        if (req%path == '/api/v1/repos') then
            if (req%method == 'POST') then
                if (index(' '//who%scopes//' ', ' admin ') == 0) then
                    call fail(res, 403, 'adding repositories needs the admin scope')
                    return
                end if
                call json_extract_string(req%body, 'name', name, found)
                if (.not. found) name = ''
                call json_extract_string(req%body, 'upstream', upstream, found)
                if (.not. found) upstream = ''
                if (.not. repo_name_ok(name) .or. (len(upstream) > 0 .and. &
                    .not. upstream_ok(upstream))) then
                    call fail(res, 400, 'body must be {"name":"...","upstream":"..."} with a'// &
                        ' lower-case name and an https or ssh URL, or without upstream for a'// &
                        ' repository pushed to fortyard')
                    return
                end if
                ! Without upstream the repository is primary: created empty here.
                if (len(upstream) == 0) then
                    call store_repo_add(db, name, '', stat, mode='primary')
                    if (stat == 0) call repo_init(repo_path(name), status)
                else
                    call store_repo_add(db, name, upstream, stat)
                end if
                if (stat /= 0) then
                    call fail(res, 409, 'repository exists')
                    return
                end if
                res%code = 201
                res%body = '{"name":"'//name//'"}'
                return
            end if
            if (.not. method_is(req, 'GET', res)) return
            res%body = repos_json(db)
            return
        end if

        rest = req%path(len('/api/v1/repos/') + 1:)
        if (req%method == 'POST' .and. len(rest) > 5) then
            if (rest(len(rest) - 4:) == '/mode') then
                call set_mode(rest(1:len(rest) - 5))
                return
            end if
        end if
        if (.not. method_is(req, 'GET', res)) return
        slash = index(rest, '/')
        if (slash < 2) then
            call fail(res, 404, 'not found')
            return
        end if
        name = rest(1:slash - 1)
        sub = rest(slash + 1:)
        inquire (file=repo_path(name)//'/HEAD', exist=exists)
        if (.not. repo_name_ok(name) .or. .not. exists) then
            call fail(res, 404, 'no such repository, or not mirrored yet')
            return
        end if
        ref = http_query_param(req%query, 'ref')
        if (len(ref) == 0) ref = 'HEAD'
        path = http_query_param(req%query, 'path')
        if (.not. repo_ref_ok(ref) .or. .not. repo_path_ok(path)) then
            call fail(res, 400, 'bad ref or path')
            return
        end if
        select case (sub)
        case ('refs')
            call repo_refs_json(repo_path(name), res%body, status)
        case ('log')
            call repo_log_json(repo_path(name), ref, 100, res%body, status)
        case ('tree')
            call repo_tree_json(repo_path(name), ref, path, res%body, status)
        case ('blob')
            call repo_blob(repo_path(name), ref, path, res%body, status)
            if (status == 413) then
                call fail(res, 413, 'file larger than 1 MiB')
                return
            end if
            res%ctype = 'text/plain; charset=utf-8'
        case default
            if (index(sub, 'commits/') /= 1 .or. .not. repo_ref_ok(sub(9:))) then
                call fail(res, 404, 'not found')
                return
            end if
            call repo_commit_json(repo_path(name), sub(9:), res%body, status)
        end select
        if (status /= 0) call fail(res, 404, 'no such ref, path or commit')

    contains

        subroutine set_mode(target)
            !! POST /api/v1/repos/NAME/mode {"mode":"primary"|"mirror"}: a primary
            !! repository takes pushes and is no longer fetched from upstream.
            character(len=*), intent(in) :: target
            character(len=:), allocatable :: mode
            logical :: ok

            if (index(' '//who%scopes//' ', ' admin ') == 0) then
                call fail(res, 403, 'changing a repository mode needs the admin scope')
                return
            end if
            call json_extract_string(req%body, 'mode', mode, ok)
            if (.not. ok) mode = ''
            if (mode /= 'primary' .and. mode /= 'mirror') then
                call fail(res, 400, 'body must be {"mode":"primary"} or {"mode":"mirror"}')
                return
            end if
            call store_repo_set_mode(db, target, mode, ok)
            if (.not. ok) then
                call fail(res, 404, 'no such repository')
                return
            end if
            res%body = '{"name":"'//json_escape_string(target)//'","mode":"'//mode//'"}'
        end subroutine set_mode

    end subroutine repos_route

    pure logical function upstream_ok(url)
        !! An https or ssh git URL, never an option or a local path.
        character(len=*), intent(in) :: url

        upstream_ok = len(url) > 8 .and. len(url) < 512 .and. index(url, ' ') == 0
        if (.not. upstream_ok) return
        upstream_ok = index(url, 'https://') == 1 .or. index(url, 'ssh://') == 1 .or. &
            (index(url, 'git@') == 1 .and. index(url, ':') > 5)
    end function upstream_ok

    subroutine worker_main(lfd, db_path, nthreads)
        integer, intent(in) :: lfd, nthreads
        character(len=*), intent(in) :: db_path
        type(db_t) :: db
        integer :: stat

        !$omp parallel num_threads(nthreads) default(none) private(db, stat) &
        !$omp shared(lfd, db_path)
        call store_open(db_path, db, stat)
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: worker thread cannot open the database'
            flush (error_unit)
            call proc_exit(1)
        end if
        call server_loop(lfd, db)
        !$omp end parallel
    end subroutine worker_main

    subroutine server_loop(lfd, db)
        integer, intent(in) :: lfd
        type(db_t), intent(in) :: db
        integer :: cfd

        do
            cfd = tcp_accept(lfd)
            if (cfd < 0) cycle
            call sock_set_timeouts(cfd, 10)
            call serve_connection(cfd, db)
            call sock_close(cfd)
        end do
    end subroutine server_loop

    subroutine serve_connection(cfd, db)
        integer, intent(in) :: cfd
        type(db_t), intent(in) :: db
        type(http_request_t) :: req
        type(response_t) :: res
        character(len=:), allocatable :: rest, repo
        integer :: status
        logical :: write

        call http_read_head(cfd, req, rest, status)
        if (status < 0) return
        if (status == 0) then
            if (git_http_route(req%path, req%query, repo, write)) then
                call git_route(cfd, req, rest, db, repo, write)
                return
            end if
            call http_read_body(cfd, req, rest, status)
        end if
        if (status /= 0) then
            call http_send_response(cfd, status, JSON, '{"error":"bad request"}')
            return
        end if
        call route(req, db, res)
        call http_send_response(cfd, res%code, res%ctype, res%body, res%headers, res%cache)
        write (error_unit, '(a,1x,a,1x,i0)') req%method, req%path, res%code
        flush (error_unit)
    end subroutine serve_connection

    subroutine git_route(cfd, req, rest, db, repo, write)
        !! Clone, fetch and push over HTTP. Git sends the access token as the
        !! Basic password (any user name) or as a bearer token. Reading needs a
        !! valid token; pushing needs the admin scope and a primary repository.
        integer, intent(in) :: cfd
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: rest, repo
        type(db_t), intent(in) :: db
        logical, intent(in) :: write
        type(token_identity_t) :: who
        type(repo_info_t), allocatable :: repos(:)
        character(len=:), allocatable :: token, mode
        integer :: status, i, stat
        logical :: exists

        token = http_basic_password(req)
        if (len(token) == 0) token = http_bearer_token(req)
        call store_token_lookup(db, token, who)
        status = 0
        if (.not. who%found) then
            call http_send_response(cfd, 401, 'text/plain; charset=utf-8', &
                'Use an access token as the password.'//new_line('a'), &
                'WWW-Authenticate: Basic realm="fortyard"'//CRLF)
            status = 401
        else
            inquire (file=repo_path(repo)//'/HEAD', exist=exists)
            mode = ''
            if (repo_name_ok(repo) .and. exists) then
                call store_repo_list(db, repos, stat)
                do i = 1, size(repos)
                    if (repos(i)%name == repo) mode = repos(i)%mode
                end do
            end if
            if (len(mode) == 0) then
                status = 404
            else if (write .and. index(' '//who%scopes//' ', ' admin ') == 0) then
                status = 403
            else if (write .and. mode /= 'primary') then
                ! A mirror follows its upstream; pushes go there.
                status = 409
            else
                call git_http_serve(cfd, req, rest, repos_dir, repo, who%account, status)
            end if
            if (status /= 0) call http_send_response(cfd, status, 'text/plain; charset=utf-8', &
                git_error(status)//new_line('a'))
        end if
        if (status == 0) status = 200
        write (error_unit, '(a,1x,a,1x,i0)') req%method, req%path, status
        flush (error_unit)
    end subroutine git_route

    function git_error(status) result(s)
        integer, intent(in) :: status
        character(len=:), allocatable :: s
        select case (status)
        case (403); s = 'Pushing needs the admin scope.'
        case (404); s = 'No such repository.'
        case (409); s = 'This repository mirrors its upstream; push there.'
        case (413); s = 'The push is too large.'
        case default; s = 'The git request failed.'
        end select
    end function git_error

    recursive subroutine route(req, db, res)
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(out) :: res
        type(token_identity_t) :: who
        character(len=:), allocatable :: rest
        integer(int64) :: id
        integer :: ios

        res%code = 200
        res%ctype = JSON
        res%body = ''
        res%headers = ''
        res%cache = 'no-store'
        select case (req%path)
        case ('/healthz')
            if (.not. method_is(req, 'GET', res)) return
            res%ctype = 'text/plain; charset=utf-8'
            res%body = 'ok '//FY_VERSION//' '//FY_COMMIT//new_line('a')
        case ('/', '/index.html')
            if (.not. method_is(req, 'GET', res)) return
            res%code = 404
            if (allocated(web_dir)) call render_page(req, db, res)
            if (res%code == 404) then
                res%code = 200
                res%ctype = 'text/html; charset=utf-8'
                res%body = status_page()
            end if
        case ('/status')
            if (.not. method_is(req, 'GET', res)) return
            res%ctype = 'text/html; charset=utf-8'
            res%body = status_page()
        case ('/api/v1/version')
            if (.not. method_is(req, 'GET', res)) return
            res%body = version_json()
        case ('/api/v1/whoami')
            if (.not. method_is(req, 'GET', res)) return
            if (.not. authenticated(req, db, who, res)) return
            res%body = '{"account":"'//json_escape_string(who%account)// &
                '","token":"'//json_escape_string(who%token_name)// &
                '","scopes":"'//json_escape_string(who%scopes)//'"}'
        case ('/api/v1/tokens')
            if (req%method == 'GET') then
                if (.not. authenticated(req, db, who, res)) return
                call list_tokens(db, who, res)
            else if (req%method == 'POST') then
                if (.not. authenticated(req, db, who, res)) return
                call create_token(req, db, who, res)
            else
                call fail(res, 405, 'use GET or POST')
            end if
        case ('/api/v1/session')
            if (req%method == 'POST') then
                call open_session(req, db, res)
            else if (req%method == 'DELETE') then
                call close_session(req, db, res)
            else
                call fail(res, 405, 'use POST or DELETE')
            end if
        case default
            if (index(req%path, '/app/') == 1) then
                if (.not. method_is(req, 'GET', res)) return
                call static_file(req, req%path(6:), res)
            else if (req%path == '/api/v1/repos' .or. index(req%path, '/api/v1/repos/') == 1) then
                call repos_route(req, db, res)
            else if (index(req%path, '/api/v1/tokens/') == 1) then
                rest = req%path(16:)
                if (index(rest, '/revoke') /= len(rest) - 6 .or. len(rest) < 8) then
                    call fail(res, 404, 'not found')
                    return
                end if
                read (rest(1:len(rest) - 7), *, iostat=ios) id
                if (ios /= 0 .or. verify(rest(1:len(rest) - 7), '0123456789') /= 0) then
                    call fail(res, 404, 'not found')
                    return
                end if
                if (.not. method_is(req, 'POST', res)) return
                if (.not. authenticated(req, db, who, res)) return
                call revoke_token(db, who, id, res)
            else
                call fail(res, 404, 'not found')
                if (route_is_page(req%path) .and. allocated(web_dir) .and. req%method == 'GET') &
                    call render_page(req, db, res)
            end if
        end select
    end subroutine route

    logical function method_is(req, method, res)
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: method
        type(response_t), intent(inout) :: res

        method_is = req%method == method
        if (.not. method_is) call fail(res, 405, 'use '//method)
    end function method_is

    subroutine fail(res, code, message)
        type(response_t), intent(inout) :: res
        integer, intent(in) :: code
        character(len=*), intent(in) :: message

        res%code = code
        res%ctype = JSON
        res%body = '{"error":"'//json_escape_string(message)//'"}'
    end subroutine fail

    logical function authenticated(req, db, who, res)
        !! A bearer token, or the browser session cookie. A request that changes
        !! state with the cookie must be JSON, which a cross-site form cannot send.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(out) :: who
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: token

        token = http_bearer_token(req)
        if (len(token) == 0) then
            token = http_cookie(req, SESSION_COOKIE)
            if (len(token) > 0 .and. req%method /= 'GET' .and. &
                index(req%content_type, 'application/json') /= 1) then
                call fail(res, 415, 'send JSON')
                authenticated = .false.
                return
            end if
        end if
        call store_token_lookup(db, token, who)
        authenticated = who%found
        if (.not. authenticated) call fail(res, 401, 'missing or invalid token')
    end function authenticated

    subroutine create_token(req, db, who, res)
        !! A token can mint tokens for its own account with at most its scopes.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: name, token
        logical :: found
        integer :: stat

        call json_extract_string(req%body, 'name', name, found)
        if (.not. found .or. len(name) == 0 .or. len(name) > 100) then
            call fail(res, 400, 'body must be {"name":"..."}')
            return
        end if
        call store_token_create(db, who%account_id, name, who%scopes, token, stat)
        if (stat /= 0) then
            call fail(res, 500, 'token not created')
            return
        end if
        res%code = 201
        res%body = '{"token":"'//token//'","name":"'//json_escape_string(name)// &
            '","scopes":"'//json_escape_string(who%scopes)//'"}'
    end subroutine create_token

    subroutine list_tokens(db, who, res)
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        type(response_t), intent(inout) :: res
        type(token_info_t), allocatable :: tokens(:)
        character(len=24) :: num
        integer :: i, stat

        call store_token_list(db, who%account_id, tokens, stat)
        if (stat /= 0) then
            call fail(res, 500, 'tokens not listed')
            return
        end if
        res%body = '{"tokens":['
        do i = 1, size(tokens)
            write (num, '(i0)') tokens(i)%id
            if (i > 1) res%body = res%body//','
            res%body = res%body//'{"id":'//trim(num)// &
                ',"name":"'//json_escape_string(tokens(i)%name)// &
                '","scopes":"'//json_escape_string(tokens(i)%scopes)// &
                '","created":"'//tokens(i)%created_at// &
                '","expires":"'//tokens(i)%expires_at// &
                '","revoked":'//json_bool(tokens(i)%revoked)// &
                ',"current":'//json_bool(tokens(i)%id == who%token_id)// &
                ',"expired":'//json_bool(tokens(i)%expired)//',"kind":"'// &
                trim(merge('session', 'token  ', tokens(i)%name == SESSION_NAME))//'"}'
        end do
        res%body = res%body//']}'
    end subroutine list_tokens

    subroutine revoke_token(db, who, id, res)
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        integer(int64), intent(in) :: id
        type(response_t), intent(inout) :: res
        logical :: found

        call store_token_revoke(db, who%account_id, id, found)
        if (.not. found) then
            call fail(res, 404, 'no such token')
            return
        end if
        res%body = '{"revoked":true}'
    end subroutine revoke_token

    subroutine open_session(req, db, res)
        !! Exchanges a token for a browser session in an HttpOnly cookie, so
        !! scripts on the page never hold a credential.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        type(token_identity_t) :: who
        character(len=:), allocatable :: token, session
        logical :: found
        integer :: stat

        if (index(req%content_type, 'application/json') /= 1) then
            call fail(res, 415, 'send JSON')
            return
        end if
        call json_extract_string(req%body, 'token', token, found)
        if (.not. found) token = ''
        call store_token_lookup(db, token, who)
        if (.not. who%found) then
            call fail(res, 401, 'missing or invalid token')
            return
        end if
        call store_token_create(db, who%account_id, SESSION_NAME, who%scopes, session, &
            stat, days=SESSION_DAYS)
        if (stat /= 0) then
            call fail(res, 500, 'session not created')
            return
        end if
        res%headers = 'Set-Cookie: '//SESSION_COOKIE//'='//session// &
            '; Path=/; HttpOnly; SameSite=Strict; Max-Age='//SESSION_MAX_AGE//CRLF
        res%body = '{"account":"'//json_escape_string(who%account)//'"}'
    end subroutine open_session

    subroutine close_session(req, db, res)
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: token

        token = http_cookie(req, SESSION_COOKIE)
        if (len(token) > 0) call store_token_revoke_hash(db, token)
        res%headers = 'Set-Cookie: '//SESSION_COOKIE// &
            '=; Path=/; HttpOnly; SameSite=Strict; Max-Age=0'//CRLF
        res%body = '{"closed":true}'
    end subroutine close_session

    function repos_json(db) result(s)
        !! The mirrored repositories as GET /api/v1/repos returns them.
        type(db_t), intent(in) :: db
        character(len=:), allocatable :: s
        type(repo_info_t), allocatable :: repos(:)
        integer :: i, stat
        logical :: exists

        call store_repo_list(db, repos, stat)
        s = '{"repos":['
        do i = 1, size(repos)
            inquire (file=repo_path(repos(i)%name)//'/HEAD', exist=exists)
            if (i > 1) s = s//','
            s = s//'{"name":"'//repos(i)%name//'","mode":"'//repos(i)%mode//'","upstream":"'// &
                json_escape_string(repos(i)%upstream)//'","fetched":"'// &
                repos(i)%fetched_at//'","ready":'//json_bool(exists)//'}'
        end do
        s = s//']}'
    end function repos_json

    recursive subroutine render_page(req, db, res)
        !! The web application's page at the request's location with its first
        !! frame already in it, composed here by the same Fortran code the
        !! browser runs, and the API answers that frame was made from, so the
        !! WebAssembly continues without asking again. Personal, so never cached.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: template, location, frame, data, title
        integer :: at, a2, code
        logical :: ok

        call read_file(web_dir//'/index.html', template, ok)
        if (.not. ok) return
        location = req%path
        if (len(req%query) > 0) location = location//'?'//req%query
        ! fortui keeps one composition at a time, and the page's requests are
        ! answered through module state.
        !$omp critical (fortui_render)
        page_db = db
        page_cookie = req%cookie
        call fetch_serve(page_fetch)
        frame = first_frame(location, http_cookie(req, SIDEBAR_COOKIE), &
            http_cookie(req, THEME_COOKIE), title, code)
        data = json_script('fy-pre', fetch_served_json())
        call fetch_serve()
        !$omp end critical (fortui_render)
        at = index(template, '<div id="app"></div>')
        if (at == 0) return
        template = template(1:at - 1)//'<div id="app">'//frame//'</div>'// &
            template(at + len('<div id="app"></div>'):)
        at = index(template, '<!--fy-data-->')
        if (at > 0) template = template(1:at - 1)//data//template(at + len('<!--fy-data-->'):)
        at = index(template, '<title>')
        a2 = index(template, '</title>')
        if (at > 0 .and. a2 > at) template = template(1:at + 6)//html_text(title)//template(a2:)
        at = index(template, '<html lang="en"')
        if (at > 0) template = template(1:at + 14)//theme_attr(http_cookie(req, THEME_COOKIE))// &
            template(at + 15:)
        res%code = code
        res%ctype = 'text/html; charset=utf-8'
        res%cache = 'no-store'
        res%body = template
    end subroutine render_page

    recursive subroutine page_fetch(path, status, body)
        !! A GET request of the page being composed, answered as the browser's
        !! would be: with the request's own session.
        character(len=*), intent(in) :: path
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: body
        type(http_request_t) :: sub
        type(response_t) :: res
        integer :: q

        q = index(path, '?')
        sub%method = 'GET'
        if (q > 0) then
            sub%path = path(1:q - 1)
            sub%query = path(q + 1:)
        else
            sub%path = path
            sub%query = ''
        end if
        sub%authorization = ''
        sub%cookie = page_cookie
        sub%content_type = ''
        sub%accept_encoding = ''
        sub%if_none_match = ''
        sub%body = ''
        status = 404
        body = ''
        ! Only the API: a page never composes another page.
        if (index(sub%path, '/api/') /= 1) return
        call route(sub, page_db, res)
        status = res%code
        body = res%body
    end subroutine page_fetch

    function theme_attr(theme) result(s)
        character(len=*), intent(in) :: theme
        character(len=:), allocatable :: s
        s = ''
        if (theme == 'light' .or. theme == 'dark') s = ' data-theme="'//theme//'"'
    end function theme_attr

    pure function html_text(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: k
        r = ''
        do k = 1, len(s)
            select case (s(k:k))
            case ('<'); r = r//'&lt;'
            case ('>'); r = r//'&gt;'
            case ('&'); r = r//'&amp;'
            case default; r = r//s(k:k)
            end select
        end do
    end function html_text

    function first_frame(location, sidebar, theme, title, code) result(html)
        character(len=*), intent(in) :: location, sidebar, theme
        character(len=:), allocatable, intent(out) :: title
        integer, intent(out) :: code
        character(len=:), allocatable :: html
        integer :: stat

        title = 'fortyard'
        code = 200
        call ui_init(headless_backend_t(), 1000, 800, "fortyard", stat=stat)
        html = ''
        if (stat /= 0) return
        call app_start(location, sidebar, theme)
        call ui_frame_begin()
        call app_compose()
        call ui_present()
        html = ui_frame_html()
        title = app_title()
        if (app_missing()) code = 404
        call ui_shutdown()
    end function first_frame

    function json_script(id, json) result(s)
        !! JSON in a page. Every '<' is written as \u003c (the same inside JSON
        !! strings), so nothing in it can end the script element.
        character(len=*), intent(in) :: id, json
        character(len=:), allocatable :: s
        integer :: k, n

        n = 0
        do k = 1, len(json)
            if (json(k:k) == '<') n = n + 1
        end do
        allocate (character(len=len(json) + 5 * n) :: s)
        n = 0
        do k = 1, len(json)
            if (json(k:k) == '<') then
                s(n + 1:n + 6) = achar(92)//'u003c'
                n = n + 6
            else
                s(n + 1:n + 1) = json(k:k)
                n = n + 1
            end if
        end do
        s = '<script type="application/json" id="'//id//'">'//s//'</script>'
    end function json_script

    subroutine static_file(req, name, res)
        !! A file of the web application from the release's web directory. Names
        !! are plain file names, so no path leaves that directory. A gzip copy
        !! (`name.gz`) is sent to clients that accept it; the release commit is
        !! the ETag, so a reload of an unchanged release transfers nothing.
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: name
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: etag
        logical :: ok

        call fail(res, 404, 'not found')
        if (.not. allocated(web_dir)) return
        if (len(web_dir) == 0 .or. len(name) == 0 .or. len(name) > 64) return
        if (name(1:1) == '.' .or. verify(name, &
            'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-') /= 0) return
        etag = ''
        if (FY_COMMIT /= 'unknown') etag = '"'//FY_COMMIT//'"'
        ok = .false.
        if (index(req%accept_encoding, 'gzip') > 0) then
            call read_file(web_dir//'/'//name//'.gz', res%body, ok)
            if (ok) res%headers = 'Content-Encoding: gzip'//CRLF
        end if
        if (.not. ok) call read_file(web_dir//'/'//name, res%body, ok)
        if (.not. ok) then
            res%headers = ''
            res%body = '{"error":"not found"}'
            return
        end if
        res%code = 200
        res%cache = 'no-cache'
        ! A name with a content hash never changes: browsers keep it for good.
        if (hashed(name)) res%cache = 'public, max-age=31536000, immutable'
        res%headers = res%headers//'Vary: Accept-Encoding'//CRLF
        if (len(etag) > 0) then
            res%headers = res%headers//'ETag: '//etag//CRLF
            if (req%if_none_match == etag) then
                res%code = 304
                res%body = ''
            end if
        end if
        res%ctype = 'application/octet-stream'
        if (ends_with(name, '.html')) res%ctype = 'text/html; charset=utf-8'
        if (ends_with(name, '.js')) res%ctype = 'text/javascript; charset=utf-8'
        if (ends_with(name, '.wasm')) res%ctype = 'application/wasm'
        if (ends_with(name, '.css')) res%ctype = 'text/css; charset=utf-8'
        if (ends_with(name, '.svg')) res%ctype = 'image/svg+xml'
    end subroutine static_file

    subroutine read_file(path, content, ok)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        logical, intent(out) :: ok
        integer :: u, ios, size_
        logical :: exists

        ok = .false.
        content = ''
        inquire (file=path, exist=exists, size=size_)
        if (.not. exists .or. size_ < 0) return
        content = repeat(' ', size_)
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            iostat=ios)
        if (ios /= 0) return
        if (size_ > 0) read (u, iostat=ios) content
        close (u)
        ok = ios == 0
    end subroutine read_file

    pure function json_bool(b) result(s)
        logical, intent(in) :: b
        character(len=:), allocatable :: s
        if (b) then
            s = 'true'
        else
            s = 'false'
        end if
    end function json_bool

    pure logical function hashed(name)
        !! NAME.<12 hex digits>.EXT, as the web build names its assets.
        character(len=*), intent(in) :: name
        integer :: dot

        hashed = .false.
        dot = index(name, '.')
        if (dot == 0 .or. dot + 13 > len(name)) return
        hashed = verify(name(dot + 1:dot + 12), '0123456789abcdef') == 0 .and. &
            name(dot + 13:dot + 13) == '.'
    end function hashed

    pure logical function ends_with(s, suffix)
        character(len=*), intent(in) :: s, suffix
        ends_with = len(s) >= len(suffix)
        if (ends_with) ends_with = s(len(s) - len(suffix) + 1:) == suffix
    end function ends_with

    subroutine server_set_web_dir(dir)
        !! Set once before workers start; read-only afterwards.
        character(len=*), intent(in) :: dir
        web_dir = dir
    end subroutine server_set_web_dir

    function version_json() result(s)
        character(len=:), allocatable :: s

        s = '{"version":"'//FY_VERSION//'","commit":"'//FY_COMMIT// &
            '","built":"'//FY_BUILT//'"}'
    end function version_json

    function status_page() result(s)
        character(len=:), allocatable :: s

        s = '<!doctype html><html lang="en"><head><meta charset="utf-8">'// &
            '<meta name="viewport" content="width=device-width,initial-scale=1">'// &
            '<title>fortyard</title><style>'// &
            ':root{color-scheme:light dark;--fg:#1d1f21;--bg:#fbfbf8;--mut:#6b6f76;'// &
            '--acc:#3b6e5a;--line:#dedcd4}'// &
            '@media (prefers-color-scheme:dark){:root{--fg:#e6e4dc;--bg:#17191b;'// &
            '--mut:#9a9da3;--acc:#7fb69c;--line:#33363a}}'// &
            'body{margin:0;background:var(--bg);color:var(--fg);'// &
            'font:16px/1.5 ui-sans-serif,system-ui,sans-serif}'// &
            'main{max-width:46rem;margin:0 auto;padding:2.5rem 1rem}'// &
            'h1{font-size:1.6rem;margin:0}p.sub{color:var(--mut);margin:.2rem 0 2rem}'// &
            'dl{display:grid;grid-template-columns:max-content 1fr;gap:.3rem 1.2rem;'// &
            'margin:0 0 2rem}dt{color:var(--mut)}dd{margin:0;font-family:ui-monospace,monospace}'// &
            'h2{font-size:1rem;border-bottom:1px solid var(--line);padding-bottom:.3rem}'// &
            'a{color:var(--acc)}code{font-family:ui-monospace,monospace}'// &
            '</style></head><body><main>'// &
            '<h1>fortyard</h1><p class="sub">Development instance. '// &
            'Updated automatically from <code>main</code>.</p>'// &
            '<dl><dt>Version</dt><dd>'//FY_VERSION//'</dd>'// &
            '<dt>Commit</dt><dd>'//FY_COMMIT//'</dd>'// &
            '<dt>Built</dt><dd>'//FY_BUILT//'</dd></dl>'// &
            '<h2>API</h2><ul>'// &
            '<li><a href="/healthz"><code>/healthz</code></a></li>'// &
            '<li><a href="/api/v1/version"><code>/api/v1/version</code></a></li>'// &
            '<li><code>/api/v1/whoami</code> (token)</li>'// &
            '<li><code>GET, POST /api/v1/tokens</code>, '// &
            '<code>POST /api/v1/tokens/ID/revoke</code> (token)</li>'// &
            '<li><code>POST, DELETE /api/v1/session</code> (browser session)</li></ul>'// &
            '<p>The web application is at <a href="/">/</a>.</p>'// &
            '<h2>Command line</h2><p><code>fy login --server URL --token TOKEN</code>, '// &
            'then <code>fy whoami</code>.</p>'// &
            '</main></body></html>'
    end function status_page

end module fy_server
