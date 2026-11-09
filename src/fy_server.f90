module fy_server
    !! Request routing for `fy serve`.
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string
    use fy_build_info, only: FY_VERSION, FY_COMMIT, FY_BUILT
    use fy_http, only: http_request_t, http_read_request, http_send_response, &
        http_bearer_token
    use fy_posix, only: tcp_accept, sock_close, sock_set_timeouts
    use fy_sqlite, only: db_t
    use fy_proc, only: proc_fork, proc_wait_any, proc_exit, proc_pid, &
        proc_die_with_parent, proc_sleep_ms
    use fy_store, only: token_identity_t, store_token_lookup, store_token_create, store_open
    implicit none
    private

    public :: server_loop, serve_connection, route, server_supervise

    character(len=*), parameter :: JSON = 'application/json'

contains

    subroutine server_supervise(lfd, db_path, nworkers, nthreads)
        !! Pre-forked workers share the listening socket, each with its own
        !! database connection. A worker that dies, for example on a Fortran
        !! runtime error, takes only its own requests down and is replaced. Inside a
        !! worker, OpenMP threads run their own accept loops, so a long-lived
        !! connection holds one thread rather than one process.
        integer, intent(in) :: lfd, nworkers, nthreads
        character(len=*), intent(in) :: db_path
        integer :: pids(nworkers)
        integer(int64) :: started(nworkers), now, rate
        integer :: i, pid, status
        integer :: me

        me = proc_pid()
        do i = 1, nworkers
            call spawn(i)
        end do
        do
            call proc_wait_any(pid, status)
            if (pid < 0) then
                call proc_sleep_ms(100)
                cycle
            end if
            do i = 1, nworkers
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
            call worker_main(lfd, db_path, nthreads)
            call proc_exit(0)
        end subroutine spawn

    end subroutine server_supervise

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
        integer :: status, code
        character(len=:), allocatable :: ctype, body

        call http_read_request(cfd, req, status)
        if (status < 0) return
        if (status /= 0) then
            call http_send_response(cfd, status, JSON, '{"error":"bad request"}')
            return
        end if
        call route(req, db, code, ctype, body)
        call http_send_response(cfd, code, ctype, body)
        write (error_unit, '(a,1x,a,1x,i0)') req%method, req%path, code
        flush (error_unit)
    end subroutine serve_connection

    subroutine route(req, db, code, ctype, body)
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: ctype, body
        type(token_identity_t) :: who

        code = 200
        ctype = JSON
        select case (req%path)
        case ('/healthz')
            ctype = 'text/plain; charset=utf-8'
            body = 'ok '//FY_VERSION//' '//FY_COMMIT//new_line('a')
        case ('/', '/index.html')
            ctype = 'text/html; charset=utf-8'
            body = status_page()
        case ('/api/v1/version')
            body = version_json()
        case ('/api/v1/whoami')
            call authenticate(req, db, who, code, body)
            if (code /= 200) return
            body = '{"account":"'//json_escape_string(who%account)// &
                '","token":"'//json_escape_string(who%token_name)// &
                '","scopes":"'//json_escape_string(who%scopes)//'"}'
        case ('/api/v1/tokens')
            if (req%method /= 'POST') then
                code = 405
                body = '{"error":"use POST"}'
                return
            end if
            call authenticate(req, db, who, code, body)
            if (code /= 200) return
            call create_token(req, db, who, code, body)
        case default
            code = 404
            body = '{"error":"not found"}'
        end select
        if (req%method /= 'GET' .and. req%path /= '/api/v1/tokens') then
            code = 405
            body = '{"error":"method not allowed"}'
        end if
    end subroutine route

    subroutine authenticate(req, db, who, code, body)
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(out) :: who
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: body

        code = 200
        body = ''
        call store_token_lookup(db, http_bearer_token(req), who)
        if (.not. who%found) then
            code = 401
            body = '{"error":"missing or invalid token"}'
        end if
    end subroutine authenticate

    subroutine create_token(req, db, who, code, body)
        !! A token can mint tokens for its own account with at most its scopes.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable :: name, token
        logical :: found
        integer :: stat

        call json_extract_string(req%body, 'name', name, found)
        if (.not. found .or. len(name) == 0 .or. len(name) > 100) then
            code = 400
            body = '{"error":"body must be {\"name\":\"...\"}"}'
            return
        end if
        call store_token_create(db, who%account_id, name, who%scopes, token, stat)
        if (stat /= 0) then
            code = 500
            body = '{"error":"token not created"}'
            return
        end if
        code = 201
        body = '{"token":"'//token//'","name":"'//json_escape_string(name)// &
            '","scopes":"'//json_escape_string(who%scopes)//'"}'
    end subroutine create_token

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
            '<li><code>POST /api/v1/tokens</code> (token)</li></ul>'// &
            '<h2>Command line</h2><p><code>fy login --server URL --token TOKEN</code>, '// &
            'then <code>fy whoami</code>.</p>'// &
            '</main></body></html>'
    end function status_page

end module fy_server
