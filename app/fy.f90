program fy
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fx_json_build, only: json_escape_string
    use fx_mcp, only: mcp_server_t, mcp_server_init, mcp_server_add_action, &
        mcp_server_run
    use fy_build_info, only: FY_VERSION, FY_COMMIT
    use fy_client, only: client_request, config_t, config_load, config_save
    use fy_posix, only: tcp_listen, activated_listen_fd, sock_bound_port
    use fy_server, only: server_supervise, server_set_web_dir, server_set_data_dir, &
        server_set_base, server_set_follow, &
        server_set_mirror_interval
    use fy_sqlite, only: db_t, db_errmsg, db_close
    use fy_store, only: store_open, store_bootstrap
    implicit none

    character(len=:), allocatable :: command

    command = arg(1)
    select case (command)
    case ('serve')
        call cmd_serve()
    case ('login')
        call cmd_login()
    case ('whoami')
        call cmd_get('/api/v1/whoami')
    case ('token')
        call cmd_token()
    case ('repo')
        call cmd_repo()
    case ('land')
        call cmd_land()
    case ('landings')
        if (len(arg(2)) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy landings REPO'
            error stop 2
        end if
        call cmd_get('/api/v1/repos/'//arg(2)//'/landings')
    case ('mcp')
        call cmd_mcp()
    case ('version', '--version')
        write (output_unit, '(a)') 'fy '//FY_VERSION//' ('//FY_COMMIT//')'
    case ('', 'help', '--help', '-h')
        call usage()
    case default
        write (error_unit, '(a)') 'fy: unknown command '//command
        call usage()
        error stop 2
    end select

contains

    subroutine usage()
        write (output_unit, '(a)') &
            'usage: fy <command> [options]', &
            '', &
            '  serve [--data DIR] [--listen HOST:PORT] [--workers P] [--threads T]', &
            '        [--web DIR] [--mirror-interval SECONDS] [--init]', &
            '                      run the server; --init creates the data directory', &
            '                      and prints the administrator token once', &
            '  login --server URL --token TOKEN', &
            '                      check and store a login', &
            '  whoami              show the account of the stored token', &
            '  token create NAME   create a token with the scopes of the current one', &
            '  repo add NAME URL   mirror a repository (https or ssh URL; admin only)', &
            '  repo create NAME    create an empty repository that takes pushes (admin)', &
            '  repo mode NAME MODE primary: take pushes; mirror: follow upstream (admin)', &
            '  repo list           list repositories', &
            '                      clone and push: git clone http://x:TOKEN@HOST/NAME.git', &
            '  land REPO BRANCH    land a pushed branch on main as one squashed commit', &
            '                      after its check passed (admin)', &
            '  landings REPO       recent landings with their state and log', &
            '  mcp                 MCP server on standard input and output', &
            '  version             print the version'
    end subroutine usage

    function arg(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        integer :: n

        if (i > command_argument_count()) then
            s = ''
            return
        end if
        call get_command_argument(i, length=n)
        allocate (character(len=n) :: s)
        if (n > 0) call get_command_argument(i, s)
    end function arg

    function option(name, default) result(s)
        character(len=*), intent(in) :: name, default
        character(len=:), allocatable :: s
        integer :: i

        s = default
        do i = 2, command_argument_count() - 1
            if (arg(i) == name) then
                s = arg(i + 1)
                return
            end if
        end do
    end function option

    logical function flag(name)
        character(len=*), intent(in) :: name
        integer :: i

        flag = .false.
        do i = 2, command_argument_count()
            if (arg(i) == name) flag = .true.
        end do
    end function flag

    subroutine cmd_serve()
        character(len=:), allocatable :: data, listen, host, token, text, web
        type(db_t) :: db
        integer :: lfd, port, stat, colon, workers, threads, interval, n
        logical :: exists

        data = option('--data', './fortyard-data')
        listen = option('--listen', '127.0.0.1:8740')
        text = option('--workers', '2')
        read (text, *, iostat=stat) workers
        if (stat /= 0 .or. workers < 1 .or. workers > 64) error stop 'fy: --workers needs 1 to 64'
        web = option('--web', beside_program('web'))
        text = option('--mirror-interval', '60')
        read (text, *, iostat=stat) interval
        if (stat /= 0 .or. interval < 1) error stop 'fy: --mirror-interval needs seconds, at least 1'
        text = option('--threads', '8')
        read (text, *, iostat=stat) threads
        if (stat /= 0 .or. threads < 1 .or. threads > 256) error stop 'fy: --threads needs 1 to 256'
        inquire (file=data//'/fortyard.db', exist=exists)
        if (flag('--init')) then
            if (exists) then
                write (error_unit, '(a)') 'fy: '//data//' is already initialized'
                error stop 1
            end if
            call execute_command_line("mkdir -p '"//data//"' && chmod 700 '"//data//"'", &
                exitstat=stat)
            if (stat /= 0) error stop 'fy: cannot create the data directory'
        else if (.not. exists) then
            write (error_unit, '(a)') 'fy: '//data//' is not initialized; run fy serve --init'
            error stop 1
        end if
        call store_open(data//'/fortyard.db', db, stat)
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: database: '//db_errmsg(db)
            error stop 1
        end if
        if (flag('--init')) then
            call store_bootstrap(db, token, stat)
            if (stat /= 0) error stop 'fy: bootstrap failed'
            write (output_unit, '(a)') 'Administrator token (shown once, store it now):', token
            return
        end if

        lfd = activated_listen_fd()
        if (lfd < 0) then
            colon = index(listen, ':', back=.true.)
            if (colon < 2) error stop 'fy: --listen needs HOST:PORT'
            host = listen(1:colon - 1)
            read (listen(colon + 1:), *, iostat=stat) port
            if (stat /= 0) error stop 'fy: bad port in --listen'
            call tcp_listen(host, port, lfd, stat)
            if (stat /= 0) then
                write (error_unit, '(a)') 'fy: cannot listen on '//listen
                error stop 1
            end if
        end if
        write (error_unit, '(a,i0)') 'fy serve '//FY_VERSION//' ('//FY_COMMIT// &
            ') listening on port ', sock_bound_port(lfd)
        flush (error_unit)
        call db_close(db)
        call server_set_web_dir(web)
        ! Below a path prefix behind a reverse proxy that strips it (e.g. /yard).
        call get_environment_variable('FY_BASE', length=n, status=stat)
        text = repeat(' ', max(n, 0))
        if (stat == 0 .and. n > 0) call get_environment_variable('FY_BASE', text)
        call server_set_base(option('--base', trim(text)))
        ! --follow LINK: re-execute the release LINK points to when it changes.
        text = option('--follow', '')
        if (len(text) > 0) call server_set_follow(text)
        call server_set_data_dir(data)
        call server_set_mirror_interval(interval)
        call server_supervise(lfd, data//'/fortyard.db', workers, threads)
    end subroutine cmd_serve

    function beside_program(name) result(path)
        !! `name` in the directory of this executable, as it was started.
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        character(len=4096) :: exe
        integer :: slash

        call get_command_argument(0, exe)
        slash = index(exe, '/', back=.true.)
        if (slash == 0) then
            path = name
        else
            path = exe(1:slash)//name
        end if
    end function beside_program

    subroutine cmd_login()
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        cfg%server = option('--server', '')
        cfg%token = option('--token', '')
        if (len(cfg%server) == 0 .or. len(cfg%token) == 0) then
            write (error_unit, '(a)') 'fy: login needs --server URL and --token TOKEN'
            error stop 2
        end if
        call client_request(cfg%server, 'GET', '/api/v1/whoami', cfg%token, '', code, &
            body, stat)
        if (stat /= 0 .or. code /= 200) then
            write (error_unit, '(a)') 'fy: login rejected by '//cfg%server
            error stop 1
        end if
        call config_save(cfg, stat)
        if (stat /= 0) error stop 'fy: cannot store credentials'
        write (output_unit, '(a)') body
    end subroutine cmd_login

    subroutine cmd_get(path)
        character(len=*), intent(in) :: path
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'GET', path, cfg%token, '', code, body, stat)
        call report(code, body, stat)
    end subroutine cmd_get

    subroutine cmd_token()
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        if (arg(2) /= 'create' .or. len(arg(3)) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy token create NAME'
            error stop 2
        end if
        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'POST', '/api/v1/tokens', cfg%token, &
            '{"name":"'//json_escape_string(arg(3))//'"}', code, body, stat)
        call report(code, body, stat)
    end subroutine cmd_token

    subroutine cmd_land()
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        if (len(arg(2)) == 0 .or. len(arg(3)) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy land REPO BRANCH'
            error stop 2
        end if
        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'POST', '/api/v1/repos/'//arg(2)//'/land', cfg%token, &
            '{"branch":"'//json_escape_string(arg(3))//'"}', code, body, stat)
        call report(code, body, stat)
    end subroutine cmd_land

    subroutine cmd_repo()
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        call config_load(cfg)
        select case (arg(2))
        case ('add')
            if (len(arg(3)) == 0 .or. len(arg(4)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo add NAME URL'
                error stop 2
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos', cfg%token, &
                '{"name":"'//json_escape_string(arg(3))//'","upstream":"'// &
                json_escape_string(arg(4))//'"}', code, body, stat)
            call report(code, body, stat)
        case ('create')
            if (len(arg(3)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo create NAME'
                error stop 2
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos', cfg%token, &
                '{"name":"'//json_escape_string(arg(3))//'"}', code, body, stat)
            call report(code, body, stat)
        case ('mode')
            if (len(arg(3)) == 0 .or. len(arg(4)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo mode NAME primary|mirror'
                error stop 2
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos/'//arg(3)//'/mode', &
                cfg%token, '{"mode":"'//json_escape_string(arg(4))//'"}', code, body, stat)
            call report(code, body, stat)
        case ('list')
            call cmd_get('/api/v1/repos')
        case default
            write (error_unit, '(a)') 'fy: usage: fy repo add NAME URL | create NAME |'// &
                ' mode NAME MODE | list'
            error stop 2
        end select
    end subroutine cmd_repo

    subroutine require_login(cfg)
        type(config_t), intent(in) :: cfg

        if (len(cfg%server) == 0 .or. len(cfg%token) == 0) then
            write (error_unit, '(a)') 'fy: not logged in; run fy login'
            error stop 1
        end if
    end subroutine require_login

    subroutine report(code, body, stat)
        integer, intent(in) :: code, stat
        character(len=*), intent(in) :: body

        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: server not reachable'
            error stop 1
        end if
        write (output_unit, '(a)') body
        if (code < 200 .or. code >= 300) error stop 1
    end subroutine report

    subroutine cmd_mcp()
        type(mcp_server_t) :: server

        call mcp_server_init(server, 'fortyard', 'fortyard forge: account and server status')
        call mcp_server_add_action(server, 'whoami', 'Account of the stored fortyard token')
        call mcp_server_add_action(server, 'version', 'Version of the fortyard server')
        call mcp_server_run(server, mcp_handler)
    end subroutine cmd_mcp

    subroutine mcp_handler(action, params, response, is_error)
        character(len=*), intent(in) :: action, params
        character(len=:), allocatable, intent(out) :: response
        logical, intent(out) :: is_error
        type(config_t) :: cfg
        character(len=:), allocatable :: path
        integer :: code, stat

        is_error = .true.
        select case (trim(action))
        case ('whoami'); path = '/api/v1/whoami'
        case ('version'); path = '/api/v1/version'
        case default
            response = '{"error":"unknown action"}'
            return
        end select
        call config_load(cfg)
        if (len(cfg%server) == 0) then
            response = '{"error":"not logged in; run fy login"}'
            return
        end if
        call client_request(cfg%server, 'GET', path, cfg%token, '', code, response, stat)
        if (stat /= 0) response = '{"error":"server not reachable"}'
        is_error = stat /= 0 .or. code /= 200
    end subroutine mcp_handler

end program fy
