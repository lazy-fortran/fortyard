program fy
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, int64
    use fx_json_build, only: json_escape_string
    use fx_mcp, only: mcp_server_t, mcp_server_init, mcp_server_add_action, &
        mcp_server_run
    use fy_build_info, only: FY_VERSION, FY_COMMIT
    use fy_runner, only: runner_main
    use fy_client, only: client_request, config_t, config_load, config_save, client_set_proxy
    use fy_cli, only: cli_table, cli_field, cli_tail
    use fy_proc, only: proc_setenv, proc_sleep_ms
    use fy_posix, only: tcp_listen, activated_listen_fd, sock_bound_port
    use fy_server, only: server_supervise, server_set_web_dir, server_set_data_dir, &
        server_set_base, server_set_follow, server_set_checks, &
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
    case ('runner')
        call cmd_runner()
    case ('land')
        call cmd_land()
    case ('landings')
        call cmd_landings()
    case ('jobs')
        call cmd_jobs()
    case ('job')
        call cmd_job()
    case ('git-credential')
        call cmd_git_credential()
    case ('mcp')
        call cmd_mcp()
    case ('version', '--version')
        write (output_unit, '(a)') 'fy '//FY_VERSION//' ('//FY_COMMIT//')'
    case ('', 'help', '--help', '-h')
        call usage()
    case default
        write (error_unit, '(a)') 'fy: unknown command '//command
        call usage()
        call quit(2)
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
            '  login --server URL [--proxy URL] [--token-file F]', &
            '                      check and store a login; the token comes from F or', &
            '                      standard input', &
            '  whoami              show the account of the stored token', &
            '  token create NAME   create a token with the scopes of the current one', &
            '  repo add NAME URL   mirror a repository (https or ssh URL; admin only)', &
            '  repo create NAME    create an empty repository that takes pushes (admin)', &
            '  repo mode NAME MODE primary: take pushes; mirror: follow upstream (admin)', &
            '  repo list           list repositories', &
            '                      clone and push: git clone http://x:TOKEN@HOST/NAME.git', &
            '  runner --server URL --token-file F --data DIR [--follow LINK]', &
            '                      perform landing checks for the server (scope runner)', &
            '  land [BRANCH] [--repo NAME] [--no-push] [--no-wait]', &
            '                      push HEAD to BRANCH, land it on main after its check', &
            '                      passed and follow it to the end (admin)', &
            '  land REPO BRANCH    land a branch that is already pushed', &
            '  landings [REPO]     recent landings of a repository', &
            '  jobs [REPO]         running, waiting and recent jobs', &
            '  job ID [--wait]     a job''s state and log; --wait follows it', &
            '                      (--json: the answer of the API instead of text)', &
            '  git-credential get  git credential helper for the stored login:', &
            '                      git config --global credential.SERVER.helper', &
            '                      ''!fy git-credential''', &
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
                call quit(1)
            end if
            call execute_command_line("mkdir -p '"//data//"' && chmod 700 '"//data//"'", &
                exitstat=stat)
            if (stat /= 0) error stop 'fy: cannot create the data directory'
        else if (.not. exists) then
            write (error_unit, '(a)') 'fy: '//data//' is not initialized; run fy serve --init'
            call quit(1)
        end if
        call store_open(data//'/fortyard.db', db, stat)
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: database: '//db_errmsg(db)
            call quit(1)
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
                call quit(1)
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
        ! FY_CHECKS=runner: landings leave their checks to a runner (fy runner).
        call get_environment_variable('FY_CHECKS', length=n, status=stat)
        text = repeat(' ', max(n, 0))
        if (stat == 0 .and. n > 0) call get_environment_variable('FY_CHECKS', text)
        if (len(text) > 0) call server_set_checks(trim(text))
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
        !! The token comes from --token-file or standard input, never from the
        !! command line, where other users of the machine could read it.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, file
        character(len=4096) :: line
        integer :: code, stat, u, ios

        cfg%server = option('--server', '')
        cfg%proxy = option('--proxy', '')
        file = option('--token-file', '')
        line = ''
        if (len(file) > 0) then
            open (newunit=u, file=file, action='read', status='old', iostat=ios)
            if (ios == 0) then
                read (u, '(a)', iostat=ios) line
                close (u)
            end if
        else
            read (*, '(a)', iostat=ios) line
        end if
        cfg%token = trim(adjustl(line))
        if (len(cfg%server) == 0 .or. len(cfg%token) == 0) then
            write (error_unit, '(a)') 'fy: login needs --server URL and a token on standard'// &
                ' input or in --token-file FILE'
            call quit(2)
        end if
        call client_set_proxy(cfg%proxy)
        call client_request(cfg%server, 'GET', '/api/v1/whoami', cfg%token, '', code, &
            body, stat)
        if (stat /= 0 .or. code /= 200) then
            write (error_unit, '(a)') 'fy: login rejected by '//cfg%server
            call quit(1)
        end if
        call config_save(cfg, stat)
        if (stat /= 0) error stop 'fy: cannot store credentials'
        write (output_unit, '(a)') 'logged in to '//cfg%server//' as '//cli_field(body, 'account')
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
            call quit(2)
        end if
        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'POST', '/api/v1/tokens', cfg%token, &
            '{"name":"'//json_escape_string(arg(3))//'"}', code, body, stat)
        call report(code, body, stat)
    end subroutine cmd_token

    subroutine cmd_runner()
        character(len=:), allocatable :: server, file, token
        integer :: u, ios
        character(len=256) :: line

        server = option('--server', 'http://127.0.0.1:8740')
        file = option('--token-file', '')
        token = ''
        if (len(file) > 0) then
            open (newunit=u, file=file, action='read', iostat=ios)
            if (ios == 0) then
                read (u, '(a)', iostat=ios) line
                close (u)
                token = trim(line)
            end if
        end if
        if (len(token) == 0) error stop 'fy runner: --token-file must hold a runner token'
        call runner_main(server, token, option('--data', './fortyard-data'), &
            option('--follow', ''))
    end subroutine cmd_runner

    subroutine cmd_land()
        !! fy land [BRANCH] [--repo NAME] [--no-push] [--no-wait]: pushes HEAD of
        !! the checkout to BRANCH (default: the current branch), queues its
        !! landing and follows it to the end; the exit status tells the outcome.
        !! The repository is the one of a remote on the server, unless --repo
        !! names it. The form fy land REPO BRANCH lands a branch already pushed.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, repo, branch, id
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        if (len(positional(2)) > 0) then
            repo = positional(1)
            branch = positional(2)
        else
            repo = option('--repo', '')
            if (len(repo) == 0) repo = remote_repo(cfg%server)
            branch = positional(1)
            if (len(branch) == 0) branch = git_line('git symbolic-ref --quiet --short HEAD')
            if (len(branch) == 0 .or. branch == 'main') then
                write (error_unit, '(a)') 'fy: name the branch to land: fy land BRANCH'
                call quit(2)
            end if
            if (.not. flag('--no-push')) call push_head(cfg, repo, branch)
        end if
        call client_request(cfg%server, 'POST', '/api/v1/repos/'//repo//'/land', cfg%token, &
            '{"branch":"'//json_escape_string(branch)//'"}', code, body, stat)
        if (stat /= 0 .or. code /= 202) call report(code, body, stat)
        id = cli_field(body, 'landing')
        write (output_unit, '(a)') 'landing '//id//' of '//repo//' '//branch//' queued'
        flush (output_unit)
        if (flag('--no-wait')) return
        call follow_job(cfg, id)
    end subroutine cmd_land

    subroutine follow_job(cfg, id)
        !! Waits until job `id` has finished and reports how; a failed job ends
        !! fy with status 1.
        type(config_t), intent(in) :: cfg
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: body, state, last
        integer :: code, stat

        last = ''
        do
            call client_request(cfg%server, 'GET', '/api/v1/jobs/'//id, cfg%token, '', code, &
                body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            state = cli_field(body, 'state')
            if (state == 'landed' .or. state == 'failed') exit
            if (state /= last) then
                write (output_unit, '(a)') 'landing '//id//' '//state
                flush (output_unit)
                last = state
            end if
            call proc_sleep_ms(2000)
        end do
        if (state == 'landed') then
            write (output_unit, '(a)') 'landing '//id//' landed as '//cli_field(body, 'commit')
            return
        end if
        write (output_unit, '(a)') 'landing '//id//' failed:', &
            cli_tail(cli_field(body, 'log'), 40)
        call quit(1)
    end subroutine follow_job

    subroutine push_head(cfg, repo, branch)
        !! git push of HEAD to BRANCH of REPO on the server. The token and proxy
        !! reach git through its environment, so no command line shows them.
        type(config_t), intent(in) :: cfg
        character(len=*), intent(in) :: repo, branch
        integer :: stat

        call proc_setenv('GIT_TERMINAL_PROMPT', '0')
        call proc_setenv('GIT_CONFIG_COUNT', '2')
        call proc_setenv('GIT_CONFIG_KEY_0', 'http.extraHeader')
        call proc_setenv('GIT_CONFIG_VALUE_0', 'Authorization: Bearer '//cfg%token)
        call proc_setenv('GIT_CONFIG_KEY_1', 'http.proxy')
        call proc_setenv('GIT_CONFIG_VALUE_1', cfg%proxy)
        call execute_command_line('git push --quiet --force "'//cfg%server//'/'//repo// &
            '.git" HEAD:refs/heads/'//branch, exitstat=stat)
        call proc_setenv('GIT_CONFIG_COUNT', '0')
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: pushing HEAD to '//repo//' '//branch//' failed'
            call quit(1)
        end if
    end subroutine push_head

    function remote_repo(server) result(repo)
        !! The repository of the first remote of the checkout on `server`.
        character(len=*), intent(in) :: server
        character(len=:), allocatable :: repo, urls, url
        integer :: at, eol

        urls = git_line('git remote -v')
        repo = ''
        at = 1
        do while (at <= len(urls))
            eol = index(urls(at:), new_line('a'))
            if (eol == 0) eol = len(urls) - at + 2
            url = urls(at:at + eol - 2)
            at = at + eol
            url = url(index(url, achar(9)) + 1:)
            if (index(url, ' ') > 0) url = url(1:index(url, ' ') - 1)
            if (index(url, server//'/') /= 1) cycle
            repo = url(len(server) + 2:)
            if (len(repo) > 4) then
                if (repo(len(repo) - 3:) == '.git') repo = repo(1:len(repo) - 4)
            end if
            return
        end do
        write (error_unit, '(a)') 'fy: no remote of this checkout is on '//server// &
            '; name the repository with --repo'
        call quit(2)
    end function remote_repo

    function git_line(command) result(out)
        !! The output of a git command, without the final newline; '' when it
        !! fails.
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: out, path
        character(len=4096) :: line
        integer :: u, ios, stat

        path = scratch_file()
        call execute_command_line(command//' > "'//path//'" 2>/dev/null', exitstat=stat)
        out = ''
        open (newunit=u, file=path, action='read', status='old', iostat=ios)
        if (ios == 0) then
            do
                read (u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len(out) > 0) out = out//new_line('a')
                out = out//trim(line)
            end do
            close (u, status='delete')
        end if
        if (stat /= 0) out = ''
    end function git_line

    function scratch_file() result(path)
        character(len=:), allocatable :: path
        character(len=4096) :: dir
        character(len=24) :: num
        integer :: n, stat
        integer(int64) :: count

        call get_environment_variable('TMPDIR', dir, n, stat)
        if (stat /= 0 .or. n == 0) dir = '/tmp'
        call system_clock(count)
        write (num, '(i0)') count
        path = trim(dir)//'/fy-'//trim(num)
    end function scratch_file

    subroutine cmd_landings()
        !! fy landings [REPO]: recent landings of a repository.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, repo
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        repo = positional(1)
        if (len(repo) == 0) repo = remote_repo(cfg%server)
        call client_request(cfg%server, 'GET', '/api/v1/repos/'//repo//'/landings', cfg%token, &
            '', code, body, stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
            return
        end if
        write (output_unit, '(a)', advance='no') cli_table(body, 'landings', &
            [character(len=8) :: 'landing', 'state', 'account', 'branch'])
    end subroutine cmd_landings

    subroutine cmd_jobs()
        !! fy jobs [REPO]: running, waiting and recent jobs.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, path
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        path = '/api/v1/jobs'
        if (len(positional(1)) > 0) path = path//'?repo='//positional(1)
        call client_request(cfg%server, 'GET', path, cfg%token, '', code, body, stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
            return
        end if
        write (output_unit, '(a)', advance='no') cli_table(body, 'jobs', &
            [character(len=8) :: 'id', 'repo', 'state', 'started', 'branch'])
    end subroutine cmd_jobs

    subroutine cmd_job()
        !! fy job ID [--wait]: a job's state and log.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, id
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        id = positional(1)
        if (len(id) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy job ID [--wait]'
            call quit(2)
        end if
        if (flag('--wait')) then
            call follow_job(cfg, id)
            return
        end if
        call client_request(cfg%server, 'GET', '/api/v1/jobs/'//id, cfg%token, '', code, &
            body, stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
            return
        end if
        write (output_unit, '(a)') 'job '//id//': '//cli_field(body, 'kind')//' of '// &
            cli_field(body, 'repo')//' '//cli_field(body, 'branch')//', '// &
            cli_field(body, 'state'), cli_field(body, 'log')
    end subroutine cmd_job

    subroutine quit(status)
        !! Ends fy with `status` after its output, without a runtime message.
        integer, intent(in) :: status
        flush (output_unit)
        flush (error_unit)
        stop status, quiet=.true.
    end subroutine quit

    subroutine cmd_git_credential()
        !! git's credential helper protocol (git help credential): answers "get"
        !! for the stored server with the stored token, so plain git fetches
        !! from and pushes to the server. Set up once with
        !!   git config --global credential.SERVER.helper '!fy git-credential'
        type(config_t) :: cfg
        character(len=4096) :: line
        character(len=:), allocatable :: protocol, host, path, url
        integer :: ios

        if (positional(1) /= 'get') return
        protocol = ''
        host = ''
        path = ''
        do
            read (*, '(a)', iostat=ios) line
            if (ios /= 0 .or. len_trim(line) == 0) exit
            if (index(line, 'protocol=') == 1) protocol = trim(line(10:))
            if (index(line, 'host=') == 1) host = trim(line(6:))
            if (index(line, 'path=') == 1) path = trim(line(6:))
        end do
        call config_load(cfg)
        if (len(cfg%server) == 0 .or. len(cfg%token) == 0) return
        url = protocol//'://'//host//'/'//path
        if (index(url, cfg%server) /= 1 .and. index(cfg%server, protocol//'://'//host) /= 1) return
        write (output_unit, '(a)') 'username=x', 'password='//cfg%token
    end subroutine cmd_git_credential

    function positional(k) result(s)
        !! The k-th argument after the command that is neither an option nor
        !! the value of one.
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        integer :: i, seen

        s = ''
        seen = 0
        i = 2
        do while (i <= command_argument_count())
            if (index(arg(i), '--') == 1) then
                select case (arg(i))
                case ('--no-push', '--no-wait', '--wait', '--json', '--init')
                    i = i + 1
                case default
                    i = i + 2
                end select
                cycle
            end if
            seen = seen + 1
            if (seen == k) then
                s = arg(i)
                return
            end if
            i = i + 1
        end do
    end function positional

    subroutine cmd_repo()
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        call config_load(cfg)
        select case (arg(2))
        case ('add')
            if (len(arg(3)) == 0 .or. len(arg(4)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo add NAME URL'
                call quit(2)
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos', cfg%token, &
                '{"name":"'//json_escape_string(arg(3))//'","upstream":"'// &
                json_escape_string(arg(4))//'"}', code, body, stat)
            call report(code, body, stat)
        case ('create')
            if (len(arg(3)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo create NAME'
                call quit(2)
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos', cfg%token, &
                '{"name":"'//json_escape_string(arg(3))//'"}', code, body, stat)
            call report(code, body, stat)
        case ('mode')
            if (len(arg(3)) == 0 .or. len(arg(4)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo mode NAME primary|mirror'
                call quit(2)
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
            call quit(2)
        end select
    end subroutine cmd_repo

    subroutine require_login(cfg)
        type(config_t), intent(in) :: cfg

        if (len(cfg%server) == 0 .or. len(cfg%token) == 0) then
            write (error_unit, '(a)') 'fy: not logged in; run fy login'
            call quit(1)
        end if
    end subroutine require_login

    subroutine report(code, body, stat)
        integer, intent(in) :: code, stat
        character(len=*), intent(in) :: body

        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: server not reachable'
            call quit(1)
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
