program fy
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64, &
        output_unit
    use fx_json_build, only: &
        json_escape_string
    use fy_audit, only: &
        audit_append
    use fy_build_info, only: &
        FY_COMMIT, &
        FY_VERSION
    use fy_cli, only: &
        cli_field, &
        cli_stats, &
        cli_table, &
        cli_tail
    use fy_client, only: &
        client_request, &
        client_set_proxy, &
        config_load, &
        config_save, &
        config_t
    use fy_land, only: &
        land_git_check, &
        land_push_check
    use fy_mcp, only: &
        mcp_main
    use fy_policy, only: &
        POLICY_KEYS, &
        policy_set, &
        policy_t
    use fy_posix, only: &
        activated_listen_fd, &
        sock_bound_port, &
        tcp_listen
    use fy_proc, only: &
        proc_setenv, &
        proc_sleep_ms
    use fy_runner, only: &
        runner_main
    use fy_server, only: &
        server_set_base, &
        server_set_checks, &
        server_set_data_dir, &
        server_set_follow, &
        server_set_mirror_interval, &
        server_set_web_dir, &
        server_supervise
    use fy_sqlite, only: &
        db_close, &
        db_errmsg, &
        db_t
    use fy_store, only: &
        store_bootstrap, &
        store_open
    implicit none

    character(len=:), allocatable :: command

    command = arg(1)
    ! Asking for help never runs a command: `fy land --help` once landed.
    if (wants_help()) command = 'help'
    select case (command)
    case ('audit')
        call cmd_audit()
    case ('board')
        call cmd_stats(.true.)
    case ('change')
        call cmd_change()
    case ('git-credential')
        call cmd_git_credential()
    case ('issue')
        call cmd_issue()
    case ('job')
        call cmd_job()
    case ('jobs')
        call cmd_jobs()
    case ('land')
        call cmd_land()
    case ('landings')
        call cmd_landings()
    case ('login')
        call cmd_login()
    case ('mcp')
        call mcp_main()
    case ('push')
        call cmd_push()
    case ('repo')
        call cmd_repo()
    case ('runner')
        call cmd_runner()
    case ('serve')
        call cmd_serve()
    case ('stats')
        call cmd_stats(.false.)
    case ('token')
        call cmd_token()
    case ('verify')
        call cmd_verify()
    case ('whoami')
        call cmd_get('/api/v1/whoami')
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
        ! the server
            '  serve [--data DIR] [--listen HOST:PORT] [--workers P] [--threads T]', &
            '        [--web DIR] [--mirror-interval SECONDS] [--init]', &
            '                      run the server; --init creates the data directory', &
            '                      and prints the administrator token once', &
        ! login and tokens
            '  login --server URL [--proxy URL] [--token-file F]', &
            '                      check and store a login; the token comes from F or', &
            '                      standard input', &
            '  whoami              show the account of the stored token', &
            '  token create NAME [--scopes S,...]', &
            '                      create a token with the scopes of the current one, or', &
            '                      with those of them named: write pushes and comments,', &
            '                      land queues landings, admin implies both; a token with', &
            '                      write or land and without admin creates none', &
        ! repositories
            '  repo add NAME URL   mirror a repository (https or ssh URL; admin only)', &
            '  repo create NAME    create an empty repository that takes pushes (admin)', &
            '  repo mode NAME MODE primary: take pushes; mirror: follow upstream (admin)', &
            '  repo numbers NAME N number the next change or issue N and count on (admin)', &
            '  repo policy NAME [KEY=VALUE ...]', &
            '                      show the policy of a repository, or set (admin):', &
            '                      tests=full|affected|none  the check before landing', &
            '                        (affected behaves as full until results exist', &
            '                        per test case; none lands unchecked)', &
            '                      review=after|before  before: a change lands only', &
            '                        in a version that is reviewed (fy change', &
            '                        reviewed N, or the review agent without a flag)', &
            '                      bisect=on|off  bisect a failing verification of main', &
            '                      batch=N  most landings checked as one candidate (1..64)', &
            '                      unreviewed_limit=N  refuse landings while N landed', &
            '                        changes wait for a review (0: no limit)', &
            '  repo list           list repositories', &
            '                      clone and push: git clone http://x:TOKEN@HOST/NAME.git', &
        ! runners
            '  runner --server URL --token-file F --data DIR [--follow LINK]', &
            '                      perform landing checks for the server (scope runner)', &
        ! landing and pushing
            '  land [BRANCH] [--repo NAME] [--no-push] [--no-wait]', &
            '                      push HEAD to BRANCH, land it on main after its check', &
            '                      passed and follow it to the end (admin)', &
            '  land REPO BRANCH    land a branch that is already pushed', &
            '  push [BRANCH]       push HEAD to BRANCH, which opens or updates its change', &
            '                      (push and land send no file over 1 MiB, or over the bytes', &
            '                      in .fortyard/size-limit, that .fortyard/allow-large does', &
            '                      not allow)', &
        ! changes
            '  change list [--all] open changes (--all: also landed and closed ones)', &
            '  change view N       a change with its versions, comments and landings', &
            '  change diff N [--version V] [--from W]', &
            '                      the diff of a version, or between versions W and V', &
            '  change comment N [-m TEXT]   comment (TEXT or standard input)', &
            '  change edit N [--title T] [--description-file F|-]', &
            '  change land N [--no-wait]    land the change and follow it', &
            '  change close N, change reopen N', &
            '  change review N     ask the review agent for a review of the change', &
            '  change reviewed N   a person reviewed it; clears its flag (admin)', &
            '  change list --flagged   changes the review agent flagged for a person', &
            '                      (change and push take --repo NAME outside a checkout)', &
        ! issues
            '  issue list [--all]  open issues (--all: also closed ones)', &
            '  issue view N        an issue with its comments', &
            '  issue new --title T [--body-file F|-]', &
            '  issue comment N [-m TEXT], issue edit N [--title T] [--body-file F|-]', &
            '  issue close N, issue reopen N', &
        ! landings, jobs and verification
            '  landings [REPO]     recent landings of a repository', &
            '  jobs [REPO]         running, waiting and recent jobs (landings and', &
            '                      verifications of main)', &
            '  verify [REPO] [--no-wait]', &
            '                      verify main''s newest commit with the full suite', &
            '  job ID [--wait]     a job''s state and log; --wait follows it', &
            '  stats [REPO] [--days N]', &
            '                      queue numbers of the last 1, 7 or 30 days (7 unless N', &
            '                      says so) of a repository or of all: time from fy land', &
            '                      to main, landings per hour, failed entries, checks', &
            '  board --stats [REPO] [--days N]', &
            '                      the same for one repository, the checkout''s by default', &
            '                      (--json: the answer of the API instead of text)', &
        ! the audit log
            '  audit list [--before ID] [--limit N] [--json]', &
            '                      the audit log, newest entry first (admin)', &
            '  audit verify [--json]', &
            '                      recompute the hash chain of the audit log and name', &
            '                      the first entry that was altered or removed (admin)', &
        ! git and agents
            '  git-credential get  git credential helper for the stored login:', &
            '                      git config --global credential.SERVER.helper', &
            '                      ''!fy git-credential''', &
            '  mcp                 MCP server on standard input and output for agents:', &
            '                      tools for repositories, changes, issues, landings', &
            '                      and jobs, each with the arguments it lists', &
        ! the program itself
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

        ! Landings are squashed by git itself, which older versions cannot do.
        call land_git_check(exists, text)
        if (.not. exists) then
            write (error_unit, '(a)') text
            call quit(1)
        end if
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
        call store_open(data//'/fortyard.db', db, stat, text)
        if (stat /= 0) then
            if (len(text) == 0) text = db_errmsg(db)
            write (error_unit, '(a)') 'fy: database: '//text
            call quit(1)
        end if
        if (flag('--init')) then
            call store_bootstrap(db, token, stat)
            if (stat /= 0) error stop 'fy: bootstrap failed'
            call audit_append(db, 'admin', 'token.created', 'bootstrap', 'scopes admin')
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
        character(len=:), allocatable :: body, scopes, payload, name, word
        integer :: code, stat, i
        logical :: bad, scoped

        ! Every argument is checked: a mistyped option must not fall back to
        ! the scopes of the current token, which is admin for the person.
        name = ''
        scopes = ''
        scoped = .false.
        bad = arg(2) /= 'create'
        i = 3
        do while (i <= command_argument_count() .and. .not. bad)
            word = arg(i)
            if (word == '--scopes') then
                i = i + 1
                scopes = arg(i)
                scoped = .true.
            else if (index(word, '--scopes=') == 1) then
                scopes = word(10:)
                scoped = .true.
            else if (index(word, '-') == 1 .or. len(name) > 0) then
                bad = .true.
            else
                name = word
            end if
            i = i + 1
        end do
        if (bad .or. len(name) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy token create NAME [--scopes S,...]'
            call quit(2)
        end if
        if (scoped .and. len_trim(scopes) == 0) then
            write (error_unit, '(a)') 'fy: --scopes needs a list such as write,land'
            call quit(2)
        end if
        payload = '{"name":"'//json_escape_string(name)//'"'
        if (scoped) payload = payload//',"scopes":"'//json_escape_string(scopes)//'"'
        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'POST', '/api/v1/tokens', cfg%token, &
            payload//'}', code, body, stat)
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
            if (state == 'landed' .or. state == 'failed' .or. state == 'passed' .or. &
                state == 'cancelled') exit
            if (state /= last) then
                write (output_unit, '(a)') cli_field(body, 'kind')//' '//id//' '//state
                flush (output_unit)
                last = state
            end if
            call proc_sleep_ms(2000)
        end do
        if (state == 'landed') then
            write (output_unit, '(a)') 'landing '//id//' landed as '//cli_field(body, 'commit')
            return
        else if (state == 'passed') then
            write (output_unit, '(a)') 'verification '//id//' of '//cli_field(body, 'commit')// &
                ' passed'
            return
        end if
        write (output_unit, '(a)') cli_field(body, 'kind')//' '//id//' '//state//':', &
            cli_tail(cli_field(body, 'log'), 40)
        call quit(1)
    end subroutine follow_job

    subroutine push_head(cfg, repo, branch)
        !! git push of HEAD to BRANCH of REPO on the server. The token and proxy
        !! reach git through its environment, so no command line shows them.
        type(config_t), intent(in) :: cfg
        character(len=*), intent(in) :: repo, branch
        character(len=:), allocatable :: remote, large
        integer :: stat

        call proc_setenv('GIT_TERMINAL_PROMPT', '0')
        call proc_setenv('GIT_CONFIG_COUNT', '2')
        call proc_setenv('GIT_CONFIG_KEY_0', 'http.extraHeader')
        call proc_setenv('GIT_CONFIG_VALUE_0', 'Authorization: Bearer '//cfg%token)
        call proc_setenv('GIT_CONFIG_KEY_1', 'http.proxy')
        call proc_setenv('GIT_CONFIG_VALUE_1', cfg%proxy)
        remote = cfg%server//'/'//repo//'.git'
        ! What the server would refuse is found before anything is sent.
        call land_push_check(remote, large)
        if (len_trim(large) > 0) then
            call proc_setenv('GIT_CONFIG_COUNT', '0')
            write (error_unit, '(a)') 'fy: not pushed, files over the size limit (allow them in '// &
                '.fortyard/allow-large):', trim(large)
            call quit(1)
        end if
        call execute_command_line('git push --quiet --force "'//remote//'" HEAD:refs/heads/'// &
            branch, exitstat=stat)
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

    subroutine cmd_verify()
        !! fy verify [REPO] [--no-wait]: verifies main's newest commit with the
        !! repository's full suite and follows it.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, repo, id
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        repo = positional(1)
        if (len(repo) == 0) repo = remote_repo(cfg%server)
        call client_request(cfg%server, 'POST', '/api/v1/repos/'//repo//'/verify', cfg%token, &
            '{}', code, body, stat)
        if (stat /= 0 .or. code /= 202) call report(code, body, stat)
        id = cli_field(body, 'job')
        write (output_unit, '(a)') 'verification '//id//' of '//repo//' main queued'
        flush (output_unit)
        if (.not. flag('--no-wait')) call follow_job(cfg, id)
    end subroutine cmd_verify

    subroutine cmd_stats(board)
        !! fy stats [REPO] [--days N]: the queue numbers of a repository, or of
        !! the instance without one. fy board --stats [REPO] [--days N]: those
        !! of a repository, the checkout's without one.
        logical, intent(in) :: board
        type(config_t) :: cfg
        character(len=:), allocatable :: body, repo, path, days
        integer :: code, stat

        if (board .and. .not. flag('--stats')) then
            write (error_unit, '(a)') 'fy: usage: fy board --stats [REPO] [--days N]'
            call quit(2)
        end if
        days = option('--days', '')
        if (verify(days, '0123456789') /= 0) then
            write (error_unit, '(a)') 'fy: --days takes 1, 7 or 30'
            call quit(2)
        end if
        call config_load(cfg)
        call require_login(cfg)
        repo = positional(1)
        if (board) then
            if (len(repo) == 0) repo = remote_repo(cfg%server)
            path = '/api/v1/repos/'//repo//'/board?stats=1'
        else
            path = '/api/v1/stats?repo='//repo
        end if
        if (len(days) > 0) path = path//'&days='//days
        call client_request(cfg%server, 'GET', path, cfg%token, '', code, body, stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
            return
        end if
        write (output_unit, '(a)', advance='no') cli_stats(body)
    end subroutine cmd_stats

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
            [character(len=8) :: 'id', 'kind', 'repo', 'state', 'started', 'branch'])
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

    subroutine cmd_push()
        !! fy push [BRANCH] [--repo NAME]: pushes HEAD to BRANCH (default: the
        !! current branch), which opens a change or adds a version to it, and
        !! names the change.
        type(config_t) :: cfg
        character(len=:), allocatable :: repo, branch, body, number
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        repo = option('--repo', '')
        if (len(repo) == 0) repo = remote_repo(cfg%server)
        branch = positional(1)
        if (len(branch) == 0) branch = git_line('git symbolic-ref --quiet --short HEAD')
        if (len(branch) == 0 .or. branch == 'main') then
            write (error_unit, '(a)') 'fy: name the branch to push: fy push BRANCH'
            call quit(2)
        end if
        call push_head(cfg, repo, branch)
        call client_request(cfg%server, 'GET', '/api/v1/repos/'//repo//'/changes', cfg%token, &
            '', code, body, stat)
        number = change_of_branch(body, branch)
        if (len(number) == 0) then
            write (output_unit, '(a)') 'pushed '//branch//' to '//repo
        else
            write (output_unit, '(a)') 'change #'//number//': '//cfg%server//'/'//repo// &
                '/-/changes/'//number
        end if
    end subroutine cmd_push

    function change_of_branch(body, branch) result(number)
        !! The number of the change of `branch` in a change list, or ''.
        character(len=*), intent(in) :: body, branch
        character(len=:), allocatable :: number, rows, line
        integer :: at, eol, sp

        number = ''
        rows = cli_table(body, 'changes', [character(len=8) :: 'branch', 'number'])
        at = index(rows, new_line('a')) + 1
        do while (at <= len(rows))
            eol = index(rows(at:), new_line('a'))
            if (eol == 0) eol = len(rows) - at + 2
            line = rows(at:at + eol - 2)
            at = at + eol
            sp = index(line, ' ')
            if (sp == 0) cycle
            if (line(1:sp - 1) == branch) then
                number = trim(adjustl(line(sp:)))
                return
            end if
        end do
    end function change_of_branch

    subroutine cmd_change()
        !! fy change list|view|diff|comment|edit|land|close|reopen ...
        type(config_t) :: cfg
        character(len=:), allocatable :: what, repo, n, body, path, payload, text, state
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        what = positional(1)
        n = positional(2)
        repo = option('--repo', '')
        if (len(repo) == 0) repo = remote_repo(cfg%server)
        path = '/api/v1/repos/'//repo//'/changes'
        if (what /= 'list' .and. (len(n) == 0 .or. verify(n, '0123456789') /= 0)) then
            write (error_unit, '(a)') 'fy: usage: fy change '//what//' NUMBER'
            call quit(2)
        end if
        select case (what)
        case ('list')
            state = 'open'
            if (flag('--all')) state = 'all'
            if (flag('--flagged')) state = 'flagged'
            call client_request(cfg%server, 'GET', path//'?state='//state, cfg%token, '', &
                code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) call report(code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) return
            if (state == 'flagged') then
                write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                    [character(len=11) :: 'number', 'state', 'title', 'flag_reason'])
                return
            end if
            write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                [character(len=8) :: 'number', 'state', 'author', 'versions', 'branch', 'title'])
        case ('view')
            call client_request(cfg%server, 'GET', path//'/'//n, cfg%token, '', code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) call report(code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) return
            write (output_unit, '(a)') '#'//n//' '//cli_field(body, 'title'), &
                cli_field(body, 'state')//', '//cli_field(body, 'versions')// &
                ' version(s) by '//cli_field(body, 'author')//', branch '// &
                cli_field(body, 'branch'), cfg%server//'/'//repo//'/-/changes/'//n, ''
            text = cli_field(body, 'review')
            if (len(text) > 0 .and. text /= 'none') then
                if (cli_field(body, 'flagged') == 'true') text = text// &
                    ', flagged for a person: '//cli_field(body, 'flag_reason')
                write (output_unit, '(a)') 'review: '//text, ''
            end if
            text = cli_field(body, 'description')
            if (len(text) > 0) write (output_unit, '(a)') text, ''
            text = cli_table(body, 'versions_list', [character(len=8) :: 'version', 'head', &
                'pusher', 'created'])
            if (len(text) > 0) write (output_unit, '(a)', advance='no') text
            text = cli_table(body, 'comments', [character(len=8) :: 'account', 'created', 'body'])
            if (len(text) > 0) write (output_unit, '(a)', advance='no') new_line('a')//text
            text = cli_table(body, 'landings', [character(len=8) :: 'landing', 'state', 'commit'])
            if (len(text) > 0) write (output_unit, '(a)', advance='no') new_line('a')//text
        case ('diff')
            text = ''
            if (len(option('--version', '')) > 0) text = '?version='//option('--version', '')
            if (len(option('--from', '')) > 0) then
                if (len(text) == 0) then
                    text = '?from='//option('--from', '')
                else
                    text = text//'&from='//option('--from', '')
                end if
            end if
            call client_request(cfg%server, 'GET', path//'/'//n//'/diff'//text, cfg%token, '', &
                code, body, stat)
            call report(code, body, stat)
        case ('comment')
            text = option('-m', '')
            if (len(text) == 0) text = read_stdin()
            call client_request(cfg%server, 'POST', path//'/'//n//'/comments', cfg%token, &
                '{"body":"'//json_escape_string(text)//'"}', code, body, stat)
            if (stat /= 0 .or. code /= 201) call report(code, body, stat)
            write (output_unit, '(a)') 'commented on #'//n
        case ('edit')
            payload = ''
            if (len(option('--title', '')) > 0) payload = '"title":"'// &
                json_escape_string(option('--title', ''))//'"'
            if (len(option('--description-file', '')) > 0) then
                if (option('--description-file', '') == '-') then
                    text = read_stdin()
                else
                    text = read_file(option('--description-file', ''))
                end if
                if (len(payload) > 0) payload = payload//','
                payload = payload//'"description":"'//json_escape_string(text)//'"'
            end if
            call client_request(cfg%server, 'POST', path//'/'//n, cfg%token, '{'//payload//'}', &
                code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            write (output_unit, '(a)') 'edited #'//n
        case ('close', 'reopen')
            state = 'closed'
            if (what == 'reopen') state = 'open'
            call client_request(cfg%server, 'POST', path//'/'//n, cfg%token, &
                '{"state":"'//state//'"}', code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            write (output_unit, '(a)') '#'//n//' '//state
        case ('review', 'reviewed')
            call client_request(cfg%server, 'POST', path//'/'//n//'/'//what, cfg%token, '', &
                code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            if (what == 'review') then
                write (output_unit, '(a)') 'review of #'//n//' requested'
            else
                write (output_unit, '(a)') '#'//n//' reviewed'
            end if
        case ('land')
            call client_request(cfg%server, 'POST', path//'/'//n//'/land', cfg%token, '', &
                code, body, stat)
            if (stat /= 0 .or. code /= 202) call report(code, body, stat)
            write (output_unit, '(a)') 'landing '//cli_field(body, 'landing')//' of #'//n// &
                ' queued'
            flush (output_unit)
            if (.not. flag('--no-wait')) call follow_job(cfg, cli_field(body, 'landing'))
        case default
            write (error_unit, '(a)') 'fy: usage: fy change list|view|diff|comment|edit|'// &
                'land|close|reopen|review|reviewed'
            call quit(2)
        end select
    end subroutine cmd_change

    function read_stdin() result(text)
        character(len=:), allocatable :: text
        character(len=4096) :: line
        integer :: ios, n
        text = ''
        do
            read (*, '(a)', iostat=ios, size=n, advance='no') line
            if (is_iostat_end(ios)) exit
            text = text//line(1:n)
            if (ios /= 0) text = text//new_line('a')
            if (ios /= 0 .and. .not. is_iostat_eor(ios)) exit
        end do
        if (len(text) > 0) then
            if (text(len(text):) == new_line('a')) text = text(1:len(text) - 1)
        end if
    end function read_stdin

    function read_file(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: u, ios, n
        text = ''
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(a)') 'fy: cannot read '//path
            call quit(1)
        end if
        inquire (unit=u, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=ios) text
        close (u)
        if (len(text) > 0) then
            if (text(len(text):) == new_line('a')) text = text(1:len(text) - 1)
        end if
    end function read_file

    subroutine cmd_issue()
        !! fy issue list|view|new|comment|edit|close|reopen ...
        type(config_t) :: cfg
        character(len=:), allocatable :: what, repo, n, body, path, payload, text, state
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        what = positional(1)
        n = positional(2)
        repo = option('--repo', '')
        if (len(repo) == 0) repo = remote_repo(cfg%server)
        path = '/api/v1/repos/'//repo//'/issues'
        if (what /= 'list' .and. what /= 'new' .and. &
            (len(n) == 0 .or. verify(n, '0123456789') /= 0)) then
            write (error_unit, '(a)') 'fy: usage: fy issue '//what//' NUMBER'
            call quit(2)
        end if
        select case (what)
        case ('list')
            state = 'open'
            if (flag('--all')) state = 'all'
            call client_request(cfg%server, 'GET', path//'?state='//state, cfg%token, '', &
                code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) call report(code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) return
            write (output_unit, '(a)', advance='no') cli_table(body, 'issues', &
                [character(len=8) :: 'number', 'state', 'author', 'comments', 'title'])
        case ('view')
            call client_request(cfg%server, 'GET', path//'/'//n, cfg%token, '', code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) call report(code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) return
            write (output_unit, '(a)') '#'//n//' '//cli_field(body, 'title'), &
                cli_field(body, 'state')//', opened by '//cli_field(body, 'author')//' on '// &
                cli_field(body, 'created'), cfg%server//'/'//repo//'/-/issues/'//n, ''
            text = cli_field(body, 'body')
            if (len(text) > 0) write (output_unit, '(a)') text
            text = cli_table(body, 'comments_list', [character(len=8) :: 'account', 'created', &
                'body'])
            if (len(text) > 0) write (output_unit, '(a)', advance='no') new_line('a')//text
        case ('new')
            text = option('--body-file', '')
            if (text == '-') then
                text = read_stdin()
            else if (len(text) > 0) then
                text = read_file(text)
            end if
            if (len(option('--title', '')) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy issue new --title T [--body-file F|-]'
                call quit(2)
            end if
            call client_request(cfg%server, 'POST', path, cfg%token, '{"title":"'// &
                json_escape_string(option('--title', ''))//'","body":"'// &
                json_escape_string(text)//'"}', code, body, stat)
            if (stat /= 0 .or. code /= 201) call report(code, body, stat)
            n = cli_field(body, 'issue')
            write (output_unit, '(a)') 'issue #'//n//': '//cfg%server//'/'//repo//'/-/issues/'//n
        case ('comment')
            text = option('-m', '')
            if (len(text) == 0) text = read_stdin()
            call client_request(cfg%server, 'POST', path//'/'//n//'/comments', cfg%token, &
                '{"body":"'//json_escape_string(text)//'"}', code, body, stat)
            if (stat /= 0 .or. code /= 201) call report(code, body, stat)
            write (output_unit, '(a)') 'commented on #'//n
        case ('edit')
            payload = ''
            if (len(option('--title', '')) > 0) payload = '"title":"'// &
                json_escape_string(option('--title', ''))//'"'
            if (len(option('--body-file', '')) > 0) then
                if (option('--body-file', '') == '-') then
                    text = read_stdin()
                else
                    text = read_file(option('--body-file', ''))
                end if
                if (len(payload) > 0) payload = payload//','
                payload = payload//'"body":"'//json_escape_string(text)//'"'
            end if
            call client_request(cfg%server, 'POST', path//'/'//n, cfg%token, '{'//payload//'}', &
                code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            write (output_unit, '(a)') 'edited #'//n
        case ('close', 'reopen')
            state = 'closed'
            if (what == 'reopen') state = 'open'
            call client_request(cfg%server, 'POST', path//'/'//n, cfg%token, &
                '{"state":"'//state//'"}', code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            write (output_unit, '(a)') '#'//n//' '//state
        case default
            write (error_unit, '(a)') 'fy: usage: fy issue list|view|new|comment|edit|close|reopen'
            call quit(2)
        end select
    end subroutine cmd_issue

    subroutine cmd_audit()
        !! fy audit list [--before ID] [--limit N]: the audit log, newest first.
        !! fy audit verify: has the server recompute the chain; status 1 and
        !! the first entry that does not fit when it is broken.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, path
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        select case (positional(1))
        case ('list')
            path = '/api/v1/audit?limit='//option('--limit', '50')
            if (len(option('--before', '')) > 0) path = path//'&before='//option('--before', '')
            call client_request(cfg%server, 'GET', path, cfg%token, '', code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
                call report(code, body, stat)
                return
            end if
            write (output_unit, '(a)', advance='no') cli_table(body, 'entries', &
                [character(len=8) :: 'id', 'at', 'actor', 'event', 'subject', 'detail'])
            if (cli_field(body, 'next') /= '0') write (output_unit, '(a)') &
                'older entries: fy audit list --before '//cli_field(body, 'next')
        case ('verify')
            call client_request(cfg%server, 'GET', '/api/v1/audit/verify', cfg%token, '', code, &
                body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            if (flag('--json')) then
                write (output_unit, '(a)') body
            else if (cli_field(body, 'intact') == 'true') then
                write (output_unit, '(a)') 'audit log intact: '//cli_field(body, 'checked')// &
                    ' entries, newest hash '//cli_field(body, 'head')
            else
                write (output_unit, '(a)') 'audit log broken at entry '// &
                    cli_field(body, 'broken')//': '//cli_field(body, 'reason')
            end if
            if (cli_field(body, 'intact') /= 'true') call quit(1)
        case default
            write (error_unit, '(a)') 'fy: usage: fy audit list [--before ID] [--limit N] [--json] |'// &
                ' verify [--json]'
            call quit(2)
        end select
    end subroutine cmd_audit

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

    logical function wants_help()
        integer :: i

        wants_help = .false.
        do i = 2, command_argument_count()
            if (arg(i) == '--help' .or. arg(i) == '-h') wants_help = .true.
        end do
    end function wants_help

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
                case ('--no-push', '--no-wait', '--wait', '--json', '--init', '--all', '--stats')
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
        case ('numbers')
            if (len(arg(3)) == 0 .or. len(arg(4)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo numbers NAME N'
                call quit(2)
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos/'//arg(3)//'/numbers', &
                cfg%token, '{"next":"'//json_escape_string(arg(4))//'"}', code, body, stat)
            call report(code, body, stat)
        case ('policy')
            if (len(arg(3)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy repo policy NAME [KEY=VALUE ...]'
                call quit(2)
            end if
            call require_login(cfg)
            if (len(arg(4)) == 0) then
                call client_request(cfg%server, 'GET', '/api/v1/repos/'//arg(3)//'/policy', &
                    cfg%token, '', code, body, stat)
            else
                call client_request(cfg%server, 'PUT', '/api/v1/repos/'//arg(3)//'/policy', &
                    cfg%token, policy_payload(), code, body, stat)
            end if
            call report(code, body, stat)
        case ('list')
            call cmd_get('/api/v1/repos')
        case default
            write (error_unit, '(a)') 'fy: usage: fy repo add NAME URL | create NAME |'// &
                ' mode NAME MODE | numbers NAME N | policy NAME [KEY=VALUE ...] | list'
            call quit(2)
        end select
    end subroutine cmd_repo

    function policy_payload() result(payload)
        !! The settings KEY=VALUE from the fourth argument on, as a JSON object;
        !! one that is not a setting ends the command.
        character(len=:), allocatable :: payload, pair, error
        type(policy_t) :: policy
        integer :: i, eq

        payload = '{'
        do i = 4, command_argument_count()
            pair = arg(i)
            eq = index(pair, '=')
            error = 'settings are KEY=VALUE: '//POLICY_KEYS
            if (eq > 1) call policy_set(policy, pair(1:eq - 1), pair(eq + 1:), error)
            if (len(error) > 0) then
                write (error_unit, '(a)') 'fy: '//error
                call quit(2)
            end if
            if (i > 4) payload = payload//','
            payload = payload//'"'//pair(1:eq - 1)//'":"'//pair(eq + 1:)//'"'
        end do
        payload = payload//'}'
    end function policy_payload

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
        if (code < 200 .or. code >= 300) call quit(1)
    end subroutine report

end program fy
