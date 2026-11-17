program fy
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64, &
        output_unit
    use fx_json_build, only: &
        json_escape_string
    use fy_audit, only: &
        audit_append
    use fy_backend_github, only: &
        github_configure, &
        github_main, &
        github_setup, &
        github_token_file
    use fy_backend_gitlab, only: &
        gitlab_backend_main, &
        gitlab_backend_t, &
        gitlab_setup
    use fy_build_info, only: &
        FY_COMMIT, &
        FY_VERSION
    use fy_cli, only: &
        cli_field, &
        cli_job, &
        cli_stats, &
        cli_table, &
        cli_tail
    use fy_client, only: &
        client_request, &
        client_set_proxy, &
        config_load, &
        config_save, &
        config_t
    use fy_executor, only: &
        executor_main
    use fy_history, only: &
        history_recover, &
        history_rewrite
    use fy_instance_lock, only: &
        instance_batch_pending, &
        instance_lock, &
        instance_pending, &
        instance_unlock
    use fy_land, only: &
        land_git_check, &
        land_push_check
    use fy_live, only: &
        live_earlier_at, &
        live_gap_line
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
    case ('admin')
        call cmd_admin()
    case ('alerts')
        call cmd_alerts()
    case ('audit')
        call cmd_audit()
    case ('backends')
        call cmd_backends()
    case ('backend')
        call cmd_backend()
    case ('board')
        call cmd_stats(.true.)
    case ('change')
        call cmd_change()
    case ('executor')
        call cmd_executor()
    case ('git-credential')
        call cmd_git_credential()
    case ('issue')
        call cmd_issue()
    case ('ingest')
        call cmd_ingest()
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
            '  ingest REPO PR --head SHA --approve-execution', &
            '                      import an approved GitHub head without running checks (admin)', &
            '  admin rewrite NAME --data DIR --bundle FILE --expect-refs FILE', &
            '        --commit-map FILE [--check] [--journal-id ID]', &
            '                      offline history replacement (local administrator)', &
            '  admin rewrite NAME --data DIR --recover JOURNAL', &
            '                      recover an interrupted offline history replacement', &
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
            '  repo public NAME on|off', &
            '                      the repository is public, which backends= needs (admin)', &
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
            '                      check_timeout=MINUTES  a runner ends a check that runs', &
            '                        longer; it runs once more, then the landing fails', &
            '                        with the reason timeout (default 60; 0: no limit)', &
            '                      verify_on=CLASS[,CLASS...]  runner classes each', &
            '                        verification of main runs on (default linux)', &
            '                        CLASS@nightly: verified on its own once a night', &
            '                      backends=github,gitlab  external backends its checks', &
            '                        may run on (public repositories only; empty: none)', &
            '                      egress=DOMAIN[,DOMAIN...]  hosts its checks reach', &
            '                        beyond the executor allowlist', &
            '  repo list           list repositories', &
            '                      clone and push: git clone http://x:TOKEN@HOST/NAME.git', &
        ! runners
            '  runner --server URL --token-file F --data DIR [--follow LINK] [--class NAME]', &
            '                      perform landing checks for the server (scope runner);', &
            '                        --class: the runner class whose checks it takes', &
            '                        (default linux; e.g. macos-arm64)', &
            '                        --executor DIR: fy executor runs the checks, sharing DIR', &
            '  backends [--json]   runner classes with their runners or backend processes,', &
            '                      running and waiting checks, limits, median wait, and', &
            '                      run time per repository with the checks it rests on', &
            '  executor DIR        run the checks a runner hands over in DIR, without its', &
            '                        token (a container of its own next to the runner)', &
            '  backend github --server URL --token-file F --owner ORG', &
            '        (--github-token-file F2 | --app-id ID --app-key FILE)', &
            '        [--classes github-linux,...] [--max-running 20] [--max-macos 5]', &
            '        [--max-minutes 360] [--api https://api.github.com] [--poll 30]', &
            '        [--start-timeout SECONDS]', &
            '                      run checks of the GitHub classes on GitHub Actions in', &
            '                      ORG''s public repositories (runner token; F2: a fine-grained', &
            '                      token with Actions read and write, read for each request)', &
            '  backend github setup GITHUB-NAME --app-id ID --app-key FILE --owner ORG', &
            '        [--github-token-file F]', &
            '                      check and prepare ORG/GITHUB-NAME for the GitHub backend', &
            '  backend gitlab --server URL --token-file F --gitlab URL --project GROUP/NAME', &
            '         --project-token-file F2 --class gitlab-NAME [--data DIR]', &
            '         [--max-minutes 15] [--start-wait 60] [--backoff 600] [--poll 5]', &
            '         [--git-url URL]', &
            '                      run checks of the class on the project''s GitLab runner:', &
            '                        one job at a time; a job not started within', &
            '                        --start-wait seconds is cancelled and the backend', &
            '                        pauses for --backoff seconds', &
            '  backend gitlab setup --gitlab URL --project GROUP/NAME', &
            '         --project-token-file F2 [--config deploy/gitlab/fortyard.yml]', &
            '                      put the fixed pipeline on the project''s protected', &
            '                        branch config and make it the project''s CI/CD file', &
        ! landing and pushing
            '  land [BRANCH] [--repo NAME] [--no-push] [--no-wait]', &
            '                      push branch BRANCH (HEAD when there is none), land it', &
            '                      on main after its check', &
            '                      passed and follow it to the end (admin)', &
            '  land [BRANCH] --urgent', &
            '                      land before every landing not yet in a candidate,', &
            '                      in a candidate of its own (admin)', &
            '  land --cancel ID    cancel a landing that has not ended (land scope), or a', &
            '                      verification of main (admin)', &
            '  land --cancel-all REPO', &
            '                      cancel every landing of REPO that has not ended (admin)', &
            '  land REPO BRANCH    land a branch that is already pushed', &
            '  push [BRANCH]       push branch BRANCH (HEAD when there is none), which', &
            '                      opens or updates its change', &
            '                      (push and land send no file over 1 MiB, or over the bytes', &
            '                      in .fortyard/size-limit, that .fortyard/allow-large does', &
            '                      not allow)', &
        ! changes
            '  change list [--state open|landed|closed|all] [--review flagged|unreviewed]', &
            '              [--repo NAME]   the changes of NAME, not of this checkout', &
            '                      [--attention needs-author] [--all-repos]', &
            '                      changes, newest activity first; the open ones of', &
            '                      this repository by default (--all: --state all)', &
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
            '  jobs [REPO] [--kind landing|test|verify] [--state STATE]', &
            '                      running, waiting and recent jobs (landings and', &
            '                      verifications of main), each with its change', &
            '  verify [REPO] [--no-wait]', &
            '                      verify main''s newest commit with the full suite', &
            '  job ID [--wait|--follow]', &
            '                      a job''s state, change, checks and log; --wait', &
            '                      follows it, --follow prints its log as it grows', &
            '  stats [REPO] [--days N]', &
            '                      queue numbers of the last 1, 7 or 30 days (7 unless N', &
            '                      says so) of a repository or of all: time from fy land', &
            '                      to main, landings per hour, failed entries, checks', &
            '  board --stats [REPO] [--days N]', &
            '                      the same for one repository, the checkout''s by default', &
            '                      (--json: the answer of the API instead of text)', &
        ! alerts
            '  alerts [--json]     the alerts that fire now, with repository, since', &
            '                      and detail; exits 1 when one fires', &
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
        integer :: lfd, port, stat, colon, workers, threads, interval, n, instance_fd
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
        call instance_lock(data, instance_fd, stat)
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: the instance is already running or under maintenance'
            call quit(1)
        end if
        if (instance_pending(data) .or. instance_batch_pending(data)) then
            write (error_unit, '(a)') 'fy: an interrupted history rewrite needs admin recovery'
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
            call db_close(db)
            call instance_unlock(instance_fd)
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

    subroutine cmd_admin()
        character(len=:), allocatable :: data, repo, journal, message, recover
        integer :: status

        if (arg(2) /= 'rewrite' .or. len(arg(3)) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy admin rewrite NAME --data DIR '// &
                '--bundle FILE --expect-refs FILE --commit-map FILE [--check] [--journal-id ID]'
            call quit(2)
        end if
        repo = arg(3)
        data = option('--data', './fortyard-data')
        recover = option('--recover', '')
        if (len(recover) > 0 .and. (flag('--check') .or. flag('--journal-id'))) then
            write (error_unit, '(a)') 'fy: --recover cannot be combined with --check or --journal-id'
            call quit(2)
        end if
        if (len(recover) > 0) then
            call history_recover(data, repo, recover, status, message)
        else if (flag('--journal-id')) then
            call history_rewrite(data, repo, option('--bundle', ''), &
                option('--expect-refs', ''), option('--commit-map', ''), journal, status, message, &
                check_only=flag('--check'), journal_id=option('--journal-id', ''))
        else
            call history_rewrite(data, repo, option('--bundle', ''), &
                option('--expect-refs', ''), option('--commit-map', ''), journal, status, message, &
                check_only=flag('--check'))
        end if
        if (status /= 0) then
            write (error_unit, '(a)') 'fy: '//message
            call quit(1)
        end if
        write (output_unit, '(a)') message
        if (allocated(journal)) then
            if (len(journal) > 0) write (output_unit, '(a)') 'Recovery backup: '//journal
        end if
    end subroutine cmd_admin

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

    subroutine cmd_backend_gitlab()
        !! fy backend gitlab [setup]: a backend process, or the setup of its project.
        type(gitlab_backend_t) :: b
        integer :: stat

        b%gitlab = option('--gitlab', 'https://gitlab.tugraz.at')
        b%project = option('--project', '')
        b%project_token = first_line(option('--project-token-file', ''))
        if (len(b%project) == 0 .or. len(b%project_token) == 0) then
            write (error_unit, '(a)') 'fy backend gitlab: --project and --project-token-file '// &
                '(a project access token) are needed'
            call quit(2)
        end if
        if (positional(2) == 'setup') then
            call gitlab_setup(b, option('--config', 'deploy/gitlab/fortyard.yml'), stat)
            if (stat /= 0) call quit(1)
            return
        end if
        b%server = option('--server', 'http://127.0.0.1:8740')
        b%token = first_line(option('--token-file', ''))
        if (len(b%token) == 0) &
            error stop 'fy backend gitlab: --token-file must hold a runner token'
        b%class = option('--class', '')
        if (index(b%class, 'gitlab-') /= 1) &
            error stop 'fy backend gitlab: --class gitlab-NAME is needed'
        b%data_dir = option('--data', './fortyard-gitlab')
        b%git_url = option('--git-url', '')
        b%max_minutes = whole(option('--max-minutes', '15'))
        b%start_wait = whole(option('--start-wait', '60'))
        b%backoff = whole(option('--backoff', '600'))
        b%poll = whole(option('--poll', '5'))
        if (min(b%max_minutes, b%poll) < 1 .or. min(b%start_wait, b%backoff) < 0) &
            error stop 'fy backend gitlab: --max-minutes and --poll take whole numbers from 1, '// &
                '--start-wait and --backoff from 0'
        call gitlab_backend_main(b)
    end subroutine cmd_backend_gitlab

    integer function whole(text)
        !! The whole number `text`, or -1.
        character(len=*), intent(in) :: text
        integer :: ios

        whole = -1
        if (len(text) == 0 .or. verify(text, '0123456789') /= 0) return
        read (text, *, iostat=ios) whole
        if (ios /= 0) whole = -1
    end function whole

    function first_line(file) result(s)
        !! The first line of `file`, '' when there is none.
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: s
        character(len=4096) :: line
        integer :: u, ios

        s = ''
        if (len(file) == 0) return
        open (newunit=u, file=file, action='read', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
        if (ios == 0) s = trim(line)
    end function first_line

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
        ! Behind an egress proxy the runner reaches its server only through it.
        call get_environment_variable('FY_PROXY', line, status=ios)
        if (ios == 0 .and. len_trim(line) > 0) call client_set_proxy(trim(line))
        call runner_main(server, token, option('--data', './fortyard-data'), &
            option('--follow', ''), option('--class', 'linux'), option('--executor', ''))
    end subroutine cmd_runner

    subroutine cmd_executor()
        !! fy executor DIR: runs the checks a controller (fy runner --executor
        !! DIR) hands over in DIR, without the runner's token.
        if (len(positional(1)) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy executor DIR'
            call quit(2)
        end if
        call executor_main(positional(1))
    end subroutine cmd_executor
    subroutine cmd_backend()
        !! fy backend github|gitlab ...: for github a backend process that runs
        !! the checks of its classes on GitHub Actions, or the setup of one
        !! repository for it; gitlab: cmd_backend_gitlab.
        character(len=:), allocatable :: text, token, gh_token
        character(len=1024) :: proxy_url
        integer :: max_running, max_macos, max_minutes, poll, start, stat

        ! Behind an egress proxy a backend reaches its server only through it.
        call get_environment_variable('FY_PROXY', proxy_url, status=stat)
        if (stat == 0 .and. len_trim(proxy_url) > 0) call client_set_proxy(trim(proxy_url))
        if (positional(1) == 'gitlab') then
            call cmd_backend_gitlab()
            return
        end if
        if (positional(1) /= 'github') then
            write (error_unit, '(a)') 'fy: usage: fy backend github|gitlab ... (see fy help)'
            call quit(2)
        end if
        gh_token = ''
        text = option('--github-token-file', '')
        if (len(text) > 0) gh_token = token_line(text)
        call github_configure(option('--app-id', ''), option('--app-key', ''), &
            option('--owner', ''), option('--api', 'https://api.github.com'), gh_token, &
            backend_number('--poll', 30), backend_number('--start-timeout', 600))
        if (len(text) > 0) call github_token_file(text)
        if (len(option('--owner', '')) == 0 .or. (len(gh_token) == 0 .and. &
            (len(option('--app-id', '')) == 0 .or. len(option('--app-key', '')) == 0))) then
            write (error_unit, '(a)') 'fy backend github: needs --owner and either'// &
                ' --github-token-file (a fine-grained token with Actions read and write)'// &
                ' or --app-id with --app-key'
            call quit(2)
        end if
        if (positional(2) == 'setup') then
            if (len(positional(3)) == 0) then
                write (error_unit, '(a)') 'fy: usage: fy backend github setup REPO ...'
                call quit(2)
            end if
            call github_setup(positional(3), stat)
            call quit(stat)
        end if
        token = token_line(option('--token-file', ''))
        if (len(token) == 0) error stop 'fy backend github: --token-file must hold a runner token'
        max_running = backend_number('--max-running', 20)
        max_macos = backend_number('--max-macos', 5)
        max_minutes = backend_number('--max-minutes', 360)
        call github_main(option('--server', 'http://127.0.0.1:8740'), token, &
            option('--classes', 'github-linux,github-macos-arm64,github-windows'), max_running, &
            max_macos, max_minutes)

    end subroutine cmd_backend

    integer function backend_number(name, default)
        !! The positive number given with option `name`, else `default`.
        character(len=*), intent(in) :: name
        integer, intent(in) :: default
        character(len=:), allocatable :: text
        integer :: ios
        text = option(name, '')
        backend_number = default
        if (len(text) == 0) return
        read (text, *, iostat=ios) backend_number
        if (ios /= 0 .or. backend_number < 1) then
            write (error_unit, '(a)') 'fy backend github: '//name//' needs a positive number'
            call quit(2)
        end if
    end function backend_number

    function token_line(file) result(s)
        !! The first line of `file` (a token), empty when it cannot be read.
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: s
        character(len=4096) :: line
        integer :: u, ios
        s = ''
        if (len(file) == 0) return
        open (newunit=u, file=file, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
        if (ios == 0) s = trim(line)
    end function token_line

    subroutine cmd_ingest()
        type(config_t) :: cfg
        character(len=:), allocatable :: repo, pr, head, body
        integer :: code, stat

        repo = positional(1)
        pr = positional(2)
        head = option('--head', '')
        if (len(repo) == 0 .or. len(pr) == 0 .or. len(head) /= 40 .or. &
                .not. flag('--approve-execution')) then
            write (error_unit, '(a)') 'fy: usage: fy ingest REPO PR --head SHA --approve-execution', &
                'Approve only the head reviewed by a person. Import queues no checks.'
            call quit(2)
        end if
        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'POST', '/api/v1/repos/'//repo//'/ingest', cfg%token, &
            '{"pr":"'//json_escape_string(pr)//'","head":"'//json_escape_string(head)// &
            '","approve_execution":true}', code, body, stat)
        if (stat /= 0 .or. (code /= 201 .and. code /= 200)) call report(code, body, stat)
        if (stat /= 0 .or. (code /= 201 .and. code /= 200)) return
        if (flag('--json')) then
            write (output_unit, '(a)') body
        else
            write (output_unit, '(a)') 'change #'//cli_field(body, 'change')//' imported at '//head, &
                'No checks queued. Land separately after review: fy change land '// &
                cli_field(body, 'change')//' --repo '//repo
        end if
    end subroutine cmd_ingest

    subroutine cmd_land()
        !! fy land [BRANCH] [--repo NAME] [--no-push] [--no-wait]: pushes the
        !! checkout's branch BRANCH, or HEAD when there is no branch of that name,
        !! to BRANCH (default: the current branch), queues its
        !! landing and follows it to the end; the exit status tells the outcome.
        !! The repository is the one of a remote on the server, unless --repo
        !! names it. The form fy land REPO BRANCH lands a branch already pushed.
        !! --urgent (admin) lands before the others; --cancel ID cancels landing ID
        !! and --cancel-all REPO every queued and running landing of REPO.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, repo, branch, id
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        if (flag('--cancel') .or. flag('--cancel-all')) then
            call cancel_landing(cfg)
            return
        end if
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
            '{"branch":"'//json_escape_string(branch)//'"'// &
            trim(merge(',"urgent":true', '              ', flag('--urgent')))//'}', code, body, stat)
        if (stat /= 0 .or. code /= 202) call report(code, body, stat)
        id = cli_field(body, 'landing')
        write (output_unit, '(a)') 'landing '//id//' of '//repo//' '//branch//' queued'
        flush (output_unit)
        if (flag('--no-wait')) return
        call follow_job(cfg, id)
    end subroutine cmd_land

    subroutine cancel_landing(cfg)
        !! fy land --cancel ID: cancels landing ID of the repository the server
        !! names for it; fy land --cancel-all REPO: all of REPO's. A landing that
        !! ended already is not cancelled and ends fy with status 1.
        type(config_t), intent(in) :: cfg
        character(len=:), allocatable :: body, id, repo
        integer :: code, stat

        if (flag('--cancel-all')) then
            repo = option('--cancel-all', '')
            if (len(repo) == 0 .or. index(repo, '--') == 1) then
                write (error_unit, '(a)') 'fy: usage: fy land --cancel-all REPO'
                call quit(2)
            end if
            call client_request(cfg%server, 'DELETE', '/api/v1/repos/'//repo//'/landings', &
                cfg%token, '', code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            write (output_unit, '(a)') 'cancelled landings of '//repo//': '//body
            return
        end if
        id = option('--cancel', '')
        if (len(id) == 0 .or. verify(id, '0123456789') /= 0) then
            write (error_unit, '(a)') 'fy: usage: fy land --cancel ID'
            call quit(2)
        end if
        call client_request(cfg%server, 'GET', '/api/v1/jobs/'//id, cfg%token, '', code, &
            body, stat)
        if (stat /= 0 .or. code /= 200) call report(code, body, stat)
        repo = cli_field(body, 'repo')
        call client_request(cfg%server, 'DELETE', '/api/v1/repos/'//repo//'/landings/'//id, &
            cfg%token, '', code, body, stat)
        if (stat /= 0 .or. code /= 200) call report(code, body, stat)
        write (output_unit, '(a)') 'landing '//id//' of '//repo//' cancelled'
    end subroutine cancel_landing

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

    subroutine follow_log(cfg, id)
        !! Prints the log of job `id` as it grows (GET /api/v1/jobs/ID/log),
        !! then its outcome; a failed job ends fy with status 1, as with --wait.
        !! Live offsets count the bytes the check wrote. When the job ends after
        !! its live log was printed, the stored log follows from the same
        !! offset: it is the output of the check's last attempt with its
        !! outcome after it, and of a long output its first and last part with
        !! a line that says how many bytes were left out; earlier attempts
        !! after it are not printed again. A stored log that does not hold
        !! what was printed last at that offset, as when the check was not
        !! run again after its runner was lost, is printed whole.
        type(config_t), intent(in) :: cfg
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: body, text, state, begun, seen, stored
        character(len=24) :: num
        character(len=1) :: last
        integer(int64) :: from, next, length
        integer :: code, stat, at
        logical :: printed_live

        from = 0
        last = new_line('a')
        printed_live = .false.
        begun = ''
        seen = ''
        do
            write (num, '(i0)') from
            call client_request(cfg%server, 'GET', '/api/v1/jobs/'//id//'/log?from='// &
                trim(num), cfg%token, '', code, body, stat)
            if (stat /= 0 .or. code /= 200) call report(code, body, stat)
            length = field_int(body, 'length')
            next = field_int(body, 'next')
            text = cli_field(body, 'text')
            if (cli_field(body, 'live') == 'true') then
                if (length < from .or. (len(begun) > 0 .and. cli_field(body, 'begun') /= begun)) &
                    then
                    ! The check started again: run again after a timeout or a
                    ! lost runner, or another check after a candidate was split.
                    write (output_unit, '(a)') new_line('a')//'--- the log starts again ---'
                    from = 0
                    last = new_line('a')
                    seen = ''
                    begun = cli_field(body, 'begun')
                    cycle
                end if
                begun = cli_field(body, 'begun')
                call put_log(text, last)
                printed_live = printed_live .or. len(text) > 0
                seen = seen//text
                if (len(seen) > 64) seen = seen(len(seen) - 63:)
                ! Without progress, as before an incomplete UTF-8 character at
                ! the end, the next request waits for more.
                if (next < length .and. next > from) then
                    from = next
                    cycle
                end if
                from = next
            else if (cli_field(body, 'ended') == 'true') then
                if (printed_live) then
                    call client_request(cfg%server, 'GET', '/api/v1/jobs/'//id//'/log', &
                        cfg%token, '', code, body, stat)
                    if (stat /= 0 .or. code /= 200) call report(code, body, stat)
                    stored = cli_field(body, 'text')
                    text = after_offset(stored, from - len(seen, int64))
                    ! Where the stored log left those bytes out, it is taken as
                    ! the one printed.
                    if (index(text, seen) == 1 .or. index(text, '... ') == 1) then
                        text = after_offset(stored, from)
                        at = live_earlier_at(text)
                        if (at > 0) text = text(1:at - 1)
                        if (index(text, '... ') == 1 .and. last /= new_line('a')) &
                            text = new_line('a')//text
                    else
                        write (output_unit, '(a)') new_line('a')//'--- the log starts again ---'
                        last = new_line('a')
                        text = stored
                    end if
                    call put_log(text, last)
                else if (from < length) then
                    call put_log(text, last)
                    if (next > from) then
                        from = next
                        if (next < length) cycle
                    end if
                end if
                exit
            end if
            call proc_sleep_ms(1000)
        end do
        if (last /= new_line('a')) write (output_unit, '(a)') ''
        call client_request(cfg%server, 'GET', '/api/v1/jobs/'//id, cfg%token, '', code, body, &
            stat)
        if (stat /= 0 .or. code /= 200) call report(code, body, stat)
        state = cli_field(body, 'state')
        if (state == 'landed') then
            write (output_unit, '(a)') 'landing '//id//' landed as '//cli_field(body, 'commit')
        else if (state == 'passed') then
            write (output_unit, '(a)') 'verification '//id//' of '//cli_field(body, 'commit')// &
                ' passed'
        else
            write (output_unit, '(a)') cli_field(body, 'kind')//' '//id//' '//state
            call quit(1)
        end if

    end subroutine follow_log

    subroutine put_log(s, last)
        !! Prints `s` as it is; `last` is the last byte printed.
        character(len=*), intent(in) :: s
        character(len=1), intent(inout) :: last
        if (len(s) == 0) return
        write (output_unit, '(a)', advance='no') s
        flush (output_unit)
        last = s(len(s):)
    end subroutine put_log

    function after_offset(log, offset) result(rest)
        !! What the stored log `log` has after byte `offset` of the check's
        !! output. Each line "... N bytes left out ..." of the log stands for N
        !! bytes of the output; an offset within them gives such a line for the
        !! rest of them first.
        character(len=*), intent(in) :: log
        integer(int64), intent(in) :: offset
        character(len=:), allocatable :: rest
        integer(int64) :: raw, n
        integer :: pos, m, e

        rest = ''
        pos = 1
        raw = 0
        do
            call next_gap(log, pos, m, n, e)
            if (m == 0 .or. offset < raw + (m - pos)) then
                if (offset - raw < len(log) - pos + 1) rest = log(pos + int(offset - raw):)
                return
            end if
            raw = raw + (m - pos)
            if (offset < raw + n) then
                rest = live_gap_line(raw + n - offset)//log(e - 1:)
                return
            end if
            raw = raw + n
            pos = e
        end do
    end function after_offset

    subroutine next_gap(log, pos, m, n, e)
        !! The first line "... N bytes left out ..." of `log` from `pos` on:
        !! where it starts (0: none), N, and where the line after it starts.
        character(len=*), intent(in) :: log
        integer, intent(in) :: pos
        integer, intent(out) :: m, e
        integer(int64), intent(out) :: n
        character(len=*), parameter :: ENDS = ' bytes left out ...'//new_line('a')
        integer :: k, d, ios

        m = 0
        n = 0
        e = 0
        k = pos
        do
            d = index(log(k:), '... ')
            if (d == 0) return
            k = k + d - 1
            if (k == 1 .or. log(max(1, k - 1):max(1, k - 1)) == new_line('a')) then
                d = verify(log(k + 4:), '0123456789')
                if (d > 1) then
                    if (log(k + 3 + d:min(len(log), k + 2 + d + len(ENDS))) == ENDS) then
                        read (log(k + 4:k + 2 + d), *, iostat=ios) n
                        if (ios == 0) then
                            m = k
                            e = k + 3 + d + len(ENDS)
                            return
                        end if
                    end if
                end if
            end if
            k = k + 4
        end do
    end subroutine next_gap

    integer(int64) function field_int(json, name) result(v)
        character(len=*), intent(in) :: json, name
        character(len=:), allocatable :: s
        integer :: ios
        v = 0
        s = cli_field(json, name)
        read (s, *, iostat=ios) v
        if (ios /= 0) v = 0
    end function field_int

    subroutine push_head(cfg, repo, branch)
        !! git push to BRANCH of REPO on the server of the checkout's own branch
        !! BRANCH, or of HEAD when the checkout has no branch of that name. The
        !! token and proxy reach git through its environment, so no command line
        !! shows them.
        type(config_t), intent(in) :: cfg
        character(len=*), intent(in) :: repo, branch
        character(len=:), allocatable :: remote, large, source
        integer :: stat

        ! A named branch may be checked out in another worktree of this one.
        source = 'HEAD'
        call execute_command_line('git rev-parse --quiet --verify refs/heads/'//branch// &
            ' > /dev/null', exitstat=stat)
        if (stat == 0) source = 'refs/heads/'//branch
        call proc_setenv('GIT_TERMINAL_PROMPT', '0')
        call proc_setenv('GIT_CONFIG_COUNT', '2')
        call proc_setenv('GIT_CONFIG_KEY_0', 'http.extraHeader')
        call proc_setenv('GIT_CONFIG_VALUE_0', 'Authorization: Bearer '//cfg%token)
        call proc_setenv('GIT_CONFIG_KEY_1', 'http.proxy')
        call proc_setenv('GIT_CONFIG_VALUE_1', cfg%proxy)
        remote = cfg%server//'/'//repo//'.git'
        ! What the server would refuse is found before anything is sent.
        call land_push_check(remote, source, large)
        if (len_trim(large) > 0) then
            call proc_setenv('GIT_CONFIG_COUNT', '0')
            write (error_unit, '(a)') 'fy: not pushed, files over the size limit (allow them in '// &
                '.fortyard/allow-large):', trim(large)
            call quit(1)
        end if
        call execute_command_line('git push --quiet --force "'//remote//'" '//source// &
            ':refs/heads/'//branch, exitstat=stat)
        call proc_setenv('GIT_CONFIG_COUNT', '0')
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy: pushing '//source//' to '//repo//' '//branch//' failed'
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

    subroutine cmd_alerts()
        !! fy alerts [--json]: the alerts that fire now; exits 1 when one does,
        !! so a monitor can run it.
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'GET', '/api/v1/alerts', cfg%token, '', code, body, stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
        else if (index(body, '"rule":') == 0) then
            write (output_unit, '(a)') 'No alerts.'
        else
            write (output_unit, '(a)', advance='no') cli_table('{"alerts":'//body//'}', &
                'alerts', [character(len=6) :: 'rule', 'repo', 'since', 'detail'])
        end if
        if (index(body, '"rule":') > 0) call quit(1)
    end subroutine cmd_alerts

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

    subroutine cmd_backends()
        !! fy backends [--json]: each runner class and the run time estimates
        !! per repository (GET /api/v1/backends).
        type(config_t) :: cfg
        character(len=:), allocatable :: body
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        call client_request(cfg%server, 'GET', '/api/v1/backends', cfg%token, '', code, body, &
            stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
            return
        end if
        write (output_unit, '(a)', advance='no') cli_table(body, 'classes', &
            [character(len=11) :: 'class', 'runners', 'running', 'queued', 'max_running', &
            'max_minutes', 'wait'])
        write (output_unit, '(a)') ''
        write (output_unit, '(a)', advance='no') cli_table(body, 'estimates', &
            [character(len=7) :: 'class', 'repo', 'run', 'checks'])
    end subroutine cmd_backends

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
        !! fy jobs [REPO] [--kind KIND] [--state STATE]: running, waiting and
        !! recent jobs, under a line naming the repositories that hold runners and
        !! how many.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, path, held, rest, name, kind, only
        integer :: code, stat, s, e

        kind = option('--kind', '')
        only = option('--state', '')
        ! Words, so that neither can add a parameter to the query.
        if (verify(kind//only, 'abcdefghijklmnopqrstuvwxyz') /= 0) &
            error stop 'fy: --kind and --state take one word'
        call config_load(cfg)
        call require_login(cfg)
        path = '/api/v1/jobs?repo='//positional(1)//'&kind='//kind//'&state='//only
        call client_request(cfg%server, 'GET', path, cfg%token, '', code, body, stat)
        if (stat /= 0 .or. code /= 200 .or. flag('--json')) then
            call report(code, body, stat)
            return
        end if
        held = ''
        rest = body(1:max(0, index(body, '"totals":') - 1))
        do
            s = index(rest, '{"name":"')
            if (s == 0) exit
            rest = rest(s + 9:)
            name = rest(1:index(rest, '"') - 1)
            s = index(rest, '"checks":')
            if (s == 0) exit
            e = verify(rest(s + 9:), '0123456789')
            if (e > 1) then
                if (rest(s + 9:s + 8 + e - 1) /= '0') then
                    if (len(held) > 0) held = held//', '
                    held = held//name//' '//rest(s + 9:s + 8 + e - 1)
                end if
            end if
        end do
        if (len(held) > 0) write (output_unit, '(a)') 'runners held: '//held
        write (output_unit, '(a)', advance='no') cli_table(body, 'jobs', &
            [character(len=8) :: 'id', 'kind', 'repo', 'state', 'started', 'branch', 'change', &
            'runner', 'title'])
    end subroutine cmd_jobs

    subroutine cmd_job()
        !! fy job ID [--wait|--follow]: a job's state and log.
        type(config_t) :: cfg
        character(len=:), allocatable :: body, id
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        id = positional(1)
        if (len(id) == 0) then
            write (error_unit, '(a)') 'fy: usage: fy job ID [--wait|--follow]'
            call quit(2)
        end if
        if (flag('--follow')) then
            call follow_log(cfg, id)
            return
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
            cli_field(body, 'state'), cli_job(body)//cli_field(body, 'log')
    end subroutine cmd_job

    subroutine cmd_push()
        !! fy push [BRANCH] [--repo NAME]: pushes the checkout's branch BRANCH, or
        !! HEAD when there is no branch of that name, to BRANCH (default: the
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
        character(len=:), allocatable :: what, repo, n, body, path, payload, text, state, &
            review, attention
        integer :: code, stat

        call config_load(cfg)
        call require_login(cfg)
        what = positional(1)
        n = positional(2)
        repo = option('--repo', '')
        ! The changes of every repository need no checkout.
        if (len(repo) == 0 .and. .not. (what == 'list' .and. flag('--all-repos'))) &
            repo = remote_repo(cfg%server)
        path = '/api/v1/repos/'//repo//'/changes'
        if (what /= 'list' .and. (len(n) == 0 .or. verify(n, '0123456789') /= 0)) then
            write (error_unit, '(a)') 'fy: usage: fy change '//what//' NUMBER'
            call quit(2)
        end if
        select case (what)
        case ('list')
            if (len(n) > 0) then
                write (error_unit, '(a)') 'fy: change list takes no argument '//n// &
                    '; another repository is --repo NAME'
                call quit(2)
            end if
            state = option('--state', 'open')
            if (flag('--all')) state = 'all'
            if (flag('--flagged')) state = 'flagged'
            review = option('--review', '')
            attention = option('--attention', '')
            if (verify(state//review//attention, 'abcdefghijklmnopqrstuvwxyz-') /= 0) then
                write (error_unit, '(a)') 'fy: usage: fy change list [--state open|landed|'// &
                    'closed|all] [--review flagged|unreviewed] [--attention needs-author]'// &
                    ' [--all-repos]'
                call quit(2)
            end if
            if (flag('--all-repos')) path = '/api/v1/changes'
            path = path//'?state='//state
            if (len(review) > 0) path = path//'&review='//review
            if (len(attention) > 0) path = path//'&attention='//attention
            call client_request(cfg%server, 'GET', path, cfg%token, '', code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) call report(code, body, stat)
            if (stat /= 0 .or. code /= 200 .or. flag('--json')) return
            if (state == 'flagged' .and. flag('--all-repos')) then
                write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                    [character(len=11) :: 'repo', 'number', 'state', 'title', 'flag_reason'])
            else if (state == 'flagged') then
                write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                    [character(len=11) :: 'number', 'state', 'title', 'flag_reason'])
            else if (flag('--all-repos')) then
                write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                    [character(len=11) :: 'repo', 'number', 'status', 'review', 'flag_reason', &
                    'author', 'title'])
            else if (len(review) > 0 .or. len(attention) > 0 .or. option('--state', '') /= '') then
                ! The words of the changes page.
                write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                    [character(len=11) :: 'number', 'status', 'review', 'flag_reason', 'author', &
                    'title'])
            else
                write (output_unit, '(a)', advance='no') cli_table(body, 'changes', &
                    [character(len=8) :: 'number', 'state', 'author', 'versions', 'branch', &
                    'title'])
            end if
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
                case ('--approve-execution', '--follow', '--no-push', '--no-wait', &
                        '--wait', '--json', '--init', &
                        '--all', '--all-repos', '--stats', '--urgent')
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
        case ('public')
            if (len(arg(3)) == 0 .or. (arg(4) /= 'on' .and. arg(4) /= 'off')) then
                write (error_unit, '(a)') 'fy: usage: fy repo public NAME on|off'
                call quit(2)
            end if
            call require_login(cfg)
            call client_request(cfg%server, 'POST', '/api/v1/repos/'//arg(3)//'/public', &
                cfg%token, '{"public":'//trim(merge('true ', 'false', arg(4) == 'on'))//'}', code, &
                body, stat)
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
                ' mode NAME MODE | numbers NAME N | policy NAME [KEY=VALUE ...] | public NAME on|off'// &
                ' | list'
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
