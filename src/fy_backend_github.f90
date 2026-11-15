module fy_backend_github
    !! The GitHub backend (fy backend github): takes checks of the classes
    !! github-linux, github-macos-arm64 and github-windows. For each it asks
    !! the server which GitHub repository (the upstream) the check would go
    !! to, reads .github/workflows of the check's commit from the server and
    !! gives the check back (error runner) unless fortyard.yml is the shipped
    !! file and every other workflow there is disabled on GitHub, asks the
    !! server to publish the commit as branch fortyard/check/<id>, finds the
    !! run of that workflow for the branch, follows it with conditional
    !! requests (cancelling it when the check is withdrawn on the server),
    !! copies the job logs, reports the result with the job's start (which
    !! deletes the branch) and deletes the run. It holds a GitHub App key and
    !! signs the App's JWT with openssl; every request uses an installation
    !! token restricted to one repository. A sweep every ten minutes deletes
    !! finished runs of fortyard/check/ branches older than an hour and
    !! cancels runs of checks that no longer run here. fy backend github
    !! setup REPO prepares a repository.
    use, intrinsic :: iso_fortran_env, only: &
        int64, &
        output_unit
    use fy_backend, only: &
        backend_git_file, &
        backend_http, &
        backend_main, &
        backend_report, &
        backend_seconds, &
        backend_time, &
        json_raw
    use fy_client, only: &
        client_request
    use fy_proc, only: &
        proc_run, &
        proc_sleep_ms
    implicit none
    private

    public :: &
        github_configure, &
        github_main, &
        github_setup, &
        github_token_file, &
        github_workflow

    character(len=*), parameter :: WORKFLOW_PATH = '.github/workflows/fortyard.yml'
    !! The actions the shipped workflow uses, pinned by commit (v7.0.1, v6.1.0).
    character(len=*), parameter :: CHECKOUT = 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
    character(len=*), parameter :: CACHE = 'actions/cache@55cc8345863c7cc4c66a329aec7e433d2d1c52a9'

    character(len=:), allocatable :: api, owner, app_id, key_file, fixed_token, token_path
    character(len=:), allocatable :: installation, sweep_server, sweep_token
    integer :: poll_ms = 5000, start_seconds = 600

contains

    subroutine github_configure(app, key, org, url, token, poll, start)
        !! The App (id and private key file), the owner of the repositories,
        !! the API's address; `token`, when not empty, is used instead of the
        !! App's installation tokens (setup with an owner's token). `poll`:
        !! seconds between requests about a run; `start`: seconds a run may
        !! take to start before its check is given back.
        character(len=*), intent(in) :: app, key, org, url, token
        integer, intent(in) :: poll, start

        app_id = app
        key_file = key
        owner = org
        api = url
        if (len(api) > 0) then
            if (api(len(api):) == '/') api = api(1:len(api) - 1)
        end if
        fixed_token = token
        installation = ''
        poll_ms = 1000*max(1, poll)
        start_seconds = max(1, start)
    end subroutine github_configure

    subroutine github_main(server, token, classes, max_running, max_macos, max_minutes)
        !! Runs the backend for the comma-separated `classes` until stopped.
        character(len=*), intent(in) :: server, token, classes
        integer, intent(in) :: max_running, max_macos, max_minutes
        character(len=64), allocatable :: names(:)
        integer, allocatable :: limits(:)
        character(len=:), allocatable :: rest
        integer :: e

        sweep_server = server
        sweep_token = token
        allocate (names(0), limits(0))
        rest = classes
        do while (len(rest) > 0)
            e = index(rest, ',')
            if (e == 0) e = len(rest) + 1
            if (e > 1) then
                names = [names, [character(len=64) :: rest(1:e - 1)]]
                if (index(rest(1:e - 1), 'macos') > 0) then
                    limits = [limits, max_macos]
                else
                    limits = [limits, max_running]
                end if
            end if
            rest = rest(min(e + 1, len(rest) + 1):)
        end do
        call backend_main(server, token, names, limits, max_running, max_minutes, github_take, &
            github_sweep, poll_ms, 600)
    end subroutine github_main

    function jwt() result(s)
        !! The App's JSON Web Token, signed RS256 with openssl, valid for ten
        !! minutes from a minute ago (GitHub allows for clock drift so).
        character(len=:), allocatable :: s
        integer :: stat

        call proc_run([character(len=1024) :: 'sh', '-c', &
            'b64() { openssl base64 -A | tr "+/" "-_" | tr -d "="; }; now=$(date +%s); '// &
            'h=$(printf %s ''{"alg":"RS256","typ":"JWT"}'' | b64); '// &
            'p=$(printf ''{"iat":%d,"exp":%d,"iss":"%s"}'' $((now - 60)) $((now + 540)) "$1" | b64);'// &
            ' sig=$(printf %s "$h.$p" | openssl dgst -sha256 -binary -sign "$2" | b64) && '// &
            '[ -n "$sig" ] && printf %s "$h.$p.$sig"', 'jwt', app_id, key_file], s, stat, 65536)
        if (stat /= 0) s = ''
    end function jwt

    subroutine gh(method, path, auth, payload, code, body, etag, raw)
        !! One request to the GitHub API with the bearer `auth`. `etag`: on
        !! entry the ETag of the answer already held (sent as If-None-Match,
        !! so an unchanged one comes back as 304), on return the new one.
        !! `raw` asks for a file's bytes instead of JSON.
        character(len=*), intent(in) :: method, path, auth, payload
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable, intent(inout), optional :: etag
        logical, intent(in), optional :: raw
        character(len=:), allocatable :: headers, tag
        integer :: stat

        headers = 'Accept: application/vnd.github+json'
        if (present(raw)) then
            if (raw) headers = 'Accept: application/vnd.github.raw'
        end if
        headers = headers//new_line('a')//'X-GitHub-Api-Version: 2022-11-28'//new_line('a')// &
            'User-Agent: fortyard'//new_line('a')//'Authorization: Bearer '//auth
        if (present(etag)) then
            if (allocated(etag)) then
                if (len(etag) > 0) headers = headers//new_line('a')//'If-None-Match: '//etag
            end if
        end if
        call backend_http(method, api//path, headers, payload, code, body, stat, tag)
        if (stat /= 0) code = 0
        if (present(etag) .and. code == 200) etag = tag
    end subroutine gh

    function installation_id() result(id)
        !! The App's installation for the owner (an organisation, else a user).
        character(len=:), allocatable :: id
        character(len=:), allocatable :: body, token
        integer :: code

        id = installation
        if (len(id) > 0) return
        token = jwt()
        if (len(token) == 0) return
        call gh('GET', '/orgs/'//owner//'/installation', token, '', code, body)
        if (code /= 200) call gh('GET', '/users/'//owner//'/installation', token, '', code, body)
        if (code == 200) installation = json_raw(body, 'id')
        id = installation
    end function installation_id

    subroutine github_token_file(path)
        !! A file with a fine-grained token (Actions read and write), read again
        !! for every request, so a renewed token needs no restart. Used instead
        !! of the App's installation tokens.
        character(len=*), intent(in) :: path

        token_path = path
    end subroutine github_token_file

    logical function fixed()
        !! A fixed token (file or setup's token) stands in for the App.
        fixed = .false.
        if (allocated(token_path)) fixed = len(token_path) > 0
        if (allocated(fixed_token)) fixed = fixed .or. len(fixed_token) > 0
    end function fixed

    function repo_token(repo) result(token)
        !! An installation token for the one repository `repo`, or with an
        !! empty `repo` one that only reads metadata (to list the installed
        !! repositories); empty when none was given.
        character(len=*), intent(in) :: repo
        character(len=:), allocatable :: token
        character(len=:), allocatable :: id, body, signed
        integer :: code

        token = ''
        if (allocated(token_path)) then
            if (len(token_path) > 0) then
                token = first_line_of(token_path)
                return
            end if
        end if
        if (allocated(fixed_token)) then
            if (len(fixed_token) > 0) then
                token = fixed_token
                return
            end if
        end if
        id = installation_id()
        if (len(id) == 0) return
        signed = jwt()
        if (len(repo) > 0) then
            call gh('POST', '/app/installations/'//id//'/access_tokens', signed, &
                '{"repositories":["'//repo//'"]}', code, body)
        else
            call gh('POST', '/app/installations/'//id//'/access_tokens', signed, &
                '{"permissions":{"metadata":"read"}}', code, body)
        end if
        if (code == 201 .or. code == 200) token = json_raw(body, 'token')
    end function repo_token

    subroutine github_take(server, token, class, id, repo, ref, kind, timeout)
        !! One check on GitHub (backend_take_i).
        character(len=*), intent(in) :: server, token, class, id, repo, ref, kind
        integer, intent(in) :: timeout
        character(len=:), allocatable :: content, body, branch, commit, gtoken, rid, etag, run
        character(len=:), allocatable :: status, started, outcome, log, path, note, full, name
        character(len=:), allocatable :: listing, conclusion, why, wpath
        integer(int64) :: t0, token_at, began
        integer :: code, stat, k, limit, polls

        ! GitHub ends the job at the workflow's timeout-minutes, 360.
        limit = 21600
        if (timeout > 0) limit = min(timeout, limit)
        polls = 0
        ! Where the server publishes: the upstream's OWNER/NAME.
        call client_request(server, 'GET', '/api/v1/checks/'//id//'/publish', token, '', code, &
            body, stat)
        full = json_raw(body, 'repository')
        if (stat /= 0 .or. code /= 200 .or. index(full, '/') < 2) then
            call give_back('The GitHub backend cannot publish this '//kind//': '//body)
            return
        end if
        name = full(index(full, '/') + 1:)
        if (lower(full(1:index(full, '/') - 1)) /= lower(owner)) then
            call give_back('The upstream '//full//' is not in '//owner//', the GitHub owner '// &
                'this backend runs checks for; the check returns to its home class.')
            return
        end if
        gtoken = repo_token(name)
        token_at = backend_seconds()
        if (len(gtoken) == 0) then
            call give_back('The GitHub backend got no installation token for '//full// &
                '; is the App installed there?')
            return
        end if
        call backend_git_file(server, token, repo, ref, WORKFLOW_PATH, content, stat, listing)
        if (stat /= 0) content = ''
        if (content /= github_workflow()) then
            call give_back('The GitHub backend gives this '//kind//' back to its home class: '// &
                WORKFLOW_PATH//' of the commit is not the workflow fortyard ships '// &
                '(deploy/github/fortyard.yml).')
            return
        end if
        ! GitHub runs every workflow of the pushed commit, and one may raise its
        ! token's rights: nothing but the shipped one may be active.
        why = foreign_workflows()
        if (len(why) > 0) then
            call give_back('The GitHub backend gives this '//kind//' back to its home class: '// &
                why)
            return
        end if
        call client_request(server, 'POST', '/api/v1/checks/'//id//'/publish', token, &
            '{"class":"'//class//'"}', code, body, stat)
        branch = json_raw(body, 'branch')
        commit = json_raw(body, 'commit')
        if (stat /= 0 .or. code /= 200 .or. len(branch) == 0 .or. len(commit) == 0) then
            call give_back('The GitHub backend could not publish the commit: '//body)
            return
        end if
        t0 = backend_seconds()
        ! The run of the shipped workflow for the branch and commit.
        rid = ''
        etag = ''
        path = '/repos/'//full//'/actions/workflows/fortyard.yml/runs?event=push'// &
            '&branch='//encoded(branch)//'&head_sha='//commit
        do
            call fresh_token()
            call gh('GET', path, gtoken, '', code, body, etag)
            if (code == 200) then
                do k = 1, 100
                    if (len(json_raw(body, 'workflow_runs['//itoa(k)//'].id')) == 0) exit
                    ! GitHub may give the path with @ and the ref.
                    wpath = json_raw(body, 'workflow_runs['//itoa(k)//'].path')
                    if (wpath /= WORKFLOW_PATH .and. index(wpath, WORKFLOW_PATH//'@') /= 1) cycle
                    if (json_raw(body, 'workflow_runs['//itoa(k)//'].head_sha') /= commit) cycle
                    rid = json_raw(body, 'workflow_runs['//itoa(k)//'].id')
                    exit
                end do
            end if
            if (len(rid) > 0) exit
            ! A run that appears later is cancelled by the sweep.
            if (withdrawn()) return
            if (backend_seconds() - t0 > start_seconds) then
                call give_back('No run of '//WORKFLOW_PATH//' for '//branch//' appeared within '// &
                    itoa(start_seconds)//' s; the check returns to its home class.')
                return
            end if
            call proc_sleep_ms(poll_ms)
        end do
        ! Followed until it completes; an unchanged run costs a 304.
        run = '/repos/'//full//'/actions/runs/'//rid
        etag = ''
        content = ''
        started = ''
        note = ''
        began = 0
        do
            call fresh_token()
            call gh('GET', run, gtoken, '', code, body, etag)
            if (code == 200) content = body
            ! Deleted by someone else: no verdict.
            if (code == 404) exit
            status = json_raw(content, 'status')
            if (status == 'completed') exit
            if (withdrawn()) then
                call cancel()
                call gh('DELETE', run, gtoken, '', code, body)
                return
            end if
            if (len(started) == 0 .and. len(status) > 0 .and. status /= 'queued' .and. &
                status /= 'waiting' .and. status /= 'pending' .and. status /= 'requested') then
                started = job_start()
                if (len(started) > 0) began = backend_seconds()
            end if
            if (len(started) == 0 .and. backend_seconds() - t0 > start_seconds .and. &
                len(note) == 0) then
                note = 'The job did not start within '//itoa(start_seconds)//' s; the run was '// &
                    'cancelled and the check returns to its home class.'
                call cancel()
            else if (len(started) > 0 .and. len(note) == 0) then
                if (backend_seconds() - began > limit) then
                    note = 'The time limit ('//itoa(limit/60)//' min) ended the check: the '// &
                        'run was cancelled. A time limit is no verdict on the change.'
                    call cancel()
                end if
            end if
            if (len(note) > 0 .and. backend_seconds() - t0 > start_seconds + 3*60 + limit) exit
            call proc_sleep_ms(poll_ms)
        end do
        if (len(started) == 0) started = job_start()
        conclusion = json_raw(content, 'conclusion')
        select case (conclusion)
        case ('success')
            outcome = 'passed'
        case ('failure')
            outcome = 'failed'
        case default
            outcome = 'runner'
            ! GitHub ends a job at timeout-minutes as timed_out or cancelled.
            if (conclusion == 'timed_out' .or. (conclusion == 'cancelled' .and. began > 0 .and. &
                backend_seconds() - began >= limit - 300)) then
                if (len(note) == 0) note = 'GitHub ended the job at the time limit ('// &
                    itoa(limit/60)//' min). A time limit is no verdict on the change.'
            else if (len(note) == 0) then
                note = 'The run ended without a verdict ('//conclusion//'); the check is '// &
                    'queued again.'
            end if
        end select
        if (index(note, 'time limit') > 0) outcome = 'timeout'
        if (outcome /= 'passed' .and. outcome /= 'failed') started = ''
        log = job_logs()
        if (len(note) > 0) log = log//note//new_line('a')
        log = log//'Ran on GitHub ('//class//'): '//api_web()//'/'//full//'/actions/runs/'// &
            rid//new_line('a')
        ! The result deletes the branch on the server's side.
        call backend_report(server, token, id, outcome, log, started)
        call gh('DELETE', run, gtoken, '', code, body)

    contains

        subroutine give_back(why)
            character(len=*), intent(in) :: why
            call backend_report(server, token, id, 'runner', why, '')
        end subroutine give_back

        logical function withdrawn()
            !! Whether the check no longer runs here (cancelled on the server,
            !! say), asked every sixth poll.
            character(len=:), allocatable :: answer
            integer :: c, s
            withdrawn = .false.
            polls = polls + 1
            if (mod(polls, 6) /= 0) return
            call client_request(server, 'GET', '/api/v1/checks/'//id//'/publish', token, '', c, &
                answer, s)
            if (s /= 0) return
            withdrawn = c == 403 .or. c == 404 .or. (c == 200 .and. &
                json_raw(answer, 'state') /= 'running')
        end function withdrawn

        function foreign_workflows() result(text)
            !! Why the commit may not be pushed: empty unless a file of its
            !! .github/workflows (`listing`) other than fortyard.yml is new or
            !! differs from main's on GitHub. Workflows of main are the
            !! repository's own; one a change adds or changes would run its
            !! code on GitHub outside fortyard.yml.
            character(len=:), allocatable :: text
            character(len=:), allocatable :: rest, file, mine, theirs
            integer :: c, e, st

            text = ''
            rest = listing
            do while (len(rest) > 0)
                e = index(rest, new_line('a'))
                if (e == 0) e = len(rest) + 1
                file = rest(1:e - 1)
                rest = rest(min(e + 1, len(rest) + 1):)
                if (len(file) == 0 .or. file == 'fortyard.yml') cycle
                call backend_git_file(server, token, repo, ref, '.github/workflows/'//file, mine, st)
                if (st /= 0) then
                    text = '.github/workflows/'//file//' of the check cannot be read.'
                    return
                end if
                call gh('GET', '/repos/'//full//'/contents/.github/workflows/'//file//'?ref=main', &
                    gtoken, '', c, theirs, raw=.true.)
                if (c == 200 .and. theirs == mine) cycle
                if (c /= 200 .and. c /= 404) then
                    text = '.github/workflows/'//file//' of main cannot be read (HTTP '//itoa(c)//').'
                    return
                end if
                text = '.github/workflows/'//file//' is new or changed in this check and would '// &
                    'run on GitHub as well; fortyard runs there only checks that leave the '// &
                    'workflows of main as they are.'
                return
            end do
        end function foreign_workflows

        subroutine fresh_token()
            !! Installation tokens hold an hour; a new one every half hour.
            character(len=:), allocatable :: t
            if (backend_seconds() - token_at < 1800) return
            t = repo_token(name)
            if (len(t) > 0) gtoken = t
            token_at = backend_seconds()
        end subroutine fresh_token

        subroutine cancel()
            character(len=:), allocatable :: answer
            integer :: c
            call gh('POST', run//'/cancel', gtoken, '', c, answer)
        end subroutine cancel

        function job_start() result(at)
            !! When the first job of the run started; empty before.
            character(len=:), allocatable :: at
            character(len=:), allocatable :: jobs, s, st
            integer :: c, j

            at = ''
            call gh('GET', run//'/jobs', gtoken, '', c, jobs)
            if (c /= 200) return
            do j = 1, 100
                st = json_raw(jobs, 'jobs['//itoa(j)//'].status')
                if (len(st) == 0) exit
                if (st == 'queued' .or. st == 'waiting' .or. st == 'pending') cycle
                s = json_raw(jobs, 'jobs['//itoa(j)//'].started_at')
                if (len(s) == 0 .or. s == 'null') cycle
                if (len(at) == 0 .or. s < at) at = s
            end do
        end function job_start

        function job_logs() result(text)
            !! The logs of the run's jobs, one after the other.
            character(len=:), allocatable :: text
            character(len=:), allocatable :: jobs, jid, part
            integer :: c, j

            text = ''
            call gh('GET', run//'/jobs', gtoken, '', c, jobs)
            if (c /= 200) return
            do j = 1, 100
                jid = json_raw(jobs, 'jobs['//itoa(j)//'].id')
                if (len(jid) == 0) exit
                call gh('GET', '/repos/'//full//'/actions/jobs/'//jid//'/logs', &
                    gtoken, '', c, part)
                text = text//'--- GitHub job '//json_raw(jobs, 'jobs['//itoa(j)//'].name')// &
                    ' ---'//new_line('a')
                if (c == 200) then
                    text = text//part
                    if (len(part) > 0) then
                        if (part(len(part):) /= new_line('a')) text = text//new_line('a')
                    end if
                else
                    text = text//'(no log)'//new_line('a')
                end if
            end do
        end function job_logs

    end subroutine github_take

    subroutine github_sweep()
        !! In every repository the App is installed for: deletes the finished
        !! runs of fortyard/check/ branches that ended more than an hour ago,
        !! and cancels the runs still going of checks that do not run here
        !! (ended, queued again, cancelled, or a run GitHub started only after
        !! its check was given back). Runs followed by a child are left alone.
        character(len=:), allocatable :: listing, cutoff, body, token, rid, answer
        character(len=:), allocatable :: head, base, num
        character(len=128), allocatable :: repos(:)
        integer :: code, k, j, q, stat

        token = repo_token('')
        if (len(token) == 0) return
        allocate (repos(0))
        if (fixed()) then
            ! A fine-grained token lists the owner's public repositories.
            call gh('GET', '/orgs/'//owner//'/repos?type=public&per_page=100', token, '', code, &
                listing)
            if (code /= 200) return
            do k = 1, 100
                if (len(json_raw(listing, '['//itoa(k)//'].name')) == 0) exit
                repos = [repos, [character(len=128) :: json_raw(listing, '['//itoa(k)//'].name')]]
            end do
        else
            call gh('GET', '/installation/repositories?per_page=100', token, '', code, listing)
            if (code /= 200) return
            do k = 1, 100
                if (len(json_raw(listing, 'repositories['//itoa(k)//'].name')) == 0) exit
                repos = [repos, [character(len=128) :: json_raw(listing, 'repositories['// &
                    itoa(k)//'].name')]]
            end do
        end if
        cutoff = backend_time(-3600)
        if (len(cutoff) /= 20) return
        do k = 1, size(repos)
            token = repo_token(trim(repos(k)))
            if (len(token) == 0) cycle
            base = '/repos/'//owner//'/'//trim(repos(k))//'/actions/runs'
            call gh('GET', base//'?per_page=100&status=completed&created=%3C'//cutoff, token, '', &
                code, body)
            if (code == 200) then
                do j = 1, 100
                    rid = json_raw(body, 'workflow_runs['//itoa(j)//'].id')
                    if (len(rid) == 0) exit
                    head = json_raw(body, 'workflow_runs['//itoa(j)//'].head_branch')
                    if (index(head, 'fortyard/check/') /= 1) cycle
                    if (json_raw(body, 'workflow_runs['//itoa(j)//'].status') /= 'completed') cycle
                    if (json_raw(body, 'workflow_runs['//itoa(j)//'].updated_at') >= cutoff) cycle
                    call gh('DELETE', base//'/'//rid, token, '', code, listing)
                end do
            end if
            do q = 1, 2
                call gh('GET', base//'?per_page=100&status='//trim(merge('in_progress', &
                    'queued     ', q == 1)), token, '', code, body)
                if (code /= 200) cycle
                do j = 1, 100
                    rid = json_raw(body, 'workflow_runs['//itoa(j)//'].id')
                    if (len(rid) == 0) exit
                    head = json_raw(body, 'workflow_runs['//itoa(j)//'].head_branch')
                    if (index(head, 'fortyard/check/') /= 1) cycle
                    if (json_raw(body, 'workflow_runs['//itoa(j)//'].status') == 'completed') cycle
                    num = head(16:)
                    if (index(num, '-') > 0) num = num(1:index(num, '-') - 1)
                    if (len(num) == 0 .or. verify(num, '0123456789') /= 0) cycle
                    call client_request(sweep_server, 'GET', '/api/v1/checks/'//num//'/publish', &
                        sweep_token, '', code, answer, stat)
                    if (stat /= 0 .or. code >= 500) cycle
                    if (code == 200 .and. json_raw(answer, 'state') == 'running') cycle
                    call gh('POST', base//'/'//rid//'/cancel', token, '', code, listing)
                end do
            end do
        end do
    end subroutine github_sweep

    subroutine github_setup(repo, stat)
        !! fy backend github setup REPO: refuses a repository that is private or
        !! whose main lacks the shipped workflow; else enables Actions. The
        !! repository's own workflows, actions and permissions stay as they
        !! are: a check cannot add or change a workflow (github_take), so its
        !! code runs only in fortyard.yml, without secrets; secrets available
        !! to the repository's workflows are only reported. Says what it found
        !! and did; stat 0 when the repository is ready.
        character(len=*), intent(in) :: repo
        integer, intent(out) :: stat
        character(len=:), allocatable :: token, base, body, envs, name, wid
        integer :: code, k

        stat = 1
        token = repo_token(repo)
        if (len(token) == 0) then
            call say('no token for '//owner//'/'//repo//': is the App installed there?')
            return
        end if
        base = '/repos/'//owner//'/'//repo
        call gh('GET', base, token, '', code, body)
        if (code /= 200) then
            call say(owner//'/'//repo//' cannot be read (HTTP '//itoa(code)//')')
            return
        end if
        if (json_raw(body, 'private') /= 'false') then
            call say(owner//'/'//repo//' is not public; the GitHub backend runs public '// &
                'repositories only')
            return
        end if
        call count_secrets(base//'/actions/secrets', 'repository secrets')
        call count_secrets(base//'/actions/organization-secrets', 'organisation secrets')
        call gh('GET', base//'/contents/'//WORKFLOW_PATH//'?ref=main', token, '', code, body, &
            raw=.true.)
        if (code /= 200 .or. body /= github_workflow()) then
            call say(WORKFLOW_PATH//' on main is not deploy/github/fortyard.yml of this '// &
                'fortyard; land that file first')
            return
        end if
        call say('public, the shipped workflow on main')
        if (.not. put(base//'/actions/permissions', '{"enabled":true,"allowed_actions":"all"}', &
            'Actions enabled')) return
        call say(owner//'/'//repo//' is ready for the GitHub backend')
        stat = 0

    contains

        subroutine count_secrets(path, what)
            !! Reports the secrets at `path`; a check's code never sees them.
            character(len=*), intent(in) :: path, what
            character(len=:), allocatable :: answer
            integer :: c
            call gh('GET', path, token, '', c, answer)
            if (c == 200 .and. json_raw(answer, 'total_count') /= '0') call say(json_raw(answer, &
                'total_count')//' '//what//' are available to the repository''s own workflows')
        end subroutine count_secrets

        logical function put(path, payload, what)
            character(len=*), intent(in) :: path, payload, what
            character(len=:), allocatable :: answer
            integer :: c
            call gh('PUT', path, token, payload, c, answer)
            put = c == 204 .or. c == 200
            if (put) then
                call say(what)
            else
                call say('failed (HTTP '//itoa(c)//'): '//what//' '//answer)
            end if
        end function put

        subroutine say(text)
            character(len=*), intent(in) :: text
            write (output_unit, '(a)') 'fy backend github setup: '//text
            flush (output_unit)
        end subroutine say

    end subroutine github_setup

    function first_line_of(path) result(line)
        !! The first line of the file at `path` without blanks; '' when unreadable.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: line
        character(len=4096) :: buf
        integer :: u, ios

        line = ''
        open (newunit=u, file=path, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) buf
        close (u)
        if (ios == 0) line = trim(adjustl(buf))
    end function first_line_of

    function encoded(text) result(s)
        !! `text` for a URL's path or query: only a-z, A-Z, 0-9 and -._~ as
        !! they are.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s
        character(len=3) :: hex
        integer :: i

        s = ''
        do i = 1, len(text)
            if (index('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~', &
                text(i:i)) > 0) then
                s = s//text(i:i)
            else
                write (hex, '(a1,z2.2)') '%', iachar(text(i:i))
                s = s//hex
            end if
        end do
    end function encoded

    function api_web() result(s)
        !! The web address that belongs to the API's.
        character(len=:), allocatable :: s
        s = api
        if (s == 'https://api.github.com') s = 'https://github.com'
    end function api_web

    function lower(text) result(s)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: s
        integer :: i
        s = text
        do i = 1, len(s)
            if (s(i:i) >= 'A' .and. s(i:i) <= 'Z') s(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function lower

    function itoa(v) result(s)
        integer, intent(in) :: v
        character(len=:), allocatable :: s
        character(len=24) :: b
        write (b, '(i0)') v
        s = trim(b)
    end function itoa

    function github_workflow() result(text)
        !! deploy/github/fortyard.yml as shipped; test_backend_github checks that
        !! the two are the same.
        character(len=:), allocatable :: text
        character(len=1), parameter :: NL = achar(10)

        text = ""
        text = text//'# fortyard checks on GitHub''s hosted runners. fortyard push'// &
            'es a check''s commit'//NL
        text = text//'# as branch fortyard/check/<id>, with -macos or -windows for'// &
            ' those machines and'//NL
        text = text//'# -verify for a verification of main, reads the result back '// &
            'and deletes run'//NL
        text = text//'# and branch. This file must equal deploy/github/fortyard.ym'// &
            'l of fortyard:'//NL
        text = text//'# fortyard runs no check whose copy differs.'//NL
        text = text//'name: fortyard'//NL
        text = text//'on:'//NL
        text = text//'  push:'//NL
        text = text//'    branches: [''fortyard/check/**'']'//NL
        text = text//'permissions: {}'//NL
        text = text//'jobs:'//NL
        text = text//'  check:'//NL
        text = text//'    runs-on: ${{ contains(github.ref_name, ''-macos'') && '''// &
            'macos-latest'' || (contains(github.ref_name, ''-windows'') &'// &
            '& ''windows-latest'' || ''ubuntu-latest'') }}'//NL
        text = text//'    timeout-minutes: 360'//NL
        text = text//'    env:'//NL
        text = text//'      FPM_VERSION: 0.13.0'//NL
        text = text//'      GIT_TERMINAL_PROMPT: 0'//NL
        text = text//'    defaults:'//NL
        text = text//'      run:'//NL
        text = text//'        shell: bash'//NL
        text = text//'    steps:'//NL
        text = text//'      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c1'// &
            '81273ba90b1 # v7.0.1'//NL
        text = text//'        with:'//NL
        text = text//'          fetch-depth: 1'//NL
        text = text//'          persist-credentials: false'//NL
        text = text//'      - name: Places'//NL
        text = text//'        run: |'//NL
        text = text//'          # fo''s main, which fortyard keeps building with i'// &
            'ts dependencies'''//NL
        text = text//'          # mains; a release falls behind them.'//NL
        text = text//'          fo=$(git ls-remote https://github.com/lazy-fortran'// &
            '/fo.git refs/heads/main | cut -f1)'//NL
        text = text//'          test -n "$fo"'//NL
        text = text//'          echo "FO_COMMIT=$fo" >> "$GITHUB_ENV"'//NL
        text = text//'          echo "FY_TOOLS=$RUNNER_TOOL_CACHE/fortyard-fo-$fo"'// &
            ' >> "$GITHUB_ENV"'//NL
        text = text//'          echo "FO_CACHE_DIR=$RUNNER_TEMP/fo-cache" >> "$GIT'// &
            'HUB_ENV"'//NL
        text = text//'          echo "CCACHE_DIR=$RUNNER_TEMP/ccache" >> "$GITHUB_'// &
            'ENV"'//NL
        text = text//'          case "$GITHUB_REF_NAME" in'//NL
        text = text//'            *-macos*) class=github-macos-arm64 ;;'//NL
        text = text//'            *-windows*) class=github-windows ;;'//NL
        text = text//'            *) class=github-linux ;;'//NL
        text = text//'          esac'//NL
        text = text//'          echo "FORTYARD_RUNNER_CLASS=$class" >> "$GITHUB_EN'// &
            'V"'//NL
        text = text//'      - name: Toolchain cache'//NL
        text = text//'        uses: actions/cache@55cc8345863c7cc4c66a329aec7e433d'// &
            '2d1c52a9 # v6.1.0'//NL
        text = text//'        with:'//NL
        text = text//'          path: ${{ env.FY_TOOLS }}'//NL
        text = text//'          key: fortyard-tools-${{ runner.os }}-${{ runner.ar'// &
            'ch }}-fo-${{ env.FO_COMMIT }}-fpm-${{ env.FPM_VERSION }}'//NL
        text = text//'      - name: Build cache'//NL
        text = text//'        uses: actions/cache@55cc8345863c7cc4c66a329aec7e433d'// &
            '2d1c52a9 # v6.1.0'//NL
        text = text//'        with:'//NL
        text = text//'          path: |'//NL
        text = text//'            ${{ env.FO_CACHE_DIR }}'//NL
        text = text//'            ${{ env.CCACHE_DIR }}'//NL
        text = text//'            build/dependencies'//NL
        text = text//'          key: fortyard-build-${{ github.repository }}-${{ e'// &
            'nv.FORTYARD_RUNNER_CLASS }}-fo-${{ env.FO_COMMIT }}-${{ hash'// &
            'Files(''fpm.toml'') }}-${{ github.sha }}'//NL
        text = text//'          restore-keys: |'//NL
        text = text//'            fortyard-build-${{ github.repository }}-${{ env.'// &
            'FORTYARD_RUNNER_CLASS }}-fo-${{ env.FO_COMMIT }}-${{ hashFil'// &
            'es(''fpm.toml'') }}-'//NL
        text = text//'            fortyard-build-${{ github.repository }}-${{ env.'// &
            'FORTYARD_RUNNER_CLASS }}-fo-${{ env.FO_COMMIT }}-'//NL
        text = text//'      - name: fo'//NL
        text = text//'        run: |'//NL
        text = text//'          # fo at FO_COMMIT, built once per machine kind and'// &
            ' commit with fpm''s'//NL
        text = text//'          # release binary of the pinned checksum.'//NL
        text = text//'          if [ ! -x "$FY_TOOLS/bin/fo" ] && [ ! -x "$FY_TOOL'// &
            'S/bin/fo.exe" ]; then'//NL
        text = text//'            case "$RUNNER_OS-$RUNNER_ARCH" in'//NL
        text = text//'              Linux-X64) asset=linux-x86_64-gcc-12'//NL
        text = text//'                sum=2e50a8649ac4136b9cf6b63c5d1b01adf26c02ee'// &
            'dc18f32e16261c0ce655d46d ;;'//NL
        text = text//'              macOS-ARM64) asset=macos-arm64-gcc-12'//NL
        text = text//'                sum=f6763de9d198ae35f9ffb246d6468ae89d15c5a8'// &
            '456e2a8fea23c319ce7362eb ;;'//NL
        text = text//'              Windows-X64) asset=windows-x86_64-gcc-12.exe'//NL
        text = text//'                sum=89f34d8cc3cface54a06785e9e1d7a2c16a07e6a'// &
            'cd18f7c5d7e5cf95a5eaa671 ;;'//NL
        text = text//'              *) echo "no fpm for $RUNNER_OS-$RUNNER_ARCH" >'// &
            '&2; exit 1 ;;'//NL
        text = text//'            esac'//NL
        text = text//'            mkdir -p "$FY_TOOLS/bin"'//NL
        text = text//'            curl -fsSL -o "$FY_TOOLS/bin/fpm" \'//NL
        text = text//'              "https://github.com/fortran-lang/fpm/releases/'// &
            'download/v$FPM_VERSION/fpm-$FPM_VERSION-$asset"'//NL
        text = text//'            # Windows images have sha256sum, macOS images shasum.'//NL
        text = text//'            if command -v sha256sum > /dev/null; then'//NL
        text = text//'              echo "$sum  $FY_TOOLS/bin/fpm" | sha256sum -c -'//NL
        text = text//'            else'//NL
        text = text//'              echo "$sum  $FY_TOOLS/bin/fpm" | shasum -a 256 -c -'//NL
        text = text//'            fi'//NL
        text = text//'            chmod +x "$FY_TOOLS/bin/fpm"'//NL
        text = text//'            if [ "$RUNNER_OS" = macOS ]; then'//NL
        text = text//'              for v in 15 14 13 12; do'//NL
        text = text//'                if command -v "gfortran-$v" > /dev/null; the'// &
            'n export FPM_FC="gfortran-$v"; break; fi'//NL
        text = text//'              done'//NL
        text = text//'              # fpm''s binary links Homebrew''s gcc@12 runtime; the'//NL
        text = text//'              # image''s gfortran ships the same libgfortran.5.'//NL
        text = text//'              lib=$("${FPM_FC:-gfortran}" -print-file-name=libgfortr'// &
            'an.5.dylib)'//NL
        text = text//'              export DYLD_FALLBACK_LIBRARY_PATH="${lib%/*}"'//NL
        text = text//'            fi'//NL
        text = text//'            git init --quiet "$RUNNER_TEMP/fo-src"'//NL
        text = text//'            git -C "$RUNNER_TEMP/fo-src" fetch --quiet --dep'// &
            'th 1 \'//NL
        text = text//'              https://github.com/lazy-fortran/fo.git "$FO_CO'// &
            'MMIT"'//NL
        text = text//'            git -C "$RUNNER_TEMP/fo-src" checkout --quiet FE'// &
            'TCH_HEAD'//NL
        text = text//'            (cd "$RUNNER_TEMP/fo-src" && "$FY_TOOLS/bin/fpm"'// &
            ' install --profile release --prefix "$FY_TOOLS")'//NL
        text = text//'          fi'//NL
        text = text//'          echo "$FY_TOOLS/bin" >> "$GITHUB_PATH"'//NL
        text = text//'      - name: Check'//NL
        text = text//'        run: |'//NL
        text = text//'          case "$GITHUB_REF_NAME" in'//NL
        text = text//'            *-verify) kind=verify ;;'//NL
        text = text//'            *) kind=check ;;'//NL
        text = text//'          esac'//NL
        text = text//'          if [ -f ".fortyard/$kind" ]; then'//NL
        text = text//'            exec "./.fortyard/$kind"'//NL
        text = text//'          fi'//NL
        text = text//'          if [ ! -f fpm.toml ]; then'//NL
        text = text//'            echo "No $kind is configured (.fortyard/$kind or'// &
            ' fpm.toml)." >&2'//NL
        text = text//'            exit 1'//NL
        text = text//'          fi'//NL
        text = text//'          if [ "$kind" = verify ]; then'//NL
        text = text//'            fo build && FO_TEST_RERUN=1 fo test'//NL
        text = text//'          else'//NL
        text = text//'            fo build && fo test'//NL
        text = text//'          fi'//NL
    end function github_workflow

end module fy_backend_github
