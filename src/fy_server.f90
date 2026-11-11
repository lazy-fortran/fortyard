module fy_server
    !! Request routing for `fy serve`.
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64, &
        output_unit
    use fortui, only: &
        ui_frame_begin, &
        ui_init, &
        ui_present, &
        ui_shutdown
    use fortui_backend_headless, only: &
        headless_backend_t
    use fortui_html, only: &
        ui_frame_html
    use fx_hash, only: &
        sha256_file
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        json_extract_string
    use fx_string, only: &
        to_lower
    use fy_audit, only: &
        audit_append, &
        audit_json, &
        audit_list, &
        audit_t, &
        audit_verify
    use fy_build_info, only: &
        FY_BUILT, &
        FY_COMMIT, &
        FY_VERSION
    use fy_cache, only: &
        cache_route, &
        cache_serve
    use fy_change, only: &
        change_api, &
        change_by_branch, &
        change_by_id, &
        change_landed, &
        change_list_json, &
        change_message, &
        change_next_number, &
        change_sync, &
        change_t
    use fy_events, only: &
        events_notify, &
        events_set_dir, &
        events_stream, &
        events_wait, &
        events_watch_close, &
        events_watch_open
    use fy_fetch, only: &
        fetch_serve, &
        fetch_served_json
    use fy_git_http, only: &
        git_http_route, &
        git_http_serve
    use fy_http, only: &
        http_basic_password, &
        http_bearer_token, &
        http_cookie, &
        http_query_param, &
        http_read_body, &
        http_read_head, &
        http_read_request, &
        http_request_t, &
        http_send_response
    use fy_issue, only: &
        issue_api
    use fy_land, only: &
        land_branch, &
        land_candidate, &
        land_check, &
        land_hook_install
    use fy_oidc, only: &
        oidc_authorize_url, &
        oidc_check, &
        oidc_claims_t, &
        oidc_config_t, &
        oidc_exchange, &
        oidc_is_admin, &
        oidc_load, &
        oidc_value_ok
    use fy_policy, only: &
        policy_from_json, &
        policy_get, &
        policy_json, &
        policy_put, &
        policy_queue, &
        policy_refusal, &
        policy_t
    use fy_posix, only: &
        sock_close, &
        sock_recv, &
        sock_set_timeouts, &
        tcp_accept
    use fy_proc, only: &
        SIGTERM, &
        proc_die_with_parent, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_pid, &
        proc_poll_child, &
        proc_readlink, &
        proc_reexec, &
        proc_sleep_ms, &
        proc_wait, &
        proc_wait_any
    use fy_repo, only: &
        repo_blob, &
        repo_clone, &
        repo_commit_json, &
        repo_fetch, &
        repo_init, &
        repo_log_json, &
        repo_name_ok, &
        repo_path_ok, &
        repo_push_tags, &
        repo_ref_ok, &
        repo_refs_json, &
        repo_tree_json
    use fy_review, only: &
        review_enabled, &
        reviewer_main
    use fy_route, only: &
        route_is_page, &
        route_set_base
    use fy_sqlite, only: &
        db_close, &
        db_t
    use fy_stats, only: &
        stats_days_ok, &
        stats_json
    use fy_store, only: &
        check_t, &
        landing_t, &
        random_token, &
        release_t, &
        repo_info_t, &
        store_account_find, &
        store_check_add, &
        store_check_claim, &
        store_check_error, &
        store_check_finish, &
        store_check_get, &
        store_check_release, &
        store_job, &
        store_job_checks, &
        store_job_counts, &
        store_job_next, &
        store_jobs, &
        store_jobs_state, &
        store_landing_add, &
        store_landing_check, &
        store_landing_finish, &
        store_landing_list, &
        store_landing_next, &
        store_landing_requeue, &
        store_landing_take, &
        store_open, &
        store_release_add, &
        store_release_list, &
        store_release_ready, &
        store_repo_add, &
        store_repo_fetched, &
        store_repo_list, &
        store_repo_set_mode, &
        store_repo_set_number, &
        store_snapshot, &
        store_token_create, &
        store_token_list, &
        store_token_lookup, &
        store_token_revoke, &
        store_token_revoke_hash, &
        token_identity_t, &
        token_info_t
    use fy_ui_app, only: &
        app_compose, &
        app_missing, &
        app_start, &
        app_title
    use fy_verify, only: &
        verifier_main, &
        verify_request
    implicit none
    private

    public :: &
        response_t, &
        route, &
        serve_connection, &
        server_loop, &
        server_set_base, &
        server_set_checks, &
        server_set_data_dir, &
        server_set_follow, &
        server_set_mirror_interval, &
        server_set_web_dir, &
        server_supervise

    character(len=*), parameter :: JSON = 'application/json'
    character(len=*), parameter :: SESSION_COOKIE = 'fy_session'
    !! State, PKCE verifier and nonce of a sign-in on its way through the provider.
    character(len=*), parameter :: LOGIN_COOKIE = 'fy_login'
    integer, parameter :: SESSION_DAYS = 14
    character(len=*), parameter :: SESSION_MAX_AGE = '1209600'
    !! Name of the tokens that are browser sessions, and the start of the
    !! name of one opened through the provider; no token takes such a name.
    character(len=*), parameter :: SESSION_NAME = 'browser session'
    !! The scopes the server checks; a token names no other.
    character(len=*), parameter :: KNOWN_SCOPES(*) = [character(len=7) :: &
        'admin', &
        'land', &
        'release', &
        'runner', &
        'write']
    !! Preferences the browser keeps in cookies, read for the first frame.
    character(len=*), parameter :: SIDEBAR_COOKIE = 'fy_sidebar', THEME_COOKIE = 'fy_theme'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)

    type :: response_t
        integer :: code = 200
        character(len=:), allocatable :: ctype, body, headers, cache
    end type response_t

    !! Directory of the web application; empty serves the status page at /.
    character(len=:), allocatable :: web_dir
    !! Sign-in through the OpenID Connect provider, from <data>/oidc.conf.
    type(oidc_config_t) :: oidc
    !! Directory of the repository mirrors.
    character(len=:), allocatable :: repos_dir
    !! Path below the host where fortyard is served (e.g. "/yard"), empty at the
    !! root. A reverse proxy removes it from requests; links and cookies need it.
    character(len=:), allocatable :: base
    !! A symbolic link to the installed release (directory with `fy`); when it
    !! changes, the server re-executes the release it points to.
    character(len=:), allocatable :: follow_link
    !! "local": landings run their checks in the server; "runner": a runner does.
    character(len=:), allocatable :: checks_mode
    !! The lander's database connection, for its runner checks.
    type(db_t) :: lander_db
    !! The landings of the candidate the lander checks, which share its check.
    integer(int64), allocatable :: candidate_jobs(:)
    !! Lander processes when runners check the landings.
    integer, parameter :: MAX_LANDERS = 4
    !! Seconds past its check_timeout after which a check whose runner has
    !! not reported is taken as lost.
    integer, parameter :: LOST_GRACE = 300
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
        !! worker, `nthreads` single-threaded processes run their own accept
        !! loops (worker_main), so a long-lived connection holds one of them.
        integer, intent(in) :: lfd, nworkers, nthreads
        character(len=*), intent(in) :: db_path
        ! Slot nworkers + 1, when there is a data directory, is the process that
        ! mirrors repositories, and the slots after it land branches: several
        ! when runners check them, so one repository's long check does not hold
        ! up the others, else one.
        integer :: pids(nworkers + 3 + MAX_LANDERS)
        integer(int64) :: started(nworkers + 3 + MAX_LANDERS), now, rate
        integer :: i, pid, status, slots, review_slot
        character(len=:), allocatable :: followed
        integer :: me

        me = proc_pid()
        slots = nworkers
        if (allocated(repos_dir)) then
            slots = nworkers + 2
            if (checks_mode_set()) then
                ! The last slot verifies main (fy_verify), on runners only.
                if (checks_mode == 'runner') slots = nworkers + 2 + MAX_LANDERS
            end if
        end if
        ! One more process reviews changes when a model is configured (fy_review).
        review_slot = 0
        if (allocated(repos_dir) .and. review_enabled()) then
            slots = slots + 1
            review_slot = slots
        end if
        if (allocated(repos_dir)) call hooks_refresh(db_path)
        do i = 1, slots
            call spawn(i)
        end do
        if (allocated(follow_link)) followed = proc_readlink(follow_link)
        do
            call proc_poll_child(pid, status)
            if (pid <= 0) then
                call proc_sleep_ms(200)
                if (allocated(follow_link)) then
                    if (proc_readlink(follow_link) /= followed .and. &
                        len(proc_readlink(follow_link)) > 0) call switch_release()
                end if
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

        subroutine switch_release()
            !! A new release was installed: stop the children and become it,
            !! keeping the listening socket, so connections wait instead of failing.
            character(len=4096), allocatable :: args(:)
            integer :: k, n, st

            write (error_unit, '(a)') 'fy: switching to '//proc_readlink(follow_link)
            flush (error_unit)
            do k = 1, slots
                if (pids(k) > 0) call proc_kill(pids(k), SIGTERM)
            end do
            do k = 1, slots
                if (pids(k) > 0) call proc_wait(pids(k), st)
            end do
            n = command_argument_count()
            allocate (args(n + 1))
            args(1) = follow_link//'/fy'
            do k = 1, n
                call get_command_argument(k, args(k + 1))
            end do
            call proc_reexec(follow_link//'/fy', args, lfd)
            write (error_unit, '(a)') 'fy: the new release does not start; staying'
            flush (error_unit)
            followed = proc_readlink(follow_link)
            do k = 1, slots
                call spawn(k)
            end do
        end subroutine switch_release

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
            if (slot == review_slot) then
                call reviewer_main(db_path, repos_dir)
            else if (slot == nworkers + 2 + MAX_LANDERS) then
                call verifier_main(db_path, repos_dir)
            else if (slot > nworkers + 1) then
                call lander_main(db_path, slot - nworkers - 1)
            else if (slot > nworkers) then
                call mirror_main(db_path)
            else
                call worker_main(lfd, db_path, nthreads)
            end if
            call proc_exit(0)
        end subroutine spawn

    end subroutine server_supervise

    subroutine hooks_refresh(db_path)
        !! Gives every primary repository the current pre-receive hook before
        !! any worker takes a push: a hook from before an upgrade may let a
        !! write token move what only an administrator may.
        character(len=*), intent(in) :: db_path
        type(db_t) :: db
        type(repo_info_t), allocatable :: repos(:)
        integer :: stat, i, status
        logical :: exists

        call store_open(db_path, db, stat)
        if (stat /= 0) return
        call store_repo_list(db, repos, stat)
        call db_close(db)
        if (.not. allocated(repos)) return
        do i = 1, size(repos)
            if (repos(i)%mode /= 'primary') cycle
            inquire (file=repo_path(repos(i)%name)//'/HEAD', exist=exists)
            if (exists) call land_hook_install(repo_path(repos(i)%name), status)
        end do
    end subroutine hooks_refresh

    subroutine mirror_main(db_path)
        !! Clones repositories that have no mirror yet and fetches the others
        !! every mirror interval; a failed fetch is recorded and retried then.
        character(len=*), intent(in) :: db_path
        type(db_t) :: db
        type(repo_info_t), allocatable :: repos(:)
        character(len=64), allocatable :: names(:)
        integer(int64), allocatable :: last(:)
        integer(int64) :: now, rate, last_snapshot = 0
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
                        if (exists) then
                            if (len(repos(i)%upstream) == 0) cycle
                            if (now - last(k) < int(mirror_interval, int64) * rate) cycle
                            last(k) = now
                            call repo_push_tags(repo_path(repos(i)%name), repos(i)%upstream, &
                                status)
                            if (status /= 0) then
                                write (error_unit, '(a,i0)') 'fy: pushing the tags of '// &
                                    repos(i)%name//' failed with status ', status
                                flush (error_unit)
                            end if
                            cycle
                        end if
                        call repo_init(repo_path(repos(i)%name), status)
                        if (status == 0) call land_hook_install(repo_path(repos(i)%name), status)
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
            ! A consistent copy of the database for the host's file backup, hourly.
            call system_clock(now, rate)
            if (now - last_snapshot >= 3600_int64 * rate .or. last_snapshot == 0) then
                call store_open(db_path, db, stat)
                if (stat == 0) then
                    call store_snapshot(db, repos_dir//'/../fortyard-snapshot.db', stat)
                    call db_close(db)
                end if
                last_snapshot = now
            end if
            call proc_sleep_ms(min(10000, 1000 * mirror_interval))
        end do
    end subroutine mirror_main

    subroutine lander_main(db_path, lander)
        !! Lander number `lander`: lands queued branches, oldest first, those of
        !! one repository together.
        character(len=*), intent(in) :: db_path
        integer, intent(in) :: lander
        type(db_t) :: db
        type(landing_t) :: l, more
        type(landing_t), allocatable :: group(:)
        type(repo_info_t), allocatable :: repos(:)
        character(len=:), allocatable :: upstream, mode, base
        type(policy_t) :: policy
        character(len=:), allocatable :: refusal
        character(len=1024) :: url
        integer :: stat, i, n, watch
        logical :: found, changed

        ! Links in commits and issues are read by people, so they use the public
        ! address when the server listens on an internal one.
        call get_environment_variable('FY_PUBLIC_URL', url, n, stat)
        if (stat /= 0 .or. n == 0) call get_environment_variable('FY_URL', url, n, stat)
        base = ''
        if (stat == 0) base = trim(url)
        call store_open(db_path, db, stat)
        if (stat /= 0) call proc_exit(1)
        ! Opened before the first look at the queue, so no landing is missed.
        watch = events_watch_open()
        call store_landing_requeue(db, lander)
        call events_notify('jobs')
        do
            call store_landing_next(db, lander, l, found)
            if (found) call events_notify('jobs')
            if (.not. found) then
                ! Sleeps until a landing is queued (the API rewrites the jobs marker).
                changed = events_wait(watch, 'jobs', -1)
                if (watch < 0) call proc_sleep_ms(2000)
                cycle
            end if
            call store_repo_list(db, repos, stat)
            mode = ''
            upstream = ''
            do i = 1, size(repos)
                if (repos(i)%name /= l%repo) cycle
                mode = repos(i)%mode
                upstream = repos(i)%upstream
            end do
            if (mode /= 'primary') then
                call store_landing_finish(db, l%id, 'failed', '', &
                    'Only a primary repository lands branches.')
                call audit_landing(l, .false., '')
                call events_notify('jobs')
                cycle
            end if
            lander_db = db
            call policy_get(db, l%repo, policy)
            ! Queued landings of one repository share a candidate and its check,
            ! at most as many as its policy says. A change whose review was
            ! due before landing and is missing by now does not land.
            if (allocated(group)) deallocate (group)
            allocate (group(0))
            more = l
            do
                refusal = policy_refusal(db, more%repo, more%change, .false.)
                if (len(refusal) > 0) then
                    call finish(more, .false., '', refusal)
                else
                    group = [group, more]
                end if
                if (size(group) >= policy%batch) exit
                call store_landing_take(db, lander, l%repo, more, found)
                if (.not. found) exit
            end do
            if (size(group) == 0) then
                cycle
            else if (size(group) == 1) then
                call land_one(group(1))
            else
                call land_group(group)
            end if
        end do

    contains

        subroutine land_one(l)
            !! Lands one branch by itself: its own commit on main, checked alone.
            type(landing_t), intent(in) :: l
            type(change_t) :: ch
            character(len=:), allocatable :: commit, log, work, change_url
            logical :: ok, has_change
            integer :: next

            ! Scratch space of a landing lives in the temporary directory and is
            ! removed when the landing ends, whatever its outcome.
            work = tmp_dir()//'/fy-land-'//itoa(int(l%id))
            has_change = .false.
            if (l%change > 0) call change_by_id(db, l%change, ch, has_change)
            ! The landing's ident is unique across instances and restarts: number
            ! and time of queueing.
            if (has_change) then
                change_url = ''
                if (len(base) > 0) change_url = base//'/'//l%repo//'/-/changes/'//itoa(ch%number)
                if (checks_mode == 'runner') then
                    call land_branch(repo_path(l%repo), work, l%repo, l%branch, &
                        itoa(int(l%id))//'@'//l%created, base, upstream, commit, log, ok, &
                        runner=runner_check, number=ch%number, &
                        message_in=change_message(ch, change_url), check=policy%tests /= 'none', &
                        expect=reviewed(ch))
                else
                    call land_branch(repo_path(l%repo), work, l%repo, l%branch, &
                        itoa(int(l%id))//'@'//l%created, base, upstream, commit, log, ok, &
                        number=ch%number, message_in=change_message(ch, change_url), &
                        check=policy%tests /= 'none', expect=reviewed(ch))
                end if
                if (ok) call change_landed(db, ch%id, commit)
            else
                ! Numbers of open changes are taken.
                next = change_next_number(db, l%repo, repo_path(l%repo))
                if (checks_mode == 'runner') then
                    call land_branch(repo_path(l%repo), work, l%repo, l%branch, &
                        itoa(int(l%id))//'@'//l%created, base, upstream, commit, log, ok, &
                        runner=runner_check, min_number=next, check=policy%tests /= 'none')
                else
                    call land_branch(repo_path(l%repo), work, l%repo, l%branch, &
                        itoa(int(l%id))//'@'//l%created, base, upstream, commit, log, ok, &
                        min_number=next, check=policy%tests /= 'none')
                end if
            end if
            call execute_command_line('rm -rf '''//work//'''')
            call finish(l, ok, commit, log)
        end subroutine land_one

        recursive subroutine land_group(g)
            !! Lands several branches of one repository with one check: each is
            !! squashed onto the candidate before it, and main moves to the last
            !! when the check passes. A failing candidate is split in halves, so a
            !! branch is blamed only when it fails on main by itself.
            type(landing_t), intent(in) :: g(:)
            type(change_t) :: ch
            integer, allocatable :: held(:), single(:)
            character(len=40), allocatable :: commits(:), tips(:)
            character(len=40) :: pinned
            character(len=1024), allocatable :: branches(:), holds(:)
            character(len=:), allocatable :: tip, hold, commit, log, work, change_url
            logical :: ok, alone, has_change
            integer :: i, k, next

            tip = ''
            allocate (held(0), single(0), commits(0), tips(0), branches(0), holds(0))
            do i = 1, size(g)
                hold = 'refs/fortyard/queue/'//itoa(int(g(i)%id))
                work = tmp_dir()//'/fy-land-'//itoa(int(g(i)%id))
                has_change = .false.
                if (g(i)%change > 0) call change_by_id(db, g(i)%change, ch, has_change)
                if (has_change) then
                    change_url = ''
                    if (len(base) > 0) change_url = base//'/'//g(i)%repo//'/-/changes/'// &
                        itoa(ch%number)
                    call land_branch(repo_path(g(i)%repo), work, g(i)%repo, g(i)%branch, &
                        itoa(int(g(i)%id))//'@'//g(i)%created, base, upstream, commit, log, ok, &
                        number=ch%number, message_in=change_message(ch, change_url), &
                        onto=tip, hold=hold, alone=alone, expect=reviewed(ch), &
                        pinned=pinned)
                else
                    next = change_next_number(db, g(i)%repo, repo_path(g(i)%repo))
                    call land_branch(repo_path(g(i)%repo), work, g(i)%repo, g(i)%branch, &
                        itoa(int(g(i)%id))//'@'//g(i)%created, base, upstream, commit, log, ok, &
                        min_number=next, onto=tip, hold=hold, alone=alone, pinned=pinned)
                end if
                call execute_command_line('rm -rf '''//work//'''')
                if (alone) then
                    single = [single, i]
                else if (ok) then
                    held = [held, i]
                    commits = [character(len=40) :: commits, commit]
                    tips = [character(len=40) :: tips, pinned]
                    branches = [character(len=1024) :: branches, g(i)%branch]
                    holds = [character(len=1024) :: holds, hold]
                    tip = hold
                else
                    call finish(g(i), .false., '', log)
                end if
            end do
            if (size(held) > 0) then
                work = tmp_dir()//'/fy-land-'//itoa(int(g(held(1))%id))
                candidate_jobs = g(held)%id
                if (checks_mode == 'runner') then
                    call land_candidate(repo_path(g(1)%repo), work, g(1)%repo, tip, branches, &
                        holds, upstream, log, ok, runner=runner_check, &
                        check=policy%tests /= 'none', pinned=tips)
                else
                    call land_candidate(repo_path(g(1)%repo), work, g(1)%repo, tip, branches, &
                        holds, upstream, log, ok, check=policy%tests /= 'none', &
                        pinned=tips)
                end if
                deallocate (candidate_jobs)
                call execute_command_line('rm -rf '''//work//'''')
                if (ok) then
                    do k = 1, size(held)
                        if (size(held) > 1) then
                            call finish(g(held(k)), .true., commits(k), 'Landed as '// &
                                commits(k)//', checked with '//itoa(size(held) - 1)// &
                                ' other landing(s) in one candidate.'//new_line('a')//log)
                        else
                            call finish(g(held(k)), .true., commits(k), log//new_line('a')// &
                                'Landed as '//commits(k)//'.')
                        end if
                        if (g(held(k))%change > 0) then
                            call change_by_id(db, g(held(k))%change, ch, has_change)
                            if (has_change) call change_landed(db, ch%id, commits(k))
                        end if
                    end do
                else if (size(held) == 1) then
                    call finish(g(held(1)), .false., '', log)
                else
                    k = size(held) / 2
                    call land_group(g(held(1:k)))
                    call land_group(g(held(k + 1:)))
                end if
            end if
            do k = 1, size(single)
                call land_one(g(single(k)))
            end do
        end subroutine land_group

        function reviewed(ch) result(head)
            !! The commit the branch of `ch` has to be at to land: the version
            !! that was reviewed when the policy asks for a review first, so a
            !! push after the review does not land with it; else '' (any).
            type(change_t), intent(in) :: ch
            character(len=:), allocatable :: head

            head = ''
            if (policy%review == 'before') head = ch%reviewed_head
        end function reviewed

        subroutine finish(l, ok, commit, log)
            type(landing_t), intent(in) :: l
            logical, intent(in) :: ok
            character(len=*), intent(in) :: commit, log

            if (ok) then
                call store_landing_finish(db, l%id, 'landed', commit, log)
            else
                call store_landing_finish(db, l%id, 'failed', '', log)
            end if
            call audit_landing(l, ok, commit)
            call events_notify('jobs')
            write (error_unit, '(a)') 'fy: landing '//itoa(int(l%id))//' of '//l%repo//' '// &
                l%branch//merge(' landed', ' failed', ok)
            flush (error_unit)
        end subroutine finish

        subroutine audit_landing(l, ok, commit)
            type(landing_t), intent(in) :: l
            logical, intent(in) :: ok
            character(len=*), intent(in) :: commit

            if (ok) then
                call audit_append(db, l%account, 'landing.landed', l%repo, 'landing '// &
                    itoa(int(l%id))//' of '//l%branch//' as '//commit)
            else
                call audit_append(db, l%account, 'landing.failed', l%repo, 'landing '// &
                    itoa(int(l%id))//' of '//l%branch)
            end if
        end subroutine audit_landing

        function tmp_dir() result(s)
            character(len=:), allocatable :: s
            character(len=1024) :: v
            integer :: n, st
            call get_environment_variable('TMPDIR', v, n, st)
            s = '/var/tmp'
            if (st == 0 .and. n > 0) s = trim(v)
        end function tmp_dir

        function itoa(k) result(s)
            integer, intent(in) :: k
            character(len=:), allocatable :: s
            character(len=16) :: b
            write (b, '(i0)') k
            s = trim(b)
        end function itoa

    end subroutine lander_main

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
        call oidc_load(dir//'/oidc.conf', oidc)
        call events_set_dir(dir//'/events')
    end subroutine server_set_data_dir

    subroutine repos_route(req, db, res)
        !! /api/v1/repos and /api/v1/repos/NAME/{refs,log,tree,blob,commits/ID,policy}.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        type(token_identity_t) :: who
        type(repo_info_t), allocatable :: repos(:)
        character(len=:), allocatable :: rest, name, sub, ref, path, upstream
        integer :: slash, i, status, stat, k
        logical :: found, exists, has_change
        type(change_t) :: ch
        integer(int64) :: number
        character(len=24) :: num

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
                    if (stat == 0 .and. status == 0) call land_hook_install(repo_path(name), status)
                else
                    call store_repo_add(db, name, upstream, stat)
                end if
                if (stat /= 0) then
                    call fail(res, 409, 'repository exists')
                    return
                end if
                if (len(upstream) == 0) then
                    call audit_append(db, who%account, 'repo.created', name, 'primary')
                else
                    call audit_append(db, who%account, 'repo.created', name, &
                        'mirror of '//upstream)
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
        k = index(rest, '/releases')
        if (k > 1) then
            call releases_route(rest(1:k - 1), rest(k + 9:))
            return
        end if
        k = index(rest, '/issues')
        if (k > 1) then
            if (len(rest) == k + 6) then
                call issues(rest(1:k - 1), '')
                return
            else if (rest(k + 7:k + 7) == '/') then
                call issues(rest(1:k - 1), rest(k + 7:))
                return
            end if
        end if
        k = index(rest, '/changes')
        if (k > 1) then
            if (len(rest) == k + 7) then
                call changes(rest(1:k - 1), '')
                return
            else if (rest(k + 8:k + 8) == '/') then
                call changes(rest(1:k - 1), rest(k + 8:))
                return
            end if
        end if
        if (req%method == 'POST' .and. len(rest) > 5) then
            ! Sorted by suffix.
            if (rest(len(rest) - 4:) == '/land') then
                call queue_landing(rest(1:len(rest) - 5))
                return
            end if
            if (rest(len(rest) - 4:) == '/mode') then
                call set_mode(rest(1:len(rest) - 5))
                return
            end if
            if (len(rest) > 8) then
                if (rest(len(rest) - 7:) == '/numbers') then
                    call set_number(rest(1:len(rest) - 8))
                    return
                end if
            end if
            if (len(rest) > 7) then
                if (rest(len(rest) - 6:) == '/verify') then
                    call queue_verify(rest(1:len(rest) - 7))
                    return
                end if
            end if
        end if
        if (len(rest) > 7) then
            ! Not a ref called policy, as in NAME/commits/policy.
            if (rest(len(rest) - 6:) == '/policy' .and. &
                index(rest(1:len(rest) - 7), '/') == 0) then
                call policy_route(rest(1:len(rest) - 7))
                return
            end if
        end if
        if (len(rest) > 6) then
            ! The board's queue numbers; the board itself is not built yet.
            if (rest(len(rest) - 5:) == '/board' .and. &
                http_query_param(req%query, 'stats') == '1') then
                if (.not. method_is(req, 'GET', res)) return
                call stats_route(req, db, rest(1:len(rest) - 6), res, named=.true.)
                return
            end if
        end if
        if (len(rest) > 9) then
            if (rest(len(rest) - 8:) == '/landings') then
                if (.not. method_is(req, 'GET', res)) return
                call list_landings(rest(1:len(rest) - 9))
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
        case ('blob')
            call repo_blob(repo_path(name), ref, path, res%body, status)
            if (status == 413) then
                call fail(res, 413, 'file larger than 1 MiB')
                return
            end if
            res%ctype = 'text/plain; charset=utf-8'
        case ('log')
            call repo_log_json(repo_path(name), ref, 100, res%body, status)
        case ('refs')
            call repo_refs_json(repo_path(name), res%body, status)
        case ('tree')
            call repo_tree_json(repo_path(name), ref, path, res%body, status)
        case default
            if (index(sub, 'commits/') /= 1 .or. .not. repo_ref_ok(sub(9:))) then
                call fail(res, 404, 'not found')
                return
            end if
            call repo_commit_json(repo_path(name), sub(9:), res%body, status)
        end select
        if (status /= 0) call fail(res, 404, 'no such ref, path or commit')

    contains

        subroutine releases_route(target, tail)
            !! GET  .../releases          the newest releases (JSON, or ?format=text
            !!                            with one line "VERSION COMMIT STATE SHA256")
            !! POST .../releases          {"version","commit","notes"}: opens a release
            !! GET  .../releases/V/release.tar  the archive of a ready release
            character(len=*), intent(in) :: target, tail
            type(release_t), allocatable :: rs(:)
            character(len=:), allocatable :: version, commit, notes, archive
            character(len=24) :: num
            integer :: j
            logical :: ok

            if (.not. repo_name_ok(target)) then
                call fail(res, 404, 'not found')
                return
            end if
            if (len(tail) == 0) then
                if (req%method == 'POST') then
                    if (index(' '//who%scopes//' ', ' admin ') == 0 .and. &
                        index(' '//who%scopes//' ', ' release ') == 0) then
                        call fail(res, 403, 'releases need the release or admin scope')
                        return
                    end if
                    call json_extract_string(req%body, 'version', version, ok)
                    if (.not. ok) version = ''
                    call json_extract_string(req%body, 'commit', commit, ok)
                    if (.not. ok) commit = ''
                    call json_extract_string(req%body, 'notes', notes, ok)
                    if (.not. ok) notes = ''
                    if (.not. version_ok(version) .or. .not. repo_ref_ok(commit) .or. &
                        len(commit) /= 40) then
                        call fail(res, 400, 'body must be {"version":"...","commit":"<40 hex>",'// &
                            '"notes":"..."}')
                        return
                    end if
                    call store_release_add(db, target, version, commit, notes, who%account, stat)
                    if (stat /= 0) then
                        call fail(res, 409, 'release exists')
                        return
                    end if
                    res%code = 201
                    res%body = '{"version":"'//version//'","state":"open"}'
                    return
                end if
                if (.not. method_is(req, 'GET', res)) return
                call store_release_list(db, target, rs, stat)
                if (http_query_param(req%query, 'format') == 'text') then
                    res%ctype = 'text/plain; charset=utf-8'
                    res%body = ''
                    do j = 1, size(rs)
                        res%body = res%body//rs(j)%version//' '//rs(j)%commit//' '// &
                            rs(j)%state//' '//rs(j)%sha256//new_line('a')
                    end do
                    return
                end if
                res%body = '{"releases":['
                do j = 1, size(rs)
                    write (num, '(i0)') rs(j)%size
                    if (j > 1) res%body = res%body//','
                    res%body = res%body//'{"version":"'//rs(j)%version//'","commit":"'// &
                        rs(j)%commit//'","state":"'//rs(j)%state//'","created":"'// &
                        rs(j)%created//'","uploader":"'//json_escape_string(rs(j)%uploader)// &
                        '","sha256":"'//rs(j)%sha256//'","size":'//trim(num)//',"notes":"'// &
                        json_escape_string(rs(j)%notes)//'"}'
                end do
                res%body = res%body//']}'
                return
            end if
            if (len(tail) > 13) then
                if (tail(1:1) == '/' .and. tail(len(tail) - 11:) == '/release.tar') then
                    version = tail(2:len(tail) - 12)
                    if (version_ok(version)) then
                        if (.not. method_is(req, 'GET', res)) return
                        call read_file(releases_dir()//'/'//target//'/'//version// &
                            '/release.tar', archive, ok)
                        if (.not. ok) then
                            call fail(res, 404, 'no such release archive')
                            return
                        end if
                        res%ctype = 'application/x-tar'
                        res%body = archive
                        return
                    end if
                end if
            end if
            call fail(res, 404, 'not found')
        end subroutine releases_route

        subroutine queue_landing(target)
            !! POST /api/v1/repos/NAME/land {"branch":"..."}: queues the branch to
            !! land on main as one squashed commit; 202 with the landing number.
            character(len=*), intent(in) :: target
            character(len=:), allocatable :: branch
            integer(int64) :: number
            character(len=24) :: num
            logical :: ok

            if (.not. has_scope(who, 'land')) then
                call fail(res, 403, 'landing needs the land or admin scope')
                return
            end if
            call json_extract_string(req%body, 'branch', branch, ok)
            if (.not. ok) branch = ''
            if (.not. repo_name_ok(target) .or. .not. repo_ref_ok(branch) .or. &
                branch == 'main' .or. len(branch) == 0) then
                call fail(res, 400, 'body must be {"branch":"..."} naming a branch other than main')
                return
            end if
            ! The branch's open change lands with it.
            call change_by_branch(db, target, branch, ch, has_change)
            if (.not. queued(target, branch, has_change, number)) return
            write (num, '(i0)') number
            res%code = 202
            res%body = '{"landing":'//trim(num)//',"state":"queued"}'
        end subroutine queue_landing

        subroutine queue_verify(target)
            !! POST /api/v1/repos/NAME/verify: verifies main's newest commit
            !! again (fy_verify); 202 with the job's id.
            character(len=*), intent(in) :: target
            integer(int64) :: id
            character(len=24) :: jn

            if (index(' '//who%scopes//' ', ' admin ') == 0) then
                call fail(res, 403, 'verifying needs the admin scope')
                return
            end if
            inquire (file=repo_path(target)//'/HEAD', exist=exists)
            if (.not. repo_name_ok(target) .or. .not. exists) then
                call fail(res, 404, 'no such repository')
                return
            end if
            id = verify_request(db, target, repo_path(target))
            if (id == 0) then
                call fail(res, 409, 'the repository has no main yet')
                return
            end if
            write (jn, '(i0)') id
            res%code = 202
            res%body = '{"job":'//trim(jn)//',"state":"queued"}'
        end subroutine queue_verify

        subroutine issues(target, sub)
            !! /api/v1/repos/NAME/issues...: see fy_issue.
            character(len=*), intent(in) :: target, sub
            character(len=:), allocatable :: answer
            integer :: code

            inquire (file=repo_path(target)//'/HEAD', exist=exists)
            if (.not. repo_name_ok(target) .or. .not. exists) then
                call fail(res, 404, 'no such repository')
                return
            end if
            if (req%method == 'POST' .and. .not. has_scope(who, 'write')) then
                call fail(res, 403, 'writing issues needs the admin or write scope')
                return
            end if
            call issue_api(db, req%method, target, repo_path(target), sub, req%query, req%body, &
                who%account, index(' '//who%scopes//' ', ' admin ') > 0, code, answer)
            res%code = code
            res%body = answer
        end subroutine issues

        subroutine changes(target, sub)
            !! /api/v1/repos/NAME/changes...: see fy_change; landing a change
            !! queues its branch like POST /land.
            character(len=*), intent(in) :: target, sub
            character(len=:), allocatable :: answer, ctype, branch
            integer :: code
            logical :: is_land

            inquire (file=repo_path(target)//'/HEAD', exist=exists)
            if (.not. repo_name_ok(target) .or. .not. exists) then
                call fail(res, 404, 'no such repository')
                return
            end if
            ! Landing is the land scope's and is checked in change_api.
            is_land = .false.
            if (len(sub) > 5) is_land = sub(len(sub) - 4:) == '/land'
            if (req%method == 'POST' .and. .not. is_land .and. .not. has_scope(who, 'write')) then
                call fail(res, 403, 'changing a change needs the admin or write scope')
                return
            end if
            call change_api(db, req%method, target, repo_path(target), sub, req%query, req%body, &
                who%account, index(' '//who%scopes//' ', ' admin ') > 0, code, answer, ctype, &
                branch, has_scope(who, 'land'))
            if (len(branch) > 0) then
                call change_by_branch(db, target, branch, ch, has_change)
                if (.not. queued(target, branch, has_change, number)) return
                write (num, '(i0)') number
                answer = '{"landing":'//trim(num)//',"state":"queued"}'
            end if
            res%code = code
            res%ctype = ctype
            res%body = answer
        end subroutine changes

        logical function queued(target, branch, has_change, number)
            !! Queues the landing of `branch`, of the change `ch` if it has
            !! one, as `number`, when the policy of the repository lets it;
            !! else the answer says why not.
            character(len=*), intent(in) :: target, branch
            logical, intent(in) :: has_change
            integer(int64), intent(out) :: number
            character(len=:), allocatable :: refusal
            integer(int64) :: change

            change = 0
            if (has_change) change = ch%id
            call policy_queue(db, target, branch, who%account, change, number, refusal, stat, &
                who%session)
            queued = stat == 0 .and. len(refusal) == 0
            if (len(refusal) > 0) then
                call fail(res, 409, refusal)
            else if (stat /= 0) then
                call fail(res, 500, 'landing not queued')
            else
                call events_notify('jobs')
            end if
        end function queued

        subroutine policy_route(target)
            !! GET /api/v1/repos/NAME/policy: the policy of the repository.
            !! PUT the same with a JSON object of settings (fy_policy): sets
            !! those and keeps the others; answers with the policy.
            character(len=*), intent(in) :: target
            type(policy_t) :: policy
            character(len=:), allocatable :: error
            logical :: ok

            call policy_get(db, target, policy, ok)
            if (.not. repo_name_ok(target) .or. .not. ok) then
                call fail(res, 404, 'no such repository')
                return
            end if
            if (req%method == 'PUT') then
                if (index(' '//who%scopes//' ', ' admin ') == 0) then
                    call fail(res, 403, 'changing a repository policy needs the admin scope')
                    return
                end if
                call policy_from_json(policy, req%body, error)
                if (len(error) > 0) then
                    call fail(res, 400, error)
                    return
                end if
                call policy_put(db, target, policy, ok)
                ! The review agent may have open changes to review now.
                call events_notify('jobs')
            else if (req%method /= 'GET') then
                call fail(res, 405, 'use GET or PUT')
                return
            end if
            res%body = policy_json(target, policy)
        end subroutine policy_route

        subroutine list_landings(target)
            character(len=*), intent(in) :: target
            type(landing_t), allocatable :: ls(:)
            character(len=24) :: num
            integer :: k

            call store_landing_list(db, target, ls, stat)
            res%body = '{"landings":['
            do k = 1, size(ls)
                write (num, '(i0)') ls(k)%id
                if (k > 1) res%body = res%body//','
                res%body = res%body//'{"landing":'//trim(num)//',"branch":"'// &
                    json_escape_string(ls(k)%branch)//'","account":"'// &
                    json_escape_string(ls(k)%account)//'","state":"'//ls(k)%state// &
                    '","created":"'//ls(k)%created//'","finished":"'//ls(k)%finished// &
                    '","commit":"'//ls(k)%commit//'","log":"'//json_escape_string(ls(k)%log)// &
                    '","session":'//text_or_null(ls(k)%session)//'}'
            end do
            res%body = res%body//']}'
        end subroutine list_landings

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
            call audit_append(db, who%account, 'repo.mode', target, mode)
            if (mode == 'primary') call land_hook_install(repo_path(target), status)
            res%body = '{"name":"'//json_escape_string(target)//'","mode":"'//mode//'"}'
        end subroutine set_mode

        subroutine set_number(target)
            !! POST /api/v1/repos/NAME/numbers {"next":"N"}: the next change or
            !! issue takes the number N, and later ones count on from it.
            character(len=*), intent(in) :: target
            character(len=:), allocatable :: text
            integer :: next, ios
            logical :: ok

            if (index(' '//who%scopes//' ', ' admin ') == 0) then
                call fail(res, 403, 'setting the numbers of a repository needs the admin scope')
                return
            end if
            call json_extract_string(req%body, 'next', text, ok)
            next = 0
            ios = 1
            if (ok) then
                if (len(text) > 0 .and. len(text) < 10 .and. verify(text, '0123456789') == 0) &
                    read (text, *, iostat=ios) next
            end if
            if (ios /= 0 .or. next < 1) then
                call fail(res, 400, 'body must be {"next":"N"} with a positive number')
                return
            end if
            call store_repo_set_number(db, target, next, ok)
            if (.not. ok) then
                call fail(res, 404, 'no such repository')
                return
            end if
            res%body = '{"name":"'//json_escape_string(target)//'","next":'//text//'}'
        end subroutine set_number

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
        !! Serves `nthreads` connections at once, each in its own
        !! single-threaded process. Not in OpenMP threads: gfortran keeps the
        !! length of every deferred-length character function result in a
        !! static variable, so threads calling such functions at once corrupt
        !! each other's strings and the heap (#91).
        integer, intent(in) :: lfd, nthreads
        character(len=*), intent(in) :: db_path
        integer :: pids(nthreads), i, pid, status, me
        integer(int64) :: started(nthreads), now, rate

        me = proc_pid()
        do i = 1, nthreads
            call start(i)
        end do
        do
            call proc_wait_any(pid, status)
            if (pid <= 0) then
                call proc_sleep_ms(200)
                cycle
            end if
            do i = 1, nthreads
                if (pids(i) /= pid) cycle
                write (error_unit, '(a,i0,a,i0)') 'fy: server process ', pid, &
                    ' ended with wait status ', status
                call system_clock(now, rate)
                if (now - started(i) < rate) call proc_sleep_ms(1000)
                call start(i)
            end do
        end do

    contains

        subroutine start(slot)
            integer, intent(in) :: slot
            type(db_t) :: db
            integer :: stat

            do
                flush (error_unit)
                call system_clock(started(slot), rate)
                pids(slot) = proc_fork()
                if (pids(slot) >= 0) exit
                call proc_sleep_ms(1000)
            end do
            if (pids(slot) /= 0) return
            if (.not. proc_die_with_parent(me)) call proc_exit(0)
            call store_open(db_path, db, stat)
            if (stat /= 0) then
                write (error_unit, '(a)') 'fy: worker cannot open the database'
                flush (error_unit)
                call proc_exit(1)
            end if
            call server_loop(lfd, db)
            call proc_exit(0)
        end subroutine start

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
        character(len=:), allocatable :: rest, repo, version, namespace, key
        integer :: status
        logical :: write

        call http_read_head(cfd, req, rest, status)
        if (status < 0) return
        ! Behind a proxy that keeps the prefix, requests arrive with it.
        if (len(b()) > 0 .and. status == 0) then
            if (req%path == b()) then
                req%path = '/'
            else if (index(req%path, b()//'/') == 1) then
                req%path = req%path(len(b()) + 1:)
            end if
        end if
        if (status == 0) then
            if (git_http_route(req%path, req%query, repo, write)) then
                call git_route(cfd, req, rest, db, repo, write)
                return
            end if
            if (req%path == '/api/v1/events' .and. req%method == 'GET') then
                call events_route(cfd, req, db)
                return
            end if
            if (release_upload_route(req, repo, version)) then
                call release_upload(cfd, req, rest, db, repo, version)
                return
            end if
            if (allocated(repos_dir) .and. cache_route(req, namespace, key)) then
                call cache_serve(cfd, req, rest, db, repos_dir//'/..', namespace, key)
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

    logical function release_upload_route(req, repo, version)
        !! PUT /api/v1/repos/NAME/releases/VERSION/release.tar
        type(http_request_t), intent(in) :: req
        character(len=:), allocatable, intent(out) :: repo, version
        character(len=*), parameter :: PREFIX = '/api/v1/repos/', SUFFIX = '/release.tar'
        character(len=:), allocatable :: mid
        integer :: k

        release_upload_route = .false.
        repo = ''
        version = ''
        if (req%method /= 'PUT' .or. index(req%path, PREFIX) /= 1) return
        if (len(req%path) <= len(PREFIX) + len(SUFFIX)) return
        if (req%path(len(req%path) - len(SUFFIX) + 1:) /= SUFFIX) return
        mid = req%path(len(PREFIX) + 1:len(req%path) - len(SUFFIX))
        k = index(mid, '/releases/')
        if (k < 2) return
        repo = mid(1:k - 1)
        version = mid(k + 10:)
        release_upload_route = repo_name_ok(repo) .and. version_ok(version)
    end function release_upload_route

    pure logical function version_ok(v)
        character(len=*), intent(in) :: v
        version_ok = len(v) > 0 .and. len(v) <= 40 .and. &
            verify(v, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-_') == 0
        if (version_ok) version_ok = v(1:1) /= '.'
    end function version_ok

    subroutine release_upload(cfd, req, rest, db, repo, version)
        !! Receives the archive of an open release, streamed to disk, and makes
        !! the release available once its SHA-256 matches X-Content-SHA256.
        integer, intent(in) :: cfd
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: rest, repo, version
        type(db_t), intent(in) :: db
        integer(int64), parameter :: MAX_ARCHIVE = 268435456_int64
        type(token_identity_t) :: who
        type(response_t) :: res
        character(len=:), allocatable :: dir, part
        character(len=65536) :: buf
        character(len=64) :: hex
        integer(int64) :: left
        integer :: u, n, ierr, stat
        logical :: found

        res%code = 201
        res%ctype = JSON
        res%headers = ''
        res%cache = 'no-store'
        res%body = ''
        call store_token_lookup(db, http_bearer_token(req), who)
        if (.not. who%found) then
            call fail(res, 401, 'missing or invalid token')
        else if (index(' '//who%scopes//' ', ' admin ') == 0 .and. &
                index(' '//who%scopes//' ', ' release ') == 0) then
            call fail(res, 403, 'uploading releases needs the release or admin scope')
        else if (req%chunked .or. .not. req%has_length) then
            call fail(res, 411, 'send Content-Length')
        else if (int(req%content_length, int64) > MAX_ARCHIVE) then
            call fail(res, 413, 'release archives are limited to 256 MiB')
        else if (len(req%content_sha256) /= 64) then
            call fail(res, 400, 'send X-Content-SHA256')
        else
            dir = releases_dir()//'/'//repo//'/'//version
            call execute_command_line('mkdir -p '''//dir//'''', exitstat=stat)
            part = dir//'/release.tar.part'
            open (newunit=u, file=part, status='replace', access='stream', form='unformatted', &
                action='write', iostat=ierr)
            if (ierr /= 0) then
                call fail(res, 500, 'release not stored')
            else
                left = int(req%content_length, int64)
                n = int(min(int(len(rest), int64), left))
                if (n > 0) write (u) rest(1:n)
                left = left - n
                do while (left > 0)
                    n = sock_recv(cfd, buf)
                    if (n <= 0) exit
                    n = int(min(int(n, int64), left))
                    write (u) buf(1:n)
                    left = left - n
                end do
                close (u)
                call sha256_file(part, hex, ierr, max_bytes=int(MAX_ARCHIVE))
                if (left /= 0 .or. ierr /= 0 .or. hex /= req%content_sha256) then
                    call execute_command_line('rm -f '''//part//'''')
                    call fail(res, 422, 'the archive is incomplete or its checksum differs')
                else
                    call store_release_ready(db, repo, version, hex, &
                        int(req%content_length, int64), found)
                    if (.not. found) then
                        call execute_command_line('rm -f '''//part//'''')
                        call fail(res, 409, 'no open release '//version//' of '//repo)
                    else
                        call execute_command_line('mv -f '''//part//''' '''//dir// &
                            '/release.tar''')
                        call audit_append(db, who%account, 'release.uploaded', &
                            repo//' '//version, 'sha256 '//hex)
                        res%body = '{"version":"'//version//'","sha256":"'//hex// &
                            '","state":"ready"}'
                    end if
                end if
            end if
        end if
        call http_send_response(cfd, res%code, res%ctype, res%body, res%headers, res%cache)
        write (error_unit, '(a,1x,a,1x,i0)') req%method, req%path, res%code
        flush (error_unit)
    end subroutine release_upload

    function releases_dir() result(path)
        character(len=:), allocatable :: path
        path = repos_dir//'/../releases'
    end function releases_dir

    subroutine events_route(cfd, req, db)
        !! GET /api/v1/events?topic=jobs: a server-sent event stream that
        !! announces each change of the topic, for pages that show it live.
        integer, intent(in) :: cfd
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(token_identity_t) :: who
        character(len=:), allocatable :: token, topic

        token = http_bearer_token(req)
        if (len(token) == 0) token = http_cookie(req, SESSION_COOKIE)
        call store_token_lookup(db, token, who)
        topic = http_query_param(req%query, 'topic')
        if (.not. who%found) then
            call http_send_response(cfd, 401, JSON, '{"error":"missing or invalid token"}')
        else if (topic /= 'jobs') then
            call http_send_response(cfd, 404, JSON, '{"error":"no such topic"}')
        else
            ! Holds this thread until the client leaves; it sleeps in poll(2).
            call events_stream(cfd, topic)
        end if
        write (error_unit, '(a,1x,a)') req%method, req%path
        flush (error_unit)
    end subroutine events_route

    subroutine git_route(cfd, req, rest, db, repo, write)
        !! Clone, fetch and push over HTTP. Git sends the access token as the
        !! Basic password (any user name) or as a bearer token. Reading needs a
        !! valid token; pushing needs the write or admin scope and a primary
        !! repository.
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
            else if (write .and. .not. has_scope(who, 'write')) then
                status = 403
            else if (write .and. mode /= 'primary') then
                ! A mirror follows its upstream; pushes go there.
                status = 409
            else
                call git_http_serve(cfd, req, rest, repos_dir, repo, who%account, status, &
                    admin=has_scope(who, 'admin'))
                ! A push opens changes for new branches and versions for moved ones.
                if (write .and. status == 0 .and. index(req%path, '/git-receive-pack') > 0) &
                    call change_sync(db, repo, repo_path(repo), who%account, who%session)
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
        case (403); s = 'Pushing needs the write or admin scope.'
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
        ! Exact paths, then prefixes, each sorted by path; no two match one request.
        select case (req%path)
        case ('/', '/index.html')
            if (.not. method_is(req, 'GET', res)) return
            res%code = 404
            if (allocated(web_dir)) call render_page(req, db, res)
            if (res%code == 404) then
                res%code = 200
                res%ctype = 'text/html; charset=utf-8'
                res%body = status_page()
            end if
        case ('/-/login/callback')
            if (.not. method_is(req, 'GET', res)) return
            call login_finish(req, db, res)
        case ('/api/v1/changes')
            ! The changes of every repository, as GET /api/v1/repos/R/changes.
            if (.not. method_is(req, 'GET', res)) return
            if (.not. authenticated(req, db, who, res)) return
            res%body = change_list_json(db, '', req%query, res%code)
        case ('/api/v1/session')
            if (req%method == 'POST') then
                call open_session(req, db, res)
            else if (req%method == 'DELETE') then
                call close_session(req, db, res)
            else
                call fail(res, 405, 'use POST or DELETE')
            end if
        case ('/api/v1/stats')
            if (.not. method_is(req, 'GET', res)) return
            if (.not. authenticated(req, db, who, res)) return
            call stats_route(req, db, http_query_param(req%query, 'repo'), res)
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
        case ('/api/v1/version')
            if (.not. method_is(req, 'GET', res)) return
            res%body = version_json()
        case ('/api/v1/whoami')
            if (.not. method_is(req, 'GET', res)) return
            if (.not. authenticated(req, db, who, res)) return
            res%body = '{"account":"'//json_escape_string(who%account)// &
                '","token":"'//json_escape_string(who%token_name)// &
                '","session":"'//json_escape_string(who%session)// &
                '","scopes":"'//json_escape_string(who%scopes)//'"}'
        case ('/healthz')
            if (.not. method_is(req, 'GET', res)) return
            res%ctype = 'text/plain; charset=utf-8'
            res%body = 'ok '//FY_VERSION//' '//FY_COMMIT//new_line('a')
        case ('/login')
            if (.not. method_is(req, 'GET', res)) return
            call login_start(res)
        case ('/status')
            if (.not. method_is(req, 'GET', res)) return
            res%ctype = 'text/html; charset=utf-8'
            res%body = status_page()
        case default
            if (req%path == '/api/v1/audit' .or. req%path == '/api/v1/audit/verify') then
                call audit_route(req, db, res)
            else if (index(req%path, '/api/v1/checks/') == 1) then
                call checks_route(req, db, res)
            else if (req%path == '/api/v1/jobs' .or. index(req%path, '/api/v1/jobs/') == 1) then
                call jobs_route(req, db, res)
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
            else if (index(req%path, '/app/') == 1) then
                if (.not. method_is(req, 'GET', res)) return
                call static_file(req, req%path(6:), res)
            else
                call fail(res, 404, 'not found')
                if (route_is_page(req%path) .and. allocated(web_dir) .and. req%method == 'GET') &
                    call render_page(req, db, res)
            end if
        end select
    end subroutine route

    subroutine runner_check(repo, ref, passed, log)
        !! Queues a check of `ref` for a runner and sleeps until it reports. A
        !! check no runner takes within an hour fails. A check whose runner has
        !! not reported for twice the time limit of its claim (a failure is
        !! run again from an empty build directory) plus 5 minutes is taken
        !! from that runner, lost, and queued again; a check that reached its
        !! time limit twice fails with the reason timeout.
        character(len=*), intent(in) :: repo, ref
        logical, intent(out) :: passed
        character(len=:), allocatable, intent(out) :: log
        character(len=:), allocatable :: state
        character(len=16) :: minutes
        integer(int64) :: id
        integer :: stat, age, fd, limit
        logical :: changed, found

        passed = .false.
        if (.not. checks_mode_set()) then
            log = 'No runner mode.'
            return
        end if
        fd = events_watch_open()
        call store_check_add(lander_db, repo, ref, id, stat)
        if (stat /= 0) then
            log = 'The check could not be queued.'
            call events_watch_close(fd)
            return
        end if
        if (allocated(candidate_jobs)) call store_landing_check(lander_db, candidate_jobs, id)
        call events_notify('checks')
        ! The pages of the jobs show the check while it waits for a runner.
        call events_notify('jobs')
        do
            call store_check_get(lander_db, id, state, log, age, limit)
            if (state == 'passed' .or. state == 'failed') exit
            write (minutes, '(i0)') limit/60
            if (state == 'timeout') then
                log = 'Reason: timeout. The check reached its time limit (check_timeout '// &
                    trim(minutes)//' minutes) a second time; the runner ended it, which is'// &
                    ' no verdict on the change.'//new_line('a')//log
                exit
            end if
            if (state == 'queued' .and. age > 3600) then
                log = 'No runner took the check within an hour.'
                exit
            end if
            if (state == 'running' .and. limit > 0 .and. age > 2*limit + LOST_GRACE) then
                call store_check_error(lander_db, id, 'Runner error: the runner did not report'// &
                    ' within twice the time limit ('//trim(minutes)//' minutes) plus 5 minutes'// &
                    ' and is taken as lost; the check is queued again.', .false., state, found, &
                    lost_grace=LOST_GRACE)
                if (found) then
                    call events_notify('checks')
                    cycle
                end if
            end if
            ! Wakes on a runner's report; the timeout only enforces the limits above.
            changed = events_wait(fd, 'checks', 60000)
        end do
        call events_watch_close(fd)
        passed = state == 'passed'
    end subroutine runner_check

    logical function checks_mode_set()
        checks_mode_set = allocated(checks_mode)
    end function checks_mode_set

    subroutine checks_route(req, db, res)
        !! POST /api/v1/checks/claim: the next queued check for the calling
        !! runner (200 with {"id","repo","ref","kind","timeout"}, timeout in
        !! seconds and 0 for none, or 204 when there is none).
        !! POST /api/v1/checks/N/result {"passed":true|false,"log":"..."}, or
        !! {"error":"timeout"|"runner","log":"..."} for a check that ended
        !! without a verdict: it is queued again, at its second timeout it
        !! ends in the state timeout. Only the runner that holds the check
        !! reports it; a runner taken as lost gets 409.
        !! POST /api/v1/checks/release: the calling runner's running checks
        !! are queued again (it restarted).
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        type(token_identity_t) :: who
        character(len=:), allocatable :: repo, ref, log, kind, error, state
        character(len=24) :: num, limit
        integer(int64) :: id
        integer :: ios, k, seconds
        logical :: found, ok

        if (.not. authenticated(req, db, who, res)) return
        if (.not. method_is(req, 'POST', res)) return
        if (index(' '//who%scopes//' ', ' admin ') == 0 .and. &
            index(' '//who%scopes//' ', ' runner ') == 0) then
            call fail(res, 403, 'checks need the runner or admin scope')
            return
        end if
        if (req%path == '/api/v1/checks/release') then
            call store_check_release(db, who%token_name, k)
            if (k > 0) call events_notify('checks')
            ! Their jobs no longer name the runner that restarted.
            if (k > 0) call events_notify('jobs')
            write (num, '(i0)') k
            res%body = '{"released":'//trim(num)//'}'
            return
        end if
        if (req%path == '/api/v1/checks/claim') then
            call store_check_claim(db, who%token_name, id, repo, ref, found, kind, seconds)
            if (.not. found) then
                res%code = 204
                res%body = ''
                return
            end if
            call events_notify('checks')
            ! A job shows its check and runner: its pages ask again.
            call events_notify('jobs')
            write (num, '(i0)') id
            write (limit, '(i0)') seconds
            res%body = '{"id":'//trim(num)//',"repo":"'//repo//'","ref":"'//ref//'","kind":"'// &
                kind//'","timeout":'//trim(limit)//'}'
            return
        end if
        k = index(req%path, '/result')
        if (index(req%path, '/api/v1/checks/') /= 1 .or. k == 0) then
            call fail(res, 404, 'not found')
            return
        end if
        read (req%path(16:k - 1), *, iostat=ios) id
        if (ios /= 0 .or. verify(req%path(16:k - 1), '0123456789') /= 0) then
            call fail(res, 404, 'not found')
            return
        end if
        ok = index(req%body, '"passed":true') > 0
        call json_extract_string(req%body, 'log', log, found)
        if (.not. found) log = ''
        call json_extract_string(req%body, 'error', error, found)
        if (found) then
            if (error /= 'timeout' .and. error /= 'runner') then
                call fail(res, 400, 'error is timeout or runner')
                return
            end if
            call store_check_error(db, id, log, error == 'timeout', state, found, &
                runner=who%token_name)
        else
            state = merge('passed', 'failed', ok)
            call store_check_finish(db, id, who%token_name, state, log, found)
        end if
        if (.not. found) then
            call fail(res, 409, 'no running check with that number')
            return
        end if
        call events_notify('checks')
        res%body = '{"recorded":true,"state":"'//state//'"}'
    end subroutine checks_route

    subroutine jobs_route(req, db, res)
        !! GET /api/v1/jobs[?repo=NAME][&kind=landing|test|verify][&state=STATE]:
        !! running and queued jobs, recent finished ones and counts per
        !! repository ("checks": the runner checks it holds now); state=all is no
        !! filter. Pages learn of changes through
        !! /api/v1/events?topic=jobs.
        !! GET /api/v1/jobs/N: one job with its log and the entry that followed
        !! it for the same change.
        !! A job names its change ("change" is the number, "title" its title),
        !! which of that change's entries it is, and its checks, one per
        !! attempt. What no row records is null.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        type(token_identity_t) :: who
        type(landing_t), allocatable :: ls(:)
        type(landing_t) :: l
        character(len=64), allocatable :: names(:)
        integer, allocatable :: counts(:, :)
        character(len=:), allocatable :: repo, state, kind, only
        character(len=24) :: num
        integer(int64) :: id
        integer :: k, stat, ios, totals(5)
        logical :: found

        if (.not. authenticated(req, db, who, res)) return
        if (.not. method_is(req, 'GET', res)) return
        if (index(req%path, '/api/v1/jobs/') == 1) then
            read (req%path(14:), *, iostat=ios) id
            if (ios /= 0 .or. verify(req%path(14:), '0123456789') /= 0) then
                call fail(res, 404, 'not found')
                return
            end if
            call store_job(db, id, l, found)
            if (.not. found) then
                call fail(res, 404, 'no such job')
                return
            end if
            res%body = job_json(l, .true.)
            return
        end if
        repo = http_query_param(req%query, 'repo')
        if (len(repo) > 0 .and. .not. repo_name_ok(repo)) then
            call fail(res, 400, 'bad repository name')
            return
        end if
        kind = http_query_param(req%query, 'kind')
        if (len(kind) > 0 .and. kind /= 'landing' .and. kind /= 'test' .and. kind /= 'verify') then
            call fail(res, 400, 'kind is landing, test or verify')
            return
        end if
        only = http_query_param(req%query, 'state')
        if (verify(only, 'abcdefghijklmnopqrstuvwxyz') /= 0) then
            call fail(res, 400, 'bad state')
            return
        end if
        state = store_jobs_state(db)
        call store_job_counts(db, names, counts, stat)
        call store_jobs(db, repo, ls, stat, kind=kind, state=only)
        totals = 0
        res%body = '{"state":"'//json_escape_string(state)//'","repos":['
        do k = 1, size(names)
            totals = totals + counts(:, k)
            if (k > 1) res%body = res%body//','
            res%body = res%body//'{"name":"'//trim(names(k))//'","running":'// &
                itoa(counts(1, k))//',"queued":'//itoa(counts(2, k))//',"landed":'// &
                itoa(counts(3, k))//',"failed":'//itoa(counts(4, k))//',"checks":'// &
                itoa(counts(5, k))//'}'
        end do
        res%body = res%body//'],"totals":{"running":'//itoa(totals(1))//',"queued":'// &
            itoa(totals(2))//',"landed":'//itoa(totals(3))//',"failed":'//itoa(totals(4))// &
            '},"jobs":['
        do k = 1, size(ls)
            if (k > 1) res%body = res%body//','
            res%body = res%body//job_json(ls(k), .false.)
        end do
        res%body = res%body//']}'

    contains

        function job_json(j, with_log) result(s)
            type(landing_t), intent(in) :: j
            logical, intent(in) :: with_log
            character(len=:), allocatable :: s, runner, next_state
            type(check_t), allocatable :: checks(:)
            integer(int64) :: next
            integer :: c

            write (num, '(i0)') j%id
            s = '{"id":'//trim(num)//',"kind":"'//j%kind//'","repo":"'//j%repo//'","branch":"'// &
                json_escape_string(j%branch)//'","account":"'//json_escape_string(j%account)// &
                '","state":"'//j%state//'","queued":"'//j%created//'","started":"'// &
                j%started//'","finished":"'//j%finished//'","commit":"'//j%commit//'"'
            ! Version, reason and candidate have no column yet; their
            ! members are there so that what fills them changes, not the form.
            s = s//',"change":'//number(j%number, 1)//',"title":'//string(j%title, j%number > 0)// &
                ',"version":null,"reason":null,"session":'//text_or_null(j%session)// &
                ',"candidate":null,"entry":'// &
                number(j%entry, 1)//',"entries":'//number(j%entries, 1)
            call store_job_checks(db, j, checks)
            runner = ''
            if (size(checks) > 0) runner = checks(size(checks))%runner
            s = s//',"runner":'//string(runner, len(runner) > 0)//',"checks":['
            do c = 1, size(checks)
                if (c > 1) s = s//','
                write (num, '(i0)') checks(c)%id
                s = s//'{"id":'//trim(num)//',"attempt":'//itoa(c)//',"kind":"'//j%kind// &
                    '","state":"'//checks(c)%state//'","runner":'// &
                    string(checks(c)%runner, len(checks(c)%runner) > 0)//',"queued":"'// &
                    checks(c)%created//'","started":'// &
                    string(checks(c)%claimed, len(checks(c)%claimed) > 0)//',"finished":'// &
                    string(checks(c)%finished, len(checks(c)%finished) > 0)//',"wait":'// &
                    number(checks(c)%wait, 0)//',"run":'//number(checks(c)%run, 0)// &
                    ',"tree":null,"command":null,"image":null,"fo_version":null,'// &
                    '"exit_status":null}'
            end do
            s = s//']'
            if (with_log) then
                call store_job_next(db, j%id, next, next_state)
                if (next > 0) then
                    write (num, '(i0)') next
                    s = s//',"next":{"id":'//trim(num)//',"state":"'//next_state//'"}'
                else
                    s = s//',"next":null'
                end if
                s = s//',"error_line":'//number(error_line(j%log), 1)//',"log":"'// &
                    json_escape_string(j%log)//'"'
            end if
            s = s//'}'
        end function job_json

        integer function error_line(log) result(n)
            !! The number of the first line of `log` that contains Error or
            !! FAIL, where a reader of a failed job starts; 0 without one.
            character(len=*), intent(in) :: log
            integer :: at, fail, k

            at = index(log, 'Error')
            fail = index(log, 'FAIL')
            if (at == 0 .or. (fail > 0 .and. fail < at)) at = fail
            n = 0
            if (at == 0) return
            n = 1
            do k = 1, at - 1
                if (log(k:k) == new_line('a')) n = n + 1
            end do
        end function error_line

        function number(v, least) result(s)
            !! `v`, or null when it is below `least` (not recorded).
            integer, intent(in) :: v, least
            character(len=:), allocatable :: s
            s = 'null'
            if (v >= least) s = itoa(v)
        end function number

        function string(v, recorded) result(s)
            character(len=*), intent(in) :: v
            logical, intent(in) :: recorded
            character(len=:), allocatable :: s
            s = 'null'
            if (recorded) s = '"'//json_escape_string(v)//'"'
        end function string

        function itoa(v) result(s)
            integer, intent(in) :: v
            character(len=:), allocatable :: s
            character(len=16) :: b
            write (b, '(i0)') v
            s = trim(b)
        end function itoa

    end subroutine jobs_route

    subroutine stats_route(req, db, repo, res, named)
        !! GET /api/v1/stats[?repo=NAME][&days=1|7|30] and
        !! GET /api/v1/repos/NAME/board?stats=1[&days=]: the queue numbers
        !! (fy_stats) of a repository, or of the instance without one, over 7
        !! days unless `days` says otherwise. `named`: the path names the
        !! repository, so there must be one.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(response_t), intent(inout) :: res
        logical, intent(in), optional :: named
        type(policy_t) :: policy
        character(len=:), allocatable :: text
        integer :: days, ios
        logical :: found

        found = len(repo) == 0
        if (present(named)) found = found .and. .not. named
        if (len(repo) > 0 .and. repo_name_ok(repo)) call policy_get(db, repo, policy, found)
        if (.not. found) then
            call fail(res, 404, 'no such repository')
            return
        end if
        days = 7
        text = http_query_param(req%query, 'days')
        if (len(text) > 0) then
            ios = 1
            if (len(text) <= 2 .and. verify(text, '0123456789') == 0) &
                read (text, *, iostat=ios) days
            if (ios /= 0 .or. .not. stats_days_ok(days)) then
                call fail(res, 400, 'days is 1, 7 or 30')
                return
            end if
        end if
        res%body = stats_json(db, repo, days)
    end subroutine stats_route

    subroutine audit_route(req, db, res)
        !! GET /api/v1/audit[?before=ID][&limit=N]: the audit log, newest entry
        !! first, at most N (50, up to 200) entries older than entry ID; "next"
        !! is the "before" of the following page, or 0 after the oldest entry.
        !! GET /api/v1/audit/verify: recomputes the chain; "broken" is the first
        !! entry that does not fit, or 0. Both need the admin scope.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        type(token_identity_t) :: who
        type(audit_t), allocatable :: rows(:)
        character(len=:), allocatable :: text, reason, head
        character(len=24) :: num, total
        integer(int64) :: before, next, count, broken
        integer :: limit, ios, k

        if (.not. authenticated(req, db, who, res)) return
        if (.not. method_is(req, 'GET', res)) return
        if (index(' '//who%scopes//' ', ' admin ') == 0) then
            call fail(res, 403, 'the audit log needs the admin scope')
            return
        end if
        if (req%path == '/api/v1/audit/verify') then
            call audit_verify(db, count, broken, reason, head)
            write (total, '(i0)') count
            write (num, '(i0)') broken
            res%body = '{"intact":'//json_bool(broken == 0)//',"checked":'//trim(total)// &
                ',"broken":'//trim(num)//',"reason":"'//json_escape_string(reason)// &
                '","head":"'//head//'"}'
            return
        end if
        before = 0
        limit = 50
        ios = 0
        text = http_query_param(req%query, 'before')
        if (len(text) > 0) then
            ios = 1
            if (len(text) < 19 .and. verify(text, '0123456789') == 0) &
                read (text, *, iostat=ios) before
        end if
        text = http_query_param(req%query, 'limit')
        if (len(text) > 0 .and. ios == 0) then
            ios = 1
            if (len(text) < 10 .and. verify(text, '0123456789') == 0) &
                read (text, *, iostat=ios) limit
        end if
        if (ios /= 0 .or. limit < 1 .or. limit > 200) then
            call fail(res, 400, 'before is an entry number and limit is 1 to 200')
            return
        end if
        call audit_list(db, before, limit, rows, next)
        res%body = '{"entries":['
        do k = 1, size(rows)
            if (k > 1) res%body = res%body//','
            res%body = res%body//audit_json(rows(k))
        end do
        write (num, '(i0)') next
        res%body = res%body//'],"next":'//trim(num)//'}'
    end subroutine audit_route

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

    logical function has_scope(who, scope)
        !! Whether the token carries `scope`; admin implies every scope.
        type(token_identity_t), intent(in) :: who
        character(len=*), intent(in) :: scope
        has_scope = index(' '//who%scopes//' ', ' '//scope//' ') > 0 .or. &
            index(' '//who%scopes//' ', ' admin ') > 0
    end function has_scope

    function text_or_null(s) result(json)
        !! A JSON string, or null for a member that was not recorded.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: json
        if (len(s) == 0) then
            json = 'null'
        else
            json = '"'//json_escape_string(s)//'"'
        end if
    end function text_or_null

    subroutine create_token(req, db, who, res)
        !! An admin token mints tokens for its own account with the scopes named
        !! in "scopes" (separated by spaces or commas), or with its own, so a
        !! runner on another host carries only the runner scope. No other token
        !! mints any: a name is taken for good, also after revocation, so a
        !! machine token could otherwise take the name of a later session.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: name, token, scopes, word
        logical :: found
        integer :: stat, at, space, i

        if (.not. has_scope(who, 'admin')) then
            call fail(res, 403, 'creating tokens needs the admin scope')
            return
        end if
        call json_extract_string(req%body, 'name', name, found)
        if (.not. found .or. len(name) == 0 .or. len(name) > 100) then
            call fail(res, 400, 'body must be {"name":"..."}')
            return
        end if
        ! A blank at either end would make a name that looks like another.
        if (iachar(name(1:1)) <= 32 .or. iachar(name(len(name):len(name))) <= 32) then
            call fail(res, 400, 'a token name has no blank at its start or end')
            return
        end if
        if (index(to_lower(name), SESSION_NAME) == 1) then
            call fail(res, 400, 'the name '//SESSION_NAME//' is kept for browser sessions')
            return
        end if
        call json_extract_string(req%body, 'scopes', scopes, found)
        if (.not. found) then
            scopes = who%scopes
        else
            if (len(scopes) > 200) scopes = ''
            scopes = scope_words(scopes)
            if (len(scopes) == 0) then
                call fail(res, 400, '"scopes" must name at least one scope, in lower-case'// &
                    ' letters, digits and :_-, separated by spaces or commas')
                return
            end if
            at = 1
            do while (at <= len(scopes))
                space = index(scopes(at:), ' ')
                if (space == 0) then
                    word = scopes(at:)
                    at = len(scopes) + 1
                else
                    word = scopes(at:at + space - 2)
                    at = at + space
                end if
                ! A mistyped scope would give a token that may do nothing.
                if (findloc(KNOWN_SCOPES, word, 1) == 0) then
                    word = 'there is no scope '//word//'; the scopes are'
                    do i = 1, size(KNOWN_SCOPES)
                        word = word//' '//trim(KNOWN_SCOPES(i))
                    end do
                    call fail(res, 400, word)
                    return
                end if
            end do
        end if
        ! Versions, landings and jobs record only the name, so it must name one
        ! token, also after that one is revoked.
        call store_token_create(db, who%account_id, name, scopes, token, stat, unique=.true.)
        if (stat == 2) then
            call fail(res, 409, 'a token named '//name//' exists; the name identifies'// &
                ' the session in every list, so choose another')
            return
        else if (stat /= 0) then
            call fail(res, 500, 'token not created')
            return
        end if
        call audit_append(db, who%account, 'token.created', name, 'scopes '//scopes)
        res%code = 201
        res%body = '{"token":"'//token//'","name":"'//json_escape_string(name)// &
            '","scopes":"'//json_escape_string(scopes)//'"}'
    end subroutine create_token

    pure function scope_words(text) result(list)
        !! The scopes named in `text`, separated by single spaces as they are
        !! stored and matched; empty when `text` names none or holds a
        !! character no scope has.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: list
        integer :: i
        logical :: gap

        list = ''
        gap = .false.
        do i = 1, len(text)
            if (text(i:i) == ' ' .or. text(i:i) == ',') then
                gap = len(list) > 0
            else if (verify(text(i:i), 'abcdefghijklmnopqrstuvwxyz0123456789:_-') /= 0) then
                list = ''
                return
            else
                if (gap) list = list//' '
                gap = .false.
                list = list//text(i:i)
            end if
        end do
    end function scope_words

    pure logical function own_token(who, t)
        !! Whether `t` is the caller's token or a browser session opened with it.
        type(token_identity_t), intent(in) :: who
        type(token_info_t), intent(in) :: t
        own_token = t%id == who%origin_id .or. t%parent == who%origin_id
    end function own_token

    subroutine list_tokens(db, who, res)
        !! The tokens of the account for an admin token; any other token sees
        !! itself and its browser sessions.
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        type(response_t), intent(inout) :: res
        type(token_info_t), allocatable :: tokens(:)
        character(len=24) :: num
        integer :: i, stat
        logical :: all

        call store_token_list(db, who%account_id, tokens, stat)
        if (stat /= 0) then
            call fail(res, 500, 'tokens not listed')
            return
        end if
        all = has_scope(who, 'admin')
        res%body = '{"tokens":['
        do i = 1, size(tokens)
            if (.not. all .and. .not. own_token(who, tokens(i))) cycle
            write (num, '(i0)') tokens(i)%id
            if (res%body(len(res%body):) /= '[') res%body = res%body//','
            res%body = res%body//'{"id":'//trim(num)// &
                ',"name":"'//json_escape_string(tokens(i)%name)// &
                '","scopes":"'//json_escape_string(tokens(i)%scopes)// &
                '","created":"'//tokens(i)%created_at// &
                '","expires":"'//tokens(i)%expires_at// &
                '","revoked":'//json_bool(tokens(i)%revoked)// &
                ',"current":'//json_bool(tokens(i)%id == who%token_id)// &
                ',"expired":'//json_bool(tokens(i)%expired)//',"kind":"'// &
                trim(merge('session', 'token  ', index(tokens(i)%name, SESSION_NAME) == 1))//'"}'
        end do
        res%body = res%body//']}'
    end subroutine list_tokens

    subroutine revoke_token(db, who, id, res)
        !! An admin token revokes any token of the account; any other token
        !! only itself and its browser sessions, so a narrow token cannot lock
        !! the administrator out.
        type(db_t), intent(in) :: db
        type(token_identity_t), intent(in) :: who
        integer(int64), intent(in) :: id
        type(response_t), intent(inout) :: res
        type(token_info_t), allocatable :: tokens(:)
        character(len=24) :: num
        logical :: found
        integer :: i, stat

        if (.not. has_scope(who, 'admin')) then
            call store_token_list(db, who%account_id, tokens, stat)
            found = .false.
            do i = 1, size(tokens)
                if (tokens(i)%id == id) found = own_token(who, tokens(i))
            end do
            if (.not. found) then
                call fail(res, 403, 'revoking another token needs the admin scope')
                return
            end if
        end if
        call store_token_revoke(db, who%account_id, id, found)
        if (.not. found) then
            call fail(res, 404, 'no such token')
            return
        end if
        write (num, '(i0)') id
        call audit_append(db, who%account, 'token.revoked', '#'//trim(num), &
            'by token '//who%token_name)
        res%body = '{"revoked":true}'
    end subroutine revoke_token

    subroutine open_session(req, db, res)
        !! Exchanges a token for a browser session in an HttpOnly cookie, so
        !! scripts on the page never hold a credential. The session acts under
        !! the name of that token and ends with it: it is no token of its own.
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
            stat, days=SESSION_DAYS, parent=who%origin_id)
        if (stat /= 0) then
            call fail(res, 500, 'session not created')
            return
        end if
        call audit_append(db, who%account, 'sign-in', 'session', 'with token '//who%token_name)
        res%headers = 'Set-Cookie: '//SESSION_COOKIE//'='//session// &
            '; Path='//cookie_path()//'; HttpOnly; SameSite=Strict'//secure()// &
            '; Max-Age='//SESSION_MAX_AGE//CRLF
        res%body = '{"account":"'//json_escape_string(who%account)//'"}'
    end subroutine open_session

    subroutine close_session(req, db, res)
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: token
        type(token_identity_t) :: who

        token = http_cookie(req, SESSION_COOKIE)
        if (len(token) > 0) then
            call store_token_lookup(db, token, who)
            call store_token_revoke_hash(db, token)
            if (who%found) call audit_append(db, who%account, 'sign-out', 'session', '')
        end if
        res%headers = 'Set-Cookie: '//SESSION_COOKIE// &
            '=; Path='//cookie_path()//'; HttpOnly; SameSite=Strict'//secure()//'; Max-Age=0'//CRLF
        res%body = '{"closed":true}'
    end subroutine close_session

    subroutine login_start(res)
        !! Sends the browser to the provider. The state, PKCE verifier and nonce
        !! wait in a Lax cookie, which the browser returns on the redirect back.
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: state, verifier, nonce

        if (.not. oidc%enabled) then
            call fail(res, 404, 'sign-in through a provider is not configured')
            return
        end if
        state = random_token()
        verifier = random_token()
        nonce = random_token()
        res%code = 302
        res%ctype = 'text/plain; charset=utf-8'
        res%body = ''
        res%headers = 'Location: '//oidc_authorize_url(oidc, state, verifier, nonce)//CRLF// &
            'Set-Cookie: '//LOGIN_COOKIE//'='//state//'.'//verifier//'.'//nonce// &
            '; Path='//b()//'/-/login; HttpOnly; SameSite=Lax'//secure()//'; Max-Age=600'//CRLF
    end subroutine login_start

    subroutine login_finish(req, db, res)
        !! The provider's redirect back: checks the state, trades the code for
        !! the ID token and opens a session. Only configured subjects get in;
        !! everyone else is refused until fortyard grants them access.
        type(http_request_t), intent(in) :: req
        type(db_t), intent(in) :: db
        type(response_t), intent(inout) :: res
        character(len=:), allocatable :: pending, state, verifier, nonce, code, id_token, &
            error, kind, session, person
        type(oidc_claims_t) :: claims
        integer(int64) :: account_id
        integer :: d1, d2, stat
        logical :: found

        res%headers = 'Set-Cookie: '//LOGIN_COOKIE//'=; Path='//b()// &
            '/-/login; HttpOnly; SameSite=Lax'//secure()//'; Max-Age=0'//CRLF
        if (.not. oidc%enabled) then
            call login_page(res, 404, 'Sign-in through a provider is not configured.')
            return
        end if
        error = http_query_param(req%query, 'error')
        if (len(error) > 0) then
            call login_page(res, 401, 'The sign-in was cancelled or refused.')
            return
        end if
        pending = http_cookie(req, LOGIN_COOKIE)
        d1 = index(pending, '.')
        d2 = index(pending, '.', back=.true.)
        state = http_query_param(req%query, 'state')
        if (d1 == 0 .or. d2 == d1 .or. len(state) == 0 .or. pending(1:max(d1 - 1, 0)) /= state) then
            call login_page(res, 400, 'This sign-in has expired. Please start again.')
            return
        end if
        verifier = pending(d1 + 1:d2 - 1)
        nonce = pending(d2 + 1:)
        code = http_query_param(req%query, 'code')
        if (.not. oidc_value_ok(code)) then
            call login_page(res, 400, 'The provider sent no usable code.')
            return
        end if
        call oidc_exchange(oidc, code, verifier, id_token, error)
        if (len(error) == 0) call oidc_check(oidc, id_token, nonce, claims, error)
        if (len(error) > 0) then
            write (error_unit, '(a)') 'fy: sign-in failed: '//error
            call login_page(res, 502, 'The sign-in could not be completed. Please try again.')
            return
        end if
        if (.not. oidc_is_admin(oidc, claims%sub)) then
            write (error_unit, '(a)') 'fy: sign-in refused for '//claims%username// &
                ' ('//claims%sub//')'
            call audit_append(db, claims%username, 'sign-in.refused', 'session', &
                'subject '//claims%sub//' of the provider has no access')
            call login_page(res, 403, 'You are signed in as '//claims%username// &
                ', but this account has no access to fortyard yet. '// &
                'Please ask an administrator.')
            return
        end if
        call store_account_find(db, 'admin', account_id, kind, found)
        if (.not. found) then
            call login_page(res, 500, 'The administrator account is missing.')
            return
        end if
        ! Named after the person, so lists tell apart who pushed and landed.
        person = claims%username
        if (len(person) == 0) person = claims%sub
        call store_token_create(db, account_id, SESSION_NAME//' of '//person, kind, session, &
            stat, days=SESSION_DAYS)
        if (stat /= 0) then
            call login_page(res, 500, 'The session could not be created.')
            return
        end if
        call audit_append(db, 'admin', 'sign-in', 'session', 'as '//claims%username// &
            ' ('//claims%sub//') through the provider')
        ! A page, not a redirect: the browser came from the provider, and a
        ! redirect would still count as cross-site and withhold the Strict cookie.
        res%code = 200
        res%ctype = 'text/html; charset=utf-8'
        res%body = '<!doctype html><html lang="en"><head><meta charset="utf-8">'// &
            '<meta http-equiv="refresh" content="0;url='//b()//'/">'// &
            '<title>fortyard</title></head><body><a href="'//b()//'/">Continue</a>'// &
            '</body></html>'
        res%headers = res%headers// &
            'Set-Cookie: '//SESSION_COOKIE//'='//session//'; Path='//cookie_path()// &
            '; HttpOnly; SameSite=Strict'//secure()//'; Max-Age='//SESSION_MAX_AGE//CRLF
    end subroutine login_finish

    subroutine login_page(res, code, message)
        type(response_t), intent(inout) :: res
        integer, intent(in) :: code
        character(len=*), intent(in) :: message

        res%code = code
        res%ctype = 'text/html; charset=utf-8'
        res%body = '<!doctype html><html lang="en"><head><meta charset="utf-8">'// &
            '<meta name="viewport" content="width=device-width,initial-scale=1">'// &
            '<title>Sign-in · fortyard</title></head><body style="font:16px/1.5 system-ui,'// &
            'sans-serif;max-width:32rem;margin:15vh auto;padding:0 16px">'// &
            '<h1 style="font-size:1.4rem">Sign-in</h1><p>'//html_escape(message)//'</p>'// &
            '<p><a href="'//b()//'/">Back to fortyard</a></p></body></html>'
    end subroutine login_page

    function html_escape(s) result(e)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: e
        integer :: k

        e = ''
        do k = 1, len(s)
            select case (s(k:k))
            case ('<'); e = e//'&lt;'
            case ('>'); e = e//'&gt;'
            case ('&'); e = e//'&amp;'
            case ('"'); e = e//'&quot;'
            case default; e = e//s(k:k)
            end select
        end do
    end function html_escape

    function secure() result(s)
        !! Cookies only over HTTPS when fortyard's public URL is HTTPS.
        character(len=:), allocatable :: s
        s = ''
        if (oidc%enabled .and. index(oidc%url, 'https://') == 1) s = '; Secure'
    end function secure

    function repos_json(db) result(s)
        !! The mirrored repositories as GET /api/v1/repos returns them.
        type(db_t), intent(in) :: db
        character(len=:), allocatable :: s
        character(len=12) :: num
        type(repo_info_t), allocatable :: repos(:)
        integer :: i, stat
        logical :: exists

        call store_repo_list(db, repos, stat)
        s = '{"repos":['
        do i = 1, size(repos)
            inquire (file=repo_path(repos(i)%name)//'/HEAD', exist=exists)
            write (num, '(i0)') repos(i)%open_issues
            if (i > 1) s = s//','
            s = s//'{"name":"'//repos(i)%name//'","mode":"'//repos(i)%mode//'","upstream":"'// &
                json_escape_string(repos(i)%upstream)//'","fetched":"'// &
                repos(i)%fetched_at//'","ready":'//json_bool(exists)// &
                ',"open_issues":'//trim(num)//'}'
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
        ! Asset and page addresses below the base.
        do
            at = index(template, '%BASE%')
            if (at == 0) exit
            template = template(1:at - 1)//b()//template(at + 6:)
        end do
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

    subroutine server_set_checks(mode)
        character(len=*), intent(in) :: mode
        checks_mode = mode
    end subroutine server_set_checks

    subroutine server_set_follow(link)
        character(len=*), intent(in) :: link
        follow_link = link
    end subroutine server_set_follow

    subroutine server_set_base(prefix)
        !! Set once before workers start.
        character(len=*), intent(in) :: prefix
        base = prefix
        if (len(base) > 0) then
            if (base(len(base):) == '/') base = base(1:len(base) - 1)
        end if
        call route_set_base(base)
    end subroutine server_set_base

    function b() result(s)
        character(len=:), allocatable :: s
        s = ''
        if (allocated(base)) s = base
    end function b

    function cookie_path() result(s)
        character(len=:), allocatable :: s
        s = b()
        if (len(s) == 0) s = '/'
    end function cookie_path

    subroutine server_set_web_dir(dir)
        !! Set once before workers start; read-only afterwards.
        character(len=*), intent(in) :: dir
        web_dir = dir
    end subroutine server_set_web_dir

    function version_json() result(s)
        character(len=:), allocatable :: s

        s = '{"version":"'//FY_VERSION//'","commit":"'//FY_COMMIT// &
            '","built":"'//FY_BUILT//'","login":'//trim(merge('true ', 'false', oidc%enabled))//'}'
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
            '<li><a href="'//b()//'/healthz"><code>/healthz</code></a></li>'// &
            '<li><a href="'//b()//'/api/v1/version"><code>/api/v1/version</code></a></li>'// &
            '<li><code>/api/v1/whoami</code> (token)</li>'// &
            '<li><code>GET, POST /api/v1/tokens</code>, '// &
            '<code>POST /api/v1/tokens/ID/revoke</code> (token)</li>'// &
            '<li><code>POST, DELETE /api/v1/session</code> (browser session)</li></ul>'// &
            '<p>The web application is at <a href="'//b()//'/">'//b()//'/</a>.</p>'// &
            '<h2>Command line</h2><p><code>fy login --server URL --token TOKEN</code>, '// &
            'then <code>fy whoami</code>.</p>'// &
            '</main></body></html>'
    end function status_page

end module fy_server
