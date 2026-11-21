program test_alerts
    !! Alerts (fy_alert) from seeded rows and mirrors: each rule fires on the
    !! rows that make its condition true and is gone once its cause has ended,
    !! the alerts come in the order of the rules, GET /api/v1/alerts gives
    !! them to any signed-in token and fy prints them, and /readyz answers 503
    !! while the store cannot be read or lacks a migration of this release.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_alert, only: alert_delivery_json, alert_json, alert_list, alert_ready, alert_t
    use fy_cli, only: cli_table
    use fortweb_http, only: http_parse_head, http_request_t
    use fy_server, only: response_t, route, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec, db_open
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_repo_fetched, &
        store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_alerts_data'
    character(len=*), parameter :: path = data//'/fortyard.db'
    character(len=*), parameter :: repos = data//'/repos', work = data//'/work'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=*), parameter :: AT = "strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-' || "
    character(len=*), parameter :: G = 'GIT_CONFIG_GLOBAL=/dev/null git -c user.name=T'// &
        ' -c user.email=t@example.org -C '//work//' '
    type(db_t) :: db, other
    type(response_t) :: res
    type(alert_t), allocatable :: alerts(:)
    character(len=:), allocatable :: admin, reader, a, c1, c2, c3, c4, seen
    character(len=40) :: line
    integer :: stat, u

    call execute_command_line('rm -rf '//data//' && mkdir -p '//repos//' '//work, exitstat=stat)
    call check(stat == 0, 'a data directory')
    call store_open(path, db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call store_repo_add(db, 'fortyard', '', stat, mode='primary')
    call store_repo_add(db, 'mir', 'https://example.org/mir.git', stat)
    call check(stat == 0, 'a store with three repositories')
    call server_set_data_dir(data)

    call list()
    call check(size(alerts) == 0 .and. alert_json(alerts) == '[]', &
        'nothing fires on a quiet instance: '//alert_json(alerts))

    ! main of demo: three commits, c1 the oldest.
    call execute_command_line('git init -q --bare '//repos//'/demo.git && git init -q -b main '// &
        work//' && '//G//'commit -q --allow-empty -m one && '//G// &
        'commit -q --allow-empty -m two && '//G//'commit -q --allow-empty -m three && '//G// &
        'push -q ../repos/demo.git main && '//G//'log --format=%H main > '//data// &
        '/demo.log', &
        exitstat=stat)
    call check(stat == 0, 'a mirror of demo with three commits')
    open (newunit=u, file=data//'/demo.log', status='old', action='read')
    read (u, '(a)') line
    c3 = trim(line)
    read (u, '(a)') line
    c2 = trim(line)
    read (u, '(a)') line
    c1 = trim(line)
    close (u)

    ! main-broken: the verification of c3 fails; bisection then verifies c2,
    ! a later row of an older commit, and it passes: main is still broken.
    call verification(c1, 'passed', 600)
    call list()
    call check(size(alerts) == 0, 'a passed verification fires nothing')
    call verification(c3, 'failed', 120)
    call list()
    call check(size(alerts) == 1, 'a failed verification fires: '//alert_json(alerts))
    call check(alerts(1)%rule == 'main-broken' .and. alerts(1)%repo == 'demo' .and. &
        alerts(1)%since == value_of('SELECT finished_at FROM landings WHERE id = 2;') .and. &
        alerts(1)%detail == 'verification 2 of main '//c3(1:7)//' failed', &
        'main-broken names the repository, the verification and its end: '//alert_json(alerts))
    call verification(c2, 'passed', 60)
    call list()
    call check(size(alerts) == 1 .and. alerts(1)%rule == 'main-broken', &
        'a passing bisection step of an older commit leaves main broken: '//alert_json(alerts))
    call verification(c3, 'passed', 30)
    call list()
    call check(size(alerts) == 0, 'main-broken ends when the newest commit passes: '// &
        alert_json(alerts))
    ! Without a mirror the newest verification by number decides.
    call verification('', 'failed', 20, 'other')
    call list()
    call check(size(alerts) == 1 .and. alerts(1)%repo == 'other', &
        'a repository without a mirror is broken by its newest verification')
    call verification('', 'passed', 10, 'other')
    call list()
    call check(size(alerts) == 0, 'and whole again by the next one')

    ! queue-stalled: an entry queued for 16 minutes with no check running.
    call sql("INSERT INTO landings (repo, branch, account, state, created_at, kind) VALUES"// &
        " ('demo', 'late', 'agent', 'queued', "//AT//"'5 minutes'), 'landing');")
    call list()
    call check(size(alerts) == 0, 'an entry queued for 5 minutes is not stalled')
    call sql("INSERT INTO landings (repo, branch, account, state, created_at, kind) VALUES"// &
        " ('demo', 'stuck', 'agent', 'queued', "//AT//"'16 minutes'), 'landing');")
    call list()
    call check(size(alerts) == 1, 'an entry queued for 16 minutes fires: '//alert_json(alerts))
    call check(alerts(1)%rule == 'queue-stalled' .and. alerts(1)%repo == 'demo' .and. &
        alerts(1)%since == value_of("SELECT created_at FROM landings WHERE branch = 'stuck';") &
        .and. &
        index(alerts(1)%detail, '1 entry waiting over 15 min, the oldest (job 8) for'// &
        ' 16 min; no check of demo runs') == 1, &
        'queue-stalled names the oldest entry and its time: '//alert_json(alerts))
    call sql("INSERT INTO checks (repo, ref, state, runner, created_at, claimed_at) VALUES"// &
        " ('demo', 'r1', 'running', 'r-1', "//AT//"'1 minutes'), "//AT//"'1 minutes'));")
    call list()
    call check(size(alerts) == 0, 'a check of the repository that runs is no stall: '// &
        alert_json(alerts))
    call sql("UPDATE checks SET state = 'passed', finished_at = "//AT//"'0 minutes')"// &
        " WHERE ref = 'r1';")
    call sql("INSERT INTO landings (repo, branch, account, state, created_at, started_at, kind)"// &
        " VALUES ('demo', 'here', 'agent', 'running', "//AT//"'2 minutes'), "//AT// &
        "'1 minutes'), 'landing');")
    call list()
    call check(size(alerts) == 0, 'nor is a landing whose check the lander runs')
    call sql("UPDATE landings SET state = 'landed' WHERE branch IN ('here', 'stuck', 'late');")
    call list()
    call check(size(alerts) == 0, 'queue-stalled ends when the entries have left the queue')

    ! runner-lost: a check queued for 3 minutes, and no runner ever seen.
    call sql("INSERT INTO checks (repo, ref, state, class, created_at) VALUES"// &
        " ('demo', 'mac', 'queued', 'macos', "//AT//"'3 minutes'));")
    call list()
    call check(size(alerts) == 1, 'a check queued for 3 minutes without a runner fires: '// &
        alert_json(alerts))
    call check(alerts(1)%rule == 'runner-lost' .and. len(alerts(1)%repo) == 0 .and. &
        alerts(1)%since == value_of("SELECT created_at FROM checks WHERE ref = 'mac';") .and. &
        alerts(1)%detail == '1 check of class macos'// &
        ' queued for 3 min; no runner of the class was ever seen', &
        'runner-lost names the class, and since the oldest check without a runner: '// &
        alert_json(alerts))
    call check(index(alert_json(alerts), '"repo":null') > 0, &
        'an alert of the instance has no repository')
    ! A runner of the class reported a check a minute ago.
    call sql("INSERT INTO checks (repo, ref, state, class, runner, created_at, claimed_at,"// &
        " finished_at) VALUES ('demo', 'mac0', 'passed', 'macos', 'mac-1', "//AT// &
        "'9 minutes'), "//AT//"'8 minutes'), "//AT//"'1 minutes'));")
    call list()
    call check(size(alerts) == 0, 'a runner seen within 2 minutes is not lost')
    call sql("UPDATE checks SET finished_at = "//AT//"'5 minutes') WHERE ref = 'mac0';")
    call list()
    seen = value_of("SELECT finished_at FROM checks WHERE ref = 'mac0';")
    call check(size(alerts) == 1 .and. alerts(1)%since == seen .and. &
        index(alerts(1)%detail, 'no runner of the class seen since '//seen) > 0, &
        'a runner last seen 5 minutes ago is lost since then: '//alert_json(alerts))
    call sql("INSERT INTO checks (repo, ref, state, class, created_at) VALUES"// &
        " ('demo', 'lin', 'queued', 'linux', "//AT//"'1 minutes'));")
    call list()
    call check(size(alerts) == 1 .and. index(alerts(1)%detail, 'class macos') > 0, &
        'a check of another class that waits only a minute is no alert')
    call sql("UPDATE checks SET state = 'running', runner = 'mac-2', claimed_at = "//AT// &
        "'0 minutes') WHERE ref = 'mac';")
    call sql("DELETE FROM checks WHERE ref = 'lin';")
    call list()
    call check(size(alerts) == 0, 'runner-lost ends when a runner claims the check')

    ! deploy-behind: main of fortyard has a commit of 20 minutes ago that the
    ! instance, serving the commit before it, does not serve; then one of 5
    ! minutes ago, while it serves c4.
    call execute_command_line('git init -q --bare '//repos//'/fortyard.git && '//G// &
        'checkout -q -b self && GIT_COMMITTER_DATE="@$(( $(date +%s) - 3600 )) +0000" '//G// &
        'commit -q --allow-empty -m served && GIT_COMMITTER_DATE="@$(( $(date +%s) - 1200 ))'// &
        ' +0000" '//G//'commit -q --allow-empty -m new && '//G// &
        'push -q ../repos/fortyard.git self:main && '//G//'log --format=%H self > '// &
        data//'/self.log', &
        exitstat=stat)
    call check(stat == 0, 'a mirror of fortyard with two commits')
    open (newunit=u, file=data//'/self.log', status='old', action='read')
    read (u, '(a)') line
    c4 = trim(line)
    read (u, '(a)') line
    a = trim(line)
    close (u)
    call alert_list(db, repos, a(1:12), 'fortyard', alerts)
    call check(size(alerts) == 1, 'main 20 minutes ahead of the served commit fires: '// &
        alert_json(alerts))
    call check(alerts(1)%rule == 'deploy-behind' .and. alerts(1)%repo == 'fortyard' .and. &
        len(alerts(1)%since) == 20 .and. alerts(1)%detail == 'main '//c4(1:7)//' is 1 commit'// &
        ' ahead of the served '//a(1:7)//'; no release yet', &
        'deploy-behind names main, the served commit and the release: '//alert_json(alerts))
    call sql("INSERT INTO releases (repo, version, commit_id, notes, uploader, created_at,"// &
        " state) VALUES ('fortyard', '2026.11.11.7', '"//c4//"', 'n', 'builder', "//AT// &
        "'18 minutes'), 'open');")
    call alert_list(db, repos, a(1:12), 'fortyard', alerts)
    call check(size(alerts) == 1 .and. index(alerts(1)%detail, '; release 2026.11.11.7 is'// &
        ' open, its archive was not accepted') > 0, 'a release left open is named: '// &
        alert_json(alerts))
    call sql("UPDATE releases SET state = 'ready';")
    call alert_list(db, repos, a(1:12), 'fortyard', alerts)
    call check(size(alerts) == 1 .and. index(alerts(1)%detail, '; release 2026.11.11.7 is'// &
        ' ready and not installed') > 0, 'a ready release not installed is named: '// &
        alert_json(alerts))
    a = alert_delivery_json(db, repos, a(1:12), '2026.11.11.6', 'fortyard')
    call check(index(a, '{"repo":"fortyard","version":"2026.11.11.6","commit":"') == 1 .and. &
        index(a, '"main":"'//c4//'","behind":1,"since":"') > 0 .and. index(a, &
        '"release":{"version":"2026.11.11.7","commit":"'//c4//'","state":"ready","created":"') &
        > 0, 'delivery: what is served against main, and the newest release: '//a)
    call alert_list(db, repos, c4(1:12), 'fortyard', alerts)
    call check(size(alerts) == 0, 'deploy-behind ends when the instance serves main')
    call execute_command_line('GIT_COMMITTER_DATE="@$(( $(date +%s) - 300 )) +0000" '//G// &
        'commit -q --allow-empty -m newer && '//G//'push -q ../repos/fortyard.git self:main', &
        exitstat=stat)
    call alert_list(db, repos, c4(1:12), 'fortyard', alerts)
    call check(stat == 0 .and. size(alerts) == 0, 'a commit of 5 minutes ago is not yet behind')
    call alert_list(db, repos, 'unknown', 'fortyard', alerts)
    call check(size(alerts) == 0, 'a build without its commit is never behind')
    a = alert_delivery_json(db, repos, 'unknown', '0.0.0-dev', 'fortyard')
    call check(index(a, '"behind":null,"since":null') > 0, 'nor says how far: '//a)
    call alert_list(db, repos, c4(1:12), 'nothere', alerts)
    call check(size(alerts) == 0 .and. alert_delivery_json(db, repos, c4(1:12), 'v', &
        'nothere') == '{"repo":null,"version":"v","commit":"'//c4(1:12)//'","main":null,'// &
        '"behind":null,"since":null,"release":null}', 'an instance that follows no repository'// &
        ' of its own is never behind')

    ! mirror-failing: no fetch of mir has succeeded since it was registered
    ! 20 minutes ago; then one does; then the last success is 11 minutes old.
    call sql("UPDATE repos SET created_at = "//AT//"'20 minutes') WHERE name = 'mir';")
    call store_repo_fetched(db, 'mir', 128)
    call list()
    call check(size(alerts) == 1, 'a mirror that never fetched fires: '//alert_json(alerts))
    call check(alerts(1)%rule == 'mirror-failing' .and. alerts(1)%repo == 'mir' .and. &
        alerts(1)%since == value_of("SELECT created_at FROM repos WHERE name = 'mir';") .and. &
        alerts(1)%detail == 'no fetch has succeeded;'// &
        ' the last failed with status 128', 'mirror-failing counts from the registration: '// &
        alert_json(alerts))
    call store_repo_fetched(db, 'mir', 0)
    call list()
    call check(size(alerts) == 0, 'mirror-failing ends with a successful fetch')
    call store_repo_fetched(db, 'mir', 1)
    call list()
    call check(size(alerts) == 0, 'a fetch that fails right after a success is no alert yet')
    call sql("UPDATE repos SET fetch_ok_at = "//AT//"'11 minutes') WHERE name = 'mir';")
    call list()
    call check(size(alerts) == 1 .and. alerts(1)%since == &
        value_of("SELECT fetch_ok_at FROM repos WHERE name = 'mir';") .and. &
        alerts(1)%detail == 'fetches fail since the last success; the last failed with status 1', &
        'a mirror whose last success is 11 minutes old fires since then: '//alert_json(alerts))

    ! Several rules at once come in the order of the design's table.
    call sql("UPDATE checks SET state = 'passed' WHERE ref = 'mac';")
    call verification(c3, 'failed', 5)
    call sql("INSERT INTO landings (repo, branch, account, state, created_at, kind) VALUES"// &
        " ('demo', 'stuck2', 'agent', 'queued', "//AT//"'20 minutes'), 'landing');")
    call list()
    call check(size(alerts) == 3 .and. alerts(1)%rule == 'main-broken' .and. &
        alerts(2)%rule == 'queue-stalled' .and. alerts(3)%rule == 'mirror-failing', &
        'alerts come in the order of the rules: '//alert_json(alerts))

    ! The route: any signed-in token, the same list; fy prints it as a table.
    call store_token_create(db, 1_int64, 'reader', 'write', reader, stat)
    call check(stat == 0, 'a token without admin')
    call get('/api/v1/alerts', '')
    call check(res%code == 401, 'the alerts need a token')
    call get('/api/v1/alerts', reader)
    call list()
    call check(res%code == 200 .and. res%body == alert_json(alerts), &
        'GET /api/v1/alerts answers the alerts to any signed-in token: '//res%body)
    call check(index(res%body, '[{"rule":"main-broken","repo":"demo","since":"') == 1, &
        'as [{"rule","repo","since","detail"}]: '//res%body)
    a = cli_table('{"alerts":'//res%body//'}', 'alerts', [character(len=6) :: 'rule', 'repo', &
        'since', 'detail'])
    call check(index(a, 'RULE') == 1 .and. index(a, new_line('a')//'main-broken ') > 0 .and. &
        index(a, new_line('a')//'mirror-failing  mir ') > 0, 'fy alerts prints a table: '//a)
    call get('/api/v1/delivery', reader)
    call check(res%code == 200 .and. index(res%body, '{"repo":"fortyard","version":"') == 1, &
        'GET /api/v1/delivery: '//res%body)

    ! /readyz: ready on the migrated store, not without a migration of this
    ! release, nor on a store that was never migrated or cannot be read.
    call get('/readyz', '')
    call check(res%code == 200 .and. index(res%body, 'ready ') == 1, '/readyz: '//res%body)
    call check(alert_ready(db, a), 'the migrated store is ready')
    call sql("DELETE FROM migrations WHERE name = 'repos-fetch-ok-at';")
    call get('/readyz', '')
    call check(res%code == 503 .and. res%body == 'not ready: migration "repos-fetch-ok-at"'// &
        ' is not applied'//new_line('a'), 'a missing migration is not ready: '//res%body)
    call db_close(db)
    call execute_command_line('rm -f '//data//'/empty.db && printf "not a database, not at'// &
        ' all; SQLite reads the first hundred bytes as its header and refuses them" > '// &
        data//'/broken.db', exitstat=stat)
    call db_open(data//'/empty.db', other, stat)
    call route_on(other, '/readyz')
    call check(res%code == 503 .and. res%body == 'not ready: the schema is not migrated'// &
        new_line('a'), 'a store that was never migrated is not ready: '//res%body)
    call db_close(other)
    call db_open(data//'/broken.db', other, stat)
    call route_on(other, '/readyz')
    call check(res%code == 503 .and. res%body == 'not ready: the database cannot be read'// &
        new_line('a'), 'a file that is no database is not ready: '//res%body)
    call db_close(other)
    call get_healthz()

    call execute_command_line('rm -rf '//data)
    print '(a)', 'alerts: all checks passed'

contains

    subroutine list()
        call alert_list(db, repos, 'unknown', 'fortyard', alerts)
    end subroutine list

    subroutine verification(commit, state, ago, repo)
        !! A finished verification of `commit` that ended `ago` seconds ago.
        character(len=*), intent(in) :: commit, state
        integer, intent(in) :: ago
        character(len=*), intent(in), optional :: repo
        character(len=:), allocatable :: name

        name = 'demo'
        if (present(repo)) name = repo
        call sql("INSERT INTO landings (repo, branch, account, state, created_at, finished_at,"// &
            " kind, commit_id) VALUES ('"//name//"', 'main', 'fortyard', '"//state//"', "// &
            AT//"'"//itoa(ago + 60)//" seconds'), "//AT//"'"//itoa(ago)//" seconds'), 'verify',"// &
            " nullif('"//commit//"', ''));")
    end subroutine verification

    function value_of(query) result(value)
        !! The text of the first column of the first row `query` answers.
        use fy_sqlite, only: SQLITE_ROW, db_prepare, stmt_finalize, stmt_step, stmt_t, stmt_text
        character(len=*), intent(in) :: query
        character(len=:), allocatable :: value
        type(stmt_t) :: st
        integer :: k

        value = ''
        call db_prepare(db, query, st, k)
        if (stmt_step(st) == SQLITE_ROW) value = stmt_text(st, 0)
        call stmt_finalize(st)
    end function value_of

    subroutine sql(text)
        character(len=*), intent(in) :: text
        call db_exec(db, text, stat)
        call check(stat == 0, 'rows are seeded: '//text)
    end subroutine sql

    subroutine get(target, token)
        character(len=*), intent(in) :: target, token
        type(http_request_t) :: req
        integer :: status

        if (len(token) > 0) then
            call http_parse_head('GET '//target//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
                token//CRLF//CRLF, req, status)
        else
            call http_parse_head('GET '//target//' HTTP/1.1'//CRLF//CRLF, req, status)
        end if
        call route(req, db, res)
    end subroutine get

    subroutine route_on(store, target)
        type(db_t), intent(in) :: store
        character(len=*), intent(in) :: target
        type(http_request_t) :: req
        integer :: status

        call http_parse_head('GET '//target//' HTTP/1.1'//CRLF//CRLF, req, status)
        call route(req, store, res)
    end subroutine route_on

    subroutine get_healthz()
        call route_on(other, '/healthz')
        call check(res%code == 200 .and. index(res%body, 'ok ') == 1, '/healthz stays: '//res%body)
    end subroutine get_healthz

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_alerts
