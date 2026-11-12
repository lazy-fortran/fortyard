program test_policy
    !! The policy of a repository: its defaults are what every repository did
    !! before; fy repo policy shows and sets it (admin scope to set); and each
    !! setting changes what landing does: batch the size of a candidate, tests
    !! whether the check runs, review whether a change must be reviewed first,
    !! unreviewed_limit how many landed changes may wait for a review.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_change, only: change_t, change_get, change_review_set, change_review_next
    use fy_policy, only: &
        policy_from_json, &
        policy_get, &
        policy_json, &
        policy_put, &
        policy_refusal, &
        policy_set, &
        policy_t, &
        policy_unreviewed
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: &
        store_bootstrap, &
        store_landing_add, &
        store_open, &
        store_repo_add, &
        store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_policy_data', work = 'build/test_policy_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, writer, server, fy, env, out, in, count, n, as_writer
    character(len=:), allocatable :: m, api
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, i, last
    logical :: exists

    sup = 0
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'writer', 'write', writer, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call store_repo_add(db, 'unit', '', stat, mode='primary')
    call settings()
    call gate()
    call db_close(db)
    call server_set_data_dir(data)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 150
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = trim(read_line(data//'/fy'))
    ! Every command of a chain uses the test's client configuration and git's.
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 && '
    in = env//'cd '//work//' && '
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! main exists, from a first landing. The check counts its runs, takes long
    ! for a commit called slow and fails when the file BAD exists.
    count = trim(cwd)//'/'//data//'/count'
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\necho x >> '//count// &
        '\ngit log -1 --format=%%s | grep -q slow && sleep 3\ntest ! -f BAD\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land')
    call check(stat == 0, 'a first landing: '//out)

    ! The command shows and sets the policy; setting it needs the admin scope.
    call run(in//fy//' repo policy demo')
    call check(stat == 0 .and. index(out, '"tests":"full","review":"after","bisect":"on",'// &
        '"batch":8,"unreviewed_limit":0') > 0, 'the defaults are shown: '//out)
    call run(in//fy//' repo policy demo tests=affected batch=2')
    call check(stat == 0 .and. index(out, '"tests":"affected","review":"after"') > 0 .and. &
        index(out, '"batch":2') > 0, 'settings are set and the others kept: '//out)
    call run(in//fy//' repo policy demo check_timeout=25')
    call check(stat == 0 .and. index(out, '"unreviewed_limit":0,"check_timeout":25,') > 0, &
        'the time limit of a check is set in minutes: '//out)
    call run(in//fy//' repo policy demo check_timeout=10081')
    call check(stat /= 0 .and. index(out, 'check_timeout is 0') > 0, &
        'a time limit over a week is refused: '//out)
    call run(in//fy//' repo policy demo verify_on=linux,macos-arm64')
    call check(stat == 0 .and. index(out, '"verify_on":"linux,macos-arm64",') > 0, &
        'main is verified on the runner classes set: '//out)
    call run(in//fy//' repo policy demo verify_on=MacOS')
    call check(stat /= 0 .and. index(out, 'verify_on is runner classes') > 0, &
        'a class with capitals is refused: '//out)
    call run(in//fy//' repo policy demo tests=some')
    call check(stat /= 0 .and. index(out, 'tests is full, affected or none') > 0, &
        'a value that is none of the choices is refused: '//out)
    call run(in//fy//' repo policy demo speed=fast')
    call check(stat /= 0 .and. index(out, 'no such setting') > 0, &
        'an unknown setting is refused: '//out)
    call run(in//fy//' repo policy nosuch')
    call check(stat /= 0 .and. index(out, 'no such repository') > 0, &
        'a repository that does not exist has no policy: '//out)
    as_writer = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/writer && '
    call run(as_writer//'printf "%s\n" '//writer//' | '//fy//' login --server '//server// &
        ' && '//fy//' repo policy demo')
    call check(stat == 0 .and. index(out, '"batch":2') > 0, &
        'a token without the admin scope reads the policy: '//out)
    call run(as_writer//fy//' repo policy demo batch=3')
    call check(stat /= 0 .and. index(out, 'admin scope') > 0, 'but does not set it: '//out)

    ! A value of the wrong JSON type is refused, not ignored; only GET and PUT
    ! are answered; and a ref called policy is still a commit of its repository.
    api = 'curl -s -H "Authorization: Bearer '//admin//'" '//server//'/api/v1/repos/demo/'
    call run(api//"policy -X PUT -d '{""bisect"":false}' -w '%{http_code}' && "//api//'policy')
    call check(index(out, 'bisect is on or off"}400') > 0 .and. index(out, '"bisect":"on"') > 0, &
        'a setting that is not a string is refused: '//out)
    call run(api//"policy -X DELETE -w '%{http_code}'")
    call check(index(out, 'use GET or PUT"}405') > 0, 'the policy is read and set only: '//out)
    call run(api//'commits/policy')
    call check(index(out, 'no such repository') == 0, 'commits/policy is a commit: '//out)

    ! batch=2: of three branches queued behind a running landing, two share a
    ! check and the third gets its own. tests=affected checks like full.
    call fetch()
    call run(in//branch('slow1', 'S1')//' && '//branch('a', 'A')//' && '//branch('b', 'B')// &
        ' && '//branch('c', 'C'))
    call check(stat == 0, 'four landings are queued: '//out)
    call settle()
    call run(in//fy//' jobs')
    call check(count_of(out, ' landed ') == 5 .and. index(out, 'failed') == 0, &
        'all of them land: '//out)
    call check(lines(count) == 4, 'a candidate holds two landings: '//read_all(count))

    ! tests=none: a branch the check would refuse lands, and no check runs.
    call run(in//fy//' repo policy demo tests=none')
    call fetch()
    call run(in//branch('bad', 'BAD', wait=.true.)//' && '//fy//' landings --json')
    call check(stat == 0 .and. index(out, 'Not checked: the policy of the repository sets'// &
        ' tests=none') > 0, 'a landing is not checked: '//out)
    call check(lines(count) == 4, 'the check did not run: '//read_all(count))
    ! Also for landings that share a candidate: two are queued behind a
    ! landing whose check runs (and fails, main has BAD) and land unchecked.
    call run(in//fy//' repo policy demo tests=full')
    call fetch()
    call run(in//branch('s2', 'S2')//' && '//branch('x', 'X')//' && '//branch('y', 'Y')// &
        ' && '//fy//' repo policy demo tests=none')
    call check(stat == 0, 'two landings are queued behind a checked one: '//out)
    call settle()
    call fetch()
    call run(in//'git ls-tree --name-only FETCH_HEAD')
    call check(index(out, 'X') > 0 .and. index(out, 'Y') > 0 .and. index(out, 'S2') == 0 .and. &
        lines(count) == 5, 'a candidate of two is not checked: '//out//read_all(count))
    call fetch()
    call run(in//'git checkout -q -b fix FETCH_HEAD && git rm -q BAD && '//git// &
        'commit -q -m "Remove BAD" && '//fy//' land')
    call check(stat == 0, 'main is repaired: '//out)
    call run(in//fy//' repo policy demo tests=full batch=8')
    call fetch()
    call run(in//branch('d', 'D', wait=.true.))
    call check(stat == 0 .and. lines(count) == 6, 'tests=full checks again: '//read_all(count))

    ! review=before: a change lands only once it is reviewed, a branch without
    ! a change not at all, and a review does not hold for a later version.
    call run(in//fy//' repo policy demo review=before')
    call fetch()
    n = pushed('r1', 'R1')
    call run(in//fy//' change land '//n)
    call check(stat /= 0 .and. index(out, 'review=before') > 0 .and. &
        index(out, 'change #'//n//' is not reviewed yet') > 0, &
        'an unreviewed change is not queued: '//out)
    call run(in//'git checkout -q -b wip/x FETCH_HEAD && printf "x\n" > W && '//git// &
        'add W && '//git//'commit -q -m "Add W" && '//fy//' land')
    call check(stat /= 0 .and. index(out, 'has no change to review') > 0, &
        'a branch without a change is not queued: '//out)
    call run(in//fy//' change reviewed '//n//' && '//fy//' change land '//n)
    call check(stat == 0 .and. index(out, 'landed as') > 0, 'a reviewed change lands: '//out)
    call fetch()
    n = pushed('r2', 'R2')
    call run(in//fy//' change reviewed '//n//' && '//git//'commit -q --amend -m "Add R2 again"'// &
        ' && '//fy//' push && '//fy//' change land '//n)
    call check(stat /= 0 .and. index(out, 'is not reviewed yet') > 0, &
        'a new version needs its own review: '//out)
    call run(in//fy//' change reviewed '//n//' && '//fy//' change land '//n)
    call check(stat == 0 .and. index(out, 'landed as') > 0, 'and lands once reviewed: '//out)
    ! The review is of one version: a branch put back to an earlier version,
    ! which nobody reviewed, does not land.
    call fetch()
    n = pushed('r3', 'R3')
    call run(in//'first=$(git rev-parse HEAD) && git rm -q R3 && printf "x\n" > R4 && '//git// &
        'add R4 && '//git//'commit -q --amend -m "Add R4" && '//fy//' push && '//fy// &
        ' change reviewed '//n//' && git reset -q --hard $first && '//fy//' push && '//fy// &
        ' change land '//n)
    call check(stat /= 0 .and. index(out, 'is not the reviewed version') > 0, &
        'an earlier version does not land on the review of a later one: '//out)
    ! A version pushed while the reviewed one waits in the queue does not
    ! land: with batch=1 the change waits behind a landing whose check runs.
    call run(in//fy//' repo policy demo batch=1')
    call fetch()
    n = pushed('q1', 'slowQ1')
    m = pushed('q2', 'Q2')
    call run(in//fy//' change reviewed '//n//' && '//fy//' change reviewed '//m//' && '//fy// &
        ' change land '//n//' --no-wait && '//fy//' change land '//m//' --no-wait && '//git// &
        'commit -q --amend -m "Add Q2 again" && '//fy//' push')
    call check(stat == 0, 'a reviewed change is queued and gets a new version: '//out)
    call settle()
    call run(in//fy//' landings --json')
    call check(count_of(out, 'is not reviewed yet') == 1, &
        'a change whose review is gone when its turn comes does not land: '//out)
    ! Nor does a version pushed while the landing runs: two reviewed changes
    ! share a candidate whose check fails (BAD), so each is tried again by
    ! itself, and one of them has moved on by then.
    call run(in//fy//' repo policy demo batch=8')
    call fetch()
    n = pushed('q3', 'slowQ3')
    call run(in//fy//' change reviewed '//n//' && '//fy//' change land '//n//' --no-wait')
    call wait_for(' running ', .true.)
    n = pushed('q4', 'BAD')
    m = pushed('q5', 'slowQ5')
    call run(in//fy//' change reviewed '//n//' && '//fy//' change reviewed '//m//' && '//fy// &
        ' change land '//n//' --no-wait && '//fy//' change land '//m//' --no-wait')
    call check(stat == 0, 'two reviewed changes are queued behind a landing: '//out)
    call wait_for(' queued ', .false.)
    call run(in//'printf "x\n" > EVIL && '//git//'add EVIL && '//git// &
        'commit -q --amend -m "Add slowQ5" && '//fy//' push')
    call check(stat == 0, 'one of them gets a new version while its landing runs: '//out)
    call settle()
    call fetch()
    call run(in//'git ls-tree --name-only FETCH_HEAD && '//fy//' landings --json')
    call check(index(out, 'slowQ3') > 0 .and. index(out, 'EVIL') == 0 .and. &
        count_of(out, 'is not the reviewed version') == 2, &
        'the version nobody reviewed does not land: '//out)

    ! unreviewed_limit=1: nothing is queued while a landed change waits for a
    ! review; once those are reviewed, one change lands and the next waits.
    call run(in//fy//' repo policy demo review=after unreviewed_limit=1')
    call fetch()
    n = pushed('u1', 'U1')
    call run(in//fy//' change land '//n)
    call check(stat /= 0 .and. index(out, 'unreviewed_limit=1') > 0, &
        'landing is refused at the limit: '//out)
    read (n, *) last
    do i = 1, last - 1
        write (num, '(i0)') i
        call run(in//fy//' change reviewed '//trim(num))
    end do
    call run(in//fy//' change land '//n)
    call check(stat == 0 .and. index(out, 'landed as') > 0, &
        'a change lands when the others are reviewed: '//out)
    call fetch()
    n = pushed('u2', 'U2')
    call run(in//fy//' change land '//n)
    call check(stat /= 0 .and. index(out, '1 change(s) of demo') > 0, &
        'the landed change counts until it is reviewed: '//out)
    call run(in//fy//' repo policy demo unreviewed_limit=0 && '//fy//' change land '//n)
    call check(stat == 0 .and. index(out, 'landed as') > 0, 'no limit, no refusal: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'policy: all checks passed'

contains

    subroutine settings()
        !! Defaults, validation and storage of the settings.
        type(policy_t) :: p
        character(len=:), allocatable :: error
        logical :: found

        call policy_get(db, 'unit', p, found)
        call check(found .and. p%tests == 'full' .and. p%review == 'after' .and. p%bisect .and. &
            p%batch == 8 .and. p%unreviewed_limit == 0 .and. p%check_timeout == 60, &
            'a new repository has the defaults')
        call policy_get(db, 'nosuch', p, found)
        call check(.not. found, 'no policy without a repository')
        call policy_set(p, 'batch', '0', error)
        call check(len(error) > 0, 'batch=0 is refused')
        call policy_set(p, 'batch', '65', error)
        call check(len(error) > 0, 'batch=65 is refused')
        call policy_set(p, 'unreviewed_limit', '-1', error)
        call check(len(error) > 0, 'a negative limit is refused')
        call policy_set(p, 'check_timeout', '-1', error)
        call check(len(error) > 0, 'a negative time limit is refused')
        call policy_set(p, 'check_timeout', '0', error)
        call check(len(error) == 0 .and. p%check_timeout == 0, 'check_timeout=0 is no limit')
        call policy_set(p, 'verify_on', 'linux,,macos-arm64', error)
        call check(len(error) > 0, 'an empty runner class is refused')
        call policy_set(p, 'verify_on', 'a,b,c,d,e,f,g,h,i', error)
        call check(len(error) > 0, 'more than eight runner classes are refused')
        call policy_set(p, 'verify_on', 'linux,github-windows@nightly', error)
        call check(len(error) == 0 .and. p%verify_on == 'linux,github-windows@nightly', &
            'a class verified at night is accepted')
        p%verify_on = 'linux'
        call policy_set(p, 'verify_on', 'linux,@nightly', error)
        call check(len(error) > 0, 'a schedule without a class is refused')
        call policy_set(p, 'verify_on', 'linux@daily', error)
        call check(len(error) > 0, 'a schedule other than nightly is refused')
        call policy_set(p, 'verify_on', 'linux,linux@nightly', error)
        call check(len(error) > 0, 'a class both with and without schedule is refused')
        call policy_set(p, 'verify_on', 'mac@nightly,linux,mac@nightly', error)
        call check(len(error) > 0, 'a nightly class named twice is refused')
        call policy_set(p, 'verify_on', 'linux,linux', error)
        call check(len(error) > 0, 'a class named twice is refused')
        call policy_set(p, 'bisect', 'maybe', error)
        call check(len(error) > 0, 'bisect=maybe is refused')
        call policy_set(p, 'review', 'never', error)
        call check(len(error) > 0, 'review=never is refused')
        call policy_from_json(p, '{"batch":4,"unreviewed_limit":"3","bisect":"off",'// &
            '"check_timeout":90}', error)
        call check(len(error) == 0 .and. p%batch == 4 .and. p%unreviewed_limit == 3 .and. &
            p%check_timeout == 90 .and. &
            .not. p%bisect .and. p%tests == 'full', 'JSON sets what it names: '//error)
        call policy_from_json(p, '{"tests":"none","review":"sometimes"}', error)
        call check(index(error, 'review is after or before') > 0, 'JSON is validated: '//error)
        call policy_from_json(p, '{"bisect":false}', error)
        call check(index(error, 'bisect is on or off') > 0, 'a boolean is no setting: '//error)
        call policy_from_json(p, '{"batch":3.5}', error)
        call check(index(error, 'batch is a number') > 0, 'nor a fraction: '//error)
        call policy_from_json(p, '{"batch":99999999999}', error)
        call check(index(error, 'batch is a number') > 0, 'nor a huge number: '//error)
        call policy_from_json(p, '{"tests":7}', error)
        call check(index(error, 'tests is full') > 0, 'nor a number for a word: '//error)
        call policy_from_json(p, '{"review":{"batch":1}}', error)
        call check(index(error, 'review is after') > 0, 'nor an object: '//error)
        call policy_from_json(p, 'garbage', error)
        call check(index(error, 'JSON object') > 0, 'a body that is not JSON is refused: '//error)
        call policy_from_json(p, '["batch"]', error)
        call check(index(error, 'JSON object') > 0, 'and one that is no object: '//error)
        call policy_from_json(p, '{"other":{"batch":1},"batch":4}', error)
        call check(len(error) == 0 .and. p%batch == 4 .and. p%unreviewed_limit == 3 .and. &
            .not. p%bisect .and. p%tests == 'full', 'a refused body sets nothing: '//error)
        p%tests = 'affected'
        call policy_put(db, 'unit', p, found)
        call policy_get(db, 'unit', p)
        call check(found .and. policy_json('unit', p) == '{"name":"unit","tests":"affected",'// &
            '"review":"after","bisect":"off","batch":4,"unreviewed_limit":3,"check_timeout":90,'// &
            '"verify_on":"linux","backends":"","egress":"","public":false}', &
            'a policy is stored: '//policy_json('unit', p))
        call policy_get(db, 'demo', p)
        call check(p%tests == 'full' .and. p%batch == 8, 'for its repository only')
    end subroutine settings

    subroutine gate()
        !! What review=before and unreviewed_limit let land, and which open
        !! changes the review agent takes, on changes written into the store.
        character(len=*), parameter :: ROW = 'INSERT INTO changes (repo, number, change_key,'// &
            " branch, title, description, author, created_at, updated_at, state) VALUES ('unit', "
        character(len=*), parameter :: VERSION = 'INSERT INTO change_versions (change, version,'// &
            ' head, base, created_at) VALUES (1, '
        type(policy_t) :: p
        type(change_t) :: c, next
        integer(int64) :: landing
        logical :: found

        call db_exec(db, ROW//"1, 'k1', 'b1', 'One', '', 'admin', 'x', 'x', 'open');", stat)
        call change_get(db, 'unit', 1, c, found)
        call db_exec(db, VERSION//"1, 'h1', 'b', 'x');", stat)
        call check(found, 'a change is written')
        call policy_get(db, 'unit', p)
        p%unreviewed_limit = 0
        call policy_put(db, 'unit', p, found)
        call change_review_next(db, next, found)
        call check(.not. found .and. len(policy_refusal(db, 'unit', c%id, .true.)) == 0 .and. &
            len(policy_refusal(db, 'unit', 0_int64, .true.)) == 0, &
            'review=after: an open change lands unreviewed and the agent leaves it')
        p%review = 'before'
        call policy_put(db, 'unit', p, found)
        call change_review_next(db, next, found)
        call check(found .and. next%number == 1, 'review=before: the agent reviews open changes')
        call check(index(policy_refusal(db, 'unit', c%id, .false.), 'is not reviewed yet') > 0, &
            'an unreviewed change is held back, also when queued already')
        call check(index(policy_refusal(db, 'unit', 0_int64, .true.), 'no change to review') > 0, &
            'a branch without a change is held back')
        call change_review_set(db, c%id, 'ai-reviewed', .true., 'numerics changed')
        call check(index(policy_refusal(db, 'unit', c%id, .true.), &
            'is flagged for a person: numerics changed') > 0, 'a flagged change is held back')
        call change_review_set(db, c%id, 'ai-reviewed', .false., '')
        call check(len(policy_refusal(db, 'unit', c%id, .true.)) == 0, &
            'a change the agent reviewed without a flag lands')
        call change_review_set(db, c%id, 'human-reviewed', .false., '')
        call check(len(policy_refusal(db, 'unit', c%id, .true.)) == 0, &
            'a change a person reviewed lands')
        ! A review holds for the version it read.
        call db_exec(db, VERSION//"2, 'h2', 'b', 'x');", stat)
        call check(index(policy_refusal(db, 'unit', c%id, .true.), &
            'is not of its newest version') > 0, 'a review does not hold for a later version')
        call change_review_set(db, c%id, 'ai-reviewed', .false., '', head='h1')
        call change_review_next(db, next, found)
        call check(index(policy_refusal(db, 'unit', c%id, .true.), &
            'is not of its newest version') > 0 .and. found, &
            'a review that read an older version lets nothing land and is done again')
        call change_review_set(db, c%id, 'ai-reviewed', .false., '', head='h2')
        call change_review_next(db, next, found)
        call check(len(policy_refusal(db, 'unit', c%id, .true.)) == 0 .and. .not. found, &
            'a review of the newest version lets it land')

        p%review = 'after'
        p%unreviewed_limit = 2
        call policy_put(db, 'unit', p, found)
        call db_exec(db, ROW//"2, 'k2', 'b2', 'Two', '', 'admin', 'x', 'x', 'landed');"// &
            ROW//"3, 'k3', 'b3', 'Three', '', 'admin', 'x', 'x', 'landed');", stat)
        call check(policy_unreviewed(db, 'unit') == 2 .and. &
            index(policy_refusal(db, 'unit', c%id, .true.), 'unreviewed_limit=2') > 0, &
            'two unreviewed landed changes reach the limit of two')
        call check(len(policy_refusal(db, 'unit', c%id, .false.)) == 0, &
            'a landing queued already is not held back by the limit')
        call change_get(db, 'unit', 2, next, found)
        call change_review_set(db, next%id, 'human-reviewed', .false., '')
        call check(policy_unreviewed(db, 'unit') == 1 .and. &
            len(policy_refusal(db, 'unit', c%id, .true.)) == 0, 'a reviewed change frees a place')
        call change_review_set(db, next%id, 'human-reviewed', .true., 'again')
        call check(policy_unreviewed(db, 'unit') == 2, 'a flagged change waits')
        call change_review_set(db, next%id, 'none', .false., '')
        call check(policy_unreviewed(db, 'unit') == 1, &
            'a change landed before there were reviews does not wait')
        ! A change on its way to main will wait for its review too.
        call change_review_set(db, c%id, 'failed', .false., '')
        call store_landing_add(db, 'unit', 'b1', 'admin', landing, stat, c%id)
        call check(policy_unreviewed(db, 'unit') == 2, 'a queued unreviewed change counts')
        call db_exec(db, "DELETE FROM landings WHERE repo = 'unit';", stat)
    end subroutine gate

    subroutine fetch()
        call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
            ' git-credential" fetch -q yard main')
        call check(stat == 0, 'main is fetched: '//out)
    end subroutine fetch

    function branch(name, file, wait) result(cmd)
        !! Commands that queue a branch from main adding `file`.
        character(len=*), intent(in) :: name, file
        logical, intent(in), optional :: wait
        character(len=:), allocatable :: cmd
        cmd = 'git checkout -q -b '//name//' FETCH_HEAD && printf "x\n" > '//file//' && '// &
            git//'add '//file//' && '//git//'commit -q -m "Add '//file// &
            merge(' slow', '     ', name(1:1) == 's')//'" && '//fy//' land'
        if (.not. present(wait)) cmd = cmd//' --no-wait'
    end function branch

    function pushed(name, file) result(number)
        !! Pushes a branch from main adding `file`; the number of its change.
        character(len=*), intent(in) :: name, file
        character(len=:), allocatable :: number
        integer :: k
        call run(in//'git checkout -q -b '//name//' FETCH_HEAD && printf "x\n" > '//file//' && '// &
            git//'add '//file//' && '//git//'commit -q -m "Add '//file//'" && '//fy//' push')
        k = index(out, 'change #')
        call check(stat == 0 .and. k > 0, 'a change is pushed: '//out)
        number = out(k + 8:k + 6 + index(out(k + 8:), ':'))
    end function pushed

    subroutine settle()
        !! Waits until no landing is queued or running.
        integer :: k
        do k = 1, 300
            call run(in//fy//' jobs')
            if (index(out, ' queued ') == 0 .and. index(out, ' running ') == 0) return
            call proc_sleep_ms(200)
        end do
    end subroutine settle

    subroutine wait_for(word, shown)
        !! Waits until the list of jobs shows `word`, or shows it no more.
        character(len=*), intent(in) :: word
        logical, intent(in) :: shown
        integer :: k
        do k = 1, 300
            call run(in//fy//' jobs')
            if ((index(out, word) > 0) .eqv. shown) return
            call proc_sleep_ms(100)
        end do
    end subroutine wait_for

    integer function count_of(text, word) result(n)
        character(len=*), intent(in) :: text, word
        integer :: pos, k
        n = 0
        pos = 1
        do
            k = index(text(pos:), word)
            if (k == 0) exit
            n = n + 1
            pos = pos + k + len(word) - 1
        end do
    end function count_of

    integer function lines(path) result(n)
        character(len=*), intent(in) :: path
        n = count_of(read_all(path), new_line('a'))
    end function lines

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_line(path) result(line)
        character(len=*), intent(in) :: path
        character(len=4096) :: line
        integer :: u, ios
        line = ''
        open (newunit=u, file=path, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
    end function read_line

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: u, ios, n
        text = ''
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=ios) text
        close (u)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_policy
