program test_token_scopes
    !! The scopes write and land (doc/parallel-development.md, "The life of a
    !! change"): a write token pushes and comments but cannot land, a land
    !! token lands, a token with both does both, and none of them does what
    !! needs the admin scope. Versions and landings carry the name of the
    !! token that made them as "session", null where it was not recorded.
    !! A token without admin moves branches only, opens browser sessions under
    !! its own name, and sees and revokes no token but itself. A token name is
    !! taken once per account in any case, only an admin token creates tokens,
    !! a scope is one the server knows, and after a restart the first push
    !! already meets the current pre-receive hook.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, sock_close, sock_bound_port
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_token_scopes_data', &
        work = 'build/test_token_scopes_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org'// &
        ' -c credential.helper= '
    type(db_t) :: db
    character(len=:), allocatable :: admin, writer, lander, both, server, host, fy, env, out, in
    character(len=:), allocatable :: tokens(:), fetch_main, cookie, builder
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, i, stall, holder
    logical :: exists

    sup = 0
    holder = 0
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'agent-w', 'write', writer, stat)
    call store_token_create(db, 1_int64, 'agent-l', 'land', lander, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    host = '127.0.0.1:'//trim(num)
    server = 'http://'//host
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
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 GIT_TERMINAL_PROMPT=0 GIT_ASKPASS=true && '
    in = env//'cd '//work//' && '
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! An administrator names the scopes of a session's token.
    call run(env//fy//' token create agent-wl --scopes write,land')
    call check(stat == 0 .and. index(out, '"name":"agent-wl","scopes":"write land"') > 0, &
        'fy token create --scopes: '//out)
    both = out(index(out, '"token":"') + 9:)
    both = both(1:index(both, '"') - 1)
    tokens = [character(len=len(both)) :: writer, lander, both]

    ! The administrator's push and landing carry its token's name; emptied
    ! afterwards, they stand for rows from before the session was recorded.
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land --wait')
    call check(stat == 0, 'an administrator still pushes and lands: '//out)
    call api(admin, 'GET', '/api/v1/jobs/1', '')
    call check(index(out, '"session":"bootstrap"') > 0, 'the job names the token: '//out)
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, 'UPDATE landings SET session = NULL; UPDATE change_versions SET'// &
        ' session = NULL;', stat)
    call db_close(db)
    call check(stat == 0, 'rows as from before the sessions')
    call api(admin, 'GET', '/api/v1/jobs/1', '')
    call check(index(out, '"session":null') > 0, 'a job from before has no session: '//out)
    call api(admin, 'GET', '/api/v1/jobs', '')
    call check(index(out, '"session":null') > 0, 'nor in the list of jobs: '//out)
    call api(admin, 'GET', '/api/v1/repos/demo/landings', '')
    call check(index(out, '"session":null') > 0, 'a landing from before has none: '//out)
    call api(admin, 'GET', '/api/v1/repos/demo/changes?state=all', '')
    call check(index(out, '"number":1,') > 0 .and. index(out, '"session":null') > 0, &
        'a change from before has none: '//out)
    call api(admin, 'GET', '/api/v1/repos/demo/changes/1', '')
    call check(count_of(out, '"session":null') == 3, &
        'nor its version and its landing: '//out)

    ! write: pushes, comments, opens and closes, and does not land.
    fetch_main = git//'fetch -q http://x:'//writer//'@'//host//'/demo.git main && '
    call run(in//fetch_main//'git checkout -q -b w1 FETCH_HEAD && printf "w\n" > W && '//git//'add W && '// &
        git//'commit -q -m "By the writer" && '//push(writer)//' HEAD:w1')
    call check(stat == 0, 'a write token pushes a branch: '//out)
    call api(writer, 'GET', '/api/v1/repos/demo/changes', '')
    call check(index(out, '"number":2,') > 0 .and. &
        index(out, '"session":"agent-w"') > 0, 'the list of changes names the session: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/changes/2/comments', '{"body":"Mine."}')
    call check(index(out, 'HTTP 201') > 0, 'a write token comments on a change: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/changes/2', '{"state":"closed"}')
    call check(index(out, 'HTTP 200') > 0 .and. index(out, '"state":"closed"') > 0, &
        'a write token closes a change: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/changes/2', '{"state":"open"}')
    call check(index(out, 'HTTP 200') > 0, 'and opens it again: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/issues', '{"title":"An issue","body":""}')
    call check(index(out, 'HTTP 201') > 0, 'a write token opens an issue: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/issues/3/comments', '{"body":"Seen."}')
    call check(index(out, 'HTTP 201') > 0, 'and comments on it: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/changes/2/land', '')
    call check(index(out, 'HTTP 403') > 0 .and. index(out, 'land or admin scope') > 0, &
        'a write token does not land a change: '//out)
    call api(writer, 'POST', '/api/v1/repos/demo/land', '{"branch":"w1"}')
    call check(index(out, 'HTTP 403') > 0, 'nor a branch: '//out)

    ! land: queues landings, and neither pushes nor comments.
    call run(in//push(lander)//' HEAD:l1')
    call check(stat /= 0 .and. index(out, 'write or admin scope') > 0, &
        'a land token does not push: '//out)
    call api(lander, 'POST', '/api/v1/repos/demo/changes/2/comments', '{"body":"No."}')
    call check(index(out, 'HTTP 403') > 0, 'nor comment on a change: '//out)
    call api(lander, 'POST', '/api/v1/repos/demo/issues', '{"title":"No","body":""}')
    call check(index(out, 'HTTP 403') > 0, 'nor open an issue: '//out)
    call api(lander, 'POST', '/api/v1/repos/demo/changes/2/land', '')
    call check(index(out, 'HTTP 202') > 0 .and. index(out, '"landing":2') > 0, &
        'a land token lands a change: '//out)
    call wait_job(2, 'landed')
    call check(index(out, '"session":"agent-l"') > 0, 'the job names the land token: '//out)
    call api(admin, 'GET', '/api/v1/jobs', '')
    call check(index(out, '"id":2,') > 0 .and. index(out, '"session":"agent-l"') > 0, &
        'so does the list of jobs: '//out)
    call api(admin, 'GET', '/api/v1/repos/demo/landings', '')
    call check(index(out, '"landing":2,') > 0 .and. index(out, '"session":"agent-l"') > 0, &
        'and the list of landings: '//out)
    call api(admin, 'GET', '/api/v1/repos/demo/changes/2', '')
    call check(index(out, '"pusher":"admin"') > 0 .and. &
        index(out, '"session":"agent-w"}],"comments"') > 0 .and. &
        index(out, '"state":"landed","commit":') > 0 .and. &
        index(out, '"session":"agent-l"}]}') > 0, &
        'the change names who pushed its version and who landed it: '//out)

    ! write and land: fy land --wait pushes, queues and follows with one token.
    call run(in//fetch_main//'git checkout -q -b wl1 FETCH_HEAD && printf "b\n" > B && '//git//'add B && '// &
        git//'commit -q -m "By both" && FY_TOKEN='//both//' '//fy//' land --wait')
    call check(stat == 0, 'a token with write and land pushes and lands: '//out)
    call api(both, 'GET', '/api/v1/repos/demo/changes/4', '')
    call check(index(out, '"state":"landed"') > 0 .and. count_of(out, '"session":"agent-wl"') == 3, &
        'its change, version and landing name it: '//out)

    ! What needed the admin scope besides still needs it.
    do i = 1, size(tokens)
        call api(tokens(i), 'POST', '/api/v1/tokens', '{"name":"more"}')
        call check(index(out, 'HTTP 403') > 0, 'no token is created: '//out)
        call api(tokens(i), 'PUT', '/api/v1/repos/demo/policy', '{"tests":"none"}')
        call check(index(out, 'HTTP 403') > 0, 'no policy is set: '//out)
        call api(tokens(i), 'POST', '/api/v1/repos/demo/mode', '{"mode":"mirror"}')
        call check(index(out, 'HTTP 403') > 0, 'no mode is set: '//out)
        call api(tokens(i), 'GET', '/api/v1/audit', '')
        call check(index(out, 'HTTP 403') > 0, 'the audit log stays closed: '//out)
        call api(tokens(i), 'POST', '/api/v1/repos/demo/releases', &
            '{"version":"1.0","commit":"'//repeat('a', 40)//'","notes":""}')
        call check(index(out, 'HTTP 403') > 0, 'no release is opened: '//out)
        call api(tokens(i), 'PUT', '/api/v1/repos/demo/releases/1.0/release.tar', 'archive')
        call check(index(out, 'HTTP 403') > 0, 'no release is uploaded: '//out)
        call api(tokens(i), 'POST', '/api/v1/repos/demo/changes/4/reviewed', '')
        call check(index(out, 'HTTP 403') > 0, 'no review is decided: '//out)
    end do

    ! A mistyped option creates nothing: without scopes the token would get
    ! those of the administrator.
    call run(env//fy//' token create typo --scope write')
    call check(stat == 2, 'an unknown option of fy token create is a usage error: '//out)
    call run(env//fy//' token create typo extra')
    call check(stat == 2, 'so is a second name: '//out)
    call api(admin, 'GET', '/api/v1/tokens', '')
    call check(index(out, '"name":"typo"') == 0, 'and no token is created: '//out)
    call run(env//fy//' token create agent-eq --scopes=write,land')
    call check(stat == 0 .and. index(out, '"scopes":"write land"') > 0, &
        '--scopes=LIST names the scopes: '//out)
    call run(env//fy//' token create agent-sp --scopes "write, land"')
    call check(stat == 0 .and. index(out, '"scopes":"write land"') > 0, &
        'scopes are stored separated by single spaces: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"agent-c","scopes":"write,land"}')
    call check(index(out, 'HTTP 201') > 0 .and. index(out, '"scopes":"write land"') > 0, &
        'the API takes scopes separated by commas: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"agent-x","scopes":"Write;land"}')
    call check(index(out, 'HTTP 400') > 0, 'and refuses what is no list of scopes: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"agent-t","scopes":"wirte,lnad"}')
    call check(index(out, 'HTTP 400') > 0 .and. index(out, 'no scope wirte') > 0, &
        'nor a scope it does not know: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"agent-c","scopes":"write"}')
    call check(index(out, 'HTTP 409') > 0, 'a second token of the same name is refused: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"browser session","scopes":"write"}')
    call check(index(out, 'HTTP 400') > 0, 'and so is the name of browser sessions: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"Agent-C","scopes":"write"}')
    call check(index(out, 'HTTP 409') > 0, 'also when it differs in case only: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"agent-c ","scopes":"write"}')
    call check(index(out, 'HTTP 400') > 0, 'a name does not end in a blank: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":" browser session of tester"}')
    call check(index(out, 'HTTP 400') > 0, 'nor start with one: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"Browser Session of tester"}')
    call check(index(out, 'HTTP 400') > 0, 'the name of browser sessions is kept in any case: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"builder","scopes":"release"}')
    call check(index(out, 'HTTP 201') > 0, 'a release token: '//out)
    builder = out(index(out, '"token":"') + 9:)
    builder = builder(1:index(builder, '"') - 1)
    call api(builder, 'POST', '/api/v1/tokens', '{"name":"agent-9","scopes":"release"}')
    call check(index(out, 'HTTP 403') > 0, &
        'takes no name a session may get later, as it creates no token: '//out)
    call api(admin, 'GET', '/api/v1/tokens', '')
    call check(index(out, '"name":"agent-t"') == 0 .and. count_of(out, '"name":"agent-c"') == 1 &
        .and. index(out, '"name":"browser session"') == 0 .and. index(out, '"name":"agent-c ') == 0 &
        .and. index(out, 'of tester') == 0 .and. index(out, 'Agent-C') == 0 .and. &
        index(out, 'agent-9') == 0, 'none of these is created: '//out)

    ! A push with the write scope moves branches only: not what the server
    ! records, checks and lands under refs/fortyard/, and no tags.
    call run(in//push(writer)//' +HEAD:refs/fortyard/changes/2/1')
    call check(stat /= 0 .and. index(out, 'kept by fortyard') > 0, &
        'a write token does not move a recorded version: '//out)
    call run(in//push(writer)//' :refs/fortyard/changes/2/1')
    call check(stat /= 0, 'nor delete it: '//out)
    call run(in//push(writer)//' +HEAD:refs/fortyard/queue/9 +HEAD:refs/fortyard/checks/9')
    call check(stat /= 0, 'nor write what the queue lands and the runner checks: '//out)
    call run(in//push(admin)//' +HEAD:refs/fortyard/queue/9')
    call check(stat /= 0, 'nor does an administrator: '//out)
    call run(in//push(writer)//' HEAD:refs/tags/v9')
    call check(stat /= 0 .and. index(out, 'needs the admin scope') > 0, &
        'a write token pushes no tag: '//out)
    call run(in//push(admin)//' HEAD:refs/tags/v9')
    call check(stat == 0, 'an administrator still does: '//out)
    call run('git -C '//data//'/repos/demo.git rev-parse refs/fortyard/changes/2/1')
    call check(stat == 0, 'the recorded version is still there: '//out)

    ! A browser session is no token of its own: it acts under the name of the
    ! token it was opened with and ends with it.
    call run('curl -s -i -H "Content-Type: application/json" -d ''{"token":"'//writer//'"}'' '// &
        server//'/api/v1/session')
    call check(index(out, 'fy_session=') > 0, 'a write token opens a browser session: '//out)
    cookie = out(index(out, 'fy_session=') + 11:)
    cookie = cookie(1:index(cookie, ';') - 1)
    call api(cookie, 'GET', '/api/v1/whoami', '')
    call check(index(out, '"session":"agent-w"') > 0 .and. index(out, '"scopes":"write"') > 0, &
        'the session carries the name of its token: '//out)
    call run(in//'git checkout -q -b w2 w1 && printf "s\n" > S && '//git//'add S && '// &
        git//'commit -q -m "Through the session" && '//push(cookie)//' HEAD:w2')
    call check(stat == 0, 'a push with the session: '//out)
    call api(admin, 'GET', '/api/v1/repos/demo/changes', '')
    call check(index(out, '"branch":"w2"') > 0 .and. index(out, '"session":"agent-w"') > 0 &
        .and. index(out, 'browser session') == 0, 'is recorded under the token''s name: '//out)
    call api(cookie, 'POST', '/api/v1/tokens', '{"name":"more"}')
    call check(index(out, 'HTTP 403') > 0, 'the session creates no token: '//out)

    ! A token without admin sees and revokes itself and its sessions only.
    call api(writer, 'GET', '/api/v1/tokens', '')
    call check(index(out, '"name":"agent-w"') > 0 .and. index(out, '"kind":"session"') > 0 .and. &
        index(out, 'bootstrap') == 0 .and. index(out, 'agent-l') == 0, &
        'a write token lists itself and its session: '//out)
    call api(writer, 'POST', '/api/v1/tokens/1/revoke', '{}')
    call check(index(out, 'HTTP 403') > 0, 'it does not revoke the administrator''s token: '//out)
    call api(lander, 'POST', '/api/v1/tokens/2/revoke', '{}')
    call check(index(out, 'HTTP 403') > 0, 'a land token does not revoke another token: '//out)
    call api(admin, 'GET', '/api/v1/whoami', '')
    call check(index(out, 'HTTP 200') > 0, 'the administrator''s token still works: '//out)
    call api(writer, 'POST', '/api/v1/tokens/2/revoke', '{}')
    call check(index(out, 'HTTP 200') > 0, 'a token revokes itself: '//out)
    call api(cookie, 'GET', '/api/v1/whoami', '')
    call check(index(out, 'HTTP 401') > 0, 'which ends its browser session: '//out)
    call api(admin, 'POST', '/api/v1/tokens', '{"name":"agent-w","scopes":"write"}')
    call check(index(out, 'HTTP 409') > 0, &
        'its name stays taken, as versions and landings record it: '//out)

    ! After an upgrade the first push already meets the current hook, even
    ! while the mirror process hangs on an upstream that never answers.
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    sup = 0
    ! Held by a process of its own, so the server and its git children do not
    ! inherit it and a clone ends when that process does.
    call tcp_listen('127.0.0.1', 0, stall, stat)
    write (num, '(i0)') sock_bound_port(stall)
    holder = proc_fork()
    if (holder == 0) then
        do
            call proc_sleep_ms(1000)
        end do
    end if
    call sock_close(stall)
    call store_open(data//'/fortyard.db', db, stat)
    call store_repo_add(db, 'aaa', 'http://127.0.0.1:'//trim(num)//'/aaa.git', stat)
    call db_close(db)
    call execute_command_line('printf ''#!/bin/sh\nwhile read old new ref; do'// &
        ' [ "$ref" = refs/heads/main ] && exit 1; done\nexit 0\n'' > '//data// &
        '/repos/demo.git/hooks/pre-receive', exitstat=stat)
    call check(stat == 0, 'a hook as from before')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    host = '127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    call run(in//push(both)//' HEAD:refs/tags/v10')
    call check(stat /= 0 .and. index(out, 'needs the admin scope') > 0, &
        'a write token pushes no tag right after a restart: '//out)
    call proc_kill(holder, SIGTERM)
    call proc_wait(holder, stat)
    holder = 0

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'token scopes: all checks passed'

contains

    function push(token) result(command)
        !! git push to the repository with `token` as the password.
        character(len=*), intent(in) :: token
        character(len=:), allocatable :: command
        command = git//'push -q http://x:'//token//'@'//host//'/demo.git'
    end function push

    subroutine api(token, method, path, body)
        !! One request; `out` is the answer and a last line "HTTP <status>".
        character(len=*), intent(in) :: token, method, path, body
        character(len=:), allocatable :: send
        send = ''
        if (len(body) > 0) send = ' -H "Content-Type: application/json" -d '''//body//''''
        call run('curl -s -X '//method//' -w "\nHTTP %{http_code}" -H "Authorization: Bearer '// &
            token//'"'//send//' "'//server//path//'"')
    end subroutine api

    subroutine wait_job(number, state)
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        character(len=16) :: id
        integer :: k
        write (id, '(i0)') number
        do k = 1, 300
            call api(admin, 'GET', '/api/v1/jobs/'//trim(id), '')
            if (index(out, '"state":"'//state//'"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'job '//trim(id)//' '//state//': '//out)
    end subroutine wait_job

    integer function count_of(text, word) result(n)
        character(len=*), intent(in) :: text, word
        integer :: at, k

        n = 0
        at = 1
        do
            k = index(text(at:), word)
            if (k == 0) exit
            n = n + 1
            at = at + k + len(word) - 1
        end do
    end function count_of

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
            if (holder > 0) call proc_kill(holder, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_token_scopes
