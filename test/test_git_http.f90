program test_git_http
    !! A git client clones, fetches and pushes over HTTP with an access token;
    !! pushes need the admin scope and a primary repository, and a push larger
    !! than git's buffer arrives intact in chunked encoding.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_mirror_interval
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_git_http_data', &
        work = 'build/test_git_http_work', up = 'build/test_git_http_up', &
        target = 'build/test_git_http_target.git'
    ! The test's repositories ignore the machine's git configuration and hooks.
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_ASKPASS=true '// &
        'GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 '// &
        'git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, reader, url, base, reply
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, i
    integer(int64) :: t0, t1, rate
    logical :: exists

    sup = 0
    call execute_command_line('rm -rf '//data//' '//work//' '//work//'-clone '//work//'-anon '//up//' '//target// &
        ' && mkdir -p '//data//' && git init -q --bare '//target// &
        ' && git init -q -b main '//up//' && '//git//'-C '//up//' commit -q --allow-empty'// &
        ' -m upstream', exitstat=stat)
    call check(stat == 0, 'directories prepared')
    call getcwd(cwd)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'reader', 'repo:read', reader, stat)
    call check(stat == 0, 'reader token created')
    call store_repo_add(db, 'demo', trim(cwd)//'/'//target, stat, mode='primary')
    call check(stat == 0, 'primary repository registered')
    call store_repo_add(db, 'mir', trim(cwd)//'/'//up, stat)
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_mirror_interval(1)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    base = '127.0.0.1:'//itoa(port)
    do i = 1, 150
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) inquire (file=data//'/repos/mir.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call check(exists, 'the primary repository is created and the mirror cloned')

    ! main takes no pushes; a pushed branch lands on it as one squashed commit
    ! after the repository's check passed, and goes on to the mirror target.
    url = 'http://x:'//admin//'@'//base//'/demo.git'
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && '//git//'-C '// &
        work//' add -A && '//git//'-C '//work//' commit -q -m "First commit" && '//git// &
        '-C '//work//' push -q '//url//' HEAD:first', exitstat=stat)
    call check(stat == 0, 'an administrator pushes a branch')
    call execute_command_line(git//'-C '//work//' push -q '//url//' HEAD:main >/dev/null 2>&1', &
        exitstat=stat)
    call check(stat /= 0, 'main takes no pushes')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call check(index(reply, 'HTTP/1.1 202') == 1, 'a landing is queued')
    call wait_landing(1, 'landed')
    reply = get('/api/v1/repos/demo/log?ref=main')
    call check(index(reply, '"subject":"First commit (#1)"') > 0, 'the branch landed as #1')
    call execute_command_line('test "$(git --git-dir='//target//' rev-parse main)" = '// &
        '"$(git --git-dir='//data//'/repos/demo.git rev-parse main)" && ! git --git-dir='// &
        data//'/repos/demo.git rev-parse -q --verify first', exitstat=stat)
    call check(stat == 0, 'main went to the mirror target and the branch is gone')
    call execute_command_line(git//'-C '//work//' tag v1 && '//git//'-C '//work// &
        ' push -q '//url//' v1', exitstat=stat)
    call check(stat == 0, 'an administrator pushes a tag')
    do i = 1, 100
        call execute_command_line('git --git-dir='//target//' rev-parse -q --verify'// &
            ' refs/tags/v1 >/dev/null', exitstat=stat)
        if (stat == 0) exit
        call proc_sleep_ms(100)
    end do
    call check(stat == 0, 'the tag went to the mirror target')
    reply = get('/api/v1/jobs')
    call check(index(reply, '"totals":{"running":0,"queued":0,"landed":1,"failed":0}') > 0 .and. &
        index(reply, '"id":1,"kind":"landing","repo":"demo","branch":"first"') > 0 .and. &
        index(reply, '"started":"20') > 0, 'the jobs list the landing with totals and start time')
    reply = get('/api/v1/jobs/1')
    call check(index(reply, '"log":"') > 0 .and. index(reply, 'Landed as') > 0, 'a job shows its log')
    ! A restart during a landing runs it again; it must find its own result.
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, "UPDATE landings SET state = 'queued' WHERE id = 1;", stat)
    call db_close(db)
    call execute_command_line('echo jobs > '//data//'/events/jobs')
    do i = 1, 300
        reply = get('/api/v1/repos/demo/landings')
        if (index(reply, 'before a restart') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"landing":1,"branch":"first","account":"admin","state":"landed"') > 0 &
        .and. index(reply, 'before a restart') > 0, 'a repeated landing finds its own commit')

    ! Read with a reading token; the URL without ".git" works too.
    call execute_command_line(git//'clone -q http://x:'//reader//'@'//base//'/demo '// &
        work//'-clone && cmp -s '//work//'/README '//work//'-clone/README', exitstat=stat)
    call check(stat == 0, 'a reader clones')
    call execute_command_line(git//'clone -q http://'//base//'/demo.git '//work// &
        '-anon >/dev/null 2>&1', exitstat=stat)
    call check(stat /= 0, 'nobody clones without a token')

    ! Told a length it never gets, git's backend answers and then waits
    ! forever, and the worker with it: only the body of a POST is passed on.
    call system_clock(t0, rate)
    reply = exchange('GET /demo.git/info/refs?service=git-upload-pack HTTP/1.1'//CRLF// &
        'Authorization: Bearer '//reader//CRLF//'Content-Length: 5'//CRLF//CRLF//'hello')
    call system_clock(t1)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. index(reply, 'refs/heads/main') > 0 &
        .and. t1 - t0 < 3*rate, 'a GET with a body is answered by git and ends')

    ! Writing needs the admin scope and a primary repository.
    call execute_command_line('printf "more\n" >> '//work//'-clone/README && '//git//'-C '// &
        work//'-clone commit -q -am "Second commit" && '//git//'-C '//work// &
        '-clone push -q http://x:'//reader//'@'//base//'/demo.git HEAD:second >/dev/null 2>&1', &
        exitstat=stat)
    call check(stat /= 0, 'a reader cannot push')
    call execute_command_line(git//'-C '//work//' push -q http://x:'//admin//'@'//base// &
        '/mir.git main:side >/dev/null 2>&1', exitstat=stat)
    call check(stat /= 0, 'a mirror refuses pushes')

    ! A branch whose check fails does not land.
    call execute_command_line(git//'-C '//work//'-clone rm -q README && '//git//'-C '//work// &
        '-clone commit -q -m "Remove README" && '//git//'-C '//work//'-clone push -q '//url// &
        ' HEAD:broken', exitstat=stat)
    call check(stat == 0, 'a branch with a failing check is pushed')
    ! A page learns of the change from the event stream, without asking.
    call execute_command_line('curl -sN -H "Authorization: Bearer '//admin//'" '// &
        'http://'//base//'/api/v1/events?topic=jobs > '//work//'-events & echo $! > '// &
        work//'-events.pid; sleep 0.5', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"broken"}')
    call wait_landing(2, 'failed')
    call execute_command_line('sleep 0.5; kill $(cat '//work//'-events.pid); grep -q "^data: now" '// &
        work//'-events && grep -q "^data: changed" '//work//'-events', exitstat=stat)
    call check(stat == 0, 'the event stream announces the change of the jobs')
    reply = get('/api/v1/repos/demo/log?ref=main')
    call check(index(reply, 'Remove README') == 0, 'main stays where it was')

    ! A push brings no file over 1 MiB, also not one a later commit removes
    ! again, unless .fortyard/allow-large of the pushed commit allows it.
    call execute_command_line('head -c 3000000 /dev/urandom > '//work//'/big && '//git// &
        '-C '//work//' add big && '//git//'-C '//work//' commit -q -m "Large file" && '// &
        git//'-C '//work//' rm -q --cached big && '//git//'-C '//work//' commit -q -m "Remove it"'// &
        ' && ! '//git//'-C '//work//' push -q '//url//' HEAD:big 2> '//work//'-refused && '// &
        'grep -q "Files over the size limit (allow them in .fortyard/allow-large):" '//work//'-refused'// &
        ' && grep -q "big (3000000 bytes)" '//work//'-refused && ! git --git-dir='//data// &
        '/repos/demo.git rev-parse -q --verify refs/heads/big', exitstat=stat)
    call check(stat == 0, 'a push with a file over 1 MiB in its history is refused and names it')
    ! Larger than git's 1 MiB post buffer: the body arrives in chunked encoding.
    call execute_command_line('printf "b?g\n" > '//work//'/.fortyard/allow-large && '//git// &
        '-C '//work//' add -A && '//git//'-C '//work//' commit -q -m "Allow it" && '// &
        git//'-C '//work//' push -q '//url//' HEAD:big && '//git//'-C '//work// &
        '-clone fetch -q origin big && '//git//'-C '//work//'-clone show FETCH_HEAD:big | '// &
        'cmp -s - '//work//'/big', exitstat=stat)
    call check(stat == 0, 'an allowed 3 MB push arrives intact and is fetched again')
    ! The same content under a second path that no pattern allows is still refused.
    call execute_command_line('cp '//work//'/big '//work//'/copy && '//git//'-C '//work// &
        ' add copy && '//git//'-C '//work//' commit -q -m "Copy" && ! '//git//'-C '//work// &
        ' push -q '//url//' HEAD:two 2> '//work//'-refused && grep -q "copy (3000000 bytes)" '// &
        work//'-refused && ! grep -q "big (" '//work//'-refused', exitstat=stat)
    call check(stat == 0, 'an allowed large file is refused under a path that is not allowed')
    ! The limit is the repository's own where .fortyard/size-limit gives one.
    call execute_command_line(git//'-C '//work//' rm -q -f copy && printf "1000\n" > '//work// &
        '/.fortyard/size-limit && head -c 2000 /dev/urandom > '//work//'/mid && '//git//'-C '// &
        work//' add -A && '//git//'-C '//work//' commit -q --amend -m "Limit" && ! '//git// &
        '-C '//work//' push -q '//url//' HEAD:two 2> '//work//'-refused && '// &
        'grep -q "mid (2000 bytes)" '//work//'-refused', exitstat=stat)
    call check(stat == 0, 'a push is held to the limit in .fortyard/size-limit')
    ! A landing must not bring a file over the limit onto main either, here on a
    ! branch that reached the repository past the check on receive.
    call execute_command_line(git//'-C '//work//'-clone fetch -q origin main && '//git//'-C '//work// &
        '-clone checkout -q -B large origin/main && printf "x\n" > '//work//'-clone/small && '// &
        git//'-C '//work//'-clone add small && '//git//'-C '//work//'-clone commit -q -m Large && '// &
        git//'-C '//work//'-clone push -q '//url//' HEAD:large && cp '//work//'/big '//work//'/mid '// &
        work//'-clone && cp '//work//'/.fortyard/size-limit '//work//'-clone/.fortyard && '// &
        git//'-C '//work//'-clone add big mid .fortyard/size-limit && '//git//'-C '//work// &
        '-clone commit -q --amend --no-edit && FY_LANDING=1 '//git//'-C '//work// &
        '-clone push -q --force '//trim(cwd)//'/'//data//'/repos/demo.git HEAD:large', exitstat=stat)
    call check(stat == 0, 'a branch with a large file is placed in the repository')
    reply = post('/api/v1/repos/demo/land', '{"branch":"large"}')
    call wait_landing(3, 'failed')
    reply = get('/api/v1/jobs/3')
    call check(index(reply, 'Files over the size limit') > 0 .and. index(reply, 'big (3000000 bytes)') > 0 &
        .and. index(reply, 'mid (2000 bytes)') > 0, 'a landing refuses files over the size limit')

    ! A branch that is main plus one merge commit lands as that merge: the
    ! merged history stays an ancestor of main.
    call execute_command_line(git//'-C '//work//'-clone fetch -q origin main && '//git//'-C '// &
        work//'-clone checkout -q -B side origin/main && printf "u\n" > '//work//'-clone/up && '// &
        git//'-C '//work//'-clone add up && '//git//'-C '//work//'-clone commit -q -m '// &
        '"Upstream change" && '//git//'-C '//work//'-clone checkout -q -B sync origin/main && '// &
        git//'-C '//work//'-clone merge -q --no-ff -m "Merge upstream" side && '//git//'-C '// &
        work//'-clone push -q '//url//' HEAD:sync', exitstat=stat)
    call check(stat == 0, 'a merge onto main is pushed')
    ! Each branch pushed above opened a change (#1 to #4), so this is #5.
    reply = post('/api/v1/repos/demo/land', '{"branch":"sync"}')
    call wait_landing(4, 'landed')
    call execute_command_line('test "$(git --git-dir='//data//'/repos/demo.git rev-parse main^2)" = '// &
        '"$('//git//'-C '//work//'-clone rev-parse side)" && test "$(git --git-dir='//data// &
        '/repos/demo.git log -1 --format=%s main)" = "Merge upstream (#5)"', exitstat=stat)
    call check(stat == 0, 'the merge landed as a merge commit with the merged history')
    ! A merge onto an older main would lose that history when squashed.
    call execute_command_line(git//'-C '//work//'-clone checkout -q -B side2 origin/main && '// &
        'printf "v\n" > '//work//'-clone/up2 && '//git//'-C '//work//'-clone add up2 && '//git// &
        '-C '//work//'-clone commit -q -m "Second upstream change" && '//git//'-C '//work// &
        '-clone checkout -q -B stale origin/main && '//git//'-C '//work//'-clone merge -q '// &
        '--no-ff -m "Merge onto old main" side2 && '//git//'-C '//work//'-clone push -q '//url// &
        ' HEAD:stale', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"stale"}')
    call wait_landing(5, 'failed')
    reply = get('/api/v1/jobs/5')
    call check(index(reply, 'merge onto an older main') > 0, 'a merge onto an older main is refused')

    ! Changed Fortran files are formatted on landing, except those
    ! .fortyard/format-exclude matches.
    call execute_command_line(git//'-C '//work//'-clone fetch -q origin main && '//git//'-C '// &
        work//'-clone checkout -q -B style origin/main && touch '//work//'-clone/CMakeLists.txt'// &
        ' && mkdir -p '//work//'-clone/ext && printf "program p\nx=1\nend program\n" > '//work// &
        '-clone/ext/p.f90 && '//git//'-C '//work//'-clone add -A && '//git//'-C '//work// &
        '-clone commit -q -m "Foreign style" && '//git//'-C '//work//'-clone push -q '//url// &
        ' HEAD:style', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"style"}')
    call wait_landing(6, 'landed')
    call execute_command_line(git//'-C '//work//'-clone fetch -q origin main && '//git// &
        '-C '//work//'-clone show origin/main:ext/p.f90 | grep -qx "x=1"', exitstat=stat)
    call check(stat /= 0, 'an unformatted Fortran file lands formatted')
    call execute_command_line(git//'-C '//work//'-clone checkout -q -B foreign origin/main && '// &
        'printf "ext/*\n" > '//work//'-clone/.fortyard/format-exclude && '// &
        'printf "program q\nx=1\nend program\n" > '//work//'-clone/ext/q.f90 && '// &
        git//'-C '//work//'-clone add -A && '//git//'-C '//work//'-clone commit -q -m '// &
        '"Exclude ext" && '//git//'-C '//work//'-clone push -q '//url//' HEAD:foreign', &
        exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"foreign"}')
    call wait_landing(7, 'landed')
    call execute_command_line(git//'-C '//work//'-clone fetch -q origin main && '//git// &
        '-C '//work//'-clone show origin/main:ext/q.f90 | grep -qx "x=1"', exitstat=stat)
    call check(stat == 0, 'a file format-exclude matches lands as it is')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'git over HTTP: all checks passed'

contains

    subroutine wait_landing(number, state)
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        integer :: k
        do k = 1, 300
            reply = get('/api/v1/jobs/'//itoa(number))
            ! The job's own state, not one of its checks'.
            if (index(reply, '"state":"'//state//'","queued"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//itoa(number)//' '//state//': '//reply)
    end subroutine wait_landing

    function post(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        r = exchange('POST '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//itoa(len(body))// &
            CRLF//CRLF//body)
    end function post

    function get(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        r = exchange('GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF//CRLF)
    end function get

    function exchange(request) result(r)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, n, st

        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, request, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            r = r//buf(1:n)
        end do
        call sock_close(cfd)
    end function exchange

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
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_git_http
