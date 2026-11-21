program test_publish
    !! POST /api/v1/checks/N/publish pushes a check's commit to the
    !! repository's upstream as fortyard/check/N, for the runner that holds the
    !! check only; DELETE of the route deletes the branch, and so does the
    !! check's result. A verification on a macOS class gets the suffixes the
    !! workflow reads, and the branch of a check that was cancelled is deleted
    !! by the server's sweep. GET of the route names the upstream as
    !! OWNER/NAME with the check's state; a mirror's check is not published.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM, &
        proc_setenv, proc_run
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create, &
        store_check_add, store_check_cancel
    implicit none

    character(len=*), parameter :: data = 'build/test_publish_data', work = 'build/test_publish_work'
    character(len=*), parameter :: up = 'build/test_publish_upstream.git'
    character(len=*), parameter :: mirror = 'build/test_publish_mirror.git'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, other, base, reply, commit
    character(len=16) :: num
    character(len=4096) :: cwd
    integer(int64) :: first, second
    integer :: lfd, port, stat, sup, i
    logical :: exists

    sup = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' '//work//' '//up//' && mkdir -p '//data// &
        ' && git init -q --bare '//up//' && git init -q --bare '//mirror, exitstat=stat)
    call check(stat == 0, 'the upstream is created')
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_token_create(db, 1_int64, 'runner-2', 'runner', other, stat)
    call store_repo_add(db, 'demo', trim(cwd)//'/'//up, stat, mode='primary')
    call store_repo_add(db, 'copy', trim(cwd)//'/'//mirror, stat, mode='mirror')
    ! A GitHub class takes checks only of public repositories that allow it.
    call db_exec(db, "UPDATE repos SET public = 1, policy_backends = 'github';", stat)
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    base = '127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 300
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('git init -q -b main '//work//' && printf "hello\n" > '//work// &
        '/README && '//git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m First'// &
        ' && '//git//'-C '//work//' push -q http://x:'//admin//'@'//base//'/demo.git HEAD:topic', &
        exitstat=stat)
    call check(stat == 0, 'a branch is pushed')
    commit = line_of(git//'-C '//work//' rev-parse HEAD')

    call store_open(data//'/fortyard.db', db, stat)
    call store_check_add(db, 'demo', 'refs/heads/topic', first, stat, class='github-linux')
    call db_close(db)
    reply = request('POST', '/api/v1/checks/claim', runner, '{"class":"github-linux"}')
    call check(index(reply, '"id":1,') > 0, 'the runner claims the check: '//reply)

    reply = request('GET', '/api/v1/checks/1/publish', runner, '')
    call check(index(reply, '{"repository":"build/test_publish_upstream","state":"running"}') > 0, &
        'GET names the upstream and the check''s state: '//reply)
    reply = request('POST', '/api/v1/checks/1/publish', other, '{}')
    call check(index(reply, ' 403 ') > 0, 'another runner cannot publish the check: '//reply)
    call check(len(upstream_head('fortyard/check/1')) == 0, 'nothing is pushed for it')
    reply = request('POST', '/api/v1/checks/1/publish', runner, '{}')
    call check(index(reply, '"branch":"fortyard/check/1","commit":"'//commit//'"') > 0, &
        'the holder publishes the check: '//reply)
    call check(upstream_head('fortyard/check/1') == commit, &
        'the upstream has the check''s commit as fortyard/check/1')
    reply = request('DELETE', '/api/v1/checks/1/publish', other, '')
    call check(index(reply, ' 403 ') > 0, 'another runner cannot delete the branch: '//reply)
    reply = request('DELETE', '/api/v1/checks/1/publish', runner, '')
    call check(index(reply, '"branch":"fortyard/check/1"') > 0, 'the holder deletes it: '//reply)
    call check(len(upstream_head('fortyard/check/1')) == 0, 'the branch is gone upstream')
    reply = request('DELETE', '/api/v1/checks/1/publish', runner, '')
    call check(index(reply, '"branch":null') > 0, 'a second delete finds nothing: '//reply)

    reply = request('POST', '/api/v1/checks/1/publish', runner, '{}')
    call check(upstream_head('fortyard/check/1') == commit, 'the check is published again')
    reply = request('POST', '/api/v1/checks/1/result', runner, &
        '{"passed":true,"log":"ok","started_at":"2026-11-12T10:00:00Z"}')
    call check(index(reply, '"state":"passed"') > 0, 'the result is recorded: '//reply)
    call check(len(upstream_head('fortyard/check/1')) == 0, 'the result deletes the branch')
    reply = request('POST', '/api/v1/checks/1/publish', runner, '{}')
    call check(index(reply, ' 409 ') > 0, 'an ended check is not published: '//reply)

    call store_open(data//'/fortyard.db', db, stat)
    call store_check_add(db, 'demo', 'refs/heads/topic', second, stat, kind='verify', &
        class='github-macos-arm64')
    call db_close(db)
    reply = request('POST', '/api/v1/checks/claim', runner, '{"class":"github-macos-arm64"}')
    call check(index(reply, '"id":2,') > 0, 'the runner claims the verification: '//reply)
    reply = request('POST', '/api/v1/checks/2/publish', runner, '{}')
    call check(index(reply, '"branch":"fortyard/check/2-macos-verify"') > 0, &
        'a macOS verification gets both suffixes: '//reply)
    call check(upstream_head('fortyard/check/2-macos-verify') == commit, 'it is pushed')
    call store_open(data//'/fortyard.db', db, stat)
    call store_check_cancel(db, second)
    call db_close(db)
    ! The server's sweep runs with each round of the mirror process.
    do i = 1, 1800
        if (len(upstream_head('fortyard/check/2-macos-verify')) == 0) exit
        call proc_sleep_ms(100)
    end do
    call check(len(upstream_head('fortyard/check/2-macos-verify')) == 0, &
        'the sweep deletes the branch of a cancelled check')

    call store_open(data//'/fortyard.db', db, stat)
    call store_check_add(db, 'copy', 'refs/heads/topic', second, stat, class='github-linux')
    call db_close(db)
    reply = request('POST', '/api/v1/checks/claim', runner, '{"class":"github-linux"}')
    call check(index(reply, '"id":3,') > 0, 'the runner claims the mirror''s check: '//reply)
    reply = request('POST', '/api/v1/checks/3/publish', runner, '{}')
    call check(index(reply, ' 409 ') > 0, 'a mirror''s check is not pushed to its source: '//reply)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    call execute_command_line('rm -rf '//data//' '//work//' '//up//' '//mirror)
    print '(a)', 'publish: all checks passed'

contains

    function upstream_head(branch) result(sha)
        character(len=*), intent(in) :: branch
        character(len=:), allocatable :: sha
        sha = line_of('git --git-dir='//up//' rev-parse -q --verify refs/heads/'//branch// &
            ' 2>/dev/null; true')
    end function upstream_head

    function line_of(command) result(s)
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: s
        integer :: st
        call proc_run([character(len=2048) :: 'sh', '-c', command], s, st, 65536)
        do while (len(s) > 0)
            if (s(len(s):) /= new_line('a')) exit
            s = s(1:len(s) - 1)
        end do
    end function line_of

    function request(method, path, token, body) result(r)
        character(len=*), intent(in) :: method, path, token, body
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        character(len=16) :: n
        integer :: cfd, k, st
        write (n, '(i0)') len(body)
        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 120)
        call sock_send_all(cfd, method//' '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)// &
            CRLF//CRLF//body, st)
        do
            k = sock_recv(cfd, buf)
            if (k <= 0) exit
            r = r//buf(1:k)
        end do
        call sock_close(cfd)
    end function request

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_publish
