program test_runner_split
    !! A runner split into controller and executor keeps its token away from
    !! the code under check. Both are fy processes with the environment their
    !! containers have; the check looks for the token (by its hash) in its
    !! environment, in the environment and command line of every process it
    !! can see, and in every file under its home, its temporary directory,
    !! the shared directory, the repository it was fetched from and each
    !! directory its environment names. Through the executor it finds nothing
    !! and the landing lands; the same check run inside the runner, as without
    !! an executor, finds the token and fails. A check that is no longer the
    !! runner's is stopped in the executor.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, &
        proc_start, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: root = 'build/test_runner_split'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, base, url, reply, top, data, home, shared, &
        work, fy, hash, env
    character(len=4096) :: cwd, path
    character(len=16) :: num
    integer :: lfd, port, stat, sup, ctl, ex, i, n
    logical :: exists

    sup = 0
    ctl = 0
    ex = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    top = trim(cwd)//'/'//root
    data = top//'/data'
    home = top//'/runner'
    shared = top//'/shared'
    work = top//'/w'
    call execute_command_line('rm -rf '//top//' && mkdir -p '//data//' '//home//'/tmp '// &
        shared//'/home '//shared//'/tmp', exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
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
    do i = 1, 150
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do

    ! The runner's directory as on a build host: its token, and its data
    ! directory with the mirrors (remote mode).
    call write_text(home//'/token', runner)
    call execute_command_line('chmod 600 '//home//'/token && printf %s '//runner// &
        ' | sha256sum | cut -c1-64 > '//top//'/hash && fy=$(ls -t build/fo/bin/fy '// &
        'build/fo/app/fy 2>/dev/null | head -1) && echo $PWD/$fy > '//top//'/fy', exitstat=stat)
    hash = trim(read_line(top//'/hash'))
    fy = trim(read_line(top//'/fy'))
    call check(len(hash) == 64 .and. len(fy) > 0, 'the token''s hash and fy are found')
    call get_environment_variable('PATH', path, n, stat)
    env = 'PATH='//trim(path)//' GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1'

    url = 'http://x:'//admin//'@'//base//'/demo.git'
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard', &
        exitstat=stat)
    call write_lines(work//'/.fortyard/check', [character(len=200) :: &
        '#!/bin/sh', &
        '# Looks for the runner token, known by its hash, wherever the check can read.', &
        'h='//hash, &
        'dirs="$HOME ${TMPDIR:-/tmp} $(cd ../.. && pwd) $(git config --get remote.origin.url)"', &
        'for v in $(env | sed -n ''s/^[A-Za-z_][A-Za-z_0-9]*=\(\/[^ :]*\)$/\1/p''); do', &
        '    [ -d "$v" ] && dirs="$dirs $v"', &
        'done', &
        'n=$(for d in $dirs; do find "$d" -type f 2>/dev/null; done | sort -u | wc -l)', &
        'echo "searched $n files in: $dirs"', &
        'found=$({ env', &
        '    for p in /proc/[0-9]*; do cat "$p/environ" "$p/cmdline" 2>/dev/null; echo; done', &
        '    for d in $dirs; do grep -rhoaE "fy_[0-9a-f]{64}" "$d" 2>/dev/null; done; } |', &
        '    grep -oaE "fy_[0-9a-f]{64}" | sort -u | while read -r t; do', &
        '    [ "$(printf %s "$t" | sha256sum | cut -c1-64)" = "$h" ] && echo yes; done)', &
        'if [ -n "$found" ]; then echo "found the runner token"; exit 1; fi', &
        '# Links in the shared directory to where the token would be must not make', &
        '# the controller read it.', &
        'x=$(cd ../.. && pwd)/exchange t=$(git config --get remote.origin.url)/../../../token', &
        'if [ -d "$x" ]; then', &
        '    for f in output log sent; do rm -f "$x/$f"; ln -s "$t" "$x/$f"; done', &
        '    echo "links to the token placed"', &
        'fi', &
        'echo "no runner token found"'])
    call execute_command_line('chmod +x '//work//'/.fortyard/check && printf "one\n" > '// &
        work//'/README && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && '//git//'-C '//work//' push -q '//url//' HEAD:first', &
        exitstat=stat)
    call check(stat == 0, 'a branch is pushed')

    ! Controller and executor, each with its container's environment.
    call proc_start([character(len=4096) :: 'sh', '-c', 'exec env -i '//env//' HOME='//home// &
        ' FY_RUNNER_POLL_MS=100 FY_RUNNER='//home//' TMPDIR='//home//'/tmp '//fy//' runner --server http://'//base// &
        ' --token-file '//home//'/token --data '//home//'/data --executor '//shared], &
        top//'/controller.log', ctl)
    call proc_start([character(len=4096) :: 'sh', '-c', 'exec env -i '//env//' HOME='//shared// &
        '/home TMPDIR='//shared//'/tmp '//fy//' executor '//shared], top//'/executor.log', ex)
    call check(ctl > 0 .and. ex > 0, 'controller and executor start')
    reply = post('/api/v1/repos/demo/land', '{"branch":"first"}')
    call wait_landing(1, 'landed')
    reply = get('/api/v1/jobs/1')
    call check(index(reply, 'no runner token found') > 0, 'the executor''s check finds no '// &
        'token: '//reply)
    call check(index(reply, 'searched 0 files') == 0 .and. index(reply, shared) > 0, &
        'the check searched the shared directory')
    call check(index(reply, 'links to the token placed') > 0 .and. index(reply, runner) == 0, &
        'links in the shared directory do not bring the token into the log')
    reply = get('/api/v1/jobs/1/log')
    call check(index(reply, runner) == 0, 'nor into the live log')
    call execute_command_line('grep -q "fy executor: check of demo" '//top//'/executor.log', &
        exitstat=stat)
    call check(stat == 0, 'the executor ran the check')
    call stop(ctl)
    call stop(ex)

    ! The same check inside the runner: the token is within its reach.
    call proc_start([character(len=4096) :: 'sh', '-c', 'exec env -i '//env//' HOME='//home// &
        ' FY_RUNNER_POLL_MS=100 FY_RUNNER='//home//' TMPDIR='//home//'/tmp '//fy//' runner --server http://'//base// &
        ' --token-file '//home//'/token --data '//home//'/data'], top//'/runner.log', ctl)
    call execute_command_line(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '// &
        work//' checkout -q -B second FETCH_HEAD && printf "two\n" >> '//work//'/README && '// &
        git//'-C '//work//' commit -q -am Second && '//git//'-C '//work//' push -q '//url// &
        ' HEAD:second', exitstat=stat)
    reply = post('/api/v1/repos/demo/land', '{"branch":"second"}')
    call wait_landing(2, 'failed')
    reply = get('/api/v1/jobs/2')
    call check(index(reply, 'found the runner token') > 0, 'a check inside the runner finds '// &
        'the token: '//reply)
    call stop(ctl)

    ! A check that is no longer the runner's (here: released, as by a
    ! restarted runner) is stopped by the executor once the server refuses
    ! its live log; the runner takes it again and it lands. Without the stop
    ! the first run would hold the runner for ten minutes.
    call proc_start([character(len=4096) :: 'sh', '-c', 'exec env -i '//env//' HOME='//home// &
        ' FY_RUNNER_POLL_MS=100 FY_RUNNER='//home//' TMPDIR='//home//'/tmp '//fy//' runner --server http://'//base// &
        ' --token-file '//home//'/token --data '//home//'/data --executor '//shared], &
        top//'/controller.log', ctl)
    call proc_start([character(len=4096) :: 'sh', '-c', 'exec env -i '//env//' HOME='//shared// &
        '/home TMPDIR='//shared//'/tmp '//fy//' executor '//shared], top//'/executor.log', ex)
    call execute_command_line(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '// &
        work//' checkout -q -B third FETCH_HEAD', exitstat=stat)
    call write_lines(work//'/.fortyard/check', [character(len=200) :: &
        '#!/bin/sh', &
        'm=$(cd ../.. && pwd)/third-ran', &
        'if [ -e "$m" ]; then echo "second run"; exit 0; fi', &
        'touch "$m"', &
        'i=0; while [ $i -lt 600 ]; do echo "first run $i"; i=$((i+1)); sleep 1; done'])
    call execute_command_line(git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m Third && '//git//'-C '//work//' push -q '//url//' HEAD:third', &
        exitstat=stat)
    call check(stat == 0, 'a third branch is pushed')
    reply = post('/api/v1/repos/demo/land', '{"branch":"third"}')
    do i = 1, 1200
        reply = get('/api/v1/jobs/3/log')
        if (index(reply, 'first run 2') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, 'first run 2') > 0, 'the first run sends its log: '//reply)
    reply = exchange('POST /api/v1/checks/release HTTP/1.1'//CRLF//'Authorization: Bearer '// &
        runner//CRLF//'Content-Type: application/json'//CRLF//'Content-Length: 2'//CRLF//CRLF// &
        '{}')
    call check(index(reply, '"released":1') > 0, 'the check is released: '//reply)
    call wait_landing(3, 'landed')
    reply = get('/api/v1/jobs/3')
    call check(index(reply, 'second run') > 0, 'the check ran again and passed: '//reply)
    call execute_command_line('grep -q "no longer this runner" '//top//'/controller.log', &
        exitstat=stat)
    call check(stat == 0, 'the controller had the first run stopped')
    call stop(ctl)
    call stop(ex)
    call stop(sup)
    call execute_command_line('rm -rf '//top)
    print '(a)', 'runner split: all checks passed'

contains

    subroutine stop(pid)
        integer, intent(inout) :: pid
        integer :: st
        if (pid <= 0) return
        call proc_kill(pid, SIGTERM)
        call proc_wait(pid, st)
        pid = 0
    end subroutine stop

    subroutine wait_landing(number, state)
        integer, intent(in) :: number
        character(len=*), intent(in) :: state
        integer :: k
        character(len=16) :: m
        write (m, '(i0)') number
        do k = 1, 1200
            reply = get('/api/v1/jobs/'//trim(m))
            ! The job's own state, not one of its checks'.
            if (index(reply, '"state":"'//state//'","queued"') > 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//trim(m)//' '//state//': '//reply)
    end subroutine wait_landing

    function post(route, body) result(r)
        character(len=*), intent(in) :: route, body
        character(len=:), allocatable :: r
        character(len=16) :: m
        write (m, '(i0)') len(body)
        r = exchange('POST '//route//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF// &
            'Content-Type: application/json'//CRLF//'Content-Length: '//trim(m)//CRLF//CRLF//body)
    end function post

    function get(route) result(r)
        character(len=*), intent(in) :: route
        character(len=:), allocatable :: r
        r = exchange('GET '//route//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin//CRLF//CRLF)
    end function get

    function exchange(request) result(r)
        character(len=*), intent(in) :: request
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, k, st
        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, request, st)
        do
            k = sock_recv(cfd, buf)
            if (k <= 0) exit
            r = r//buf(1:k)
        end do
        call sock_close(cfd)
    end function exchange

    subroutine write_text(file, text)
        character(len=*), intent(in) :: file, text
        integer :: u
        open (newunit=u, file=file, status='replace', action='write')
        write (u, '(a)') text
        close (u)
    end subroutine write_text

    subroutine write_lines(file, lines)
        character(len=*), intent(in) :: file
        character(len=*), intent(in) :: lines(:)
        integer :: u, k
        open (newunit=u, file=file, status='replace', action='write')
        do k = 1, size(lines)
            write (u, '(a)') trim(lines(k))
        end do
        close (u)
    end subroutine write_lines

    function read_line(file) result(line)
        character(len=*), intent(in) :: file
        character(len=4096) :: line
        integer :: u, ios
        line = ''
        open (newunit=u, file=file, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
    end function read_line

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (ctl > 0) call proc_kill(ctl, SIGTERM)
            if (ex > 0) call proc_kill(ex, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_runner_split
