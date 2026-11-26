program test_verify_shards
    !! A split verification (policy verify_shards=3): one verification of main
    !! queues three checks, each runs with its own FORTYARD_SHARD (1/3, 2/3
    !! and 3/3, each once), and the verification fails when one shard fails,
    !! naming that shard.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_recv, &
        sock_send_all, &
        sock_set_timeouts, &
        tcp_connect, &
        tcp_listen
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_wait
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_runner, only: runner_main
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_verify_shards_data', &
        work = 'build/test_verify_shards_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, base, reply, seen
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, run, i
    logical :: exists

    call proc_setenv('FY_RUNNER_POLL_MS', '100')
    sup = 0
    run = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_exec(db, 'UPDATE repos SET policy_bisect = 0;', stat)
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
    reply = put('/api/v1/repos/demo/policy', '{"verify_shards":3}')
    call check(index(reply, '"verify_shards":3') > 0, 'the policy splits verifications: '//reply)
    run = proc_fork()
    if (run == 0) then
        call runner_main('http://'//base, runner, data, '')
        call proc_exit(0)
    end if

    ! The verification records its shard and fails in shard 2/3 only.
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf ''#!/bin/sh\necho "$FORTYARD_SHARD" >> '//trim(cwd)//'/'//data// &
        '/shards.log\ntest "$FORTYARD_SHARD" != 2/3\n'' > '//work//'/.fortyard/verify && chmod +x '// &
        work//'/.fortyard/verify && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && git --git-dir '//data//'/repos/demo.git fetch -q '//work// &
        ' main:main > '//data//'/setup.log 2>&1', exitstat=stat)
    if (stat /= 0) call execute_command_line('cat '//data//'/setup.log')
    call check(stat == 0, 'main is in the repository')
    reply = post('/api/v1/repos/demo/verify', '')
    call check(index(reply, 'HTTP/1.1 202') == 1, 'a verification is queued: '//reply)

    do i = 1, 1800
        reply = get('/api/v1/jobs/1')
        if (index(reply, '"state":"failed","queued"') > 0) exit
        call proc_sleep_ms(100)
    end do
    call check(index(reply, '"state":"failed","queued"') > 0, 'the verification fails: '//reply)
    call check(index(reply, 'In shard 2/3 on the runner class linux.') > 0, &
        'its log names the failing shard: '//reply)
    seen = slurp(data//'/shards.log')
    call check(seen == '1/3'//new_line('a')//'2/3'//new_line('a')//'3/3'//new_line('a'), &
        'each shard ran once: '//seen)

    call proc_kill(run, SIGTERM)
    call proc_wait(run, stat)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)

contains

    function post(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        r = send('POST', path, body)
    end function post

    function put(path, body) result(r)
        character(len=*), intent(in) :: path, body
        character(len=:), allocatable :: r
        r = send('PUT', path, body)
    end function put

    function send(method, path, body) result(r)
        character(len=*), intent(in) :: method, path, body
        character(len=:), allocatable :: r
        character(len=16) :: n
        write (n, '(i0)') len(body)
        r = exchange(method//' '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '//admin// &
            CRLF//'Content-Type: application/json'//CRLF//'Content-Length: '//trim(n)//CRLF// &
            CRLF//body)
    end function send

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

    function slurp(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        character(len=256) :: line
        integer :: u, ios

        text = ''
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text//trim(line)//new_line('a')
        end do
        close (u)
    end function slurp

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (run > 0) call proc_kill(run, SIGTERM)
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_verify_shards
