program test_git_http
    !! A git client clones, fetches and pushes over HTTP with an access token;
    !! pushes need the admin scope and a primary repository, and a push larger
    !! than git's buffer arrives intact in chunked encoding.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, tcp_connect, sock_send_all, sock_recv, sock_close, &
        sock_bound_port, sock_set_timeouts
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_mirror_interval
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_git_http_data', &
        work = 'build/test_git_http_work', up = 'build/test_git_http_up'
    ! The test's repositories ignore the machine's git configuration and hooks.
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_ASKPASS=true '// &
        'GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 '// &
        'git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: admin, reader, url, base, reply
    character(len=4096) :: cwd
    integer :: lfd, port, stat, sup, i
    logical :: exists

    sup = 0
    call execute_command_line('rm -rf '//data//' '//work//' '//work//'-clone '//work//'-anon '//up// &
        ' && mkdir -p '//data// &
        ' && git init -q -b main '//up//' && '//git//'-C '//up//' commit -q --allow-empty'// &
        ' -m upstream', exitstat=stat)
    call check(stat == 0, 'directories prepared')
    call getcwd(cwd)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'reader', 'repo:read', reader, stat)
    call check(stat == 0, 'reader token created')
    call store_repo_add(db, 'demo', '', stat, mode='primary')
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

    ! Push a first commit with the administrator token.
    url = 'http://x:'//admin//'@'//base//'/demo.git'
    call execute_command_line('git init -q -b main '//work//' && printf "hello\n" > '//work// &
        '/README && '//git//'-C '//work//' add README && '//git//'-C '//work// &
        ' commit -q -m "First commit" && '//git//'-C '//work//' push -q '//url//' main', &
        exitstat=stat)
    call check(stat == 0, 'an administrator pushes')
    reply = get('/api/v1/repos/demo/log?ref=main')
    call check(index(reply, '"subject":"First commit"') > 0, 'the pushed commit is in the API')

    ! Read with a reading token; the URL without ".git" works too.
    call execute_command_line(git//'clone -q http://x:'//reader//'@'//base//'/demo '// &
        work//'-clone && cmp -s '//work//'/README '//work//'-clone/README', exitstat=stat)
    call check(stat == 0, 'a reader clones')
    call execute_command_line(git//'clone -q http://'//base//'/demo.git '//work// &
        '-anon >/dev/null 2>&1', exitstat=stat)
    call check(stat /= 0, 'nobody clones without a token')

    ! Writing needs the admin scope and a primary repository.
    call execute_command_line('printf "more\n" >> '//work//'/README && '//git//'-C '//work// &
        ' commit -q -am "Second commit" && '//git//'-C '//work//' push -q http://x:'// &
        reader//'@'//base//'/demo.git main >/dev/null 2>&1', exitstat=stat)
    call check(stat /= 0, 'a reader cannot push')
    call execute_command_line(git//'-C '//work//' push -q http://x:'//admin//'@'//base// &
        '/mir.git main:side >/dev/null 2>&1', exitstat=stat)
    call check(stat /= 0, 'a mirror refuses pushes')

    ! Larger than git's 1 MiB post buffer: the body arrives in chunked encoding.
    call execute_command_line('head -c 3000000 /dev/urandom > '//work//'/big && '//git// &
        '-C '//work//' add big && '//git//'-C '//work//' commit -q -m "Large file" && '// &
        git//'-C '//work//' push -q '//url//' main && '//git//'-C '//work// &
        '-clone pull -q && cmp -s '//work//'/big '//work//'-clone/big', exitstat=stat)
    call check(stat == 0, 'a 3 MB push arrives intact and is fetched again')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'git over HTTP: all checks passed'

contains

    function get(path) result(r)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: r
        character(len=4096) :: buf
        integer :: cfd, n, st

        r = ''
        call tcp_connect('127.0.0.1', port, cfd, st)
        if (st /= 0) return
        call sock_set_timeouts(cfd, 5)
        call sock_send_all(cfd, 'GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            admin//CRLF//CRLF, st)
        do
            n = sock_recv(cfd, buf)
            if (n <= 0) exit
            r = r//buf(1:n)
        end do
        call sock_close(cfd)
    end function get

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
