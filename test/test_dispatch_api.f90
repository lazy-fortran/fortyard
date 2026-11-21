program test_dispatch_api
    !! Backends through the server and the command line: fy repo public, the
    !! policy keys backends= and egress= (backends only for a public
    !! repository), a backend process's claim with its time and concurrency
    !! limits, the result's started_at, GET /api/v1/backends with fy
    !! backends, and the extra hosts and private flag in a claim. Here the
    !! test is the backend process.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec, db_prepare, stmt_finalize, stmt_step, stmt_t, &
        stmt_text, SQLITE_ROW
    use fy_store, only: store_open, store_bootstrap, store_check_add, store_repo_add, &
        store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_dispatch_api_data'
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, server, fy, env, out, body, started
    character(len=4096) :: cwd
    character(len=16) :: num
    integer(int64) :: first, second, third
    integer :: lfd, stat, sup, i, code, k, run_s
    logical :: exists, seen_private, seen_public

    sup = 0
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'gh-1', 'runner', runner, stat)
    call store_repo_add(db, 'fx', '', stat, mode='primary')
    call store_repo_add(db, 'px', '', stat, mode='primary')
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
    do i = 1, 300
        inquire (file=data//'/repos/fx.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = trim(read_line(data//'/fy'))
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config && '
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! backends= needs a public repository; egress= takes host names.
    call run(env//fy//' repo policy fx backends=github')
    call check(stat /= 0 .and. index(out, 'public') > 0, 'a private repository names no backend: '//out)
    call run(env//fy//' repo policy fx egress=10.0.0.1')
    call check(stat /= 0, 'egress refuses an address: '//out)
    call run(env//fy//' repo public fx on')
    call check(stat == 0 .and. index(out, '"public":true') > 0, 'fy repo public: '//out)
    call run(env//fy//' repo policy fx backends=github egress=dl.example.org')
    call check(stat == 0, 'a public repository may use GitHub: '//out)
    call run(env//fy//' repo policy fx')
    call check(index(out, '"backends":"github"') > 0 .and. index(out, '"egress":"dl.example.org"') &
        > 0 .and. index(out, '"public":true') > 0, 'fy repo policy shows them: '//out)
    call run(env//fy//' repo public fx off')
    call check(stat /= 0 .and. index(out, 'backends') > 0, 'it stays public while it names them: '//out)

    ! Two linux checks and no linux runner: the backend stands in, up to its limit.
    call store_check_add(db, 'fx', 'refs/heads/a', first, stat)
    call store_check_add(db, 'fx', 'refs/heads/b', second, stat)
    call sql("UPDATE checks SET created_at = strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-1 hour');")
    call http('POST', '/api/v1/checks/claim', '{"class":"github-linux","max_running":"x"}')
    call check(code == 400, 'a limit is a number: '//body)
    call http('POST', '/api/v1/checks/claim', &
        '{"class":"github-linux","max_minutes":360,"max_running":1}')
    call check(code == 200 .and. index(body, '"id":'//itoa64(first)//',') > 0, &
        'the backend takes a check: '//body)
    call http('POST', '/api/v1/checks/claim', &
        '{"class":"github-linux","max_minutes":360,"max_running":1}')
    call check(code == 204, 'not beyond max_running: '//body)

    ! The result names when the external job started.
    call http('POST', '/api/v1/checks/'//itoa64(first)//'/result', &
        '{"passed":true,"log":"ok","started_at":"yesterday"}')
    call check(code == 400, 'started_at is a time: '//body)
    call http('POST', '/api/v1/checks/'//itoa64(first)//'/result', &
        '{"passed":true,"log":"ok","started_at":"2026-13-45T99:99:99Z"}')
    call check(code == 400, 'started_at is a valid time: '//body)
    started = value("SELECT strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-30 minutes');")
    call http('POST', '/api/v1/checks/'//itoa64(first)//'/result', &
        '{"passed":true,"log":"ok","started_at":"'//started//'"}')
    call check(code == 200, 'the result is recorded: '//body)
    call check(value('SELECT started_at FROM checks WHERE id = '//itoa64(first)//';') == started, &
        'with its start')

    ! GET /api/v1/backends and fy backends.
    call run(env//fy//' backends --json')
    call check(index(out, '"class":"github-linux","backend":true,"runners":"gh-1","running":0,'// &
        '"queued":0,"max_running":1,"max_minutes":360,"wait":180') > 0, &
        'the backend with its process, limits and median wait: '//out)
    ! 1.5 times its 1800 s and the seconds the result took to arrive.
    k = index(out, '"class":"github-linux","repo":"fx","run":') + 41
    run_s = 0
    if (k > 41) read (out(k:k + 3), *, iostat=stat) run_s
    call check(run_s >= 2700 .and. run_s < 2800 .and. out(k + 4:k + 15) == ',"checks":1}', &
        'and run(R, C) with the checks it rests on (one of 1800 s, no linux history:'// &
        ' 1.5 times it): '//out)
    call check(index(out, '"class":"linux","backend":false,"runners":"","running":0,"queued":1,') &
        > 0, 'linux with its queued check: '//out)
    call run(env//fy//' backends')
    call check(stat == 0 .and. index(out, 'github-linux') > 0 .and. index(out, 'gh-1') > 0 .and. &
        index(out, 'fx') > 0, 'fy backends: '//out)

    ! The claim names the repository's extra hosts and whether it is private,
    ! for the runner's executor proxy.
    call run(env//fy//' repo policy px egress=a.example,b.example')
    call check(stat == 0, 'a private repository may name extra hosts: '//out)
    call store_check_add(db, 'px', 'refs/heads/c', third, stat)
    seen_private = .false.
    seen_public = .false.
    do k = 1, 2
        call http('POST', '/api/v1/checks/claim', '{"class":"linux"}')
        call check(code == 200, 'a linux claim: '//body)
        if (index(body, '"repo":"px"') > 0) then
            call check(index(body, '"egress":"a.example,b.example","private":true}') > 0, &
                'the claim of a private repository carries its hosts: '//body)
            seen_private = .true.
        else
            call check(index(body, '"egress":"dl.example.org","private":false}') > 0, &
                'the claim of a public repository carries its hosts: '//body)
            seen_public = .true.
        end if
    end do
    call check(seen_private .and. seen_public, 'claims of both the private and the public '// &
        'repository')

    call db_close(db)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    sup = 0
    call execute_command_line('rm -rf '//data, exitstat=stat)

contains

    subroutine http(method, path, payload)
        character(len=*), intent(in) :: method, path, payload
        character(len=16) :: c
        integer :: ios
        call execute_command_line('curl -s -o '//data//'/body -w "%{http_code}" -X '//method// &
            ' -H "Authorization: Bearer '//runner//'" -H "Content-Type: application/json"'// &
            ' -d '''//payload//''' '//server//path//' > '//data//'/code', exitstat=stat)
        body = read_all(data//'/body')
        c = read_all(data//'/code')
        code = 0
        read (c, *, iostat=ios) code
    end subroutine http

    subroutine sql(text)
        character(len=*), intent(in) :: text
        integer :: s
        call db_exec(db, text, s)
        call check(s == 0, 'sql: '//text)
    end subroutine sql

    function value(query) result(v)
        character(len=*), intent(in) :: query
        character(len=:), allocatable :: v
        type(stmt_t) :: st
        integer :: s
        v = ''
        call db_prepare(db, query, st, s)
        if (stmt_step(st) == SQLITE_ROW) v = stmt_text(st, 0)
        call stmt_finalize(st)
    end function value

    function itoa64(k) result(s)
        integer(int64), intent(in) :: k
        character(len=:), allocatable :: s
        character(len=24) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa64

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

end program test_dispatch_api
