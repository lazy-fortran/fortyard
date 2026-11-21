program test_cli
    !! The command line client against a running server: fy login takes the
    !! token on standard input; in a checkout with a remote on the server,
    !! fy land BRANCH pushes that branch (HEAD when there is none), lands it and
    !! exits 0 once it landed, and 1
    !! with the check's output when it failed; fy jobs and fy landings print
    !! tables.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_cli_data', work = 'build/test_cli_work'
    character(len=*), parameter :: git = 'env GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 '// &
        'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, server, fy, env, out
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, i
    logical :: exists

    sup = 0
    call getcwd(cwd)
    ! The repository exists from an earlier version of the server, with its hook.
    call execute_command_line('rm -rf '//data//' '//work//' '//work//'-other && mkdir -p '//data// &
        ' && git init -q --bare '//data//'/repos/demo.git && printf "#!/bin/sh\nexit 0\n" > '// &
        data//'/repos/demo.git/hooks/pre-receive', exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
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
        call execute_command_line('grep -q allow-large '//data// &
            '/repos/demo.git/hooks/pre-receive', exitstat=stat)
        exists = stat == 0
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call check(exists, 'a server start replaces the hook of an existing repository')
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = trim(read_line(data//'/fy'))
    ! The client's own configuration and git's stay inside the test.
    env = 'env XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 '

    call run('printf "%s\n" '//admin//' | '//env//fy//' login --server '//server)
    call check(stat == 0 .and. index(out, 'logged in to '//server//' as admin') > 0, &
        'fy login takes the token on standard input: '//out)

    call run(env//fy//' ingest demo 7 --head '//repeat('a', 40))
    call check(stat /= 0 .and. index(out, '--approve-execution') > 0, &
        'ingest requires explicit approval before any request: '//out)
    call run(env//fy//' ingest demo 7 --approve-execution --head '//repeat('a', 40)//' --json')
    call check(stat /= 0 .and. index(out, 'ingest needs a GitHub upstream') > 0, &
        'approved ingest reaches the server with parsed arguments: '//out)

    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\necho check ran\n'// &
        'test -f README\n" > '//work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check'// &
        ' && '//git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m First && '// &
        git//'-C '//work//' remote add yard '//server//'/demo.git && '//git//'-C '//work// &
        ' checkout -q -b first', exitstat=stat)
    call check(stat == 0, 'a checkout with a remote on the server')
    call run('cd '//work//' && '//env//fy//' land --wait')
    call check(stat == 0 .and. index(out, 'landing 1 of demo first queued') > 0 .and. &
        index(out, 'landed as ') > 0, 'fy land pushes the branch and follows it: '//out)

    ! With fy as git's credential helper, plain git fetches from the server.
    call run('cd '//work//' && '//env//'git -c credential.helper= -c "credential.helper=!'// &
        fy//' git-credential" fetch -q yard main && git checkout -q -b broken FETCH_HEAD')
    call check(stat == 0, 'git fetches through fy''s credential helper: '//out)
    call execute_command_line(git//'-C '//work//' rm -q README && '//git//'-C '// &
        work//' commit -q -m Broken', exitstat=stat)
    call run('cd '//work//' && '//env//fy//' land broken --wait')
    call check(stat /= 0 .and. index(out, 'landing 2 failed') > 0 .and. &
        index(out, 'check ran') > 0, 'a failed landing exits 1 with the check''s output: '//out)

    call run(env//fy//' jobs')
    call check(index(out, 'ID  KIND     REPO  STATE   STARTED') == 1 .and. index(out, 'landed') > 0 &
        .and. index(out, 'broken') > 0, 'fy jobs prints a table: '//out)
    call run('cd '//work//' && '//env//fy//' landings')
    call check(index(out, 'LANDING  STATE') == 1 .and. index(out, 'first') > 0, &
        'fy landings finds the repository from the checkout: '//out)
    call run('cd '//work//' && '//env//fy//' land --help && '//env//fy//' jobs')
    call check(stat == 0 .and. index(out, 'queued') == 0 .and. index(out, 'land ') > 0 .and. &
        index(out, new_line('a')//'3 ') == 0, 'asking for help lands nothing: '//out)

    ! Agents use the same operations through fy mcp.
    call run('printf "%s\n" ''{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":'// &
        '"fortyard","arguments":{"action":"changes","repo":"demo"}}}'' | '//env//fy//' mcp')
    call check(index(out, '\"changes\":[') > 0 .and. index(out, '"isError":false') > 0, &
        'fy mcp lists the changes of a repository: '//out)

    ! The queue numbers of the one landed and the one failed entry.
    call run(env//fy//' stats')
    call check(stat == 0 .and. index(out, 'Queue numbers · all repositories · 7 days') == 1 &
        .and. index(out, ' landed          1   ') > 0 .and. index(out, ' land to main    p50 ') > 0 &
        .and. index(out, ' failed entries  1   50 % of the finished entries') > 0 .and. &
        index(out, ' first entry     100 % landed') > 0, 'fy stats prints the numbers: '//out)
    call run(env//fy//' stats demo --days 30 --json')
    call check(stat == 0 .and. index(out, '{"repo":"demo","days":30,') == 1 .and. &
        index(out, '"landed":1,') > 0, 'fy stats REPO --days N --json prints the answer: '//out)
    call run('cd '//work//' && '//env//fy//' board --stats --days 1')
    call check(stat == 0 .and. index(out, 'Queue numbers · demo · 1 day'//new_line('a')) == 1 .and. &
        index(out, ' landed          1   ') > 0, &
        'fy board --stats prints those of the checkout''s repository: '//out)
    call run(env//fy//' stats --days 5')
    call check(stat /= 0 .and. index(out, 'days is 1, 7 or 30') > 0, &
        'fy stats refuses another window: '//out)
    call run(env//fy//' stats nowhere')
    call check(stat /= 0 .and. index(out, 'no such repository') > 0, &
        'fy stats refuses a repository that is not there: '//out)
    call run('printf "%s\n" ''{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":'// &
        '"stats","arguments":{"repo":"demo","days":30}}}'' | '//env//fy//' mcp')
    call check(index(out, '\"days\":30') > 0 .and. index(out, '\"landed\":1') > 0 .and. &
        index(out, '"isError":false') > 0, 'the MCP tool stats answers the numbers: '//out)
    call run('printf "%s\n" ''{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":'// &
        '"stats","arguments":{}}}'' | '//env//fy//' mcp')
    call check(index(out, '\"repo\":\"\",\"days\":7') > 0 .and. index(out, '"isError":false') > 0, &
        'the MCP tool stats without arguments answers for the instance: '//out)

    ! fy push refuses a file over 1 MiB before it sends anything, unless
    ! .fortyard/allow-large allows it.
    call run('cd '//work//' && git checkout -q -b heavy FETCH_HEAD && head -c 2000000 '// &
        '/dev/urandom > data.bin && '//git//'add data.bin && '//git//'commit -q -m Heavy && '// &
        env//fy//' push')
    call check(stat /= 0 .and. index(out, 'fy: not pushed, files over the size limit') > 0 .and. &
        index(out, 'data.bin (2000000 bytes)') > 0, 'fy push names a file over 1 MiB: '//out)
    call run('git --git-dir='//data//'/repos/demo.git rev-parse -q --verify refs/heads/heavy')
    call check(stat /= 0, 'nothing of the refused push reached the server')
    call run('cd '//work//' && printf "*.bin\n" > .fortyard/allow-large && '//git//'add -A && '// &
        git//'commit -q -m "Allow it" && '//env//fy//' push')
    call check(stat == 0 .and. index(out, 'change #') > 0, 'fy push sends an allowed file: '//out)

    ! History the server already has is not held against a push, also where
    ! main has moved on since: it once held a large file and allowed it.
    call execute_command_line('cd '//work//' && git checkout -q -b old FETCH_HEAD && '// &
        'head -c 2000000 /dev/urandom > old.bin && printf "*.bin\n" > .fortyard/allow-large'// &
        ' && '//git//'add -A && '//git//'commit -q -m "Large and allowed" && '//git// &
        'rm -q old.bin .fortyard/allow-large && '//git//'commit -q -m "Gone" && '// &
        'FY_LANDING=1 '//git//'push -q '//trim(cwd)//'/'//data//'/repos/demo.git HEAD:main && '// &
        git//'clone -q -b main '//trim(cwd)//'/'//data//'/repos/demo.git '//trim(cwd)//'/'//work// &
        '-other && cd '//trim(cwd)//'/'//work//'-other && printf "on\n" > moved && '//git// &
        'add moved && '//git//'commit -q -m "Main moves on" && FY_LANDING=1 '//git//'push -q '// &
        'origin HEAD:main', exitstat=stat)
    call check(stat == 0, 'main moves on past a large file that is gone again')
    call run('cd '//work//' && git checkout -q -b behind && printf "more\n" >> README && '// &
        git//'commit -q -am "Behind main" && '//env//fy//' push')
    call check(stat == 0 .and. index(out, 'change #') > 0, &
        'fy push from a checkout behind main sends what the server accepts: '//out)

    ! A named branch is pushed as it is, also from a checkout on another branch.
    call run('cd '//work//' && git checkout -q -b second && printf "two\n" > SECOND && '// &
        git//'add SECOND && '//git//'commit -q -m Second && git checkout -q behind && '// &
        env//fy//' push second && git rev-parse second && git --git-dir='//trim(cwd)//'/'// &
        data//'/repos/demo.git rev-parse refs/heads/second')
    call check(stat == 0 .and. index(out, 'change #') > 0 .and. same_last_lines(out), &
        'fy push BRANCH sends that branch, not HEAD: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'cli: all checks passed'

contains

    subroutine run(command)
        !! Runs a shell command; `out` is its output, `stat` its exit status.
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

    logical function same_last_lines(text)
        !! The last two lines of `text`, two commit ids, are the same.
        character(len=*), intent(in) :: text
        integer :: last, before

        last = index(text(:len(text) - 1), new_line('a'), back=.true.)
        before = index(text(:max(last - 1, 0)), new_line('a'), back=.true.)
        same_last_lines = last > 0 .and. len(text) - last > 40 .and. &
            text(before + 1:last - 1) == text(last + 1:len(text) - 1)
    end function same_last_lines

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_cli
