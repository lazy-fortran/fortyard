program test_cli
    !! The command line client against a running server: fy login takes the
    !! token on standard input; in a checkout with a remote on the server,
    !! fy land BRANCH pushes HEAD, lands it and exits 0 once it landed, and 1
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
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
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
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = trim(read_line(data//'/fy'))
    ! The client's own configuration and git's stay inside the test.
    env = 'env XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 '

    call run('printf "%s\n" '//admin//' | '//env//fy//' login --server '//server)
    call check(stat == 0 .and. index(out, 'logged in to '//server//' as admin') > 0, &
        'fy login takes the token on standard input: '//out)

    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\necho check ran\n'// &
        'test -f README\n" > '//work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check'// &
        ' && '//git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m First && '// &
        git//'-C '//work//' remote add yard '//server//'/demo.git && '//git//'-C '//work// &
        ' checkout -q -b first', exitstat=stat)
    call check(stat == 0, 'a checkout with a remote on the server')
    call run('cd '//work//' && '//env//fy//' land')
    call check(stat == 0 .and. index(out, 'landing 1 of demo first queued') > 0 .and. &
        index(out, 'landed as ') > 0, 'fy land pushes the branch and follows it: '//out)

    ! With fy as git's credential helper, plain git fetches from the server.
    call run('cd '//work//' && '//env//'git -c credential.helper= -c "credential.helper=!'// &
        fy//' git-credential" fetch -q yard main && git checkout -q -b broken FETCH_HEAD')
    call check(stat == 0, 'git fetches through fy''s credential helper: '//out)
    call execute_command_line(git//'-C '//work//' rm -q README && '//git//'-C '// &
        work//' commit -q -m Broken', exitstat=stat)
    call run('cd '//work//' && '//env//fy//' land broken')
    call check(stat /= 0 .and. index(out, 'landing 2 failed') > 0 .and. &
        index(out, 'check ran') > 0, 'a failed landing exits 1 with the check''s output: '//out)

    call run(env//fy//' jobs')
    call check(index(out, 'ID  KIND     REPO  STATE   STARTED') == 1 .and. index(out, 'landed') > 0 &
        .and. index(out, 'broken') > 0, 'fy jobs prints a table: '//out)
    call run('cd '//work//' && '//env//fy//' landings')
    call check(index(out, 'LANDING  STATE') == 1 .and. index(out, 'first') > 0, &
        'fy landings finds the repository from the checkout: '//out)

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
