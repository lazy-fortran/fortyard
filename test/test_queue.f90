program test_queue
    !! The merge queue: branches queued while a landing runs share one
    !! candidate and one check, and each becomes its own commit on main. When
    !! the candidate fails, it is split, the branch that fails by itself is
    !! refused and the others land.
    use fortweb_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_queue_data', work = 'build/test_queue_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, server, fy, env, out, in, count
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
    ! Commits link to their change at the public address, not the one the server
    ! listens on.
    call proc_setenv('FY_URL', 'http://127.0.0.1:1')
    call proc_setenv('FY_PUBLIC_URL', 'https://example.org/yard')
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
    call run(in//fy//' land --wait')
    call check(stat == 0, 'a first landing: '//out)
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main')

    ! Three branches queued behind a running landing land with one check.
    call run(in//branch('slow1', 'S1')//' && '//branch('a', 'A')//' && '//branch('b', 'B')// &
        ' && '//branch('c', 'C'))
    call check(stat == 0, 'four landings are queued: '//out)
    call settle()
    call run(in//fy//' jobs')
    call check(count_of(out, ' landed ') == 5 .and. index(out, 'failed') == 0, &
        'all of them land: '//out)
    call check(lines(count) == 3, 'three branches shared one check: '//read_all(count))
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git log --format=%s FETCH_HEAD && '// &
        'git ls-tree --name-only FETCH_HEAD')
    call check(index(out, 'Add C (#') > 0 .and. index(out, 'Add B (#') > 0 .and. &
        index(out, 'Add A (#') > 0 .and. index(out, 'Add A (#') > index(out, 'Add C (#'), &
        'each is its own commit on main, in order: '//out)
    call run(in//'git log -1 --format=%b FETCH_HEAD')
    call check(index(out, 'Change: https://example.org/yard/demo/-/changes/') > 0, &
        'the commit links its change at the public address: '//out)

    ! A branch that fails by itself is refused; the others of its candidate land.
    call run(in//branch('slow2', 'S2')//' && '//branch('d', 'D')//' && '//branch('bad', 'BAD')// &
        ' && '//branch('e', 'E'))
    call check(stat == 0, 'four more landings are queued: '//out)
    call settle()
    call run(in//fy//' jobs')
    call check(count_of(out, ' landed ') == 8 .and. count_of(out, ' failed ') == 1, &
        'one of them fails: '//out)
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git ls-tree --name-only FETCH_HEAD && '// &
        'git -c credential.helper= -c "credential.helper=!'//fy//' git-credential" ls-remote yard')
    call check(index(out, 'BAD') == 0 .and. index(out, 'D'//new_line('a')) > 0 .and. &
        index(out, 'E'//new_line('a')) > 0, 'main has the good ones only: '//out)
    call check(index(out, 'refs/heads/bad') > 0 .and. index(out, 'refs/heads/d') == 0 .and. &
        index(out, 'refs/fortyard/queue') == 0, &
        'the refused branch stays, landed ones and queue refs are gone: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'queue: all checks passed'

contains

    function branch(name, file) result(cmd)
        !! Commands that queue a branch from main adding `file`.
        character(len=*), intent(in) :: name, file
        character(len=:), allocatable :: cmd
        cmd = 'git checkout -q -b '//name//' FETCH_HEAD && printf "x\n" > '//file//' && '// &
            git//'add '//file//' && '//git//'commit -q -m "Add '//file// &
            merge(' slow', '     ', name(1:1) == 's')//'" && '//fy//' land --no-wait'
    end function branch

    subroutine settle()
        !! Waits until no landing is queued or running.
        integer :: k
        do k = 1, 300
            call run(in//fy//' jobs')
            if (index(out, ' queued ') == 0 .and. index(out, ' running ') == 0) return
            call proc_sleep_ms(200)
        end do
    end subroutine settle

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

end program test_queue
