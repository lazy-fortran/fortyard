program test_change_titles
    !! A change's title and description follow its newest version: a change
    !! opened on top of another unlanded branch takes that branch's commit as
    !! title, and after a rebase onto main with fewer commits it takes the
    !! title and list of its own, which its landing carries. Main merged into
    !! a branch leaves its title and description alone. A title edited by a
    !! person survives a new version, which still recomputes the description.
    use fortweb_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_change_titles_data', &
        work = 'build/test_change_titles_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=*), parameter :: nl = new_line('a')
    type(db_t) :: db
    character(len=:), allocatable :: admin, server, fy, env, out, in, fetch
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
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 && '
    in = env//'cd '//work//' && '
    fetch = 'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main'
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land --no-wait')
    call check(stat == 0, 'a first landing: '//out)
    call wait_main('Start')

    ! #2 is an unlanded branch; #3 is opened on top of it, so its commits
    ! relative to main begin with #2's.
    call run(in//fetch//' && git checkout -q -b lower FETCH_HEAD && printf "a\n" > A && '// &
        git//'add A && '//git//'commit -q -m "Lower work" && '//fy//' push')
    call check(stat == 0 .and. index(out, 'change #2') > 0, 'the lower branch opens #2: '//out)
    call run(in//'git checkout -q -b upper && printf "b\n" > B && '//git//'add B && '//git// &
        'commit -q -m "Upper one" && printf "c\n" > C && '//git//'add C && '//git// &
        'commit -q -m "Upper two" && '//fy//' push')
    call check(stat == 0 .and. index(out, 'change #3') > 0, 'the upper branch opens #3: '//out)
    call run(in//fy//' change view 3')
    call check(index(out, '#3 Lower work') == 1 .and. index(out, '- Lower work'//nl// &
        '- Upper one'//nl//'- Upper two') > 0, 'opened on #2, #3 takes its first commit: '//out)

    ! #4 starts on main; it gets main merged into it below.
    call run(in//fetch//' && git checkout -q -b solo FETCH_HEAD && printf "e\n" > E && '// &
        git//'add E && '//git//'commit -q -m "Solo work" -m "Why it matters." && '//fy//' push')
    call check(stat == 0 .and. index(out, 'change #4') > 0, 'the solo branch opens #4: '//out)

    ! Rebased onto main without #2's commit: the new version describes itself,
    ! and nobody edited it, so its landing takes the newest subject and list.
    call run(in//'git checkout -q upper && '//git//'rebase -q --onto FETCH_HEAD lower upper && '// &
        fy//' push && '//fy//' change view 3')
    call check(index(out, '#3 Upper one') > 0 .and. index(out, '2 version(s)') > 0 .and. &
        index(out, '- Upper one'//nl//'- Upper two') > 0 .and. index(out, 'Lower work') == 0, &
        'the rebased version takes its own title and list: '//out)
    call run(in//fy//' change land 3 --no-wait')
    call check(stat == 0, 'fy change land 3: '//out)
    call wait_main('(#3)')
    call run('git --git-dir='//data//'/repos/demo.git log -1 --format=%B main')
    call check(index(out, 'Upper one (#3)') == 1 .and. index(out, '- Upper one'//nl// &
        '- Upper two') > 0 .and. index(out, 'Lower work') == 0, &
        'the landing takes the newest subject and list: '//out)

    ! Main merged into a branch is no merge onto main: #4 keeps its commit's
    ! subject and body.
    call run(in//fetch//' && git checkout -q solo && '//git//'merge -q --no-edit FETCH_HEAD && '// &
        fy//' push && '//fy//' change view 4')
    call check(index(out, '#4 Solo work') > 0 .and. index(out, '2 version(s)') > 0 .and. &
        index(out, 'Why it matters.') > 0 .and. index(out, 'Merge') == 0, &
        'a merge of main keeps the branch''s own title and description: '//out)

    ! A title edited by a person stays; the description still follows.
    call run(in//fy//' change edit 4 --title "Solo edited" && printf "f\n" > F && '//git// &
        'add F && '//git//'commit -q -m "Solo more" && '//fy//' push && '//fy//' change view 4')
    call check(index(out, '#4 Solo edited') > 0 .and. index(out, '3 version(s)') > 0 .and. &
        index(out, '- Solo work'//nl//'- Solo more') > 0, &
        'an edited title survives a new version: '//out)
    call run(in//fy//' change view 4 --json')
    call check(index(out, '"title_edited":true') > 0 .and. &
        index(out, '"description_edited":false') > 0, 'the change is marked as edited: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    call execute_command_line('rm -rf '//data//' '//work)
    print '(a)', 'change titles: all checks passed'

contains

    subroutine wait_main(subject)
        !! Until main's newest commit names `subject` (the CLI polls only every 2 s).
        character(len=*), intent(in) :: subject
        integer :: k
        do k = 1, 1200
            call run('git --git-dir='//data//'/repos/demo.git log -1 --format=%s main')
            if (stat == 0 .and. index(out, subject) > 0) return
            call proc_sleep_ms(50)
        end do
        call check(.false., 'main never got '//subject//': '//out)
    end subroutine wait_main

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

end program test_change_titles
