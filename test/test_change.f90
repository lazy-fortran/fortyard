program test_change
    !! Changes through the command line client: fy push opens a change with the
    !! commit's subject and body; an amended push adds a version, matched by its
    !! Change-Id, also under another branch name; versions have their diff and
    !! interdiff; comments and edits show in fy change view; fy change land
    !! lands it as "TITLE (#N)" with the description and the Change-Id, and
    !! marks it landed. A landing without a change takes a number after those
    !! of open changes, and a closed change can be reopened. Issues share the
    !! numbers. fy change list filters by state, review and attention, also
    !! over every repository.
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_change_data', work = 'build/test_change_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, server, fy, env, out, in
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
    ! Every command of a chain uses the test's client configuration and git's.
    env = 'export XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 && '
    in = env//'cd '//work//' && '
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! main exists, from a first landing.
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land')
    call check(stat == 0, 'a first landing: '//out)

    ! A branch already there, as a mirror brings them, is not what the push asks to review.
    call run('cd '//data//'/repos/demo.git && git update-ref refs/heads/old-work $('//git// &
        '--git-dir . commit-tree main^{tree} -p main -m Old)')
    call check(stat == 0, 'an old branch: '//out)
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git checkout -q -b feature FETCH_HEAD && '// &
        'printf "two\n" >> README && '//git//'commit -q -am "Add a line" -m "Why it is needed."'// &
        ' -m "Change-Id: I0123abcd" && '//fy//' push')
    call check(stat == 0 .and. index(out, 'change #2: '//server//'/demo/-/changes/2') > 0, &
        'fy push opens change #2: '//out)
    call run(in//fy//' change list')
    call check(index(out, 'NUMBER  STATE  AUTHOR  VERSIONS  BRANCH   TITLE') == 1 .and. &
        index(out, '2       open   admin   1         feature  Add a line') > 0, &
        'fy change list: '//out)
    call run(in//fy//' change list demo')
    call check(stat /= 0 .and. index(out, '--repo NAME') > 0, 'a repository is named with --repo: '//out)
    call check(index(out, 'old-work') == 0, 'no change for the old branch: '//out)

    ! An amended commit pushed under another name is a new version of #2.
    call run(in//'printf "three\n" >> README && '//git//'commit -q -a --amend --no-edit && '// &
        'git checkout -q -b feature-2 && '//fy//' push')
    call check(stat == 0 .and. index(out, 'change #2') > 0, 'the amended push updates #2: '//out)
    call run(in//fy//' change view 2')
    call check(index(out, '#2 Add a line') == 1 .and. index(out, 'open, 2 version(s) by admin'// &
        ', branch feature-2') > 0 .and. index(out, 'Why it is needed.') > 0 .and. &
        index(out, 'Change-Id') == 0, 'fy change view: '//out)
    call run(in//fy//' change diff 2')
    call check(index(out, '+two') > 0 .and. index(out, '+three') > 0, 'the diff of a version: '//out)
    call run(in//fy//' change diff 2 --from 1')
    call check(index(out, '+three') > 0 .and. index(out, '+two') == 0, 'the interdiff: '//out)

    call run(in//'printf "Looks right.\n" | '//fy//' change comment 2 && '//fy// &
        ' change edit 2 --title "Add lines" && '//fy//' change view 2')
    call check(index(out, '#2 Add lines') > 0 .and. index(out, 'Looks right.') > 0, &
        'comments and edits show: '//out)

    ! A branch without a change of its own number lands after the open ones.
    call run(in//'git checkout -q -b other FETCH_HEAD && printf "x\n" > X && '//git// &
        'add X && '//git//'commit -q -m Other && '//fy//' push && '//fy//' change close 3 && '// &
        fy//' change list')
    call check(index(out, '#3 closed') > 0 .and. index(out, 'Other') == 0, &
        'a closed change leaves the list: '//out)
    call run(in//fy//' change reopen 3 && '//fy//' change list')
    call check(index(out, 'Other') > 0, 'a reopened change is listed again: '//out)

    call run(in//fy//' change land 2')
    call check(stat == 0 .and. index(out, 'landed as') > 0, 'fy change land: '//out)
    call run('git --git-dir='//data//'/repos/demo.git log -1 --format=%B main')
    call check(index(out, 'Add lines (#2)') == 1 .and. index(out, 'Why it is needed.') > 0 .and. &
        index(out, 'Change-Id: I0123abcd') > 0, 'the landed commit carries the change: '//out)
    call run(in//fy//' change view 2')
    call check(index(out, 'landed, 2 version(s) by admin') > 0, 'the change is landed with its 2 versions: '//out)

    ! Issues take numbers from the same sequence as changes.
    call run(in//'printf "Steps:\n1. run it\n" | '//fy//' issue new --title "It fails" '// &
        '--body-file - && '//fy//' issue comment 4 -m "Seen too." && '//fy//' issue view 4')
    call check(stat == 0 .and. index(out, 'issue #4: '//server//'/demo/-/issues/4') > 0 .and. &
        index(out, '#4 It fails') > 0 .and. index(out, '1. run it') > 0 .and. &
        index(out, 'Seen too.') > 0, 'an issue is opened, commented and shown: '//out)
    call run(in//fy//' repo list')
    call check(index(out, '"open_issues":1') > 0, 'the repository counts its open issue: '//out)
    call run(in//fy//' issue close 4 && '//fy//' issue list --all')
    call check(index(out, '4       closed  admin   1         It fails') > 0, &
        'fy issue list --all shows the closed issue: '//out)
    call run(in//fy//' repo list')
    call check(index(out, '"open_issues":0') > 0, 'and no closed one: '//out)
    call run(in//'git checkout -q -b later FETCH_HEAD && printf "y\n" > Y && '//git// &
        'add Y && '//git//'commit -q -m Later && '//fy//' push')
    call check(index(out, 'change #5') > 0, 'the next change follows the issue: '//out)

    ! A set counter replaces the numbers found on main and skips taken ones.
    call run(in//fy//' repo numbers demo 4 && git checkout -q -b counted FETCH_HEAD && '// &
        'printf "z\n" > Z && '//git//'add Z && '//git//'commit -q -m Counted && '//fy//' push'// &
        ' && '//fy//' issue new --title "Counted too" --body-file /dev/null')
    call check(stat == 0 .and. index(out, 'change #6') > 0 .and. index(out, 'issue #7') > 0, &
        'a set counter numbers the next change and issue: '//out)

    ! The list in every state, as the changes page filters it.
    call run(in//fy//' change list --state landed')
    call check(index(out, 'NUMBER  STATUS  REVIEW') == 1 .and. index(out, '2       landed') > 0 &
        .and. index(out, 'Other') == 0, 'fy change list --state landed: '//out)
    call run(in//fy//' change list --state all --json')
    call check(index(out, '"number":2,') > 0 .and. index(out, '"status":"landed"') > 0 .and. &
        index(out, '"status":"draft"') > 0 .and. index(out, '"next":""') > 0, &
        'every state with its word: '//out)
    ! A change whose check fails needs its author.
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git checkout -q -b broken FETCH_HEAD && '// &
        'git rm -q README && '//git//'commit -q -m "Remove the README" && '//fy//' push')
    call check(stat == 0 .and. index(out, 'change #8') > 0, 'a change that fails its check: '//out)
    call run(in//fy//' change land 8')
    call check(stat /= 0, 'its landing fails: '//out)
    call run(in//fy//' change list --attention needs-author')
    call check(index(out, '8       needs author') > 0 .and. index(out, 'Later') == 0, &
        'fy change list --attention needs-author: '//out)
    call run(in//fy//' change list --review flagged')
    call check(stat == 0 .and. len(out) == 0, 'no change is flagged: '//out)
    ! The changes of every repository need no checkout.
    call run(env//'cd / && '//fy//' change list --all-repos --state all')
    call check(index(out, 'REPO  NUMBER  STATUS') == 1 .and. index(out, 'demo  2       landed') > 0 &
        .and. index(out, 'demo  8       needs author') > 0, 'fy change list --all-repos: '//out)
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, "UPDATE changes SET flagged = 1, flag_reason = 'numerics' WHERE number = 2;", &
        stat)
    call db_close(db)
    call run(env//'cd / && '//fy//' change list --all-repos --flagged')
    call check(index(out, 'REPO  NUMBER  STATE') == 1 .and. index(out, 'demo  2       landed') > 0 &
        .and. index(out, 'numerics') > 0, 'the flagged changes of every repository name it: '//out)
    call run(in//fy//' change list')
    call check(index(out, 'NUMBER  STATE  AUTHOR  VERSIONS  BRANCH') == 1 .and. &
        index(out, 'Remove the README') < index(out, 'Other') .and. index(out, 'Add lines') == 0, &
        'the open changes as before, newest activity first: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'change: all checks passed'

contains

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

end program test_change
