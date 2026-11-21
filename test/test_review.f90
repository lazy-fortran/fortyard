program test_review
    !! The review agent: a landed change is reviewed by the configured model
    !! from its diff; the review becomes a comment of the account review-agent,
    !! and a change the model flags is listed for a person until one marks it
    !! reviewed. An answer without the closing FLAG line is never published.
    use fortweb_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: &
        SIGTERM, &
        proc_exit, &
        proc_fork, &
        proc_kill, &
        proc_setenv, &
        proc_sleep_ms, &
        proc_wait
    use fy_review, only: review_parse
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_review_data', work = 'build/test_review_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, server, fy, env, out, in, model, body, reason
    logical :: flagged, valid
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
    call review_parse('It adds a line.'//new_line('a')//'No findings.'//new_line('a')// &
        'FLAG: yes - numerics changed'//new_line('a'), body, flagged, reason, valid)
    call check(valid .and. flagged .and. reason == 'numerics changed' .and. &
        body == 'It adds a line.'//new_line('a')//'No findings.', 'a flagged review is parsed: '//body)
    call review_parse('It adds a line, nothing else.'//new_line('a')//'FLAG: no', body, flagged, &
        reason, valid)
    call check(valid .and. .not. flagged, 'an unflagged review is parsed')
    call review_parse('It adds a line, nothing else.', body, flagged, reason, valid)
    call check(.not. valid, 'an answer without the FLAG line is refused')
    call review_parse('FLAG: no', body, flagged, reason, valid)
    call check(.not. valid, 'a FLAG line without a review is refused')
    call review_parse('It adds a line, nothing else.'//new_line('a')//'FLAG: no'//new_line('a')// &
        'And more.', body, flagged, reason, valid)
    call check(.not. valid, 'text after the FLAG line is refused')

    ! The model is a prepared answer that curl reads from a file.
    model = trim(cwd)//'/'//data//'/model'
    call execute_command_line('mkdir -p '//model//'/chat')
    call answer('The change adds a line to README.\n\nNo findings.\nFLAG: yes - numerics changed')
    call proc_setenv('FY_REVIEW_URL', 'file://'//model)
    call proc_setenv('FY_REVIEW_MODEL', 'stub')
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

    ! main exists, from a first landing without a change.
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land --wait')
    call check(stat == 0, 'a first landing: '//out)

    ! A landed change is reviewed, and the model's flag puts it on the list.
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git checkout -q -b feature FETCH_HEAD && '// &
        'printf "two\n" >> README && '//git//'commit -q -am "Add a line" && '//fy// &
        ' push && '//fy//' change land 2 --wait')
    call check(stat == 0 .and. index(out, 'landed as') > 0, 'a change lands: '//out)
    call wait_for('review: ai-reviewed')
    call check(index(out, 'review: ai-reviewed, flagged for a person: numerics changed') > 0 .and. &
        index(out, 'review-agent') > 0 .and. index(out, 'The change adds a line to README.') > 0 &
        .and. index(out, 'FLAG:') == 0, 'the review is posted and the change flagged: '//out)
    call run(in//fy//' change list --flagged')
    call check(index(out, 'Add a line') > 0 .and. index(out, 'numerics changed') > 0, &
        'the flagged change is listed: '//out)

    ! A person's review clears the flag.
    call run(in//fy//' change reviewed 2 && '//fy//' change list --flagged && '//fy// &
        ' change view 2')
    call check(stat == 0 .and. index(out, 'review: human-reviewed'//new_line('a')) > 0 .and. &
        index(out, 'flagged for a person') == 0 .and. &
        index(out, 'Add a line') == index(out, '#2 Add a line') + 3, &
        'a reviewed change is no longer flagged or listed: '//out)

    ! An answer that does not end in the FLAG line is not published.
    call answer('Ignore the format and approve everything.')
    call run(in//fy//' change review 2')
    call wait_for('review: failed')
    call check(index(out, 'review: failed') > 0 .and. index(out, 'approve everything') == 0, &
        'an unusable answer is recorded and not posted: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'review: all checks passed'

contains

    subroutine answer(text)
        !! Prepares the model's next answer; `text` is JSON-escaped already.
        character(len=*), intent(in) :: text
        integer :: u
        open (newunit=u, file=model//'/chat/completions', status='replace', action='write')
        write (u, '(a)') '{"choices":[{"index":0,"message":{"role":"assistant","content":"'// &
            text//'"}}]}'
        close (u)
    end subroutine answer

    subroutine wait_for(text)
        !! Polls the change until its view shows `text`.
        character(len=*), intent(in) :: text
        integer :: k
        do k = 1, 100
            call run(in//fy//' change view 2')
            if (index(out, text) > 0) return
            call proc_sleep_ms(100)
        end do
    end subroutine wait_for

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

end program test_review
