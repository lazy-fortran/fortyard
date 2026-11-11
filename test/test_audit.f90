program test_audit
    !! The audit log: the chain of its entries holds, an altered or removed
    !! entry is found, also at the end of the log, and the server records landings, tokens, repositories,
    !! changes, releases and sign-ins, which fy audit lists and verifies.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_audit, only: audit_t, audit_append, audit_list, audit_verify, AUDIT_GENESIS
    use fy_posix, only: tcp_listen, sock_close, sock_bound_port
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_audit_data', work = 'build/test_audit_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=*), parameter :: DROP = 'DROP TRIGGER audit_keeps_rows;'// &
        ' DROP TRIGGER audit_keeps_content;'
    type(db_t) :: db
    character(len=:), allocatable :: admin, reader, builder, server, fy, env, out, in, curl
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, i
    logical :: exists

    sup = 0
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call check_chain()
    call check_end()

    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'reader', 'repo:read', reader, stat)
    call store_token_create(db, 1_int64, 'builder', 'release', builder, stat)
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
    curl = 'curl -s -H "Content-Type: application/json" '
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! One landing that passes its check and one that fails it.
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf "#!/bin/sh\ntest -f README\n" > '// &
        work//'/.fortyard/check && chmod +x '//work//'/.fortyard/check && cd '//work// &
        ' && '//git//'add -A && '//git//'commit -q -m Start && '//git// &
        'remote add yard '//server//'/demo.git && '//git//'checkout -q -b start', &
        exitstat=stat)
    call run(in//fy//' land')
    call check(stat == 0, 'a landing that passes: '//out)
    call run(in//'git checkout -q -b broken start && git rm -q README && '//git// &
        'commit -q -m Broken && '//fy//' land')
    call check(stat /= 0 .and. index(out, 'failed') > 0, 'a landing that fails: '//out)

    call run(in//'git checkout -q -b other start && printf "x\n" > X && '//git//'add X && '// &
        git//'commit -q -m Other && '//fy//' push && '//fy//' change close 3 && '//fy// &
        ' change close 3 && '//fy//' change reopen 3')
    call check(stat == 0, 'a change is closed and reopened: '//out)
    call run(in//fy//' token create laptop && '//fy//' repo create second && '//fy// &
        ' repo mode second mirror')
    call check(stat == 0, 'a token and a repository are created: '//out)
    call run(curl//'-X POST -H "Authorization: Bearer '//admin//'" '//server// &
        '/api/v1/tokens/4/revoke')
    call check(index(out, '"revoked":true') > 0, 'a token is revoked: '//out)
    ! A NUL byte in a name ends the text a reader of the store gets.
    call run(curl//'-H "Authorization: Bearer '//reader//'" -d ''{"name":"x\u0000y"}'' '// &
        server//'/api/v1/tokens')
    call check(index(out, '"token":') > 0, 'a token with a NUL byte in its name: '//out)
    call run(curl//'-c '//data//'/jar -d ''{"token":"'//admin//'"}'' '//server// &
        '/api/v1/session && '//curl//'-b '//data//'/jar -X DELETE '//server//'/api/v1/session')
    call check(index(out, '"closed":true') > 0, 'a session is opened and closed: '//out)
    call run('printf archive > '//data//'/release.tar && '//curl//'-H "Authorization: Bearer '// &
        builder//'" -d ''{"version":"1.0","commit":"'//repeat('a', 40)//'","notes":""}'' '// &
        server//'/api/v1/repos/demo/releases && curl -s -H "Authorization: Bearer '//builder// &
        '" -T '//data//'/release.tar -H "X-Content-SHA256: $(sha256sum '//data// &
        '/release.tar | cut -c1-64)" '//server//'/api/v1/repos/demo/releases/1.0/release.tar')
    call check(index(out, '"state":"ready"') > 0, 'a release is uploaded: '//out)

    call run(in//fy//' audit list --limit 200')
    call check(stat == 0 .and. index(out, 'ID  AT ') == 1, 'fy audit list: '//out)
    call check(index(out, 'admin  landing.landed    demo') > 0 .and. &
        index(out, 'landing 1 of start as ') > 0, 'the landing is recorded: '//out)
    call check(index(out, 'landing.failed    demo') > 0 .and. &
        index(out, 'landing 2 of broken') > 0, 'the failed landing is recorded: '//out)
    call check(count_of(out, 'change.closed') == 1 .and. index(out, 'demo#3') > 0 .and. &
        count_of(out, 'change.reopened') == 1, 'closing and reopening once each: '//out)
    call check(index(out, 'token.created     laptop') > 0 .and. &
        index(out, 'token.revoked     #4') > 0 .and. &
        index(out, 'token.created     x\0y') > 0, 'tokens are recorded: '//out)
    call check(index(out, 'repo.created      second') > 0 .and. &
        index(out, 'repo.mode         second') > 0 .and. index(out, 'mirror') > 0, &
        'repositories are recorded: '//out)
    call check(index(out, 'sign-in           session') > 0 .and. &
        index(out, 'sign-out          session') > 0, 'sign-ins are recorded: '//out)
    call check(index(out, 'release.uploaded  demo 1.0') > 0 .and. index(out, 'sha256 ') > 0, &
        'the release is recorded: '//out)

    ! Pages go from the newest entry back to the first.
    call run(in//fy//' audit list --limit 2')
    call check(index(out, 'release.uploaded') > 0 .and. index(out, 'sign-out') > 0 .and. &
        index(out, 'sign-in  ') == 0 .and. &
        index(out, 'older entries: fy audit list --before ') > 0, &
        'the first page holds the two newest entries: '//out)
    call run(in//fy//' audit list --before 2 --json')
    call check(index(out, '"id":1,') > 0 .and. index(out, '"id":2,') == 0 .and. &
        index(out, '"next":0}') > 0 .and. index(out, '"prev":"'//AUDIT_GENESIS//'"') > 0, &
        'the last page ends the log: '//out)
    call run(curl//'-H "Authorization: Bearer '//reader//'" '//server//'/api/v1/audit && '// &
        curl//'-H "Authorization: Bearer '//reader//'" '//server//'/api/v1/audit/verify')
    call check(count_of(out, 'needs the admin scope') == 2, 'the log needs the admin scope: '//out)
    call run(curl//'-H "Authorization: Bearer '//admin//'" "'//server//'/api/v1/audit?limit=0"')
    call check(index(out, '"error"') > 0, 'a page of no entries is refused: '//out)

    call run(in//fy//' audit verify')
    call check(stat == 0 .and. index(out, 'audit log intact: ') == 1, 'fy audit verify: '//out)
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, DROP//" UPDATE audit SET actor = 'nobody' WHERE id = 3;", stat)
    call check(stat == 0, 'an entry is altered behind the server')
    call run(in//fy//' audit verify')
    call check(stat == 1 .and. index(out, 'audit log broken at entry 3: its content') == 1, &
        'fy audit verify names the altered entry: '//out)
    call db_exec(db, "UPDATE audit SET actor = 'admin' WHERE id = 3; DELETE FROM audit"// &
        ' WHERE id = (SELECT max(id) FROM audit);', stat)
    call run(in//fy//' audit verify')
    call check(stat == 1 .and. index(out, 'audit log broken at entry ') == 1 .and. &
        index(out, ': it and the entries after it are missing') > 0, &
        'fy audit verify names a removed newest entry: '//out)
    call db_exec(db, 'DELETE FROM audit WHERE id = 5;', stat)
    call run(in//fy//' audit verify')
    call check(stat == 1 .and. index(out, 'audit log broken at entry 6: entries before') == 1, &
        'fy audit verify names the entry after a removed one: '//out)
    call db_close(db)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'audit: all checks passed'

contains

    subroutine check_chain()
        !! The chain itself, on a database of its own.
        type(audit_t), allocatable :: rows(:)
        character(len=:), allocatable :: reason, head, first
        integer(int64) :: count, broken, next
        integer :: k

        call store_open(data//'/chain.db', db, stat)
        call check(stat == 0, 'store opens')
        call audit_verify(db, count, broken, reason, head)
        call check(count == 0 .and. broken == 0 .and. head == AUDIT_GENESIS, 'an empty log is intact')
        do k = 1, 5
            write (num, '(i0)') k
            call audit_append(db, 'admin', 'token.created', 'token '//trim(num), 'scopes admin', &
                stat)
            call check(stat == 0, 'an entry is appended')
        end do
        call audit_verify(db, count, broken, reason, first)
        call check(count == 5 .and. broken == 0 .and. len(reason) == 0, 'five entries are intact')

        call audit_list(db, 0_int64, 2, rows, next)
        call check(size(rows) == 2 .and. rows(1)%id == 5 .and. rows(2)%id == 4 .and. next == 4, &
            'the newest entries come first')
        call check(rows(1)%hash == first .and. rows(1)%prev == rows(2)%hash .and. &
            rows(1)%hash /= rows(2)%hash, 'an entry carries the hash of the one before it')
        call audit_list(db, 4_int64, 10, rows, next)
        call check(size(rows) == 3 .and. rows(3)%id == 1 .and. next == 0 .and. &
            rows(3)%prev == AUDIT_GENESIS, 'the last page reaches the first entry')

        call db_exec(db, "UPDATE audit SET detail = 'scopes none' WHERE id = 3;", stat)
        call check(stat /= 0, 'the store refuses to change an entry')
        call db_exec(db, 'DELETE FROM audit WHERE id = 3;', stat)
        call check(stat /= 0, 'the store refuses to remove an entry')
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 0 .and. head == first, 'a refused change leaves the log intact')

        ! Someone with the database file removes the guards first.
        call db_exec(db, DROP//" UPDATE audit SET detail = 'scopes none' WHERE id = 3;", stat)
        call check(stat == 0, 'an entry is altered')
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 3 .and. index(reason, 'altered') > 0 .and. head /= first, &
            'the altered entry is found: '//reason)
        call db_exec(db, "UPDATE audit SET detail = 'scopes admin', hash = '"//repeat('a', 64)// &
            "' WHERE id = 3;", stat)
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 3, 'an entry with another hash is found')
        ! Entry 4 still carries the hash entry 3 had.
        call db_exec(db, 'UPDATE audit SET hash = (SELECT prev FROM audit WHERE id = 4)'// &
            ' WHERE id = 3;', stat)
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 0 .and. head == first, 'the restored log is intact again')

        call db_exec(db, 'DELETE FROM audit WHERE id = 2;', stat)
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 3 .and. index(reason, 'missing') > 0, &
            'a removed entry is found at its successor: '//reason)
        ! Renumbering the later entries does not close the gap.
        call db_exec(db, 'UPDATE audit SET id = id - 1 WHERE id > 2;', stat)
        call audit_verify(db, count, broken, reason, head)
        call check(stat == 0 .and. count == 2 .and. broken == 2 .and. &
            index(reason, 'continue') > 0, 'renumbered entries are found: '//reason)
        call db_exec(db, 'DELETE FROM audit WHERE id = 1;', stat)
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 2, 'a removed first entry is found')
        call db_close(db)
    end subroutine check_chain

    subroutine check_end()
        !! The newest entries have no successor that would miss them.
        type(audit_t), allocatable :: rows(:)
        character(len=:), allocatable :: reason, head
        integer(int64) :: count, broken, next
        integer :: k

        call store_open(data//'/end.db', db, stat)
        do k = 1, 4
            call audit_append(db, 'admin', 'token.created', 'a'//achar(0)//'b', 'scopes admin', stat)
        end do
        call audit_list(db, 0_int64, 1, rows, next)
        call audit_verify(db, count, broken, reason, head)
        call check(rows(1)%subject == 'a\0b' .and. count == 4 .and. broken == 0, &
            'an entry with a NUL byte is kept whole and intact: '//reason)

        call db_exec(db, DROP//' DELETE FROM audit WHERE id > 2;', stat)
        call check(stat == 0, 'the newest entries are removed')
        call audit_verify(db, count, broken, reason, head)
        call check(count == 2 .and. broken == 3 .and. index(reason, 'missing') > 0, &
            'removed newest entries are found: '//reason)
        ! The next entry does not take the place of a removed one.
        call audit_append(db, 'admin', 'token.created', 'later', 'scopes admin', stat)
        call audit_list(db, 0_int64, 1, rows, next)
        call audit_verify(db, count, broken, reason, head)
        call check(stat == 0 .and. rows(1)%id == 5 .and. broken == 5 .and. &
            index(reason, 'missing') > 0, 'a later entry keeps the gap: '//reason)
        call db_exec(db, 'DELETE FROM audit_head;', stat)
        call db_exec(db, 'DELETE FROM audit WHERE id = 5;', stat)
        call audit_verify(db, count, broken, reason, head)
        call check(broken == 2 .and. index(reason, 'on record') > 0, &
            'a removed record of the newest entry is found: '//reason)
        call db_close(db)
    end subroutine check_end

    integer function count_of(text, word) result(n)
        character(len=*), intent(in) :: text, word
        integer :: at, k

        n = 0
        at = 1
        do
            k = index(text(at:), word)
            if (k == 0) exit
            n = n + 1
            at = at + k + len(word) - 1
        end do
    end function count_of

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

end program test_audit
