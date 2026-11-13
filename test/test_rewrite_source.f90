program test_rewrite_source
    use fy_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, &
        tcp_accept, tcp_connect, tcp_listen
    use fy_proc, only: proc_run, proc_setenv
    use fy_rewrite_store, only: rewrite_store
    use fy_server, only: serve_connection, server_set_data_dir
    use fy_sqlite, only: SQLITE_ROW, db_close, db_exec, db_prepare, db_t, &
        stmt_finalize, stmt_int, stmt_step, stmt_t
    use fy_store, only: store_bootstrap, store_open
    implicit none

    character(len=*), parameter :: root = 'build/test_rewrite_source'
    character(len=*), parameter :: data = root//'/data'
    character(len=*), parameter :: repo = data//'/repos/demo.git'
    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    type(db_t) :: db
    character(len=:), allocatable :: token, reply, out, old, new, middle, message
    integer :: lfd, port, status

    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call sh('rm -rf '//root//' && mkdir -p '//root//'/original '//root//'/clean '//data//'/repos')
    call sh('git init -q -b main '//root//'/original && printf PRIVATE_ORIGINAL > '// &
        root//'/original/private.txt && git -C '//root//'/original add private.txt && '// &
        'git -c user.name=T -c user.email=t@example.org -C '//root// &
        '/original commit -q -m Original')
    old = head(root//'/original')
    call sh('git init -q -b main '//root//'/clean && printf CLEAN_CONTENT > '// &
        root//'/clean/README && git -C '//root//'/clean add README && '// &
        'git -c user.name=T -c user.email=t@example.org -C '//root// &
        '/clean commit -q -m Clean')
    new = head(root//'/clean')
    call sh('git clone -q --bare '//root//'/clean '//repo)
    call proc_run([character(len=512) :: 'git', '--git-dir', repo, 'cat-file', '-e', old], &
        out, status, 1024)
    call check(status /= 0, 'the source repository contains no original commit')
    call store_open(data//'/fortyard.db', db, status)
    call check(status == 0, 'store opens')
    call store_bootstrap(db, token, status)
    call check(status == 0, 'administrator token created')
    call sql("INSERT INTO history_rewrites VALUES ('demo','"//old//"','"//new//"','now');")
    call sql("INSERT INTO landings (repo,branch,account,state,created_at,commit_id,kind)"// &
        " VALUES ('demo','main','author','passed','now','"//old//"','verify');")
    call server_set_data_dir(data)
    call tcp_listen('127.0.0.1', 0, lfd, status)
    call check(status == 0, 'source test listens')
    port = sock_bound_port(lfd)

    reply = request('/api/v1/repos/demo/tree?ref='//old)
    call rewritten(reply, 'old tree resolves')
    call check(index(reply, '"name":"README"') > 0 .and. index(reply, 'private.txt') == 0, &
        'only the cleaned tree is visible')
    reply = request('/api/v1/repos/demo/blob?ref='//old//'&path=README')
    call rewritten(reply, 'old blob source resolves')
    call check(index(reply, 'CLEAN_CONTENT') > 0, 'cleaned blob is returned')
    reply = request('/api/v1/repos/demo/log?ref='//old)
    call rewritten(reply, 'old log source resolves')
    call check(index(reply, '"id":"'//new//'"') > 0, 'log names the replacement commit')
    reply = request('/api/v1/repos/demo/commits/'//old)
    call rewritten(reply, 'old commit source resolves')
    call check(index(reply, '"id":"'//new//'"') > 0 .and. &
        index(reply, 'CLEAN_CONTENT') > 0, 'commit and patch are from the cleaned graph')
    reply = request('/api/v1/repos/demo/blob?ref='//old//'&path=private.txt')
    call check(index(reply, 'HTTP/1.1 404') == 1 .and. &
        index(reply, 'PRIVATE_ORIGINAL') == 0, 'an alias never exposes a removed file')
    reply = request('/api/v1/repos/demo/blob?ref=main&path=README')
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. &
        index(reply, 'X-Fortyard-Rewritten-From:') == 0, 'ordinary branch refs have no alias header')
    reply = request('/api/v1/repos/demo/refs?ref='//old)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. &
        index(reply, 'X-Fortyard-Rewritten-From:') == 0, 'ref listing is not rewritten')
    call check(count("SELECT count(*) FROM landings WHERE commit_id='"//old// &
        "' AND state='passed';") == 1, 'source navigation leaves old verification evidence intact')
    call check(count("SELECT count(*) FROM landings WHERE commit_id='"//new//"';") == 0, &
        'source navigation never supplies a verification for the new commit')

    middle = new
    call sh('printf LATEST_CLEAN_CONTENT > '//root//'/clean/README && '// &
        'printf LATEST_ONLY > '//root//'/clean/latest.txt && '// &
        'git -C '//root//'/clean add README latest.txt && '// &
        'git -c user.name=T -c user.email=t@example.org -C '//root// &
        '/clean commit -q -m Clean-again && git --git-dir '//repo//' fetch -q '// &
        root//'/clean +main:refs/heads/main')
    new = head(root//'/clean')
    call sql('BEGIN IMMEDIATE;')
    call rewrite_store(db, 'demo', [middle], [new], status, message)
    call check(status == 0, 'the second rewrite map is stored: '//message)
    call sql('COMMIT;')
    reply = request('/api/v1/repos/demo/tree?ref='//old)
    call rewritten(reply, 'the original tree URL resolves after a second rewrite')
    call check(index(reply, '"name":"latest.txt"') > 0, 'the tree contains the latest commit files')
    reply = request('/api/v1/repos/demo/blob?ref='//old//'&path=README')
    call rewritten(reply, 'the original blob URL resolves after a second rewrite')
    call check(index(reply, 'LATEST_CLEAN_CONTENT') > 0, 'the blob is from the latest cleaned commit')
    reply = request('/api/v1/repos/demo/log?ref='//old)
    call rewritten(reply, 'the original log URL resolves after a second rewrite')
    call check(index(reply, '"id":"'//new//'"') > 0, 'the log names the latest cleaned commit')
    reply = request('/api/v1/repos/demo/commits/'//old)
    call rewritten(reply, 'the original commit URL resolves after a second rewrite')
    call check(index(reply, '"id":"'//new//'"') > 0 .and. &
        index(reply, 'LATEST_CLEAN_CONTENT') > 0, 'the original commit URL returns the latest commit and patch')
    call check(count("SELECT count(*) FROM landings WHERE commit_id='"//old// &
        "' AND state='passed';") == 1, 'a second rewrite preserves the exact original verification identity')
    call check(count("SELECT count(*) FROM landings WHERE commit_id IN ('"//middle//"','"//new//"');") == 0, &
        'neither replacement inherits the original verification')

    call sql("INSERT INTO history_rewrites VALUES ('demo','"//new//"','"//old//"','now');")
    reply = request('/api/v1/repos/demo/commits/'//old)
    call check(index(reply, 'HTTP/1.1 500') == 1 .and. &
        index(reply, 'CLEAN_CONTENT') == 0, 'a corrupt alias does not fall through to source')
    call sql('DROP TABLE history_rewrites;')
    reply = request('/api/v1/repos/demo/tree?ref='//old)
    call check(index(reply, 'HTTP/1.1 500') == 1, 'unreadable alias metadata fails explicitly')
    reply = request('/api/v1/repos/demo/tree?ref=main')
    call check(index(reply, 'HTTP/1.1 200') == 1, 'ordinary refs do not depend on alias lookup')
    call sock_close(lfd)
    call db_close(db)
    print '(a)', 'rewritten source navigation checks passed'

contains

    function head(directory) result(commit)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: commit, text
        integer :: rc
        call proc_run([character(len=512) :: 'git', '-C', directory, 'rev-parse', 'HEAD'], &
            text, rc, 1024)
        call check(rc == 0 .and. len(text) >= 40, 'fixture commit exists')
        commit = text(1:40)
    end function head

    subroutine rewritten(response, label)
        character(len=*), intent(in) :: response, label
        call check(index(response, 'HTTP/1.1 200') == 1, label)
        call check(index(response, 'X-Fortyard-Rewritten-From: '//old//CRLF) > 0 .and. &
            index(response, 'X-Fortyard-Resolved-Ref: '//new//CRLF) > 0, &
            'both original and resolved source identities are explicit')
        call check(index(response, 'PRIVATE_ORIGINAL') == 0, 'original content is never returned')
    end subroutine rewritten

    function request(path) result(response)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: response
        character(len=4096) :: buffer
        integer :: cfd, sfd, n, rc
        call tcp_connect('127.0.0.1', port, cfd, rc)
        call check(rc == 0, 'client connects')
        call sock_send_all(cfd, 'GET '//path//' HTTP/1.1'//CRLF//'Authorization: Bearer '// &
            token//CRLF//CRLF, rc)
        sfd = tcp_accept(lfd)
        call serve_connection(sfd, db)
        call sock_close(sfd)
        response = ''
        do
            n = sock_recv(cfd, buffer)
            if (n <= 0) exit
            response = response//buffer(1:n)
        end do
        call sock_close(cfd)
    end function request

    subroutine sql(statement)
        character(len=*), intent(in) :: statement
        integer :: rc
        call db_exec(db, statement, rc)
        call check(rc == 0, 'test SQL succeeds')
    end subroutine sql

    integer function count(statement) result(value)
        character(len=*), intent(in) :: statement
        type(stmt_t) :: st
        integer :: rc
        call db_prepare(db, statement, st, rc)
        call check(rc == 0, 'count prepares')
        call check(stmt_step(st) == SQLITE_ROW, 'count returns a row')
        value = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function count

    subroutine sh(command)
        character(len=*), intent(in) :: command
        integer :: rc
        call execute_command_line(command, exitstat=rc)
        call check(rc == 0, 'Git fixture command succeeds')
    end subroutine sh

    subroutine check(ok, label)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: label
        if (.not. ok) then
            print '(a)', 'FAIL: '//label
            error stop 1
        end if
    end subroutine check

end program test_rewrite_source
