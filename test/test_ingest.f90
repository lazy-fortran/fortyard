program test_ingest
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_client, only: client_request
    use fortweb_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, &
        proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: root = 'build/test_ingest', &
        source = 'https://github.com/example/proposal.git'
    type(db_t) :: db
    character(len=:), allocatable :: admin, writer, server, head, changed, body, payload, branch
    character(len=4096) :: cwd
    character(len=24) :: port
    integer :: stat, code, lfd, sup, i
    logical :: exists

    sup = 0
    call getcwd(cwd)
    call proc_setenv('GIT_CONFIG_GLOBAL', trim(cwd)//'/'//root//'/gitconfig')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call run('rm -rf '//root//' && mkdir -p '//root//'/data/repos && '// &
        'git init -q -b main '//root//'/work && git -C '//root// &
        '/work config user.name Contributor && git -C '//root// &
        '/work config user.email contributor@example.org && mkdir '//root//'/work/.fortyard && '// &
        'printf "#!/bin/sh\necho ran > '//trim(cwd)//'/'//root//'/executed\n" > '// &
        root//'/work/.fortyard/check && chmod +x '//root//'/work/.fortyard/check && '// &
        'echo base > '//root//'/work/README && git -C '//root//'/work add . && '// &
        'git -C '//root//'/work commit -qm Base && git clone -q --bare '//root// &
        '/work '//root//'/data/repos/demo.git && echo proposal >> '//root//'/work/README && '// &
        'git -C '//root//'/work commit -qam Proposal && git -C '//root// &
        '/work update-ref refs/pull/7/head HEAD && git -C '//root// &
        '/work rev-parse HEAD > '//root//'/head && git config --file '//root// &
        '/gitconfig url."'//trim(cwd)//'/'//root//'/work".insteadOf '//source)
    head = trim(read_line(root//'/head'))
    call store_open(root//'/data/fortyard.db', db, stat)
    call check(stat == 0, 'database opened')
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'writer', 'write,land', writer, stat)
    call store_repo_add(db, 'demo', source, stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(root//'/data')
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (port, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(port)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, root//'/data/fortyard.db', 1, 1)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 100
        call client_request(server, 'GET', '/api/v1/whoami', admin, '', code, body, stat)
        if (stat == 0 .and. code == 200) exit
        call proc_sleep_ms(20)
    end do
    call check(code == 200, 'server ready')
    payload = '{"pr":"7","head":"'//head//'","approve_execution":true}'
    call post(writer, payload)
    call check(code == 403, 'write and land scopes cannot approve external execution')
    call post(admin, '{"pr":"7","head":"'//head//'"}')
    call check(code == 403, 'missing approval refused')
    call post(admin, '{"pr":"7","head":"'//head//'","approve_execution":"true"}')
    call check(code == 403, 'string approval refused')
    call post(admin, '{"pr":"7","head":"'//head//'","approve_execution":false}')
    call check(code == 403, 'false approval refused')
    call post(admin, '{"pr":"7","head":"123","approve_execution":true}')
    call check(code == 400, 'abbreviated head refused')
    call post(admin, '{"pr":"../7","head":"'//head//'","approve_execution":true}')
    call check(code == 400, 'invalid ref refused')
    call post(admin, '{"pr":"7","head":"'//repeat('0', 40)//'","approve_execution":true}')
    call check(code == 409, 'head mismatch refused')
    call client_request(server, 'GET', '/api/v1/changes', admin, '', code, body, stat)
    call check(code == 200 .and. index(body, '"changes":[]') > 0, 'refusals open no changes')

    call post(admin, payload)
    call check(code == 201 .and. index(body, '"checks_queued":false') > 0, 'approved import: '//body)
    branch = 'external/github/7/'//head
    call run('git --git-dir '//root//'/data/repos/demo.git log -1 --format=%ae '// &
        branch//' > '//root//'/author')
    call check(trim(read_line(root//'/author')) == 'contributor@example.org', 'author preserved')
    call post(admin, payload)
    call check(code == 200, 'same head import is idempotent')
    call client_request(server, 'GET', '/api/v1/repos/demo/changes/1', admin, '', code, body, stat)
    call check(index(body, '"review":"human-reviewed"') > 0 .and. &
        index(body, head) > 0, 'review and provenance recorded')
    call client_request(server, 'GET', '/api/v1/jobs', admin, '', code, body, stat)
    call check(index(body, '"jobs":[]') > 0, 'import queues no execution')
    inquire (file=root//'/executed', exist=exists)
    call check(.not. exists, 'import did not execute the proposal check')
    call run('echo changed >> '//root//'/work/README && git -C '//root// &
        '/work commit -qam Changed && git -C '//root//'/work update-ref refs/pull/7/head HEAD && '// &
        'git -C '//root//'/work rev-parse HEAD > '//root//'/changed')
    changed = trim(read_line(root//'/changed'))
    call post(admin, payload)
    call check(code == 409, 'a changed PR head does not inherit approval')
    call execute_command_line('FY_PUSH_ADMIN=1 git -C '//root//'/work push '// &
        trim(cwd)//'/'//root//'/data/repos/demo.git HEAD:refs/heads/'//branch// &
        ' > '//root//'/push-log 2>&1', exitstat=stat)
    call check(stat /= 0, 'even an admin push cannot replace the approved import')
    call client_request(server, 'GET', '/api/v1/audit', admin, '', code, body, stat)
    call check(index(body, 'external.execution-approved') > 0 .and. &
        index(body, head) > 0, 'approval is audited at the exact head')
    call client_request(server, 'POST', '/api/v1/repos/demo/changes/1/land', &
        admin, '', code, body, stat)
    call check(code == 202, 'approved change can be landed separately')
    do i = 1, 200
        inquire (file=root//'/executed', exist=exists)
        if (exists) exit
        call proc_sleep_ms(20)
    end do
    call check(exists, 'only the explicit landing executes the synthetic check')
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'ingest: all checks passed'

contains

    subroutine post(token, text)
        character(len=*), intent(in) :: token, text
        call client_request(server, 'POST', '/api/v1/repos/demo/ingest', token, text, &
            code, body, stat)
        call check(stat == 0, 'request completed')
    end subroutine post

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line(command, exitstat=stat)
        call check(stat == 0, 'fixture command completed')
    end subroutine run

    function read_line(path) result(line)
        character(len=*), intent(in) :: path
        character(len=4096) :: line
        integer :: u, ios
        line = ''
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
    end function read_line

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (ok) return
        print '(a)', 'FAIL: '//what
        if (sup > 0) call proc_kill(sup, SIGTERM)
        error stop 1
    end subroutine check

end program test_ingest
