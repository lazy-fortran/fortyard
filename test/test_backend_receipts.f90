program test_backend_receipts
    !! Closing receipts of the GitHub backend against a fake GitHub API and a
    !! server whose repository fx has the upstream acme/demo. A landed branch
    !! with an open pull request gets one comment that names the commit and
    !! the change, and the pull request is closed, never merged; so does the
    !! pull request an ingested change came from. A pull request of a fork
    !! with the same branch name and one of another branch are untouched, a
    !! landing without a pull request is recorded without a request beyond
    !! the listing, and failed landings and verifications get no receipt. A
    !! pull request whose commit GitHub's main lacks waits for a later sweep.
    !! A restarted backend sends nothing for recorded receipts, and a receipt
    !! cut short after its comment is finished without a second comment.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_backend_github, only: github_configure, github_main
    use fortweb_http, only: http_request_t, http_read_request, http_send_response, http_query_param
    use fortweb_posix, only: &
        sock_bound_port, &
        sock_close, &
        sock_set_timeouts, &
        tcp_accept, &
        tcp_listen
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM, &
        proc_setenv
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close, db_exec, db_prepare, stmt_t, stmt_step, stmt_text, &
        stmt_finalize, SQLITE_ROW
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: dir = 'build/test_backend_receipts'
    character(len=*), parameter :: data = dir//'/data', api_log = dir//'/api.log'
    character(len=*), parameter :: C1 = repeat('1', 40), C2 = repeat('2', 40), &
        C3 = repeat('3', 40), C4 = repeat('4', 40), C5 = repeat('5', 40), C6 = repeat('6', 40)
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, base, root, log, landed
    character(len=16) :: num
    character(len=4096) :: cwd
    integer :: lfd, flfd, fport, port, stat, sup, fake, back, i, mark
    type(http_request_t) :: req
    character(len=:), allocatable :: body, path

    sup = 0
    fake = 0
    back = 0
    call getcwd(cwd)
    root = trim(cwd)//'/'//dir
    call execute_command_line('rm -rf '//dir//' && mkdir -p '//data//' '//dir//'/tmp '// &
        dir//'/state && touch '//dir//'/state/late-missing', exitstat=stat)
    call check(stat == 0, 'the directories are made')
    call proc_setenv('TMPDIR', root//'/tmp')

    call tcp_listen('127.0.0.1', 0, flfd, stat)
    fport = sock_bound_port(flfd)
    fake = proc_fork()
    if (fake == 0) then
        call fake_github(flfd)
        call proc_exit(0)
    end if
    call sock_close(flfd)

    ! The upstream's address names github.com/acme/demo, but no request leaves
    ! the host: nothing is pushed, as the landings are written here.
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'github-backend', 'runner', runner, stat)
    call store_repo_add(db, 'fx', root//'/github.com/acme/demo.git', stat, mode='primary')
    call db_exec(db, "INSERT INTO changes (repo, number, change_key, branch, title,"// &
        " description, author, state, created_at, updated_at) VALUES ('fx', 7, 'k7',"// &
        " 'feature', 'Feature', '', 'admin', 'landed', '2026-01-01T00:00:00Z',"// &
        " '2026-01-01T00:00:00Z');", stat)
    call check(stat == 0, 'the change is stored')
    call landing(1, 'feature', 'landed', 'landing', C1, '1')
    call landing(2, 'external/github/21/'//repeat('a', 40), 'landed', 'landing', C2, 'NULL')
    call landing(3, 'nopr', 'landed', 'landing', C3, 'NULL')
    call landing(4, 'broken', 'failed', 'landing', C4, 'NULL')
    call landing(5, 'main', 'landed', 'verify', C5, 'NULL')
    call landing(6, 'late', 'landed', 'landing', C6, 'NULL')
    call db_close(db)
    call proc_setenv('FY_PUBLIC_URL', 'https://yard.example/y')
    call server_set_data_dir(data)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    write (num, '(i0)') port
    base = 'http://127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    call start_backend()
    call wait_receipts('1,2,3')
    call stop_backend()
    log = slurp(api_log)
    call check(count_of(log, 'POST /repos/acme/demo/issues/11/comments ') == 1 .and. &
        count_of(log, 'PATCH /repos/acme/demo/pulls/11 {"state":"closed"}') == 1, &
        'the open pull request of the landed branch gets one comment and is closed: '//log)
    call check(index(log, 'Landed as '//C1//' on main through fortyard: '// &
        'https://yard.example/y/fx/-/changes/7.') > 0, &
        'the comment names the commit and the change: '//log)
    call check(count_of(log, 'POST /repos/acme/demo/issues/21/comments ') == 1 .and. &
        count_of(log, 'PATCH /repos/acme/demo/pulls/21 ') == 1 .and. &
        index(log, 'Landed as '//C2//' on main through fortyard. ') > 0, &
        'the pull request an ingested change came from is closed with the commit: '//log)
    call check(index(log, '/12') == 0 .and. index(log, '/13') == 0, &
        'pull requests of another head are untouched: '//log)
    call check(index(log, 'head=acme%3Anopr') > 0 .and. index(log, C3) == 0, &
        'a landing without a pull request costs one listing: '//log)
    call check(index(log, 'broken') == 0 .and. index(log, C4) == 0 .and. index(log, C5) == 0, &
        'failed landings and verifications get no receipt: '//log)
    call check(index(log, '/compare/'//C6//'...main') > 0 .and. &
        index(log, '/issues/14') == 0 .and. index(log, '/pulls/14') == 0, &
        'a pull request waits while GitHub''s main lacks the commit: '//log)
    call check(index(log, '/merge') == 0 .and. index(log, '"merged"') == 0, &
        'nothing is merged on GitHub')
    call check(receipt_of(1) == '11' .and. receipt_of(2) == '21' .and. &
        receipt_of(3) == '' .and. .not. has_receipt(6), 'the server records the receipts')

    ! A restart after the comment on 11 but before its record: 11 is open again.
    mark = len(log)
    call store_open(data//'/fortyard.db', db, stat)
    call db_exec(db, 'DELETE FROM github_receipts WHERE landing = 1;', stat)
    call db_close(db)
    call execute_command_line('rm -f '//dir//'/state/closed-11 '//dir//'/state/late-missing')
    call start_backend()
    call wait_receipts('1,2,3,6')
    call stop_backend()
    log = slurp(api_log)
    log = log(mark + 1:)
    call check(count_of(log, 'POST /repos/acme/demo/issues/11/comments ') == 0 .and. &
        count_of(log, 'PATCH /repos/acme/demo/pulls/11 ') == 1, &
        'a receipt cut short is finished without a second comment: '//log)
    call check(index(log, '/21') == 0 .and. index(log, 'nopr') == 0, &
        'recorded receipts send nothing again: '//log)
    call check(count_of(log, 'POST /repos/acme/demo/issues/14/comments ') == 1 .and. &
        count_of(log, 'PATCH /repos/acme/demo/pulls/14 ') == 1 .and. receipt_of(6) == '14', &
        'the waiting pull request is closed once main has the commit: '//log)

    call stop_all()
    call execute_command_line('rm -rf '//dir)
    print '(a)', 'backend receipts: all checks passed'

contains

    subroutine landing(id, branch, state, kind, commit, change)
        integer, intent(in) :: id
        character(len=*), intent(in) :: branch, state, kind, commit, change
        character(len=16) :: n
        write (n, '(i0)') id
        call db_exec(db, 'INSERT INTO landings (id, repo, branch, account, state, created_at,'// &
            ' finished_at, commit_id, kind, change) VALUES ('//trim(n)//", 'fx', '"//branch// &
            "', 'admin', '"//state//"', strftime('%Y-%m-%dT%H:%M:%SZ','now'),"// &
            " strftime('%Y-%m-%dT%H:%M:%SZ','now'), '"//commit//"', '"//kind//"', "// &
            change//');', stat)
        call check(stat == 0, 'landing '//trim(n)//' is stored')
    end subroutine landing

    subroutine start_backend()
        back = proc_fork()
        if (back == 0) then
            write (num, '(i0)') fport
            call github_configure('', '', 'acme', 'http://127.0.0.1:'//trim(num), 'ghs_test', &
                1, 3)
            call github_main(base, runner, 'github-linux', 1, 1, 60)
            call proc_exit(0)
        end if
    end subroutine start_backend

    subroutine stop_backend()
        call proc_kill(back, SIGTERM)
        call proc_wait(back, stat)
        back = 0
    end subroutine stop_backend

    subroutine wait_receipts(want)
        !! Until exactly the landings `want` (comma-separated ids) have receipts.
        character(len=*), intent(in) :: want
        integer :: k
        do k = 1, 600
            landed = receipt_ids()
            if (landed == want) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'receipts '//want//' are recorded, not '//landed)
    end subroutine wait_receipts

    function receipt_ids() result(s)
        character(len=:), allocatable :: s
        type(stmt_t) :: st
        integer :: st2
        s = ''
        call store_open(data//'/fortyard.db', db, st2)
        call db_prepare(db, "SELECT group_concat(landing, ',') FROM (SELECT landing FROM"// &
            ' github_receipts ORDER BY landing);', st, st2)
        if (stmt_step(st) == SQLITE_ROW) s = stmt_text(st, 0)
        call stmt_finalize(st)
        call db_close(db)
    end function receipt_ids

    logical function has_receipt(id)
        integer, intent(in) :: id
        character(len=16) :: n
        write (n, '(i0)') id
        has_receipt = index(','//receipt_ids()//',', ','//trim(n)//',') > 0
    end function has_receipt

    function receipt_of(id) result(s)
        integer, intent(in) :: id
        character(len=:), allocatable :: s
        type(stmt_t) :: st
        integer :: st2
        character(len=16) :: n
        write (n, '(i0)') id
        s = '?'
        call store_open(data//'/fortyard.db', db, st2)
        call db_prepare(db, 'SELECT prs FROM github_receipts WHERE landing = '//trim(n)//';', &
            st, st2)
        if (stmt_step(st) == SQLITE_ROW) s = stmt_text(st, 0)
        call stmt_finalize(st)
        call db_close(db)
    end function receipt_of

    subroutine fake_github(listen)
        !! The pull request routes of GitHub's REST API. Open pull requests:
        !! 11 (acme:feature), 12 (fork:feature), 13 (acme:other), 14
        !! (acme:late) and 21 (the ingested one, from a fork). A closed one
        !! has a file state/closed-N, its comments are in state/comments-N;
        !! while state/late-missing exists, main lacks the commit of late.
        integer, intent(in) :: listen
        integer :: cfd, st, u

        do
            cfd = tcp_accept(listen)
            if (cfd < 0) cycle
            call sock_set_timeouts(cfd, 30)
            call http_read_request(cfd, req, st)
            if (st /= 0) then
                call sock_close(cfd)
                cycle
            end if
            path = req%path
            body = ''
            if (req%authorization /= 'Bearer ghs_test') then
                st = 401
            else
                call answer(st)
            end if
            open (newunit=u, file=api_log, position='append', action='write')
            if (len(req%query) > 0) then
                write (u, '(a)') req%method//' '//path//'?'//req%query//' '//req%body
            else
                write (u, '(a)') req%method//' '//path//' '//req%body
            end if
            close (u)
            call http_send_response(cfd, st, 'application/json', body, '')
            call sock_close(cfd)
        end do
    end subroutine fake_github

    subroutine answer(code)
        integer, intent(out) :: code
        character(len=*), parameter :: P = '/repos/acme/demo/'
        character(len=:), allocatable :: head, n
        integer :: k
        integer, parameter :: nums(5) = [11, 12, 13, 14, 21]
        character(len=*), parameter :: heads(5) = [character(len=13) :: 'acme:feature', &
            'fork:feature', 'acme:other', 'acme:late', 'fork:fix']

        code = 200
        if (path == P//'pulls' .and. req%method == 'GET') then
            if (http_query_param(req%query, 'state') /= 'open') then
                code = 422
                return
            end if
            head = http_query_param(req%query, 'head')
            body = '['
            do k = 1, size(nums)
                if (trim(heads(k)) /= head .or. closed(nums(k))) cycle
                if (len(body) > 1) body = body//','
                body = body//pull(k)
            end do
            body = body//']'
            return
        end if
        if (index(path, P//'compare/') == 1) then
            body = '{"status":"ahead"}'
            if (index(path, C6) > 0 .and. exists(dir//'/state/late-missing')) &
                body = '{"status":"diverged"}'
            return
        end if
        do k = 1, size(nums)
            write (num, '(i0)') nums(k)
            n = trim(num)
            if (path == P//'pulls/'//n) then
                if (req%method == 'PATCH') then
                    if (req%body /= '{"state":"closed"}') then
                        code = 422
                        return
                    end if
                    call touch(dir//'/state/closed-'//n, '')
                end if
                body = pull(k)
                return
            end if
            if (path == P//'issues/'//n//'/comments') then
                if (req%method == 'POST') then
                    code = 201
                    call touch(dir//'/state/comments-'//n, ','//req%body)
                    body = '{"id":1}'
                else
                    body = '['//slurp(dir//'/state/comments-'//n)
                    if (len(body) > 1) body = '['//body(3:)
                    body = body//']'
                end if
                return
            end if
        end do
        code = 404
        body = '{"message":"Not Found"}'
    end subroutine answer

    function pull(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        integer, parameter :: nums(5) = [11, 12, 13, 14, 21]
        character(len=*), parameter :: heads(5) = [character(len=13) :: 'acme:feature', &
            'fork:feature', 'acme:other', 'acme:late', 'fork:fix']
        write (num, '(i0)') nums(k)
        s = '{"number":'//trim(num)//',"state":"'// &
            trim(merge('closed', 'open  ', closed(nums(k))))//'","head":{"label":"'// &
            trim(heads(k))//'","ref":"x","user":{"login":"y"}}}'
    end function pull

    logical function closed(n)
        integer, intent(in) :: n
        character(len=16) :: b
        write (b, '(i0)') n
        closed = exists(dir//'/state/closed-'//trim(b))
    end function closed

    logical function exists(file)
        character(len=*), intent(in) :: file
        inquire (file=file, exist=exists)
    end function exists

    subroutine touch(file, text)
        character(len=*), intent(in) :: file, text
        integer :: u
        open (newunit=u, file=file, position='append', action='write', access='stream', &
            form='unformatted')
        if (len(text) > 0) write (u) text
        close (u)
    end subroutine touch

    integer function count_of(text, part) result(n)
        character(len=*), intent(in) :: text, part
        integer :: p, k
        n = 0
        p = 1
        do
            k = index(text(p:), part)
            if (k == 0) exit
            n = n + 1
            p = p + k + len(part) - 1
            if (p > len(text)) exit
        end do
    end function count_of

    function slurp(file) result(s)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: s
        integer :: u, ios, n
        s = ''
        open (newunit=u, file=file, access='stream', form='unformatted', status='old', &
            action='read', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (s)
        allocate (character(len=n) :: s)
        if (n > 0) read (u, iostat=ios) s
        close (u)
    end function slurp

    subroutine stop_all()
        if (back > 0) call stop_backend()
        if (sup > 0) then
            call proc_kill(sup, SIGTERM)
            call proc_wait(sup, stat)
        end if
        if (fake > 0) then
            call proc_kill(fake, SIGTERM)
            call proc_wait(fake, stat)
        end if
        sup = 0
        fake = 0
    end subroutine stop_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            call stop_all()
            error stop 1
        end if
    end subroutine check

end program test_backend_receipts
