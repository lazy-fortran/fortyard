program test_land_cancel_runner
    !! A landing cancelled while a runner checks its candidate: the lander
    !! withdraws that check at once instead of waiting for it, the runner's
    !! result is refused, and the rest of the candidate is checked again
    !! without the cancelled commit and lands. Here the test is the runner.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortweb_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir, server_set_checks
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_land_cancel_runner_data'
    character(len=*), parameter :: work = 'build/test_land_cancel_runner_work'
    character(len=*), parameter :: git = 'env GIT_TERMINAL_PROMPT=0 GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 git -c user.name=T -c user.email=t@example.org -c credential.helper= '
    type(db_t) :: db
    character(len=:), allocatable :: admin, runner, server, url, body, out, id, ref, a, b, h
    character(len=16) :: num
    integer :: lfd, stat, sup, i, code
    logical :: exists

    sup = 0
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'runner-1', 'runner', runner, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call server_set_checks('runner')
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
    url = 'http://x:'//admin//'@127.0.0.1:'//trim(num)//'/demo.git'

    ! main, from a first landing.
    call run('git init -q -b main '//work//' && printf "one\n" > '//work//'/README && '// &
        git//'-C '//work//' add -A && '//git//'-C '//work//' commit -q -m Start && '// &
        git//'-C '//work//' push -q '//url//' HEAD:start')
    call check(stat == 0, 'a branch is pushed: '//out)
    h = land('start')
    call pass(claim())
    call wait_state(h, 'landed')
    call run(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '//work// &
        ' checkout -q -B base FETCH_HEAD')
    call check(stat == 0, 'main is fetched: '//out)

    ! A runner holds the check of h while a and b are queued; they share the next candidate.
    h = land(branch('h', 'H'))
    id = claim()
    a = land(branch('a', 'A'))
    b = land(branch('b', 'B'))
    call pass(id)
    call wait_state(h, 'landed')
    id = claim()
    call check(index(files(ref), ' A ') > 0 .and. index(files(ref), ' B ') > 0, &
        'a and b share a candidate: '//files(ref))
    call http('DELETE', '/api/v1/repos/demo/landings/'//b, '', admin)
    call check(code == 200, 'b is cancelled while its check runs: '//body)
    ! The withdrawn check is not waited for: the next one is queued before
    ! the runner reports.
    i = int(to_int(id))
    id = claim()
    call check(to_int(id) > i, 'a new check is queued at once: '//id)
    call check(index(files(ref), ' A ') > 0 .and. index(files(ref), ' B ') == 0, &
        'the new candidate is built without b: '//files(ref))
    write (num, '(i0)') i
    call http('POST', '/api/v1/checks/'//trim(num)//'/result', '{"passed":true,"log":"ok"}', runner)
    call check(code == 409, 'the result of the withdrawn check is refused: '//body)
    call pass(id)
    call wait_state(a, 'landed')
    call check(state(b) == 'cancelled', 'b stays cancelled')
    call run(git//'-C '//work//' fetch -q '//url//' main && '//git//'-C '//work// &
        ' ls-tree --name-only FETCH_HEAD')
    call check(index(out, 'A'//new_line('a')) > 0 .and. index(out, 'B'//new_line('a')) == 0, &
        'main has a and not b: '//out)

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'land cancel runner: all checks passed'

contains

    function branch(name, file) result(n)
        !! Pushes the branch `name` from main, adding `file`.
        character(len=*), intent(in) :: name, file
        character(len=:), allocatable :: n
        call run(git//'-C '//work//' checkout -q -b '//name//' base && printf "x\n" > '//work// &
            '/'//file//' && '//git//'-C '//work//' add '//file//' && '//git//'-C '//work// &
            ' commit -q -m "Add '//file//'" && '//git//'-C '//work//' push -q '//url//' '//name)
        call check(stat == 0, 'pushed '//name//': '//out)
        n = name
    end function branch

    function land(name) result(landing)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: landing
        integer :: k, e
        call http('POST', '/api/v1/repos/demo/land', '{"branch":"'//name//'"}', admin)
        call check(code == 202, 'landing queued: '//body)
        k = index(body, '"landing":') + 10
        e = k
        do while (index('0123456789', body(e:e)) > 0)
            e = e + 1
        end do
        landing = body(k:e - 1)
    end function land

    function claim() result(check_id)
        !! Claims the next check as the runner, waiting until one is queued;
        !! sets `ref` to what it checks.
        character(len=:), allocatable :: check_id
        integer :: k, s, e
        check_id = ''
        do k = 1, 600
            call http('POST', '/api/v1/checks/claim', '{}', runner)
            s = index(body, '"id":')
            if (s > 0) then
                s = s + 5
                e = s
                do while (index('0123456789', body(e:e)) > 0)
                    e = e + 1
                end do
                check_id = body(s:e - 1)
                s = index(body, '"ref":"') + 7
                ref = body(s:s + index(body(s:), '"') - 2)
                return
            end if
            call proc_sleep_ms(100)
        end do
        call check(.false., 'a check is queued for the runner: '//body)
    end function claim

    subroutine pass(check_id)
        character(len=*), intent(in) :: check_id
        call http('POST', '/api/v1/checks/'//check_id//'/result', '{"passed":true,"log":"ok"}', &
            runner)
        call check(code == 200, 'the check passes: '//body)
    end subroutine pass

    function files(r) result(list)
        !! The files of the commit at the ref `r` of the repository.
        character(len=*), intent(in) :: r
        character(len=:), allocatable :: list
        call run('git --git-dir '//data//'/repos/demo.git ls-tree --name-only '//r// &
            ' | tr "\n" " "')
        list = ' '//out
    end function files

    function state(landing) result(s)
        character(len=*), intent(in) :: landing
        character(len=:), allocatable :: s
        integer :: k
        call http('GET', '/api/v1/jobs/'//landing, '', admin)
        s = ''
        k = index(body, '"state":"')
        if (k > 0) s = body(k + 9:k + 7 + index(body(k + 9:), '"'))
    end function state

    subroutine wait_state(landing, want)
        character(len=*), intent(in) :: landing, want
        integer :: k
        do k = 1, 600
            if (state(landing) == want) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'landing '//landing//' is '//want//': '//body)
    end subroutine wait_state

    integer(int64) function to_int(text)
        character(len=*), intent(in) :: text
        integer :: ios
        to_int = 0
        read (text, *, iostat=ios) to_int
    end function to_int

    subroutine http(method, path, payload, token)
        character(len=*), intent(in) :: method, path, payload, token
        character(len=16) :: c
        character(len=:), allocatable :: send
        integer :: ios
        send = ''
        if (len(payload) > 0) send = ' -d '''//payload//''''
        call execute_command_line('curl -s -o '//data//'/body -w "%{http_code}" -X '//method// &
            ' -H "Authorization: Bearer '//token//'" -H "Content-Type: application/json"'// &
            send//' '//server//path//' > '//data//'/code', exitstat=stat)
        body = read_all(data//'/body')
        c = read_all(data//'/code')
        code = 0
        read (c, *, iostat=ios) code
    end subroutine http

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: unit, ios, n
        text = ''
        open (newunit=unit, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=unit, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (unit, iostat=ios) text
        close (unit)
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

end program test_land_cancel_runner
