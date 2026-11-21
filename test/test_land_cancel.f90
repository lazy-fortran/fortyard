program test_land_cancel
    !! Taking landings out of the queue and moving one forward. A queued
    !! landing is cancelled at once; a member of a candidate whose check runs
    !! is dropped at the lander's next step, main never gets it and the others
    !! land after a new check, also in a half after a split and across a
    !! server restart. A landing that ended cannot be cancelled. An urgent
    !! landing goes before the queue and forms a candidate alone. The land
    !! scope cancels one landing, admin all of a repository.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, proc_setenv, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: &
        db_close, &
        db_exec, &
        db_open, &
        db_t
    use fy_store, only: store_open, store_bootstrap, store_repo_add, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_land_cancel_data'
    character(len=*), parameter :: work = 'build/test_land_cancel_work'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    character(len=:), allocatable :: admin, writer, lander, server, fy, env, out, in, dir, body
    character(len=:), allocatable :: line, a, b, c, d, bad, e, f, u, g1, p, q, h, k1, k2
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, i, seen, code
    logical :: exists

    sup = 0
    seen = 0
    call getcwd(cwd)
    dir = trim(cwd)//'/'//data
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'writer', 'write', writer, stat)
    call store_token_create(db, 1_int64, 'lander', 'land', lander, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call proc_setenv('FY_URL', 'http://127.0.0.1:1')
    ! Kept open for the whole test: the restarted server listens on it too.
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
    call start()
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
    call run(env//'printf "%s\n" '//admin//' | '//fy//' login --server '//server)
    call check(stat == 0, 'login: '//out)

    ! The check notes the files of each candidate it checks, fails at once
    ! when there is a file BAD and else waits until the test lets one check
    ! pass (the file go, taken by the first check that sees it) or stops it.
    call execute_command_line(env//'git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "one\n" > '//work//'/README && printf ''#!/bin/sh\necho $$ > '//dir// &
        '/pid\nf=" $(ls | tr "\\n" " ")"\necho "$f" >> '//dir//'/runs\ncase "$f" in *" BAD "*) '// &
        'exit 1;; esac\nwhile ! mv '//dir//'/go '//dir//'/go.$$ 2>/dev/null; do\n  [ -f '//dir// &
        '/stop ] && exit 1\n  sleep 0.1\ndone\n'' > '//work//'/.fortyard/check && chmod +x '// &
        work//'/.fortyard/check && cd '//work//' && '//git//'add -A && '//git// &
        'commit -q -m Start && '//git//'remote add yard '//server//'/demo.git && '//git// &
        'checkout -q -b start', exitstat=stat)
    call pass()
    call run(in//fy//' land --wait')
    call check(stat == 0, 'a first landing: '//out)
    line = next_run()
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git checkout -q -b base FETCH_HEAD')
    call check(stat == 0, 'main is fetched: '//out)

    ! A queued landing is cancelled at once; write cannot cancel, land can.
    h = queue('h1', 'H1')
    line = next_run()
    a = queue('a', 'A')
    b = queue('b', 'B')
    c = queue('c', 'C')
    call http('DELETE', '/api/v1/repos/demo/landings/'//a, writer)
    call check(code == 403, 'the write scope cannot cancel: '//body)
    call http('DELETE', '/api/v1/repos/demo/landings/'//a, lander)
    call check(code == 200 .and. index(body, '"state":"cancelled"') > 0, &
        'the land scope cancels a queued landing: '//body)
    call check(job_state(a) == 'cancelled', 'it is cancelled at once')
    call http('GET', '/api/v1/jobs/'//a, admin)
    call check(index(body, 'Cancelled by admin (lander) at 20') > 0, &
        'its log says who cancelled it and when: '//body)

    ! A member of a candidate whose check runs: main never gets it, the
    ! others land after a new check.
    call pass()
    line = next_run()
    call check(has(line, 'B') .and. has(line, 'C') .and. .not. has(line, 'A'), &
        'the next candidate leaves the cancelled landing out: '//line)
    call run(in//fy//' land --cancel '//b)
    call check(stat == 0 .and. index(out, 'landing '//b//' of demo cancelled') > 0, &
        'fy land --cancel cancels a member of the candidate being checked: '//out)
    call check(job_state(b) == 'cancelled', 'the member is cancelled at once')
    call pass()
    line = next_run()
    call check(has(line, 'C') .and. .not. has(line, 'B'), &
        'the check that included it decides nothing; the rest is checked again: '//line)
    call pass()
    call settle()
    call check(job_state(c) == 'landed' .and. job_state(b) == 'cancelled' .and. &
        job_state(h) == 'landed', 'the others land, the cancelled one stays cancelled')
    call main_files()
    call check(has(out, 'C') .and. has(out, 'H1') .and. .not. has(out, 'B') .and. &
        .not. has(out, 'A'), 'main has no cancelled landing: '//out)

    ! A member of a half that is built again after a split.
    h = queue('h2', 'H2')
    line = next_run()
    d = queue('d', 'D')
    bad = queue('bad', 'BAD')
    e = queue('e', 'E')
    f = queue('f', 'F')
    call pass()
    line = next_run()
    call check(has(line, 'D') .and. has(line, 'BAD') .and. has(line, 'E') .and. has(line, 'F'), &
        'four landings share a candidate: '//line)
    line = next_run()
    call check(has(line, 'D') .and. has(line, 'BAD') .and. .not. has(line, 'E'), &
        'the failing candidate is split: '//line)
    line = next_run()
    call check(has(line, 'D') .and. .not. has(line, 'BAD'), 'and split again: '//line)
    call http('DELETE', '/api/v1/repos/demo/landings/'//e, lander)
    call check(code == 200 .and. job_state(e) == 'cancelled', &
        'a member of the other half is cancelled: '//body)
    call pass()
    line = next_run()
    call check(has(line, 'BAD') .and. .not. has(line, 'F'), 'the failing one is checked alone: '//line)
    line = next_run()
    call check(has(line, 'F') .and. .not. has(line, 'E') .and. .not. has(line, 'BAD'), &
        'the other half is built again without it: '//line)
    call pass()
    call settle()
    call check(job_state(d) == 'landed' .and. job_state(f) == 'landed' .and. &
        job_state(bad) == 'failed' .and. job_state(e) == 'cancelled', &
        'the good ones land, the failing one fails, the cancelled one stays cancelled')
    call main_files()
    call check(has(out, 'D') .and. has(out, 'F') .and. .not. has(out, 'E') .and. &
        .not. has(out, 'BAD'), 'main has neither the failing nor the cancelled one: '//out)

    ! A landing that ended cannot be cancelled.
    call http('DELETE', '/api/v1/repos/demo/landings/'//d, lander)
    call check(code == 409 .and. index(body, '"state":"landed"') > 0, &
        'a landed one is refused with its state: '//body)
    call http('DELETE', '/api/v1/repos/demo/landings/'//bad, admin)
    call check(code == 409 .and. index(body, '"state":"failed"') > 0, &
        'a failed one is refused with its state: '//body)
    call run(in//fy//' land --cancel '//d)
    call check(stat /= 0 .and. index(out, 'landed') > 0, 'fy land --cancel says why: '//out)

    ! An urgent landing goes before those not yet in a candidate, alone.
    h = queue('h3', 'H3')
    line = next_run()
    g1 = queue('g1', 'G1')
    line = queue('g2', 'G2')
    call run(in//git//'checkout -q -b u base && printf "x\n" > U && '//git//'add U && '// &
        git//'commit -q -m "Add U" && '// &
        'curl -s -H "Authorization: Bearer '//lander//'" -H "Content-Type: application/json" '// &
        '-d ''{"branch":"u","urgent":true}'' '//server//'/api/v1/repos/demo/land')
    call check(index(out, 'needs the admin scope') > 0, 'urgent needs the admin scope: '//out)
    call run(in//fy//' land --urgent --no-wait')
    call check(stat == 0, 'fy land --urgent queues: '//out)
    u = landing_id(out)
    call pass()
    line = next_run()
    call check(has(line, 'U') .and. .not. has(line, 'G1') .and. .not. has(line, 'G2'), &
        'the urgent landing comes first, in a candidate of its own: '//line)
    call pass()
    line = next_run()
    call check(has(line, 'G1') .and. has(line, 'G2'), 'then the others: '//line)
    call pass()
    call settle()
    call check(job_state(u) == 'landed' .and. job_state(g1) == 'landed', 'all of them land')
    call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
        ' git-credential" fetch -q yard main && git log --format=%s FETCH_HEAD')
    call check(index(out, 'Add U (#') > index(out, 'Add G1 (#') .and. &
        index(out, 'Add H3 (#') > index(out, 'Add U (#'), &
        'main has the urgent one before the others queued before it: '//out)

    ! A restart between the cancel and the lander's next step keeps it.
    h = queue('h4', 'H4')
    line = next_run()
    p = queue('p', 'P')
    q = queue('q', 'Q')
    call pass()
    line = next_run()
    call check(has(line, 'P') .and. has(line, 'Q'), 'a candidate of two is checked: '//line)
    call http('DELETE', '/api/v1/repos/demo/landings/'//q, lander)
    call check(code == 200, 'one of them is cancelled: '//body)
    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    ! The check of the stopped server is ended, so it takes no go meant for the next.
    call execute_command_line('touch '//dir//'/stop && p=$(cat '//dir//'/pid) && '// &
        'while kill -0 "$p" 2>/dev/null; do sleep 0.1; done; rm -f '//dir//'/stop', exitstat=stat)
    call start()
    line = next_run()
    call check(has(line, 'P') .and. .not. has(line, 'Q'), &
        'after the restart the candidate is built without it: '//line)
    call pass()
    call settle()
    call check(job_state(p) == 'landed' .and. job_state(q) == 'cancelled', &
        'the other lands, the cancelled one stays cancelled')
    call main_files()
    call check(has(out, 'P') .and. .not. has(out, 'Q'), 'main has no cancelled landing: '//out)

    ! A database locked longer than the busy wait when the lander seals a
    ! passing landing delays it; nobody cancelled it, so it lands.
    h = queue('lk', 'LK')
    line = next_run()
    call pass()
    call execute_command_line('p=$(cat '//dir//'/pid) && while kill -0 "$p" 2>/dev/null; '// &
        'do sleep 0.1; done', exitstat=stat)
    call db_open(data//'/fortyard.db', db, stat)
    call db_exec(db, 'BEGIN IMMEDIATE;', stat)
    call check(stat == 0, 'the test holds the write lock')
    ! Longer than the five seconds a connection waits for a lock.
    call proc_sleep_ms(7000)
    call db_exec(db, 'COMMIT;', stat)
    call db_close(db)
    call settle()
    call check(job_state(h) == 'landed', 'the landing lands after the lock is released')
    call http('GET', '/api/v1/jobs/'//h, admin)
    call check(index(body, 'cancelled') == 0, 'its log names no cancel: '//body)

    ! Cancelling all landings of a repository is the admin's.
    h = queue('h5', 'H5')
    line = next_run()
    k1 = queue('k1', 'K1')
    k2 = queue('k2', 'K2')
    call http('DELETE', '/api/v1/repos/demo/landings', lander)
    call check(code == 403, 'the land scope cannot cancel all: '//body)
    call run(in//fy//' land --cancel-all demo')
    call check(stat == 0 .and. index(out, h) > 0 .and. index(out, k1) > 0 .and. &
        index(out, k2) > 0, 'fy land --cancel-all cancels the running and the queued: '//out)
    call check(job_state(h) == 'cancelled' .and. job_state(k1) == 'cancelled' .and. &
        job_state(k2) == 'cancelled', 'all of them are cancelled at once')
    call pass()
    call settle()
    call check(job_state(h) == 'cancelled', 'the one whose check passed stays cancelled')
    call main_files()
    call check(.not. has(out, 'H5') .and. .not. has(out, 'K1') .and. .not. has(out, 'K2'), &
        'main has none of them: '//out)
    ! The branch of a cancelled landing stays on the server, so it is queued
    ! again by name without a push, from whatever is checked out.
    call run(in//fy//' land demo k1 --no-wait')
    call check(stat == 0, 'a cancelled branch is queued again: '//out)
    k1 = landing_id(out)
    line = next_run()
    call check(has(line, 'K1') .and. .not. has(line, 'K2'), 'it is checked as it was: '//line)
    call pass()
    call settle()
    call check(job_state(k1) == 'landed', 'and lands')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    call sock_close(lfd)
    call execute_command_line('touch '//dir//'/stop')
    print '(a)', 'land cancel: all checks passed'

contains

    subroutine start()
        sup = proc_fork()
        if (sup == 0) then
            call server_supervise(lfd, data//'/fortyard.db', 1, 2)
            call proc_exit(0)
        end if
    end subroutine start

    subroutine pass()
        !! Lets the check that waits, or the next one, pass.
        call execute_command_line('touch '//dir//'/go', exitstat=stat)
    end subroutine pass

    function next_run() result(l)
        !! The files of the next candidate the check sees, once it sees it.
        character(len=:), allocatable :: l
        integer :: k
        do k = 1, 600
            if (lines(dir//'/runs') > seen) exit
            call proc_sleep_ms(100)
        end do
        call check(lines(dir//'/runs') > seen, 'a check starts')
        seen = seen + 1
        call execute_command_line('sed -n '//itoa(seen)//'p '//dir//'/runs > '//dir//'/line', &
            exitstat=stat)
        l = trim(read_line(dir//'/line'))//' '
    end function next_run

    function queue(name, file) result(id)
        !! Queues a branch from the first commit of main that adds `file`.
        character(len=*), intent(in) :: name, file
        character(len=:), allocatable :: id
        call run(in//'git checkout -q -b '//name//' base && printf "x\n" > '//file//' && '// &
            git//'add '//file//' && '//git//'commit -q -m "Add '//file//'" && '//fy// &
            ' land --no-wait')
        call check(stat == 0, 'queued '//name//': '//out)
        id = landing_id(out)
    end function queue

    function landing_id(text) result(id)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: id
        integer :: k, j
        k = index(text, 'landing ', back=.true.)
        j = 0
        if (k > 0) j = index(text(k:), ' of ')
        id = ''
        if (j > 9) id = text(k + 8:k + j - 2)
        call check(len(id) > 0, 'a landing number: '//text)
    end function landing_id

    function job_state(id) result(state)
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: state
        integer :: k, j
        call http('GET', '/api/v1/jobs/'//id, admin)
        state = ''
        k = index(body, '"state":"')
        if (k == 0) return
        j = index(body(k + 9:), '"')
        state = body(k + 9:k + 7 + j)
    end function job_state

    subroutine settle()
        !! Waits until no landing is queued or running.
        integer :: k
        do k = 1, 600
            call http('GET', '/api/v1/jobs?repo=demo', admin)
            if (index(body, '"state":"queued"') == 0 .and. &
                index(body, '"state":"running"') == 0) return
            call proc_sleep_ms(100)
        end do
        call check(.false., 'the queue settles: '//body)
    end subroutine settle

    subroutine main_files()
        call run(in//'git -c credential.helper= -c "credential.helper=!'//fy// &
            ' git-credential" fetch -q yard main && echo " $(git ls-tree --name-only FETCH_HEAD'// &
            ' | tr "\n" " ")"')
    end subroutine main_files

    logical function has(text, file)
        character(len=*), intent(in) :: text, file
        has = index(text, ' '//file//' ') > 0
    end function has

    subroutine http(method, path, token)
        character(len=*), intent(in) :: method, path, token
        character(len=16) :: c_
        call execute_command_line('curl -s -o '//data//'/body -w "%{http_code}" -X '//method// &
            ' -H "Authorization: Bearer '//token//'" '//server//path//' > '//data//'/code', &
            exitstat=stat)
        body = read_all(data//'/body')
        c_ = read_line(data//'/code')
        code = 0
        read (c_, *, iostat=stat) code
    end subroutine http

    integer function lines(path) result(n)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: k
        text = read_all(path)
        n = 0
        do k = 1, len(text)
            if (text(k:k) == new_line('a')) n = n + 1
        end do
    end function lines

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, '(i0)') k
        s = trim(buf)
    end function itoa

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_line(path) result(l)
        character(len=*), intent(in) :: path
        character(len=4096) :: l
        integer :: unit, ios
        l = ''
        open (newunit=unit, file=path, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (unit, '(a)', iostat=ios) l
        close (unit)
    end function read_line

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
            call execute_command_line('touch '//dir//'/stop')
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_land_cancel
