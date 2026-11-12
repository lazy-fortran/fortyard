module fy_backend
    !! What backend processes share: a backend process is a runner of a
    !! backend class that hands each check it claims to an external system
    !! (GitHub Actions) instead of running it. backend_main claims checks of
    !! its classes within its limits and gives each to a child process of its
    !! own, which follows the external run and reports the result; the parent
    !! also calls a periodic sweep. Requests to external APIs go through curl,
    !! with credentials on curl's standard input, never on a command line.
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        JSON_ARRAY_END, &
        JSON_ARRAY_START, &
        JSON_BOOL, &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_INTEGER, &
        JSON_KEY, &
        JSON_NULL_VAL, &
        JSON_OBJECT_END, &
        JSON_OBJECT_START, &
        JSON_REAL, &
        JSON_STRING, &
        json_event_t, &
        json_extract_string, &
        json_parser_init_strict, &
        json_parser_next, &
        json_parser_t
    use fy_client, only: &
        client_request
    use fy_proc, only: &
        fd_close, &
        fd_read, &
        fd_write_all, &
        proc_die_with_parent, &
        proc_exit, &
        proc_exit_status, &
        proc_find, &
        proc_fork, &
        proc_pid, &
        proc_run, &
        proc_sleep_ms, &
        proc_spawn, &
        proc_try_wait
    implicit none
    private

    public :: &
        backend_git_file, &
        backend_http, &
        backend_main, &
        backend_report, &
        backend_seconds, &
        backend_take_i, &
        backend_tick_i, &
        backend_time, &
        json_raw

    !! The longest log a backend reports: the server takes bodies up to
    !! 1 MiB, and escaping can double the bytes.
    integer, parameter, public :: BACKEND_LOG_MAX = 400000

    abstract interface
        subroutine backend_take_i(server, token, class, id, repo, ref, kind, timeout)
            !! Runs check `id` (of `repo` at `ref`, kind "check" or "verify",
            !! time limit `timeout` seconds, 0 for none) on the external
            !! system for runner class `class` and reports its result.
            character(len=*), intent(in) :: server, token, class, id, repo, ref, kind
            integer, intent(in) :: timeout
        end subroutine backend_take_i

        subroutine backend_tick_i()
            !! The backend's periodic work, such as its sweep.
        end subroutine backend_tick_i
    end interface

contains

    subroutine backend_main(server, token, classes, limits, max_running, max_minutes, take, &
            tick, idle_ms, tick_seconds)
        !! Claims checks of `classes` ({"class","max_minutes","max_running"},
        !! with limits(k) the most running checks of classes(k)) while fewer
        !! than `max_running` run in all and fewer than limits(k) of that
        !! class, and gives each to `take` in a child process, with the
        !! check's time limit at most `max_minutes`. Calls `tick`
        !! at the start and every `tick_seconds`. Waits `idle_ms` when no
        !! check is given. Runs until it is stopped.
        character(len=*), intent(in) :: server, token, classes(:)
        integer, intent(in) :: limits(:), max_running, max_minutes, idle_ms, tick_seconds
        procedure(backend_take_i) :: take
        procedure(backend_tick_i) :: tick
        character(len=:), allocatable :: body, repo, ref, kind
        character(len=24) :: lim, mins, num
        integer, allocatable :: pids(:), owner(:)
        integer :: code, stat, k, timeout, parent, pid
        integer(int64) :: now, rate, last_tick
        logical :: found, given, done

        allocate (pids(0), owner(0))
        ! Checks this process held when it stopped are queued again.
        call client_request(server, 'POST', '/api/v1/checks/release', token, '{}', code, body, &
            stat)
        parent = proc_pid()
        last_tick = -1
        write (mins, '(i0)') max_minutes
        do
            call system_clock(now, rate)
            if (last_tick < 0 .or. now - last_tick >= int(tick_seconds, int64)*rate) then
                call tick()
                last_tick = now
            end if
            call reap()
            given = .false.
            do k = 1, size(classes)
                if (size(pids) >= max_running) exit
                if (count(owner == k) >= limits(k)) cycle
                write (lim, '(i0)') min(limits(k), max_running)
                call client_request(server, 'POST', '/api/v1/checks/claim', token, &
                    '{"class":"'//trim(classes(k))//'","max_minutes":'//trim(mins)// &
                    ',"max_running":'//trim(lim)//'}', code, body, stat)
                if (stat /= 0 .or. code /= 200) cycle
                call json_extract_string(body, 'repo', repo, found)
                call json_extract_string(body, 'ref', ref, found)
                call json_extract_string(body, 'kind', kind, found)
                if (.not. found) kind = 'check'
                num = json_raw(body, 'id')
                timeout = 3600
                if (kind == 'verify') timeout = 21600
                lim = json_raw(body, 'timeout')
                if (len_trim(lim) > 0) read (lim, *, iostat=stat) timeout
                ! The external system ends a job at max_minutes, so no limit is longer.
                if (max_minutes > 0) then
                    if (timeout <= 0 .or. timeout > 60*max_minutes) timeout = 60*max_minutes
                end if
                write (error_unit, '(a)') 'fy backend: check '//trim(num)//' of '//repo//' '// &
                    ref//' on '//trim(classes(k))
                flush (error_unit)
                pid = proc_fork()
                if (pid == 0) then
                    if (.not. proc_die_with_parent(parent)) call proc_exit(1)
                    call take(server, token, trim(classes(k)), trim(num), repo, ref, kind, timeout)
                    call proc_exit(0)
                end if
                if (pid > 0) then
                    pids = [pids, pid]
                    owner = [owner, k]
                    given = .true.
                end if
            end do
            if (.not. given) call proc_sleep_ms(idle_ms)
        end do

    contains

        subroutine reap()
            integer :: i, st
            i = 1
            do while (i <= size(pids))
                call proc_try_wait(pids(i), done, st)
                if (done) then
                    pids = [pids(1:i - 1), pids(i + 1:)]
                    owner = [owner(1:i - 1), owner(i + 1:)]
                else
                    i = i + 1
                end if
            end do
        end subroutine reap

    end subroutine backend_main

    subroutine backend_report(server, token, id, outcome, log, started_at)
        !! Reports check `id`: outcome "passed" or "failed" with `started_at`
        !! (when the external job started; empty when unknown), or "runner"
        !! or "timeout" for a run that ended without a verdict, which queues
        !! the check again. The log keeps its end when it is too long.
        character(len=*), intent(in) :: server, token, id, outcome, log, started_at
        character(len=:), allocatable :: body, text, payload
        integer :: code, stat, try

        text = log
        if (len(text) > BACKEND_LOG_MAX) text = '[...]'//text(len(text) - BACKEND_LOG_MAX + 1:)
        if (outcome == 'passed' .or. outcome == 'failed') then
            payload = '{"passed":'//merge('true ', 'false', outcome == 'passed')
            payload = trim(payload)//',"log":"'//json_escape_string(text)//'"'
            if (len(started_at) > 0) payload = payload//',"started_at":"'// &
                json_escape_string(started_at)//'"'
            payload = payload//'}'
        else
            payload = '{"error":"'//outcome//'","log":"'//json_escape_string(text)//'"}'
        end if
        ! A server that restarts is asked again; any answer is final.
        do try = 1, 12
            call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, payload, &
                code, body, stat)
            if (stat == 0 .and. code < 500) exit
            call proc_sleep_ms(5000)
        end do
    end subroutine backend_report

    subroutine backend_http(method, url, headers, payload, code, body, stat, etag)
        !! One request through curl, which follows redirects (to another
        !! host without the Authorization header). `headers` are header
        !! lines separated by line feeds; they and the payload go to curl on
        !! standard input. `etag`: the ETag of the answer, empty for none.
        !! stat is 0 when an answer came.
        character(len=*), intent(in) :: method, url, headers, payload
        integer, intent(out) :: code, stat
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable, intent(out), optional :: etag
        character(len=:), allocatable :: curl, conf, rest, data
        character(len=65536) :: buf
        character(len=16) :: argv(3)
        character(len=4096) :: env(4), value
        integer :: pid, to_child, from_child, n, cut, ios, e, k, ne
        character(len=12), parameter :: PASS(3) = [character(len=12) :: 'HTTPS_PROXY', &
            'HTTP_PROXY', 'NO_PROXY']

        code = 0
        body = ''
        if (present(etag)) etag = ''
        stat = 1
        curl = proc_find('curl')
        if (len(curl) == 0) return
        conf = 'url = "'//quoted(url)//'"'//new_line('a')//'request = "'//method//'"'// &
            new_line('a')//'max-time = 120'//new_line('a')//'silent'//new_line('a')// &
            'location'//new_line('a')//'proto = "=http,https"'//new_line('a')// &
            'proto-redir = "=http,https"'//new_line('a')// &
            'write-out = "\n%header{etag}\n%{http_code}"'//new_line('a')
        rest = headers
        do while (len(rest) > 0)
            e = index(rest, new_line('a'))
            if (e == 0) e = len(rest) + 1
            if (e > 1) conf = conf//'header = "'//quoted(rest(1:e - 1))//'"'//new_line('a')
            rest = rest(min(e + 1, len(rest) + 1):)
        end do
        if (len(payload) > 0) conf = conf//'header = "Content-Type: application/json"'// &
            new_line('a')//'data-binary = "'//quoted(payload)//'"'//new_line('a')
        argv(1) = 'curl'
        argv(2) = '-K'
        argv(3) = '-'
        ne = 1
        env(1) = 'PATH=/usr/local/bin:/usr/bin:/bin'
        ! The egress proxy, where the backend runs behind one.
        do k = 1, size(PASS)
            call get_environment_variable(trim(PASS(k)), value, n, ios)
            if (ios /= 0 .or. n == 0) cycle
            ne = ne + 1
            env(ne) = trim(PASS(k))//'='//value(1:n)
        end do
        call proc_spawn(curl, argv, env(1:ne), pid, to_child, from_child)
        if (pid < 0) return
        call fd_write_all(to_child, conf, stat)
        call fd_close(to_child)
        data = ''
        do
            n = fd_read(from_child, buf)
            if (n <= 0) exit
            if (len(data) + n > 67108864) exit
            data = data//buf(1:n)
        end do
        call fd_close(from_child)
        stat = proc_exit_status(pid)
        if (stat /= 0) return
        stat = 1
        cut = index(data, new_line('a'), back=.true.)
        if (cut == 0) return
        read (data(cut + 1:), *, iostat=ios) code
        if (ios /= 0) return
        data = data(1:cut - 1)
        cut = index(data, new_line('a'), back=.true.)
        if (cut == 0) return
        if (present(etag)) etag = trim(data(cut + 1:))
        body = data(1:cut - 1)
        stat = 0
    end subroutine backend_http

    function quoted(text) result(out)
        !! text inside a double-quoted curl configuration value.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i

        out = ''
        do i = 1, len(text)
            select case (text(i:i))
            case ('"', '\')
                out = out//'\'//text(i:i)
            case (achar(10))
                out = out//'\n'
            case (achar(13))
                out = out//'\r'
            case (achar(9))
                out = out//'\t'
            case default
                out = out//text(i:i)
            end select
        end do
    end function quoted

    subroutine backend_git_file(server, token, repo, ref, path, content, stat, listing)
        !! The file `path` of the commit `ref` of `repo`, fetched from the
        !! server over its git endpoint as remote runners fetch (one commit,
        !! into a fresh private directory that is removed again). stat /= 0
        !! when the commit or the file could not be read. `listing`: the
        !! names in the directory of `path`, one per line.
        character(len=*), intent(in) :: server, token, repo, ref, path
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out), optional :: listing
        character(len=:), allocatable :: dir, out, host, parent
        integer :: u, ios, s, e

        content = ''
        if (present(listing)) listing = ''
        ! mktemp, so that no other user can make the directory first and read
        ! the credentials written into it.
        call proc_run([character(len=1024) :: 'sh', '-c', 'd=$(mktemp -d "${TMPDIR:-/tmp}/'// &
            'fy-backend-XXXXXXXX") && git init --quiet --bare "$d/r.git" && printf %s "$d"', &
            'init'], dir, stat, 4096)
        if (stat /= 0 .or. len(dir) == 0) then
            stat = 1
            return
        end if
        s = index(server, '://') + 3
        e = index(server(s:), '/')
        host = server(s:)
        if (e > 0) host = server(s:s + e - 2)
        open (newunit=u, file=dir//'/credentials', status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = 1
            call proc_run([character(len=1024) :: 'rm', '-rf', dir], out, s, 4096)
            return
        end if
        write (u, '(a)') server(1:s - 1)//'runner:'//token//'@'//host
        close (u)
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'GIT_TERMINAL_PROMPT=0 git -c credential.helper= -c "credential.helper=store '// &
            '--file=$1/credentials" --git-dir "$1/r.git" fetch --quiet --depth 1 --no-tags '// &
            '"$2" "$3" && git --git-dir "$1/r.git" cat-file blob "FETCH_HEAD:$4"', 'fetch', dir, &
            server//'/'//repo//'.git', ref, path], content, stat, 1048576)
        if (stat == 0 .and. present(listing)) then
            parent = ''
            e = index(path, '/', back=.true.)
            if (e > 1) parent = path(1:e - 1)
            call proc_run([character(len=1024) :: 'git', '--git-dir', dir//'/r.git', 'ls-tree', &
                '--name-only', 'FETCH_HEAD:'//parent], listing, stat, 1048576)
        end if
        call proc_run([character(len=1024) :: 'rm', '-rf', dir], out, s, 4096)
    end subroutine backend_git_file

    function backend_seconds() result(t)
        !! Seconds of a monotonic clock.
        integer(int64) :: t
        integer(int64) :: now, rate
        call system_clock(now, rate)
        t = now/max(1_int64, rate)
    end function backend_seconds

    function backend_time(offset) result(iso)
        !! The UTC time `offset` seconds from now, as 2026-11-12T10:00:00Z.
        integer, intent(in) :: offset
        character(len=:), allocatable :: iso
        character(len=24) :: num
        integer :: stat

        write (num, '(i0)') offset
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'date -u -d "@$(( $(date +%s) + $1 ))" +%Y-%m-%dT%H:%M:%SZ', 'time', trim(num)], iso, &
            stat, 256)
        iso = trim(adjustl(iso(1:max(0, index(iso//new_line('a'), new_line('a')) - 1))))
    end function backend_time

    function json_raw(input, path) result(value)
        !! The scalar at `path` ("a.b[1].c", arrays counted from 1) of the
        !! JSON text `input`: a string's text, a number as written, true,
        !! false or null; empty when there is none.
        character(len=*), intent(in) :: input, path
        character(len=:), allocatable :: value
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=256) :: segs(64), keys(64)
        integer :: idx(64), nseg, depth, i, start
        logical :: arr(64)

        value = ''
        nseg = 0
        start = 1
        do i = 1, len(path) + 1
            if (i <= len(path)) then
                if (index('.[]', path(i:i)) == 0) cycle
            end if
            if (i > start) then
                nseg = nseg + 1
                if (nseg > size(segs)) return
                segs(nseg) = path(start:i - 1)
            end if
            start = i + 1
        end do
        ! Strict, so that ids beyond 32 bits (GitHub run ids) are read.
        call json_parser_init_strict(p, input)
        depth = 0
        keys = ''
        idx = 0
        arr = .false.
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                return
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                if (depth >= size(keys)) return
                if (depth > 0) then
                    if (arr(depth)) idx(depth) = idx(depth) + 1
                end if
                depth = depth + 1
                arr(depth) = ev%event_type == JSON_ARRAY_START
                idx(depth) = 0
                keys(depth) = ''
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                if (depth < 1) return
                depth = depth - 1
                if (depth > 0) then
                    if (.not. arr(depth)) keys(depth) = ''
                end if
            case (JSON_KEY)
                if (depth < 1) return
                if (allocated(ev%string_val)) keys(depth) = ev%string_val
            case (JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_BOOL, JSON_NULL_VAL)
                if (depth > 0) then
                    if (arr(depth)) idx(depth) = idx(depth) + 1
                end if
                if (matches()) then
                    select case (ev%event_type)
                    case (JSON_STRING)
                        if (allocated(ev%string_val)) value = ev%string_val
                    case (JSON_BOOL)
                        value = trim(merge('true ', 'false', ev%bool_val))
                    case (JSON_NULL_VAL)
                        value = 'null'
                    case default
                        if (allocated(ev%raw_val)) value = ev%raw_val
                    end select
                    return
                end if
                if (depth > 0) then
                    if (.not. arr(depth)) keys(depth) = ''
                end if
            end select
        end do

    contains

        logical function matches()
            character(len=24) :: num
            integer :: j
            matches = .false.
            if (depth /= nseg) return
            do j = 1, depth
                if (arr(j)) then
                    write (num, '(i0)') idx(j)
                    if (trim(segs(j)) /= trim(num)) return
                else
                    if (trim(keys(j)) /= trim(segs(j))) return
                end if
            end do
            matches = .true.
        end function matches

    end function json_raw

end module fy_backend
