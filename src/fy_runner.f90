module fy_runner
    !! fy runner: performs the checks of landings in its own container, with its
    !! own limits. Next to the server it reads repositories and change markers
    !! from the server's data directory (mounted read-only) and sleeps on
    !! inotify while there is nothing to do. On another host (no repositories
    !! in the data directory) it is remote: it keeps a mirror of each
    !! repository fetched over the API's git endpoint, asks for work every few
    !! seconds, and stops when the server runs a different release, so that it
    !! restarts on the same one. Either way it talks to the server over its API.
    !! With --executor it is the controller of a split runner (fy_executor):
    !! another process, without the token, checks out and runs the checks.
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        json_extract_string
    use fy_build_info, only: &
        FY_COMMIT
    use fy_client, only: &
        client_request
    use fy_events, only: &
        events_set_dir, &
        events_wait, &
        events_watch_open
    use fy_executor, only: &
        executor_run
    use fy_land, only: &
        land_check, &
        land_live_clean, &
        land_set_live
    use fy_live, only: &
        live_outcome
    use fy_proc, only: &
        proc_readlink, &
        proc_run, &
        proc_setenv, &
        proc_sleep_ms
    implicit none
    private

    public :: &
        runner_main

    !! Appends are at most this long; the server takes bodies up to 1 MiB.
    integer, parameter :: LIVE_CHUNK = 524288
    !! The check whose output is sent while it runs: server, token, number,
    !! the bytes the server has, the offset at which the output file of the
    !! current run starts, the length of that file, and the line between a
    !! failed run and its rerun. `streaming` is false once the server does not
    !! take appends (an older release, or the check is no longer ours).
    character(len=:), allocatable :: live_server, live_token, live_id, live_between
    integer(int64) :: live_sent = 0, live_base = 0, live_have = 0
    logical :: streaming = .false.
    !! The server no longer counts the check that runs as this runner's.
    logical :: withdrawn = .false.
    !! The directory shared with the executor that runs the checks (fy
    !! executor); empty: they run in this process.
    character(len=:), allocatable :: exec_dir
    character(len=:), allocatable :: runner_class

contains

    subroutine runner_main(server, token, data_dir, follow, class, executor)
        !! Takes checks of the runner class `class` until the release that
        !! `follow` points to changes (then returns, so that the runner
        !! restarts on the new release). With `executor`, a directory shared
        !! with fy executor, that process runs the checks.
        character(len=*), intent(in) :: server, token, data_dir, follow
        character(len=*), intent(in), optional :: class, executor
        character(len=:), allocatable :: body, release, cls
        integer :: code, stat, fd
        logical :: changed, remote

        cls = 'linux'
        if (present(class)) cls = class
        runner_class = cls
        exec_dir = ''
        if (present(executor)) exec_dir = executor
        ! A check may ask which class of runner it runs on.
        call proc_setenv('FORTYARD_RUNNER_CLASS', cls)
        ! Output files of checks a stopped runner left behind.
        call land_live_clean()
        inquire (file=data_dir//'/repos/.', exist=remote)
        remote = .not. remote
        if (remote) then
            call remote_main(server, token, data_dir, cls)
            return
        end if
        call release_checks(server, token)
        call events_set_dir(data_dir//'/events')
        release = ''
        if (len(follow) > 0) release = proc_readlink(follow)
        fd = events_watch_open()
        if (fd < 0) then
            write (error_unit, '(a)') 'fy runner: cannot watch '//data_dir//'/events'
            return
        end if
        do
            if (len(follow) > 0) then
                if (proc_readlink(follow) /= release) return
            end if
            call client_request(server, 'POST', '/api/v1/checks/claim', token, &
                '{"class":"'//cls//'"}', code, body, stat)
            if (stat == 0 .and. code == 200) then
                call run_one(server, token, data_dir, body)
                cycle
            end if
            ! The server is restarting: try again in a few seconds; otherwise
            ! sleep until a check is queued.
            if (stat /= 0) then
                changed = events_wait(fd, 'checks', 5000)
            else
                changed = events_wait(fd, 'checks', 600000)
            end if
        end do
    end subroutine runner_main

    subroutine remote_main(server, token, data_dir, class)
        !! The remote runner's loop; data_dir holds its mirrors and the
        !! credentials file git reads (mode 600, so the token is never on a
        !! command line).
        character(len=*), intent(in) :: server, token, data_dir, class
        character(len=:), allocatable :: body, out, commit, host
        integer :: code, stat, u, ios, s, e
        integer(int64) :: now, rate, last_cleanup

        call proc_run([character(len=1024) :: 'mkdir', '-p', data_dir//'/mirrors'], out, &
            stat, 4096)
        s = index(server, '://') + 3
        e = index(server(s:), '/')
        host = server(s:)
        if (e > 0) host = server(s:s + e - 2)
        open (newunit=u, file=data_dir//'/credentials', status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(a)') 'fy runner: cannot write '//data_dir//'/credentials'
            return
        end if
        write (u, '(a)') server(1:s - 1)//'runner:'//token//'@'//host
        close (u)
        call proc_run([character(len=1024) :: 'chmod', '600', data_dir//'/credentials'], &
            out, stat, 4096)
        call release_checks(server, token)
        last_cleanup = -1
        do
            call system_clock(now, rate)
            if (last_cleanup < 0 .or. now - last_cleanup > 3600*rate) then
                call expire_state(data_dir)
                last_cleanup = now
            end if
            call client_request(server, 'GET', '/api/v1/version', '', '', code, body, stat)
            if (stat == 0 .and. code == 200) then
                commit = ''
                s = index(body, '"commit":"')
                if (s > 0) then
                    commit = body(s + 10:)
                    commit = commit(1:index(commit, '"') - 1)
                end if
                if (len(commit) > 0 .and. commit /= FY_COMMIT) then
                    write (error_unit, '(a)') 'fy runner: the server runs '//commit// &
                        ', this runner '//FY_COMMIT//'; restarting'
                    return
                end if
            end if
            call client_request(server, 'POST', '/api/v1/checks/claim', token, &
                '{"class":"'//class//'"}', code, body, stat)
            if (stat == 0 .and. code == 200) then
                call run_one(server, token, data_dir, body, remote=.true.)
                cycle
            end if
            call proc_sleep_ms(5000)
        end do
    end subroutine remote_main

    subroutine release_checks(server, token)
        !! A starting runner hands back the checks it held when it stopped
        !! (killed or updated), so they run again instead of waiting for a
        !! result that never comes. Each runner has a token of its own.
        character(len=*), intent(in) :: server, token
        character(len=:), allocatable :: body
        integer :: code, stat
        call client_request(server, 'POST', '/api/v1/checks/release', token, '{}', code, body, &
            stat)
    end subroutine release_checks

    subroutine run_one(server, token, data_dir, claim, remote)
        character(len=*), intent(in) :: server, token, data_dir, claim
        logical, intent(in), optional :: remote
        character(len=:), allocatable :: repo, ref, out, body, work, id, kept, kind, state
        character(len=16) :: minutes
        character(len=256) :: tmp
        ! Fixed length: gfortran overflows typed array constructors around
        ! deferred-length strings.
        character(len=1024) :: w, src, r
        integer :: code, stat, n, s, e, limit
        logical :: found, timed_out, build

        call json_extract_string(claim, 'repo', repo, found)
        call json_extract_string(claim, 'ref', ref, found)
        call json_extract_string(claim, 'kind', kind, found)
        if (.not. found) kind = 'check'
        s = index(claim, '"id":') + 5
        e = s
        do while (e <= len(claim))
            if (index('0123456789', claim(e:e)) == 0) exit
            e = e + 1
        end do
        id = claim(s:e - 1)
        ! Seconds, 0 for none; a server without the field gives the old hour.
        limit = 3600
        if (kind == 'verify') limit = 21600
        s = index(claim, '"timeout":') + 10
        if (s > 10) then
            e = s
            do while (e <= len(claim))
                if (index('0123456789', claim(e:e)) == 0) exit
                e = e + 1
            end do
            if (e > s) read (claim(s:e - 1), *) limit
        end if
        call get_environment_variable('TMPDIR', tmp, n, stat)
        if (stat /= 0 .or. n == 0) tmp = '/tmp'
        ! One work tree per repository, kept between checks with its build
        ! directory, so a check builds incrementally instead of from scratch.
        work = trim(tmp)//'/fy-work/'//repo
        if (len(exec_dir) > 0) work = exec_dir//'/fy-work/'//repo
        w = work
        src = data_dir//'/repos/'//repo//'.git'
        r = ref
        if (present(remote)) then
            if (remote) then
                ! A mirror per repository, updated before each check; the
                ! work tree borrows its objects.
                src = data_dir//'/mirrors/'//repo//'.git'
                call proc_run([character(len=1024) :: 'sh', '-c', &
                ! Absolute, since git -C resolves it from the mirror.
                    'h="credential.helper=store --file=$(cd "$1" && pwd)/credentials"; '// &
                    'if [ -d "$2" ]; then git -c "$h" -C "$2" fetch --quiet --prune origin; '// &
                    'else git -c "$h" clone --quiet --mirror "$3/$4.git" "$2"; fi 2>&1', &
                    'mirror', data_dir, src, server, repo], out, stat, 65536)
                if (stat /= 0) then
                    out = 'The runner could not update its mirror of '//repo//'.'// &
                        new_line('a')//out
                    call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', &
                        token, '{"passed":false,"log":"'//json_escape_string(out)//'"}', &
                        code, body, stat)
                    return
                end if
            end if
        end if
        write (error_unit, '(a)') 'fy runner: check '//id//' of '//repo//' '//ref
        flush (error_unit)
        if (len(exec_dir) > 0) then
            call run_split()
            return
        end if
        call proc_run([character(len=1024) :: 'sh', '-c', &
            '{ [ -d "$2/.git" ] || { rm -rf "$2" && mkdir -p "$(dirname "$2")" && '// &
            'git clone --quiet --shared "$1" "$2"; }; } && cd "$2" && '// &
            'git fetch --quiet origin "$3" && git checkout --quiet --force FETCH_HEAD && '// &
            'git clean -ffdxq -e /build/ 2>&1', 'checkout', src, w, r], out, stat, 65536)
        if (stat /= 0) then
            out = 'The runner could not check out '//ref//'.'//new_line('a')//out
            code = 1
        else
            ! The work tree is kept between checks with its build directory.
            ! A failure is confirmed from an empty build directory before it
            ! is reported, so stale build state can never fail a landing.
            call proc_run([character(len=1024) :: 'touch', w], body, stat, 4096)
            ! The output goes to the server while the check runs.
            live_server = server
            live_token = token
            live_id = id
            live_sent = 0
            live_base = 0
            live_have = 0
            live_between = ''
            streaming = .true.
            call land_set_live(send_live)
            call land_check(work, out, code, verify=kind == 'verify', limit=limit, &
                timed_out=timed_out)
            if (timed_out) then
                call report_timeout()
                return
            end if
            if (code /= 0) then
                inquire (file=work//'/build/.', exist=found)
                if (found) then
                    kept = out
                    call proc_run([character(len=1024) :: 'rm', '-rf', trim(w)//'/build'], &
                        body, stat, 4096)
                    ! The live log and the stored one have the same line between
                    ! the runs, so that each line keeps its number.
                    live_between = '--- the check failed with the kept build directory and is '// &
                        'rerun from an empty build directory ---'//new_line('a')
                    if (len(kept) > 0) then
                        if (kept(len(kept):) /= new_line('a')) live_between = new_line('a')// &
                            live_between
                    end if
                    live_base = live_base + live_have + len(live_between, int64)
                    call land_check(work, out, code, verify=kind == 'verify', limit=limit, &
                        timed_out=timed_out)
                    out = kept//live_between//out
                    if (timed_out) then
                        call report_timeout()
                        return
                    end if
                end if
            end if
            call land_set_live()
            out = out//new_line('a')//disk_report(trim(tmp), data_dir)
        end if
        call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, &
            '{"passed":'//merge('true ', 'false', code == 0)//',"log":"'// &
            json_escape_string(out)//'"}', code, body, stat)

    contains

        subroutine run_split()
            !! The executor checks out and runs the check; this process sends
            !! its output and reports, as for a check that runs here.
            live_server = server
            live_token = token
            live_id = id
            live_sent = 0
            live_base = 0
            live_have = 0
            live_between = ''
            streaming = .true.
            withdrawn = .false.
            call executor_run(exec_dir, id, repo, trim(src), ref, work, kind == 'verify', limit, &
                runner_class, .false., out, code, timed_out, build, state, send_live, &
                check_withdrawn)
            if (.not. outcome()) return
            if (code /= 0 .and. build) then
                kept = out
                live_between = '--- the check failed with the kept build directory and is '// &
                    'rerun from an empty build directory ---'//new_line('a')
                if (len(kept) > 0) then
                    if (kept(len(kept):) /= new_line('a')) live_between = new_line('a')// &
                        live_between
                end if
                live_base = live_base + live_have + len(live_between, int64)
                call executor_run(exec_dir, id, repo, trim(src), ref, work, kind == 'verify', &
                    limit, runner_class, .true., out, code, timed_out, build, state, send_live, &
                    check_withdrawn)
                out = kept//live_between//out
                if (.not. outcome()) return
            end if
            out = out//new_line('a')//disk_report(exec_dir, data_dir)
            call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, &
                '{"passed":'//merge('true ', 'false', code == 0)//',"log":"'// &
                json_escape_string(out)//'"}', code, body, stat)
        end subroutine run_split

        logical function outcome() result(verdict)
            !! Reports what is no verdict; true when the check ended with one.
            verdict = .false.
            if (state == 'cancelled') then
                write (error_unit, '(a)') 'fy runner: check '//id// &
                    ' is no longer this runner''s; the executor stopped it'
            else if (state == 'lost') then
                call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, &
                    '{"error":"runner","log":"'//json_escape_string(out)//'"}', code, body, stat)
            else if (timed_out) then
                call report_timeout()
            else
                verdict = .true.
            end if
        end function outcome

        subroutine report_timeout()
            !! An error, not a result: the server queues the check again, and
            !! fails the landing with the reason timeout at the second one. The
            !! output comes first, as in the live log.
            call land_set_live()
            write (minutes, '(i0)') limit/60
            out = live_outcome(out, 'The time limit ('//trim(minutes)//' min) ended the check:'// &
                ' the runner stopped it and every process it started. A time limit is no'// &
                ' verdict on the change; a second one fails the landing or verification with'// &
                ' the reason timeout.')
            call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, &
                '{"error":"timeout","log":"'//json_escape_string(out)//'"}', code, body, stat)
        end subroutine report_timeout

    end subroutine run_one

    subroutine send_live(path, used)
        !! Sends what the check wrote to `path` since the last time, as appends
        !! that name their offset, so a repeated or lost one does no harm. A
        !! request that fails is tried again the next time. `used`: the bytes
        !! of the file the server has.
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: used
        character(len=:), allocatable :: bytes
        integer(int64) :: have, before, at
        integer :: u, ios, n

        inquire (file=path, size=have)
        have = max(0_int64, have)
        used = have
        live_have = have
        if (.not. streaming) return
        ! The line between a failed run and its rerun.
        if (live_sent < live_base) then
            at = live_base - len(live_between, int64)
            ! The server lost what came before it: the final log has it.
            if (live_sent < at) then
                streaming = .false.
                return
            end if
            call post_live(live_between(live_sent - at + 1:))
            if (live_sent < live_base) return
        end if
        do while (streaming .and. live_sent < live_base + have)
            if (live_sent < live_base) then
                streaming = .false.
                return
            end if
            n = int(min(int(LIVE_CHUNK, int64), live_base + have - live_sent))
            open (newunit=u, file=path, status='old', access='stream', form='unformatted', &
                action='read', iostat=ios)
            if (ios /= 0) exit
            allocate (character(len=n) :: bytes)
            read (u, pos=live_sent - live_base + 1, iostat=ios) bytes
            close (u)
            if (ios /= 0) exit
            before = live_sent
            call post_live(bytes)
            deallocate (bytes)
            ! Not taken now: again at the next call, not at once.
            if (live_sent <= before) exit
        end do
        if (streaming) used = max(0_int64, min(have, live_sent - live_base))
    end subroutine send_live

    subroutine post_live(bytes)
        !! One append at offset live_sent. The server answers with the length
        !! it holds; a gap is refused with that length (409), and the runner
        !! goes on from there. A request that does not reach the server or an
        !! answer of a server error or a proxy (5xx) leaves live_sent as it is
        !! for the next try. Any other answer, such as an older server's 404 or
        !! a 403 for a check that is no longer this runner's, ends the sending:
        !! the result at the end carries the log anyway.
        character(len=*), intent(in) :: bytes
        character(len=:), allocatable :: body
        character(len=24) :: num
        integer(int64) :: length
        integer :: code, stat, s, e, ios

        if (.not. streaming) return
        write (num, '(i0)') live_sent
        call client_request(live_server, 'POST', '/api/v1/checks/'//live_id//'/log?offset='// &
            trim(num), live_token, bytes, code, body, stat)
        if (stat /= 0 .or. code >= 500) return
        length = -1
        s = index(body, '"length":')
        if (s > 0) then
            s = s + 9
            e = s
            do while (e <= len(body))
                if (index('0123456789', body(e:e)) == 0) exit
                e = e + 1
            end do
            read (body(s:e - 1), *, iostat=ios) length
            if (ios /= 0) length = -1
        end if
        if ((code == 200 .or. code == 409) .and. length >= 0) then
            live_sent = length
        else
            streaming = .false.
            ! The check ended or went to another runner: an executor stops it.
            ! An older server's 404 says nothing about the check.
            withdrawn = code == 403 .or. code == 409
        end if
    end subroutine post_live

    logical function check_withdrawn()
        check_withdrawn = withdrawn
    end function check_withdrawn

    subroutine expire_state(data_dir)
        !! Work trees and mirrors of repositories not checked for 30 days are
        !! removed, so a persistent runner's disk use stays bounded; ccache
        !! and fo's cache bound themselves (CCACHE_MAXSIZE, FO_CACHE_MAX_SIZE).
        character(len=*), intent(in) :: data_dir
        character(len=256) :: tmp
        character(len=:), allocatable :: out, base
        integer :: n, stat

        call get_environment_variable('TMPDIR', tmp, n, stat)
        if (stat /= 0 .or. n == 0) tmp = '/tmp'
        base = trim(tmp)//'/fy-work'
        ! Split: the executor expires the work trees, which the checks can
        ! change; this process removes nothing there.
        if (len(exec_dir) > 0) base = data_dir//'/mirrors'
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'for d in "$1" "$2/mirrors"; do [ -d "$d" ] && '// &
            'find "$d" -mindepth 1 -maxdepth 1 -mtime +30 -exec rm -rf {} +; done; true', &
            'expire', base, data_dir], out, stat, 4096)
    end subroutine expire_state

    function disk_report(tmp, data_dir) result(text)
        !! The runner's disk use, appended to every check log so growth shows.
        character(len=*), intent(in) :: tmp, data_dir
        character(len=:), allocatable :: text
        integer :: stat

        call proc_run([character(len=1024) :: 'sh', '-c', &
            'printf "runner disk:"; for d in "$1/fy-work" "$2/mirrors" "${CCACHE_DIR:-}" '// &
            '"${FO_CACHE_DIR:-}"; do [ -n "$d" ] && [ -d "$d" ] && '// &
            'printf " %s %s" "$(basename "$d")" "$(du -sh "$d" 2>/dev/null | cut -f1)"; done; '// &
            'printf "; free %s\\n" "$(df -h "$1" | awk ''NR==2 {print $4}'')"', &
            'disk', tmp, data_dir], text, stat, 4096)
    end function disk_report

end module fy_runner
