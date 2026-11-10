module fy_runner
    !! fy runner: performs the checks of landings in its own container, with its
    !! own limits. Next to the server it reads repositories and change markers
    !! from the server's data directory (mounted read-only) and sleeps on
    !! inotify while there is nothing to do. On another host (no repositories
    !! in the data directory) it is remote: it keeps a mirror of each
    !! repository fetched over the API's git endpoint, asks for work every few
    !! seconds, and stops when the server runs a different release, so that it
    !! restarts on the same one. Either way it talks to the server over its API.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string
    use fy_client, only: client_request
    use fy_events, only: events_set_dir, events_watch_open, events_wait
    use fy_land, only: land_check
    use fy_proc, only: proc_run, proc_readlink, proc_sleep_ms
    use fy_build_info, only: FY_COMMIT
    implicit none
    private

    public :: runner_main

contains

    subroutine runner_main(server, token, data_dir, follow)
        !! Takes checks until the release that `follow` points to changes (then
        !! returns, so that the runner restarts on the new release).
        character(len=*), intent(in) :: server, token, data_dir, follow
        character(len=:), allocatable :: body, release
        integer :: code, stat, fd
        logical :: changed, remote

        inquire (file=data_dir//'/repos/.', exist=remote)
        remote = .not. remote
        if (remote) then
            call remote_main(server, token, data_dir)
            return
        end if
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
            call client_request(server, 'POST', '/api/v1/checks/claim', token, '{}', code, body, &
                stat)
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

    subroutine remote_main(server, token, data_dir)
        !! The remote runner's loop; data_dir holds its mirrors and the
        !! credentials file git reads (mode 600, so the token is never on a
        !! command line).
        character(len=*), intent(in) :: server, token, data_dir
        character(len=:), allocatable :: body, out, commit, host
        integer :: code, stat, u, ios, s, e

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
        do
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
            call client_request(server, 'POST', '/api/v1/checks/claim', token, '{}', code, &
                body, stat)
            if (stat == 0 .and. code == 200) then
                call run_one(server, token, data_dir, body, remote=.true.)
                cycle
            end if
            call proc_sleep_ms(5000)
        end do
    end subroutine remote_main

    subroutine run_one(server, token, data_dir, claim, remote)
        character(len=*), intent(in) :: server, token, data_dir, claim
        logical, intent(in), optional :: remote
        character(len=:), allocatable :: repo, ref, out, body, work, id
        character(len=256) :: tmp
        ! Fixed length: gfortran overflows typed array constructors around
        ! deferred-length strings.
        character(len=1024) :: w, src, r
        integer :: code, stat, n, s, e
        logical :: found

        call json_extract_string(claim, 'repo', repo, found)
        call json_extract_string(claim, 'ref', ref, found)
        s = index(claim, '"id":') + 5
        e = s
        do while (e <= len(claim))
            if (index('0123456789', claim(e:e)) == 0) exit
            e = e + 1
        end do
        id = claim(s:e - 1)
        call get_environment_variable('TMPDIR', tmp, n, stat)
        if (stat /= 0 .or. n == 0) tmp = '/tmp'
        ! One work tree per repository, kept between checks with its build
        ! directory, so a check builds incrementally instead of from scratch.
        work = trim(tmp)//'/fy-work/'//repo
        w = work
        src = data_dir//'/repos/'//repo//'.git'
        r = ref
        if (present(remote)) then
            if (remote) then
                ! A mirror per repository, updated before each check; the
                ! work tree borrows its objects.
                src = data_dir//'/mirrors/'//repo//'.git'
                call proc_run([character(len=1024) :: 'sh', '-c', &
                    'h="credential.helper=store --file=$1/credentials"; '// &
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
        call proc_run([character(len=1024) :: 'sh', '-c', &
            '{ [ -d "$2/.git" ] || { rm -rf "$2" && mkdir -p "$(dirname "$2")" && '// &
            'git clone --quiet --shared "$1" "$2"; }; } && cd "$2" && '// &
            'git fetch --quiet origin "$3" && git checkout --quiet --force FETCH_HEAD && '// &
            'git clean -ffdxq -e /build/ 2>&1', 'checkout', src, w, r], out, stat, 65536)
        if (stat /= 0) then
            out = 'The runner could not check out '//ref//'.'//new_line('a')//out
            code = 1
        else
            call land_check(work, out, code)
        end if
        call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, &
            '{"passed":'//merge('true ', 'false', code == 0)//',"log":"'// &
            json_escape_string(out)//'"}', code, body, stat)
    end subroutine run_one

end module fy_runner
