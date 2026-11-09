module fy_runner
    !! fy runner: performs the checks of landings in its own container, with its
    !! own limits, next to the server. It reads repositories and change markers
    !! from the server's data directory (mounted read-only), talks to the server
    !! over its API, and sleeps on inotify while there is nothing to do.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string
    use fy_client, only: client_request
    use fy_events, only: events_set_dir, events_watch_open, events_wait
    use fy_land, only: land_check
    use fy_proc, only: proc_run, proc_readlink
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
        logical :: changed

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

    subroutine run_one(server, token, data_dir, claim)
        character(len=*), intent(in) :: server, token, data_dir, claim
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
        work = trim(tmp)//'/fy-check-'//id
        w = work
        src = data_dir//'/repos/'//repo//'.git'
        r = ref
        write (error_unit, '(a)') 'fy runner: check '//id//' of '//repo//' '//ref
        flush (error_unit)
        call proc_run([character(len=1024) :: 'rm', '-rf', w], out, stat, 4096)
        call proc_run([character(len=1024) :: 'sh', '-c', 'git clone --quiet --shared "$1" "$2" '// &
            '&& cd "$2" && git fetch --quiet origin "$3" && git checkout --quiet FETCH_HEAD 2>&1', &
            'checkout', src, w, r], out, stat, 65536)
        if (stat /= 0) then
            out = 'The runner could not check out '//ref//'.'//new_line('a')//out
            code = 1
        else
            call land_check(work, out, code)
        end if
        call proc_run([character(len=1024) :: 'rm', '-rf', w], body, stat, 4096)
        call client_request(server, 'POST', '/api/v1/checks/'//id//'/result', token, &
            '{"passed":'//merge('true ', 'false', code == 0)//',"log":"'// &
            json_escape_string(out)//'"}', code, body, stat)
    end subroutine run_one

end module fy_runner
