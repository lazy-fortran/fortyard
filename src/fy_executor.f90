module fy_executor
    !! The runner in two parts. The controller (fy runner --executor DIR) holds
    !! the runner token, claims, fetches, sends the live log and reports; the
    !! executor (fy executor DIR), in a container of its own without the token,
    !! checks out and runs the check. They share DIR: the work trees in
    !! DIR/fy-work and the hand-over files in DIR/exchange:
    !!
    !! - job: what to check, written by the controller (key=value lines);
    !!   the executor renames it to `taken` when it starts;
    !! - output: the check's output while it runs;
    !! - sent: the bytes of `output` the controller has sent, so the executor
    !!   may free them;
    !! - cancel: written by the controller, the executor stops the check;
    !! - log, then status: the folded output and the outcome
    !!   (code, timed_out, cancelled, lost, build, id).
    !!
    !! The code under check can change DIR. The controller therefore works in
    !! DIR/exchange only through a descriptor of that directory, reads only
    !! regular files there, never through a link and only up to a bound, and
    !! leaves DIR/fy-work to the executor, so the check can make it neither
    !! read (and send) nor remove its own files.
    use, intrinsic :: iso_c_binding, only: &
        c_char, &
        c_int, &
        c_long, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64
    use fy_build_info, only: &
        FY_COMMIT
    use fy_land, only: &
        land_check, &
        land_live_i, &
        land_set_abort, &
        land_set_live
    use fy_proc, only: &
        proc_run, &
        proc_setenv, &
        proc_sleep_ms
    implicit none
    private

    public :: &
        executor_cancel_i, &
        executor_main, &
        executor_run

    abstract interface
        logical function executor_cancel_i()
            !! Whether the check that runs is to be stopped.
        end function executor_cancel_i

        subroutine executor_taken_i()
            !! The executor has taken the check: no earlier one runs there.
        end subroutine executor_taken_i
    end interface

    ! fy_platform.c.
    interface
        function c_dir_open(path) bind(C, name='fy_dir_open') result(fd)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: fd
        end function c_dir_open

        function c_open_regular_at(dfd, name) bind(C, name='fy_open_regular_at') result(fd)
            import :: c_char, c_int
            integer(c_int), value :: dfd
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: fd
        end function c_open_regular_at

        function c_exists_at(dfd, name) bind(C, name='fy_exists_at') result(rc)
            import :: c_char, c_int
            integer(c_int), value :: dfd
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: rc
        end function c_exists_at

        function c_unlink_at(dfd, name) bind(C, name='fy_unlink_at') result(rc)
            import :: c_char, c_int
            integer(c_int), value :: dfd
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: rc
        end function c_unlink_at

        function c_put_at(dfd, name, data, n) bind(C, name='fy_put_at') result(rc)
            import :: c_char, c_int, c_long
            integer(c_int), value :: dfd
            character(kind=c_char), intent(in) :: name(*), data(*)
            integer(c_long), value :: n
            integer(c_int) :: rc
        end function c_put_at

        function c_read_upto(fd, buf, n) bind(C, name='fy_read_upto') result(got)
            import :: c_char, c_int, c_long
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_long), value :: n
            integer(c_long) :: got
        end function c_read_upto

        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close

        function c_rename(from, to) bind(C, name='rename') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: from(*), to(*)
            integer(c_int) :: rc
        end function c_rename
    end interface

    character(len=*), parameter :: FILES(9) = [character(len=10) :: 'job', 'taken', 'output', &
        'sent', 'cancel', 'log', 'status', 'log.new', 'status.new']
    !! The controller gives a check back when no executor takes it in this time.
    integer, parameter :: TAKE_SECONDS = 600
    !! Past the time limit and timeout's 30 s grace: the controller asks the
    !! executor to stop after the first margin and gives the check up after
    !! the second.
    integer, parameter :: LATE_CANCEL = 150, LATE_LOST = 300
    integer, parameter :: LIVE_EVERY_MS = 2000
    !! The most the controller reads of a hand-over file: the folded log is
    !! at most 64 KiB and a few lines (fy_land's LOG_BYTES).
    integer, parameter :: LOG_BYTES = 131072, STATUS_BYTES = 4096, JOB_BYTES = 65536

    !! The executor's hand-over directory, for its sink and its stop test.
    character(len=:), allocatable :: exch

contains

    subroutine executor_run(dir, id, repo, src, ref, work, verify, limit, class, rerun, &
            out, code, timed_out, build, state, sink, cancel, waits, taken_hook, nightly, shard)
        !! The controller's side: has the executor at `dir` check `ref` of
        !! `repo` (fetched from `src`, a path the executor sees as well) in
        !! `work`, or with `rerun` run the check again in `work` without its
        !! build directory, and waits for it. `sink` hears of the output while
        !! it grows; when `cancel` turns true the executor stops the check.
        !! `state`: done, cancelled (stopped because `cancel` asked) or lost
        !! (no executor took it, or it did not report; `out` says which).
        !! A check that runs well past its time limit is stopped and
        !! reported as timed out. `build`: the work tree has a build
        !! directory after the check. `waits`: the seconds of TAKE_SECONDS,
        !! LATE_CANCEL and LATE_LOST instead of those. `taken_hook`
        !! is called when the executor has taken the check, which it does only
        !! when no other runs there. `shard`: the part K/N of a split
        !! verification, which the check sees as FORTYARD_SHARD.
        character(len=*), intent(in) :: dir, id, repo, src, ref, work, class
        logical, intent(in) :: verify, rerun
        integer, intent(in) :: limit
        character(len=:), allocatable, intent(out) :: out, state
        integer, intent(out) :: code
        logical, intent(out) :: timed_out, build
        procedure(land_live_i), optional :: sink
        procedure(executor_cancel_i), optional :: cancel
        integer, intent(in), optional :: waits(3)
        procedure(executor_taken_i), optional :: taken_hook
        logical, intent(in), optional :: nightly
        character(len=*), intent(in), optional :: shard
        character(len=:), allocatable :: status, nonce, fdpath, kind_name, part
        character(len=24) :: num
        integer(int64) :: start, now, rate, began, told, used
        integer :: fd, xfd, rc, stat, take, cancel_after, lost_after
        logical :: taken, asked, late, ok

        out = ''
        code = -1
        timed_out = .false.
        build = .false.
        state = 'lost'
        kind_name = 'check'
        if (verify) kind_name = 'verify'
        if (present(nightly)) then
            if (nightly) kind_name = 'nightly'
        end if
        part = ''
        if (present(shard)) part = shard
        take = TAKE_SECONDS
        cancel_after = LATE_CANCEL
        lost_after = LATE_LOST
        if (present(waits)) then
            take = waits(1)
            cancel_after = waits(2)
            lost_after = waits(3)
        end if
        call proc_run([character(len=1024) :: 'mkdir', '-p', dir], status, stat, 4096)
        xfd = c_dir_open(cstr(dir//'/exchange'))
        if (xfd < 0) then
            out = 'The runner could not use its hand-over directory '//dir//'/exchange; the '// &
                'check is queued again.'
            return
        end if
        call settle(xfd)
        call clear(xfd)
        call system_clock(start, rate)
        write (num, '(i0)') start
        nonce = id//'-'//trim(num)
        write (num, '(i0)') limit
        call put(xfd, 'job', 'commit='//FY_COMMIT//new_line('a')//'id='//nonce//new_line('a')// &
            'repo='//repo//new_line('a')//'src='//absolute(src)//new_line('a')//'ref='//ref// &
            new_line('a')//'work='//absolute(work)//new_line('a')//'kind='// &
            trim(kind_name)//new_line('a')//'limit='//trim(num)// &
            new_line('a')//'class='//class//new_line('a')//'rerun='//merge('1', '0', rerun)// &
            new_line('a')//'shard='//part//new_line('a'))
        fd = -1
        fdpath = ''
        taken = .false.
        asked = .false.
        late = .false.
        began = start
        told = start
        do
            call system_clock(now)
            if (.not. taken) then
                if (.not. exists(xfd, 'job')) then
                    taken = .true.
                    began = now
                    if (present(taken_hook)) call taken_hook()
                end if
            end if
            if (exists(xfd, 'status')) exit
            if (present(cancel) .and. .not. (asked .or. late)) then
                if (cancel()) then
                    if (.not. taken .and. c_unlink_at(xfd, cstr('job')) == 0) then
                        state = 'cancelled'
                        call finish()
                        return
                    end if
                    call put(xfd, 'cancel', '')
                    asked = .true.
                end if
            end if
            if (taken .and. fd < 0) then
                fd = c_open_regular_at(xfd, cstr('output'))
                if (fd >= 0) then
                    write (num, '(i0)') fd
                    fdpath = '/proc/self/fd/'//trim(num)
                    ! The check's time limit starts with its output, after
                    ! the checkout.
                    began = now
                end if
            end if
            if (present(sink) .and. fd >= 0 .and. (now - told)*1000 >= LIVE_EVERY_MS*rate) then
                call sink(fdpath, used)
                write (num, '(i0)') used
                call put(xfd, 'sent', trim(num))
                told = now
            end if
            if (.not. taken .and. now - start > int(take, int64)*rate) then
                if (c_unlink_at(xfd, cstr('job')) == 0) then
                    out = 'No executor took the check within '//itoa(take)// &
                        ' seconds; it is queued again.'
                    call finish()
                    return
                end if
            end if
            if (taken .and. limit > 0) then
                if (.not. (asked .or. late) .and. &
                    now - began > int(limit + cancel_after, int64)*rate) then
                    call put(xfd, 'cancel', '')
                    late = .true.
                end if
                if (now - began > int(limit + lost_after, int64)*rate) then
                    out = 'The executor did not report the check after its time limit; it is '// &
                        'queued again.'
                    call finish()
                    return
                end if
            end if
            call proc_sleep_ms(100)
        end do
        if (fd < 0) then
            fd = c_open_regular_at(xfd, cstr('output'))
            if (fd >= 0) then
                write (num, '(i0)') fd
                fdpath = '/proc/self/fd/'//trim(num)
            end if
        end if
        if (present(sink) .and. fd >= 0) call sink(fdpath, used)
        call read_regular(xfd, 'status', STATUS_BYTES, status, ok)
        if (.not. ok .or. field(status, 'id') /= nonce) then
            out = 'The executor reported another check than this one; it is queued again.'
            call finish()
            return
        end if
        call read_regular(xfd, 'log', LOG_BYTES, out, ok)
        if (.not. ok) out = ''
        code = number(field(status, 'code'), -1)
        timed_out = field(status, 'timed_out') == '1'
        build = field(status, 'build') == '1'
        state = 'done'
        ! A cancel marker counts only when this side wrote it: the check can
        ! write one as well, and then just ends itself.
        if (field(status, 'cancelled') == '1') then
            if (asked) then
                state = 'cancelled'
            else if (late) then
                timed_out = .true.
            end if
        end if
        if (field(status, 'lost') == '1') state = 'lost'
        call finish()

    contains

        subroutine finish()
            if (fd >= 0) rc = c_close(fd)
            call clear(xfd)
            rc = c_close(xfd)
        end subroutine finish

    end subroutine executor_run

    subroutine settle(xfd)
        !! A check that an earlier controller handed over and that still runs
        !! is stopped first, so its outcome is not taken for the next one's.
        integer(c_int), intent(in) :: xfd
        integer :: k

        if (.not. exists(xfd, 'taken') .or. exists(xfd, 'status')) return
        call put(xfd, 'cancel', '')
        do k = 1, 1200
            if (exists(xfd, 'status')) return
            call proc_sleep_ms(100)
        end do
    end subroutine settle

    subroutine executor_main(dir)
        !! The executor: takes the hand-overs in `dir` one after the other.
        !! Returns when a hand-over comes from a controller of another
        !! release, which it leaves in place for the executor of that release.
        !! It also removes the work trees not used for 30 days.
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: job, out
        logical :: there, done
        integer :: stat
        integer(int64) :: now, rate, expired

        exch = dir//'/exchange'
        call proc_run([character(len=1024) :: 'mkdir', '-p', exch, dir//'/fy-work'], out, stat, &
            4096)
        ! A check the executor ran when it was stopped has no outcome.
        inquire (file=exch//'/taken', exist=there)
        inquire (file=exch//'/status', exist=done)
        if (there .and. .not. done) then
            call read_file(exch//'/taken', JOB_BYTES, job)
            call report(field(job, 'id'), 'The executor stopped during the check; it is '// &
                'queued again.', -1, .false., .false., .true., .false.)
        end if
        call system_clock(expired, rate)
        call expire(dir)
        do
            call system_clock(now)
            if (now - expired > 86400_int64*rate) then
                call expire(dir)
                expired = now
            end if
            inquire (file=exch//'/job', exist=there)
            if (.not. there) then
                call proc_sleep_ms(200)
                cycle
            end if
            call read_file(exch//'/job', JOB_BYTES, job)
            if (field(job, 'commit') /= FY_COMMIT) then
                write (error_unit, '(a)') 'fy executor: the controller runs '// &
                    field(job, 'commit')//', this executor '//FY_COMMIT//'; leaving'
                return
            end if
            if (c_rename(cstr(exch//'/job'), cstr(exch//'/taken')) /= 0) cycle
            call read_file(exch//'/taken', JOB_BYTES, job)
            call execute(job)
        end do
    end subroutine executor_main

    subroutine expire(dir)
        !! Here, not in the controller: the checks can change this directory,
        !! and a link they put there must not lead the controller's removal
        !! into the controller's own files.
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: out
        integer :: stat
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'find "$1/fy-work" -mindepth 1 -maxdepth 1 -mtime +30 -exec rm -rf {} +; true', &
            'expire', dir], out, stat, 4096)
    end subroutine expire

    subroutine execute(job)
        character(len=*), intent(in) :: job
        character(len=:), allocatable :: work, out, ignored
        ! Fixed length: gfortran overflows typed array constructors around
        ! deferred-length strings.
        character(len=1024) :: w, src, r
        integer :: code, limit, stat
        logical :: timed_out, build, stopped

        work = field(job, 'work')
        w = work
        src = field(job, 'src')
        r = field(job, 'ref')
        limit = number(field(job, 'limit'), 3600)
        call proc_setenv('FORTYARD_RUNNER_CLASS', field(job, 'class'))
        call proc_setenv('FORTYARD_SHARD', field(job, 'shard'))
        timed_out = .false.
        write (error_unit, '(a)') 'fy executor: '//field(job, 'kind')//' of '// &
            field(job, 'repo')//' '//field(job, 'ref')
        flush (error_unit)
        if (field(job, 'rerun') == '1') then
            ! Again from an empty build directory, in the same work tree.
            call proc_run([character(len=1024) :: 'rm', '-rf', trim(w)//'/build'], ignored, &
                stat, 4096)
        else
            call proc_run([character(len=1024) :: 'sh', '-c', &
                '{ [ -d "$2/.git" ] || { rm -rf "$2" && mkdir -p "$(dirname "$2")" && '// &
                'git clone --quiet --shared "$1" "$2"; }; } && cd "$2" && '// &
            ! origin/main as main is now (the work tree is kept), so that a check
            ! can diff the candidate against it; a repository without main skips it
                '{ git fetch --quiet origin +refs/heads/main:refs/remotes/origin/main || :; } && '// &
                'git fetch --quiet origin "$3" && git checkout --quiet --force FETCH_HEAD && '// &
                'git clean -ffdxq -e /build/ 2>&1', 'checkout', src, w, r], out, stat, 65536)
            if (stat /= 0) then
                call report(field(job, 'id'), 'The runner could not check out '// &
                    field(job, 'ref')//'.'//new_line('a')//out, 1, .false., .false., .false., &
                    .false.)
                return
            end if
            ! The work tree's age says when it was last used.
            call proc_run([character(len=1024) :: 'touch', w], ignored, stat, 4096)
        end if
        call land_set_live(sink)
        call land_set_abort(stop_now)
        call land_check(work, out, code, verify=field(job, 'kind') == 'verify' .or. &
            field(job, 'kind') == 'nightly', nightly=field(job, 'kind') == 'nightly', &
            limit=limit, timed_out=timed_out, output=exch//'/output')
        call land_set_live()
        call land_set_abort()
        inquire (file=work//'/build/.', exist=build)
        inquire (file=exch//'/cancel', exist=stopped)
        call report(field(job, 'id'), out, code, timed_out, stopped, .false., build)
    end subroutine execute

    subroutine report(id, out, code, timed_out, cancelled, lost, build)
        !! The outcome, log first: the controller reads it once status is there.
        character(len=*), intent(in) :: id, out
        integer, intent(in) :: code
        logical, intent(in) :: timed_out, cancelled, lost, build
        integer(c_int) :: xfd
        integer :: rc

        xfd = c_dir_open(cstr(exch))
        if (xfd < 0) return
        call put(xfd, 'log', out)
        call put(xfd, 'status', 'id='//id//new_line('a')//'code='//itoa(code)//new_line('a')// &
            'timed_out='//flag(timed_out)//new_line('a')//'cancelled='//flag(cancelled)// &
            new_line('a')//'lost='//flag(lost)//new_line('a')//'build='//flag(build)// &
            new_line('a'))
        rc = c_close(xfd)
    end subroutine report

    subroutine sink(path, used)
        !! The executor frees only what the controller has sent.
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: used
        character(len=:), allocatable :: text
        integer(int64) :: have
        integer :: ios

        used = 0
        inquire (file=path, size=have)
        call read_file(exch//'/sent', 64, text)
        if (len(text) == 0) return
        read (text, *, iostat=ios) used
        if (ios /= 0) used = 0
        used = max(0_int64, min(used, have))
    end subroutine sink

    logical function stop_now()
        inquire (file=exch//'/cancel', exist=stop_now)
    end function stop_now

    subroutine put(xfd, name, text)
        !! Writes `name` in the directory `xfd` whole or not at all: aside,
        !! created anew (never through a link someone left there), then
        !! renamed into place.
        integer(c_int), intent(in) :: xfd
        character(len=*), intent(in) :: name, text
        integer :: rc
        rc = c_put_at(xfd, cstr(name), text, len(text, c_long))
    end subroutine put

    subroutine clear(xfd)
        integer(c_int), intent(in) :: xfd
        integer :: k, rc
        do k = 1, size(FILES)
            rc = c_unlink_at(xfd, cstr(trim(FILES(k))))
        end do
        rc = c_unlink_at(xfd, cstr('job.new'))
        rc = c_unlink_at(xfd, cstr('sent.new'))
        rc = c_unlink_at(xfd, cstr('cancel.new'))
    end subroutine clear

    logical function exists(xfd, name)
        integer(c_int), intent(in) :: xfd
        character(len=*), intent(in) :: name
        exists = c_exists_at(xfd, cstr(name)) /= 0
    end function exists

    subroutine read_regular(xfd, name, most, text, ok)
        !! At most `most` bytes of `name` in the directory `xfd` when it is a
        !! regular file, read without following a link.
        integer(c_int), intent(in) :: xfd
        character(len=*), intent(in) :: name
        integer, intent(in) :: most
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: ok
        character(kind=c_char, len=:), allocatable :: buf
        integer(c_long) :: n
        integer :: fd, rc

        text = ''
        fd = c_open_regular_at(xfd, cstr(name))
        ok = fd >= 0
        if (.not. ok) return
        allocate (character(kind=c_char, len=most) :: buf)
        n = c_read_upto(fd, buf, int(most, c_long))
        rc = c_close(fd)
        if (n > 0) text = buf(1:n)
    end subroutine read_regular

    subroutine read_file(path, most, text)
        !! At most `most` bytes of `path`.
        character(len=*), intent(in) :: path
        integer, intent(in) :: most
        character(len=:), allocatable, intent(out) :: text
        integer(int64) :: n
        integer :: u, ios

        text = ''
        inquire (file=path, size=n)
        n = min(n, int(most, int64))
        if (n <= 0) return
        open (newunit=u, file=path, status='old', access='stream', form='unformatted', &
            action='read', iostat=ios)
        if (ios /= 0) return
        deallocate (text)
        allocate (character(len=int(n)) :: text)
        read (u, iostat=ios) text
        close (u)
        if (ios /= 0) text = ''
    end subroutine read_file

    function field(text, key) result(value)
        !! The value of the line `key=value` in `text`, or empty.
        character(len=*), intent(in) :: text, key
        character(len=:), allocatable :: value
        integer :: s, e

        value = ''
        s = index(new_line('a')//text, new_line('a')//key//'=')
        if (s == 0) return
        s = s + len(key) + 1
        e = index(text(s:), new_line('a'))
        if (e == 0) then
            value = text(s:)
        else
            value = text(s:s + e - 2)
        end if
    end function field

    function absolute(path) result(full)
        !! `path` from the root, as the executor may run elsewhere.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: full
        character(len=4096) :: cwd

        full = path
        if (len(path) > 0) then
            if (path(1:1) == '/') return
        end if
        call getcwd(cwd)
        full = trim(cwd)//'/'//path
    end function absolute

    integer function number(text, default) result(n)
        character(len=*), intent(in) :: text
        integer, intent(in) :: default
        integer :: ios
        n = default
        if (len(text) == 0) return
        read (text, *, iostat=ios) n
        if (ios /= 0) n = default
    end function number

    function cstr(s) result(c)
        character(len=*), intent(in) :: s
        character(kind=c_char, len=:), allocatable :: c
        c = s//c_null_char
    end function cstr

    function flag(b) result(s)
        logical, intent(in) :: b
        character(len=1) :: s
        s = merge('1', '0', b)
    end function flag

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_executor
