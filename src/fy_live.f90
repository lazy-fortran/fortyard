module fy_live
    !! The growing log of a running check, kept as files in the data
    !! directory (live/KEY.log, KEY.tail, KEY.skip, KEY.begun) rather than in the
    !! database, so that an append costs a write of the new bytes only and the
    !! rows read for every list stay small. KEY is check-N for a runner's check
    !! and job-N for a check the lander runs itself. Offsets count every byte
    !! the check wrote; of a check that writes more than LIVE_HEAD + LIVE_TAIL
    !! bytes the first LIVE_HEAD and the last LIVE_TAIL are kept, and a reader
    !! is told in a line of its own how much was left out. Appends hold an
    !! exclusive lock on KEY.log, reads a shared one, since every connection
    !! slot is a process of its own.
    use, intrinsic :: iso_c_binding, only: &
        c_char, &
        c_int, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: &
        int64
    implicit none
    private

    public :: &
        LIVE_EARLIER, &
        LIVE_HEAD, &
        LIVE_TAIL, &
        live_append, &
        live_earlier_at, &
        live_exists, &
        live_gap_line, &
        live_length, &
        live_outcome, &
        live_read, &
        live_remove, &
        live_set_dir, &
        live_tail_line

    integer(int64), parameter :: LIVE_HEAD = 4194304_int64, LIVE_TAIL = 4194304_int64
    !! The stored log of a check holds the output of its last attempt first, so
    !! that its lines keep their live numbers, then how it ended, and then each
    !! earlier attempt after this line (a check queued again after a timeout or
    !! a lost runner).
    character(len=*), parameter :: LIVE_EARLIER = '--- an earlier attempt of this check ---'
    !! The tail file is cut back to LIVE_TAIL once it is this much longer, so
    !! that it is not rewritten on every append.
    integer(int64), parameter :: SLACK = 1048576_int64
    integer(c_int), parameter :: O_RDONLY = 0, O_RDWR = 2, O_CREAT = 64, O_CLOEXEC = 524288
    integer(c_int), parameter :: LOCK_SH = 1, LOCK_EX = 2

    interface
        function c_open(path, flags, mode) bind(C, name='open') result(fd)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags, mode
            integer(c_int) :: fd
        end function c_open

        function c_flock(fd, op) bind(C, name='flock') result(rc)
            import :: c_int
            integer(c_int), value :: fd, op
            integer(c_int) :: rc
        end function c_flock

        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close

        function c_rename(from, to) bind(C, name='rename') result(rc)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: from(*), to(*)
            integer(c_int) :: rc
        end function c_rename
    end interface

    character(len=:), allocatable :: dir

contains

    subroutine live_set_dir(path)
        !! Set once before the processes start; created if missing.
        character(len=*), intent(in) :: path
        dir = path
        call execute_command_line('mkdir -p '''//dir//'''')
    end subroutine live_set_dir

    subroutine live_append(key, offset, data, length, stat)
        !! Appends the bytes `data` that the check wrote from byte `offset` on.
        !! Bytes already held are skipped, so a repeated append changes
        !! nothing; an offset beyond the end is a gap and refused (stat 1).
        !! `length` is the log's length afterwards; stat 2: not stored.
        character(len=*), intent(in) :: key, data
        integer(int64), intent(in) :: offset
        integer(int64), intent(out) :: length
        integer, intent(out) :: stat
        integer(int64) :: head, start, tail, skip, k
        integer(c_int) :: fd, rc
        logical :: has_tail

        length = 0
        stat = 2
        if (.not. allocated(dir)) return
        fd = c_open(cstr(path(key, 'log')), ior(ior(O_RDWR, O_CREAT), O_CLOEXEC), int(o'644', c_int))
        if (fd < 0) return
        rc = c_flock(fd, LOCK_EX)
        call sizes(key, head, has_tail, start, tail)
        length = merge(start + tail, head, has_tail)
        if (length == 0) call mark_begun(key)
        stat = 1
        if (offset > length) then
            rc = c_close(fd)
            return
        end if
        stat = 0
        skip = length - offset
        if (skip >= len(data, int64)) then
            rc = c_close(fd)
            return
        end if
        k = skip
        if (.not. has_tail .and. head < LIVE_HEAD) then
            k = min(len(data, int64), skip + LIVE_HEAD - head)
            if (.not. put(path(key, 'log'), data(skip + 1:k))) stat = 2
            head = head + k - skip
        end if
        if (stat == 0 .and. k < len(data, int64)) then
            if (.not. has_tail) then
                start = head
                if (.not. put(path(key, 'skip'), number(start)//new_line('a'), replace=.true.)) stat = 2
                has_tail = .true.
                tail = 0
            end if
            if (.not. put(path(key, 'tail'), data(k + 1:))) stat = 2
            tail = tail + len(data, int64) - k
            if (stat == 0 .and. tail > LIVE_TAIL + SLACK) call cut(key, start, tail)
        end if
        call sizes(key, head, has_tail, start, tail)
        length = merge(start + tail, head, has_tail)
        rc = c_close(fd)
    end subroutine live_append

    subroutine live_read(key, from, max_bytes, text, next, length, found, begun)
        !! The log from byte `from` on, at most about `max_bytes` of it: `next`
        !! is the offset after the bytes in `text`, `length` the log's length.
        !! Where bytes were left out, `text` has a line that says how many.
        !! The text never ends inside a UTF-8 character. `begun` names this
        !! log among the logs the key had: it changes when the log starts
        !! again, also when the new one is already longer than a reader got.
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: from
        integer, intent(in) :: max_bytes
        character(len=:), allocatable, intent(out) :: text
        integer(int64), intent(out) :: next, length
        logical, intent(out) :: found
        character(len=:), allocatable, intent(out), optional :: begun
        integer(int64) :: head, start, tail, shown, p, n, budget
        integer(c_int) :: fd, rc
        logical :: has_tail
        character(len=:), allocatable :: part, lead

        text = ''
        next = 0
        length = 0
        found = .false.
        if (present(begun)) begun = ''
        if (.not. allocated(dir)) return
        fd = c_open(cstr(path(key, 'log')), ior(O_RDONLY, O_CLOEXEC), 0_c_int)
        if (fd < 0) return
        found = .true.
        rc = c_flock(fd, LOCK_SH)
        if (present(begun)) then
            begun = get(path(key, 'begun'), 0_int64, 32_int64)
            if (index(begun, new_line('a')) > 0) begun = begun(1:index(begun, new_line('a')) - 1)
        end if
        call sizes(key, head, has_tail, start, tail)
        length = merge(start + tail, head, has_tail)
        ! Of the tail file only the last LIVE_TAIL bytes are shown.
        shown = length
        if (has_tail) shown = max(start, length - LIVE_TAIL)
        p = max(0_int64, min(from, length))
        budget = max_bytes
        if (p < head) then
            n = min(head - p, budget)
            text = get(path(key, 'log'), p, n)
            p = p + len(text, int64)
            budget = budget - len(text, int64)
        end if
        if (has_tail .and. budget > 0 .and. p < length) then
            if (p < shown) then
                ! The line that says so stands on its own, also after a reader's
                ! earlier read that ended within a line.
                if (len(text) > 0) then
                    lead = text(len(text):)
                else if (p == 0) then
                    lead = new_line('a')
                else if (p - 1 < head) then
                    lead = get(path(key, 'log'), p - 1, 1_int64)
                else if (p - 1 >= start) then
                    lead = get(path(key, 'tail'), p - 1 - start, 1_int64)
                else
                    lead = ''
                end if
                if (lead /= new_line('a')) text = text//new_line('a')
                text = text//live_gap_line(shown - head)//new_line('a')
                p = shown
            end if
            n = min(length - p, budget)
            part = get(path(key, 'tail'), p - start, n)
            text = text//part
            p = p + len(part, int64)
        end if
        rc = c_close(fd)
        n = whole(text)
        p = p - (len(text) - n)
        text = text(1:n)
        next = p
    end subroutine live_read

    function live_gap_line(n) result(line)
        !! The line that stands for `n` bytes of a log that were left out; the
        !! stored log of a long check has it too, so a reader maps offsets.
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: line
        line = '... '//number(n)//' bytes left out ...'
    end function live_gap_line

    logical function live_exists(key)
        character(len=*), intent(in) :: key
        live_exists = .false.
        if (allocated(dir)) inquire (file=path(key, 'log'), exist=live_exists)
    end function live_exists

    integer(int64) function live_length(key) result(length)
        !! The log's length in bytes; -1 when there is none.
        character(len=*), intent(in) :: key
        integer(int64) :: head, start, tail
        logical :: has_tail

        length = -1
        if (.not. live_exists(key)) return
        call sizes(key, head, has_tail, start, tail)
        length = merge(start + tail, head, has_tail)
    end function live_length

    function live_tail_line(key) result(line)
        !! The last line of the log that has text, at most 200 bytes of it.
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: line, text
        integer(int64) :: length, next
        integer :: e, b
        logical :: found

        line = ''
        length = live_length(key)
        if (length <= 0) return
        call live_read(key, max(0_int64, length - 4096), 4096, text, next, length, found)
        e = len(text)
        do while (e > 0)
            if (text(e:e) /= new_line('a') .and. text(e:e) /= achar(13) .and. text(e:e) /= ' ') exit
            e = e - 1
        end do
        b = index(text(1:e), new_line('a'), back=.true.) + 1
        line = text(b:e)
        if (len(line) > 200) line = line(1:whole(line(1:200)))
    end function live_tail_line

    subroutine live_remove(key)
        character(len=*), intent(in) :: key
        integer :: u, ios
        character(len=5), parameter :: ENDS(4) = [character(len=5) :: 'tail', 'skip', 'begun', 'log']
        integer :: k

        if (.not. allocated(dir)) return
        do k = 1, size(ENDS)
            open (newunit=u, file=path(key, trim(ENDS(k))), status='old', iostat=ios)
            if (ios == 0) close (u, status='delete')
        end do
    end subroutine live_remove

    subroutine mark_begun(key)
        !! Names a log that starts: by the clock, which differs for each.
        character(len=*), intent(in) :: key
        integer(int64) :: now
        logical :: has

        inquire (file=path(key, 'begun'), exist=has)
        if (has) return
        call system_clock(now)
        has = put(path(key, 'begun'), number(now)//new_line('a'), replace=.true.)
    end subroutine mark_begun

    integer function live_earlier_at(log) result(at)
        !! Where the first earlier attempt's line LIVE_EARLIER starts in the
        !! stored log `log`; 0 when it has none.
        character(len=*), intent(in) :: log
        character(len=*), parameter :: LF = new_line('a')

        at = 0
        if (index(log, LIVE_EARLIER//LF) == 1 .or. log == LIVE_EARLIER) then
            at = 1
            return
        end if
        at = index(log, LF//LIVE_EARLIER//LF)
        if (at == 0 .and. len(log) > len(LIVE_EARLIER)) then
            if (log(len(log) - len(LIVE_EARLIER):) == LF//LIVE_EARLIER) &
                at = len(log) - len(LIVE_EARLIER)
        end if
        if (at > 0) at = at + 1
    end function live_earlier_at

    function live_outcome(log, line) result(full)
        !! The stored log `log` with `line` on a line of its own after the last
        !! attempt's output, before the earlier attempts.
        character(len=*), intent(in) :: log, line
        character(len=:), allocatable :: full, rest
        integer :: at

        at = live_earlier_at(log)
        if (at == 0) then
            full = log
            rest = ''
        else
            full = log(1:at - 1)
            rest = log(at:)
        end if
        if (len(full) > 0) then
            if (full(len(full):) /= new_line('a')) full = full//new_line('a')
        end if
        full = full//line
        if (len(rest) > 0) full = full//new_line('a')//rest
    end function live_outcome

    subroutine sizes(key, head, has_tail, start, tail)
        !! Bytes in KEY.log; whether there is a tail, the offset its first byte
        !! has and its bytes.
        character(len=*), intent(in) :: key
        integer(int64), intent(out) :: head, start, tail
        logical, intent(out) :: has_tail
        character(len=32) :: line
        integer :: u, ios

        head = 0
        start = 0
        tail = 0
        inquire (file=path(key, 'log'), size=head)
        head = max(head, 0_int64)
        inquire (file=path(key, 'tail'), exist=has_tail, size=tail)
        if (.not. has_tail) return
        tail = max(tail, 0_int64)
        open (newunit=u, file=path(key, 'skip'), status='old', action='read', iostat=ios)
        if (ios == 0) then
            read (u, '(a)', iostat=ios) line
            close (u)
            if (ios == 0) read (line, *, iostat=ios) start
        end if
        if (ios /= 0) start = head
    end subroutine sizes

    subroutine cut(key, start, tail)
        !! Keeps the last LIVE_TAIL bytes of the tail file.
        character(len=*), intent(in) :: key
        integer(int64), intent(inout) :: start, tail
        character(len=:), allocatable :: kept
        integer(c_int) :: rc

        kept = get(path(key, 'tail'), tail - LIVE_TAIL, LIVE_TAIL)
        if (.not. put(path(key, 'tail.new'), kept, replace=.true.)) return
        if (.not. put(path(key, 'skip.new'), number(start + tail - LIVE_TAIL)//new_line('a'), &
            replace=.true.)) return
        rc = c_rename(cstr(path(key, 'skip.new')), cstr(path(key, 'skip')))
        rc = c_rename(cstr(path(key, 'tail.new')), cstr(path(key, 'tail')))
        start = start + tail - LIVE_TAIL
        tail = LIVE_TAIL
    end subroutine cut

    logical function put(file, bytes, replace) result(ok)
        character(len=*), intent(in) :: file, bytes
        logical, intent(in), optional :: replace
        integer :: u, ios
        logical :: fresh

        fresh = .false.
        if (present(replace)) fresh = replace
        if (fresh) then
            open (newunit=u, file=file, status='replace', access='stream', form='unformatted', &
                action='write', iostat=ios)
        else
            open (newunit=u, file=file, status='unknown', access='stream', form='unformatted', &
                action='write', position='append', iostat=ios)
        end if
        ok = ios == 0
        if (.not. ok) return
        write (u, iostat=ios) bytes
        ok = ios == 0
        close (u)
    end function put

    function get(file, offset, n) result(bytes)
        !! Up to `n` bytes of `file` from byte `offset` (0 the first).
        character(len=*), intent(in) :: file
        integer(int64), intent(in) :: offset, n
        character(len=:), allocatable :: bytes
        integer(int64) :: have
        integer :: u, ios

        bytes = ''
        inquire (file=file, size=have)
        if (have <= offset .or. n <= 0) return
        open (newunit=u, file=file, status='old', access='stream', form='unformatted', &
            action='read', iostat=ios)
        if (ios /= 0) return
        deallocate (bytes)
        allocate (character(len=int(min(n, have - offset))) :: bytes)
        read (u, pos=offset + 1, iostat=ios) bytes
        close (u)
        if (ios /= 0) bytes = ''
    end function get

    pure integer function whole(text) result(n)
        !! The length of `text` without a UTF-8 character cut off at its end.
        character(len=*), intent(in) :: text
        integer :: k, c, need

        n = len(text)
        do k = len(text), max(1, len(text) - 3), -1
            c = iachar(text(k:k))
            if (c < 128) return
            if (c >= 192) then
                need = 2
                if (c >= 224) need = 3
                if (c >= 240) need = 4
                if (len(text) - k + 1 < need) n = k - 1
                return
            end if
        end do
    end function whole

    function path(key, ending) result(p)
        character(len=*), intent(in) :: key, ending
        character(len=:), allocatable :: p
        p = dir//'/'//key//'.'//ending
    end function path

    function number(v) result(s)
        integer(int64), intent(in) :: v
        character(len=:), allocatable :: s
        character(len=24) :: b
        write (b, '(i0)') v
        s = trim(b)
    end function number

    function cstr(s) result(c)
        character(len=*), intent(in) :: s
        character(kind=c_char) :: c(len(s) + 1)
        integer :: k
        do k = 1, len(s)
            c(k) = s(k:k)
        end do
        c(len(s) + 1) = c_null_char
    end function cstr

end module fy_live
