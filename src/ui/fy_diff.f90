module fy_diff
    !! A unified diff (git show --patch) as files, hunks and numbered lines.
    implicit none
    private

    public :: &
        diff_file_t, &
        diff_line_t, &
        parse_patch
    integer, parameter, public :: DL_CONTEXT = 0, DL_ADDED = 1, DL_REMOVED = 2, DL_HUNK = 3, &
        DL_NOTE = 4

    type :: diff_line_t
        integer :: kind = DL_CONTEXT
        !! Line numbers in the old and new file; 0 where the line has none.
        integer :: old_no = 0, new_no = 0
        character(len=:), allocatable :: text
    end type diff_line_t

    type :: diff_file_t
        character(len=:), allocatable :: path, old_path
        !! "modified", "added", "deleted", "renamed" or "binary".
        character(len=:), allocatable :: status
        integer :: additions = 0, deletions = 0
        type(diff_line_t), allocatable :: lines(:)
        integer :: count = 0
    end type diff_file_t

contains

    subroutine parse_patch(patch, files, max_lines)
        !! Files of `patch`; after `max_lines` lines in total, further lines are
        !! counted but not kept.
        character(len=*), intent(in) :: patch
        type(diff_file_t), allocatable, intent(out) :: files(:)
        integer, intent(in) :: max_lines
        character(len=:), allocatable :: line
        integer :: pos, nl, nf, old_no, new_no, kept
        logical :: in_hunk

        allocate (files(0))
        nf = 0
        pos = 1
        kept = 0
        in_hunk = .false.
        old_no = 0
        new_no = 0
        do while (pos <= len(patch))
            nl = index(patch(pos:), new_line('a'))
            if (nl == 0) then
                line = patch(pos:)
                pos = len(patch) + 1
            else
                line = patch(pos:pos + nl - 2)
                pos = pos + nl
            end if
            if (starts(line, 'diff --git ')) then
                files = [files, diff_file_t()]
                nf = size(files)
                allocate (files(nf)%lines(16))
                files(nf)%path = git_path(line)
                files(nf)%old_path = files(nf)%path
                files(nf)%status = 'modified'
                in_hunk = .false.
                cycle
            end if
            if (nf == 0) cycle
            if (.not. in_hunk) then
                if (starts(line, 'new file')) files(nf)%status = 'added'
                if (starts(line, 'deleted file')) files(nf)%status = 'deleted'
                if (starts(line, 'rename from ')) then
                    files(nf)%old_path = line(13:)
                    files(nf)%status = 'renamed'
                end if
                if (starts(line, 'rename to ')) files(nf)%path = line(11:)
                if (starts(line, 'Binary files')) files(nf)%status = 'binary'
                if (.not. starts(line, '@@')) cycle
            end if
            if (starts(line, '@@')) then
                in_hunk = .true.
                call hunk_start(line, old_no, new_no)
                call add(DL_HUNK, 0, 0, line)
                cycle
            end if
            if (len(line) == 0) then
                call add(DL_CONTEXT, old_no, new_no, '')
                old_no = old_no + 1
                new_no = new_no + 1
                cycle
            end if
            select case (line(1:1))
            case ('+')
                files(nf)%additions = files(nf)%additions + 1
                call add(DL_ADDED, 0, new_no, line(2:))
                new_no = new_no + 1
            case ('-')
                files(nf)%deletions = files(nf)%deletions + 1
                call add(DL_REMOVED, old_no, 0, line(2:))
                old_no = old_no + 1
            case ('\')
                call add(DL_NOTE, 0, 0, line(2:))
            case default
                call add(DL_CONTEXT, old_no, new_no, line(2:))
                old_no = old_no + 1
                new_no = new_no + 1
            end select
        end do

    contains

        subroutine add(kind, o, n, text)
            integer, intent(in) :: kind, o, n
            character(len=*), intent(in) :: text
            type(diff_line_t), allocatable :: grown(:)

            if (kept >= max_lines) return
            kept = kept + 1
            associate (f => files(nf))
                if (f%count >= size(f%lines)) then
                    allocate (grown(2 * size(f%lines)))
                    grown(1:f%count) = f%lines(1:f%count)
                    call move_alloc(grown, f%lines)
                end if
                f%count = f%count + 1
                f%lines(f%count)%kind = kind
                f%lines(f%count)%old_no = o
                f%lines(f%count)%new_no = n
                f%lines(f%count)%text = text
            end associate
        end subroutine add

    end subroutine parse_patch

    subroutine hunk_start(line, old_no, new_no)
        !! "@@ -a,b +c,d @@ ...": the first old and new line numbers.
        character(len=*), intent(in) :: line
        integer, intent(out) :: old_no, new_no
        integer :: m, p

        old_no = 0
        new_no = 0
        m = index(line, ' -')
        p = index(line, ' +')
        if (m > 0) old_no = leading_int(line(m + 2:))
        if (p > 0) new_no = leading_int(line(p + 2:))
    end subroutine hunk_start

    pure integer function leading_int(s) result(v)
        character(len=*), intent(in) :: s
        integer :: i
        v = 0
        do i = 1, len(s)
            if (s(i:i) < '0' .or. s(i:i) > '9') exit
            v = 10 * v + iachar(s(i:i)) - 48
        end do
    end function leading_int

    function git_path(line) result(path)
        !! The new path of "diff --git a/X b/Y".
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: path
        integer :: b
        b = index(line, ' b/', back=.true.)
        if (b > 0) then
            path = line(b + 3:)
        else
            path = line(12:)
        end if
    end function git_path

    pure logical function starts(s, prefix)
        character(len=*), intent(in) :: s, prefix
        starts = len(s) >= len(prefix)
        if (starts) starts = s(1:len(prefix)) == prefix
    end function starts

end module fy_diff
