module fy_code_lines
    !! Lines of code as the pages show them, each a highlighted run of text
    !! with muted line numbers: a file's lines and a diff's files, shared by
    !! the repository and change pages.
    use fortui
    use fy_highlight, only: highlight, lang_of
    use fy_diff, only: diff_file_t, parse_patch, DL_ADDED, DL_REMOVED, DL_HUNK, DL_NOTE
    implicit none
    private

    public :: line_t, shown_file_t, diff_load, diff_compose, numbered, plain, plain_lines, &
        gutter, right, untab, badge_tone, MAX_DIFF_LINES

    type :: line_t
        character(len=:), allocatable :: s
        integer, allocatable :: ends(:), tones(:)
        integer :: tone = 0
    end type line_t

    type :: shown_file_t
        !! A file of a commit's diff, ready to compose.
        character(len=:), allocatable :: path, status
        integer :: additions = 0, deletions = 0
        type(line_t), allocatable :: lines(:)
    end type shown_file_t

    integer, parameter :: MAX_DIFF_LINES = 6000
    real, parameter :: BODY = 14.0, SMALL = 12.0, CODE = 12.0
    real, parameter :: GAP = 8.0, ROW_H = 40.0

contains

    subroutine diff_load(patch, diff_files, diff_cut)
        !! Each file of a commit's patch with its lines: both line numbers as a
        !! muted run, the sign and the highlighted code.
        character(len=*), intent(in) :: patch
        type(shown_file_t), allocatable, intent(inout) :: diff_files(:)
        logical, intent(out) :: diff_cut
        type(diff_file_t), allocatable :: files(:)
        integer :: f, k, state, lang, kept

        call parse_patch(patch, files, MAX_DIFF_LINES)
        if (allocated(diff_files)) deallocate (diff_files)
        allocate (diff_files(size(files)))
        kept = 0
        do f = 1, size(files)
            diff_files(f)%path = files(f)%path
            if (files(f)%status == 'renamed') diff_files(f)%path = files(f)%old_path// &
                ' → '//files(f)%path
            diff_files(f)%status = files(f)%status
            diff_files(f)%additions = files(f)%additions
            diff_files(f)%deletions = files(f)%deletions
            allocate (diff_files(f)%lines(files(f)%count))
            lang = lang_of(files(f)%path)
            state = 0
            do k = 1, files(f)%count
                associate (l => files(f)%lines(k), d => diff_files(f)%lines(k))
                    select case (l%kind)
                    case (DL_HUNK)
                        call plain(d, l%text, UI_TONE_HUNK)
                        d%tone = UI_TONE_HUNK
                    case (DL_NOTE)
                        call plain(d, l%text, UI_TONE_MUTED)
                    case (DL_ADDED)
                        call numbered(d, gutter(0, l%new_no)//'+ ', untab(l%text), lang, state, &
                            UI_TONE_ADDED)
                    case (DL_REMOVED)
                        call numbered(d, gutter(l%old_no, 0)//'- ', untab(l%text), lang, state, &
                            UI_TONE_REMOVED)
                    case default
                        call numbered(d, gutter(l%old_no, l%new_no)//'  ', untab(l%text), lang, &
                            state, 0)
                    end select
                end associate
            end do
            kept = kept + files(f)%count
        end do
        diff_cut = kept >= MAX_DIFF_LINES
    end subroutine diff_load

    subroutine diff_compose(diff_files, diff_cut)
        !! The files of a diff: a summary line, then each file with its lines.
        type(shown_file_t), intent(in) :: diff_files(:)
        logical, intent(in) :: diff_cut
        integer :: f, k, adds, dels

        adds = 0
        dels = 0
        do f = 1, size(diff_files)
            adds = adds + diff_files(f)%additions
            dels = dels + diff_files(f)%deletions
        end do
        block
            call row_begin(GAP, id="diffstat", height=ROW_H)
            if (size(diff_files) == 1) then
                call text("1 file changed", size=BODY)
            else
                call text(ui_str(size(diff_files))//" files changed", size=BODY)
            end if
            call text("+"//ui_str(adds), size=BODY, tone=UI_TONE_POSITIVE)
            call text("−"//ui_str(dels), size=BODY, tone=UI_TONE_NEGATIVE)
            call row_end()
        end block

        do f = 1, size(diff_files)
            call column_begin(0.0, id="card-diff"//ui_str(f))
            call row_begin(GAP, id="dhead", height=ROW_H)
            if (diff_files(f)%status /= 'modified') call text(diff_files(f)%status, size=SMALL, &
                id="badge", tone=badge_tone(diff_files(f)%status))
            call text(diff_files(f)%path, style=UI_CODE, size=SMALL, width=UI_FILL, id="path")
            call text("+"//ui_str(diff_files(f)%additions), size=SMALL, tone=UI_TONE_POSITIVE)
            call text("−"//ui_str(diff_files(f)%deletions), size=SMALL, tone=UI_TONE_NEGATIVE)
            call row_end()
            call column_begin(0.0, id="dcode")
            do k = 1, size(diff_files(f)%lines)
                associate (l => diff_files(f)%lines(k))
                    call text(l%s, style=UI_CODE, size=CODE, width=UI_FILL, tone=l%tone, &
                        ends=l%ends, tones=l%tones)
                end associate
            end do
            call column_end()
            call column_end()
        end do
        if (diff_cut) call text("The diff is longer; only its first "//ui_str(MAX_DIFF_LINES)// &
            " lines are shown.", size=SMALL, tone=UI_TONE_MUTED)
    end subroutine diff_compose

    subroutine numbered(l, prefix, code, lang, state, tone)
        !! A line whose prefix (line numbers) is a muted run, then highlighted code.
        type(line_t), intent(inout) :: l
        character(len=*), intent(in) :: prefix, code
        integer, intent(in) :: lang, tone
        integer, intent(inout) :: state
        integer, allocatable :: ends(:), tones(:)

        call highlight(code, lang, state, ends, tones)
        l%s = prefix//code
        l%ends = [len(prefix), ends + len(prefix)]
        l%tones = [UI_TONE_MUTED, tones]
        l%tone = tone
    end subroutine numbered

    subroutine plain(l, s, tone)
        type(line_t), intent(inout) :: l
        character(len=*), intent(in) :: s
        integer, intent(in) :: tone
        l%s = s
        l%ends = [len(s)]
        l%tones = [tone]
    end subroutine plain

    subroutine plain_lines(lines, s)
        type(line_t), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: s
        if (allocated(lines)) deallocate (lines)
        allocate (lines(1))
        call plain(lines(1), s, UI_TONE_MUTED)
    end subroutine plain_lines

    function gutter(old_no, new_no) result(s)
        integer, intent(in) :: old_no, new_no
        character(len=:), allocatable :: s
        s = right(number(old_no), 5)//' '//right(number(new_no), 5)//'  '
    contains
        function number(k) result(t)
            integer, intent(in) :: k
            character(len=:), allocatable :: t
            t = ''
            if (k > 0) t = ui_str(k)
        end function number
    end function gutter

    function right(s, width) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: width
        character(len=:), allocatable :: r
        r = repeat(' ', max(0, width - len_trim(s)))//trim(s)
    end function right

    function untab(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: k
        t = ''
        do k = 1, len(s)
            if (s(k:k) == achar(9)) then
                t = t//repeat(' ', 4 - mod(len(t), 4))
            else if (s(k:k) /= achar(13)) then
                t = t//s(k:k)
            end if
        end do
    end function untab

    integer function badge_tone(status)
        character(len=*), intent(in) :: status
        select case (status)
        case ('added'); badge_tone = UI_TONE_POSITIVE
        case ('deleted'); badge_tone = UI_TONE_NEGATIVE
        case default; badge_tone = UI_TONE_MUTED
        end select
    end function badge_tone

end module fy_code_lines
