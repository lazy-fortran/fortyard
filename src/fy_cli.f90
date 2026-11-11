module fy_cli
    !! Text output of the command line client: API answers as aligned tables
    !! and single fields, for people rather than for scripts.
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
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    implicit none
    private

    public :: &
        cli_field, &
        cli_table, &
        cli_tail

    type :: cell_t
        character(len=:), allocatable :: s
    end type cell_t

contains

    function cli_table(body, list, fields) result(text)
        !! The objects of the top-level array `list` of `body`, one line each,
        !! with the values of `fields` in aligned columns under a header.
        character(len=*), intent(in) :: body, list
        character(len=*), intent(in) :: fields(:)
        character(len=:), allocatable :: text
        type(cell_t), allocatable :: cells(:, :), grown(:, :)
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key, line
        integer, allocatable :: widths(:)
        integer :: depth, rows, k, j
        logical :: in_list

        allocate (cells(size(fields), 0))
        rows = 0
        depth = 0
        in_list = .false.
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                if (ev%event_type == JSON_ARRAY_START .and. depth == 1 .and. key == list) &
                    in_list = .true.
                depth = depth + 1
                if (in_list .and. depth == 3 .and. ev%event_type == JSON_OBJECT_START) then
                    allocate (grown(size(fields), rows + 1))
                    grown(:, 1:rows) = cells
                    do k = 1, size(fields)
                        grown(k, rows + 1)%s = ''
                    end do
                    call move_alloc(grown, cells)
                    rows = rows + 1
                end if
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
                if (depth == 1) in_list = .false.
            case (JSON_KEY)
                key = ev%string_val
            case default
                if (in_list .and. depth == 3) then
                    k = findloc([(trim(fields(j)) == key, j=1, size(fields))], .true., 1)
                    if (k > 0) cells(k, rows)%s = scalar(ev)
                end if
            end select
        end do

        text = ''
        if (rows == 0) return
        allocate (widths(size(fields)))
        do k = 1, size(fields)
            widths(k) = len_trim(fields(k))
            do j = 1, rows
                widths(k) = max(widths(k), len(cells(k, j)%s))
            end do
        end do
        ! The last column is not padded.
        widths(size(fields)) = 0
        do k = 1, size(fields)
            text = text//pad(upper(trim(fields(k))), widths(k))
        end do
        text = trim(text)//new_line('a')
        do j = 1, rows
            line = ''
            do k = 1, size(fields)
                line = line//pad(cells(k, j)%s, widths(k))
            end do
            text = text//trim(line)//new_line('a')
        end do
    end function cli_table

    function cli_field(body, name) result(value)
        !! The scalar value of the top-level member `name` of `body`, or ''.
        character(len=*), intent(in) :: body, name
        character(len=:), allocatable :: value
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key
        integer :: depth

        value = ''
        key = ''
        depth = 0
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                depth = depth + 1
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
            case (JSON_KEY)
                key = ev%string_val
            case default
                if (depth == 1 .and. key == name) then
                    value = scalar(ev)
                    return
                end if
            end select
        end do
    end function cli_field

    function cli_tail(text, lines) result(tail)
        !! The last `lines` lines of `text`.
        character(len=*), intent(in) :: text
        integer, intent(in) :: lines
        character(len=:), allocatable :: tail
        integer :: k, seen, n

        n = len(text)
        do while (n > 0)
            if (text(n:n) /= new_line('a')) exit
            n = n - 1
        end do
        seen = 0
        do k = n, 1, -1
            if (text(k:k) == new_line('a')) then
                seen = seen + 1
                if (seen == lines) then
                    tail = text(k + 1:n)
                    return
                end if
            end if
        end do
        tail = text(1:n)
    end function cli_tail

    function scalar(ev) result(s)
        type(json_event_t), intent(in) :: ev
        character(len=:), allocatable :: s
        character(len=32) :: b

        select case (ev%event_type)
        case (JSON_STRING)
            s = ev%string_val
        case (JSON_INTEGER)
            write (b, '(i0)') ev%int64_val
            s = trim(b)
        case (JSON_REAL)
            write (b, '(g0)') ev%real_val
            s = trim(b)
        case (JSON_BOOL)
            s = merge('true ', 'false', ev%bool_val)
            s = trim(s)
        case (JSON_NULL_VAL)
            s = ''
        case default
            s = ''
        end select
    end function scalar

    function pad(s, width) result(out)
        character(len=*), intent(in) :: s
        integer, intent(in) :: width
        character(len=:), allocatable :: out
        if (width == 0) then
            out = s
        else
            out = s//repeat(' ', width - len(s) + 2)
        end if
    end function pad

    function upper(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: k
        out = s
        do k = 1, len(s)
            if (s(k:k) >= 'a' .and. s(k:k) <= 'z') out(k:k) = achar(iachar(s(k:k)) - 32)
        end do
    end function upper

end module fy_cli
