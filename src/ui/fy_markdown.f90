module fy_markdown
    !! Markdown (READMEs) as fortui blocks: headings, paragraphs, lists, quotes,
    !! fenced code with highlighting, rules; inline code, strong, emphasis and
    !! links as runs. Parsed once, composed every frame.
    use fortui
    use fy_highlight, only: &
        highlight, &
        lang_of
    implicit none
    private

    public :: &
        md_compose, &
        md_doc_t, &
        md_parse

    integer, parameter :: B_PARA = 1, B_HEAD = 2, B_ITEM = 3, B_QUOTE = 4, B_CODE = 5, &
        B_RULE = 6, B_CELL = 7, B_HEAD_CELL = 8

    type :: md_block_t
        integer :: kind = B_PARA
        integer :: level = 0
        character(len=:), allocatable :: text
        integer, allocatable :: ends(:), tones(:)
    end type md_block_t

    type :: md_doc_t
        type(md_block_t), allocatable :: blocks(:)
    end type md_doc_t

contains

    subroutine md_parse(source, doc)
        character(len=*), intent(in) :: source
        type(md_doc_t), intent(out) :: doc
        character(len=:), allocatable :: line, para, fence_lang, item
        integer :: pos, nl, t, level, state, lang, item_level
        logical :: in_fence, have_item

        allocate (doc%blocks(0))
        para = ''
        in_fence = .false.
        have_item = .false.
        item = ""
        item_level = 0
        state = 0
        lang = 0
        pos = 1
        do while (pos <= len(source))
            nl = index(source(pos:), new_line('a'))
            if (nl == 0) then
                line = source(pos:)
                pos = len(source) + 1
            else
                line = source(pos:pos + nl - 2)
                pos = pos + nl
            end if
            if (len(line) > 0) then
                if (line(len(line):) == achar(13)) line = line(1:len(line) - 1)
            end if
            t = verify(line, ' ')
            if (in_fence) then
                if (t > 0) then
                    if (line(t:min(t + 2, len(line))) == '```') then
                        in_fence = .false.
                        cycle
                    end if
                end if
                call code_block(line, lang, state)
                cycle
            end if
            if (t == 0) then
                call flush_para()
                cycle
            end if
            if (line(t:min(t + 2, len(line))) == '```') then
                call flush_para()
                in_fence = .true.
                fence_lang = trim(line(t + 3:))
                lang = lang_of('x.'//fence_lang)
                if (fence_lang == 'fortran') lang = lang_of('x.f90')
                if (fence_lang == 'sh' .or. fence_lang == 'bash' .or. fence_lang == 'console') &
                    lang = lang_of('x.sh')
                state = 0
                cycle
            end if
            if (line(t:t) == '#') then
                call flush_para()
                level = verify(line(t:), '#') - 1
                if (level > 0 .and. level <= 6) then
                    call inline_block(B_HEAD, level, trim(adjustl(line(t + level:))))
                    cycle
                end if
            end if
            if (line(t:) == '---' .or. line(t:) == '***' .or. line(t:) == '___') then
                call flush_para()
                doc%blocks = [doc%blocks, md_block_t(B_RULE, 0, '', [integer ::], [integer ::])]
                cycle
            end if
            if (line(t:t) == '|') then
                call flush_para()
                call table_row(trim(line(t:)))
                cycle
            end if
            if (len(line) >= t + 1) then
                if ((line(t:t) == '-' .or. line(t:t) == '*' .or. line(t:t) == '+') .and. &
                    line(t + 1:t + 1) == ' ') then
                    call flush_para()
                    call start_item(t / 2, trim(line(t + 2:)))
                    cycle
                end if
            end if
            level = verify(line(t:), '0123456789')
            if (level > 1 .and. t + level <= len(line)) then
                if (line(t + level - 1:t + level) == '. ') then
                    call flush_para()
                    call start_item(t / 2, line(t:t + level - 1)//' '// &
                        trim(line(t + level + 1:)))
                    cycle
                end if
            end if
            if (line(t:t) == '>') then
                call flush_para()
                call inline_block(B_QUOTE, 0, trim(adjustl(line(t + 1:))))
                cycle
            end if
            ! A line after a list item continues it, as on GitHub.
            if (have_item) then
                item = item//' '//trim(line(t:))
                cycle
            end if
            if (len(para) > 0) para = para//' '
            para = para//trim(line(t:))
        end do
        call flush_para()

    contains

        subroutine flush_para()
            if (have_item) call inline_block(B_ITEM, item_level, item)
            have_item = .false.
            if (len(para) == 0) return
            call inline_block(B_PARA, 0, para)
            para = ''
        end subroutine flush_para

        subroutine table_row(row)
            !! One row of a table as its cells; the first row is the header and
            !! the delimiter row (|---|:--:|) is dropped.
            character(len=*), intent(in) :: row
            character(len=:), allocatable :: rest
            integer :: col, bar, kind

            if (verify(row, '|-: ') == 0) return
            kind = B_CELL
            if (size(doc%blocks) == 0) then
                kind = B_HEAD_CELL
            else if (doc%blocks(size(doc%blocks))%kind /= B_CELL .and. &
                    doc%blocks(size(doc%blocks))%kind /= B_HEAD_CELL) then
                kind = B_HEAD_CELL
            end if
            rest = row(2:)
            if (len(rest) > 0) then
                if (rest(len(rest):) == '|') rest = rest(1:len(rest) - 1)
            end if
            col = 0
            do
                col = col + 1
                bar = index(rest, '|')
                if (bar == 0) then
                    call inline_block(kind, col, trim(adjustl(rest)))
                    exit
                end if
                call inline_block(kind, col, trim(adjustl(rest(1:bar - 1))))
                rest = rest(bar + 1:)
            end do
        end subroutine table_row

        subroutine start_item(level, text)
            integer, intent(in) :: level
            character(len=*), intent(in) :: text
            have_item = .true.
            item_level = level
            item = text
        end subroutine start_item

        subroutine inline_block(kind, level, text)
            integer, intent(in) :: kind, level
            character(len=*), intent(in) :: text
            type(md_block_t) :: b
            b%kind = kind
            b%level = level
            call inlines(text, b%text, b%ends, b%tones)
            doc%blocks = [doc%blocks, b]
        end subroutine inline_block

        subroutine code_block(text, lang, state)
            character(len=*), intent(in) :: text
            integer, intent(in) :: lang
            integer, intent(inout) :: state
            type(md_block_t) :: b
            b%kind = B_CODE
            b%text = text
            call highlight(text, lang, state, b%ends, b%tones)
            doc%blocks = [doc%blocks, b]
        end subroutine code_block

    end subroutine md_parse

    subroutine inlines(src, text, ends, tones)
        !! Inline markup removed from `src`, with runs: `code`, **strong**,
        !! *emphasis* or _emphasis_, [link](url) and ![image](url) as its text.
        character(len=*), intent(in) :: src
        character(len=:), allocatable, intent(out) :: text
        integer, allocatable, intent(out) :: ends(:), tones(:)
        integer :: i, k, close_
        logical :: img

        text = ''
        allocate (ends(0), tones(0))
        i = 1
        do while (i <= len(src))
            select case (src(i:i))
            case ('`')
                k = index(src(i + 1:), '`')
                if (k > 0) then
                    call run(src(i + 1:i + k - 1), UI_TONE_STRING)
                    i = i + k + 1
                    cycle
                end if
            case ('*', '_')
                if (src(i:min(i + 1, len(src))) == '**' .or. src(i:min(i + 1, len(src))) == '__') then
                    k = index(src(i + 2:), src(i:i + 1))
                    if (k > 1) then
                        call run(src(i + 2:i + k), UI_TONE_STRONG)
                        i = i + k + 3
                        cycle
                    end if
                else if (i < len(src)) then
                    if (src(i + 1:i + 1) /= ' ') then
                        k = index(src(i + 1:), src(i:i))
                        if (k > 1) then
                            call run(src(i + 1:i + k - 1), UI_TONE_EMPHASIS)
                            i = i + k + 1
                            cycle
                        end if
                    end if
                end if
            case ('[', '!')
                img = src(i:i) == '!'
                if (img .and. i < len(src)) then
                    if (src(i + 1:i + 1) /= '[') img = .false.
                end if
                if (src(i:i) == '[' .or. img) then
                    close_ = index(src(i:), '](')
                    if (close_ > 0) then
                        k = index(src(i + close_ + 1:), ')')
                        if (k > 0) then
                            if (img) then
                                call run(src(i + 2:i + close_ - 2), UI_TONE_MUTED)
                            else
                                call run(unticked(src(i + 1:i + close_ - 2)), UI_TONE_LINK)
                            end if
                            i = i + close_ + k + 1
                            cycle
                        end if
                    end if
                end if
            end select
            call run(src(i:i), 0)
            i = i + 1
        end do

    contains

        function unticked(s) result(t)
            !! Link text without the backticks of code inside it.
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: t
            integer :: j
            t = ''
            do j = 1, len(s)
                if (s(j:j) /= '`') t = t//s(j:j)
            end do
        end function unticked

        subroutine run(s, tone)
            character(len=*), intent(in) :: s
            integer, intent(in) :: tone
            if (len(s) == 0) return
            text = text//s
            if (size(tones) > 0) then
                if (tones(size(tones)) == tone) then
                    ends(size(ends)) = len(text)
                    return
                end if
            end if
            ends = [ends, len(text)]
            tones = [tones, tone]
        end subroutine run

    end subroutine inlines

    subroutine md_compose(doc)
        !! The document in the current container; consecutive code lines form
        !! one code block.
        type(md_doc_t), intent(in) :: doc
        integer :: k
        real, parameter :: HEAD_SIZE(6) = [32.0, 24.0, 20.0, 16.0, 14.0, 13.6], BODY = 16.0

        k = 1
        do while (k <= size(doc%blocks))
            associate (b => doc%blocks(k))
                select case (b%kind)
                case (B_HEAD)
                    call text(b%text, style=UI_HEADING, size=HEAD_SIZE(b%level), &
                        id="h"//ui_str(b%level)//"-"//ui_str(k), width=UI_FILL, ends=b%ends, tones=b%tones)
                case (B_ITEM)
                    call row_begin(6.0, id="li"//ui_str(k))
                    call text(repeat("  ", b%level)//"•", style=UI_BODY, size=BODY, &
                        tone=UI_TONE_MUTED)
                    call text(b%text, size=BODY, width=UI_FILL, ends=b%ends, tones=b%tones)
                    call row_end()
                case (B_QUOTE)
                    call text(b%text, size=BODY, width=UI_FILL, tone=UI_TONE_MUTED, &
                        ends=b%ends, tones=b%tones)
                case (B_CELL, B_HEAD_CELL)
                    call table(doc, k)
                    cycle
                case (B_RULE)
                    call text("", width=UI_FILL, id="hr"//ui_str(k))
                case (B_CODE)
                    call column_begin(0.0, id="mdcode"//ui_str(k))
                    do while (k <= size(doc%blocks))
                        if (doc%blocks(k)%kind /= B_CODE) exit
                        call text(doc%blocks(k)%text, style=UI_CODE, size=13.5, width=UI_FILL, &
                            ends=doc%blocks(k)%ends, tones=doc%blocks(k)%tones)
                        k = k + 1
                    end do
                    call column_end()
                    cycle
                case default
                    call text(b%text, size=BODY, width=UI_FILL, ends=b%ends, tones=b%tones)
                end select
            end associate
            k = k + 1
        end do
    end subroutine md_compose

    subroutine table(doc, k)
        !! The table whose first cell is block k; k ends after its last cell.
        type(md_doc_t), intent(in) :: doc
        integer, intent(inout) :: k
        integer :: row

        call column_begin(0.0, id="mdtable"//ui_str(k))
        row = 0
        do while (k <= size(doc%blocks))
            associate (b => doc%blocks(k))
                if (b%kind /= B_CELL .and. b%kind /= B_HEAD_CELL) exit
                if (b%level == 1) then
                    if (row > 0) call row_end()
                    row = row + 1
                    call row_begin(0.0, id="tr"//ui_str(row))
                end if
                if (b%kind == B_HEAD_CELL) then
                    call text(b%text, style=UI_HEADING, size=14.0, width=UI_FILL, &
                        ends=b%ends, tones=b%tones)
                else
                    call text(b%text, size=14.0, width=UI_FILL, ends=b%ends, tones=b%tones)
                end if
            end associate
            k = k + 1
        end do
        if (row > 0) call row_end()
        call column_end()
    end subroutine table

end module fy_markdown
