module fy_highlight
    !! Syntax highlighting of source lines as runs for fortui rich text: the
    !! byte where each run ends and its tone. One line at a time; `state`
    !! carries constructs that span lines (C block comments, Python
    !! triple-quoted strings) from one line to the next.
    use fortui, only: UI_TONE_KEYWORD, UI_TONE_STRING, UI_TONE_COMMENT, UI_TONE_NUMBER, &
        UI_TONE_TYPE, UI_TONE_PREPROC, UI_TONE_STRONG
    implicit none
    private

    public :: lang_of, highlight
    integer, parameter, public :: LANG_NONE = 0, LANG_FORTRAN = 1, LANG_C = 2, &
        LANG_SHELL = 3, LANG_PYTHON = 4, LANG_TOML = 5, LANG_YAML = 6, LANG_CMAKE = 7, &
        LANG_MARKDOWN = 8, LANG_JS = 9
    integer, parameter :: IN_CODE = 0, IN_BLOCK_COMMENT = 1, IN_TRIPLE = 2

    character(len=*), parameter :: F_KEYWORDS = ' program module submodule subroutine &
        &function end contains use only implicit none intent in out inout parameter &
        &allocatable pointer target dimension save public private protected interface &
        &procedure abstract extends call return if then else elseif endif do enddo while &
        &select case default where elsewhere forall associate block exit cycle stop error &
        &go to goto allocate deallocate nullify result recursive pure elemental impure &
        &bind value optional import include print write read open close inquire format &
        &contiguous volatile sequence enum enumerator concurrent critical sync all images &
        &deferred non_overridable nopass pass generic final operator assignment is rank &
        &endsubroutine endfunction endmodule endprogram endtype endinterface endselect &
        &endassociate endblock endwhere '
    character(len=*), parameter :: F_TYPES = ' integer real character logical complex &
        &double precision type class '
    character(len=*), parameter :: C_KEYWORDS = ' if else for while do switch case default &
        &break continue return goto struct union enum typedef static extern const volatile &
        &inline sizeof namespace template typename class public private protected virtual &
        &override new delete this using try catch throw nullptr true false constexpr auto &
        &operator friend explicit noexcept static_assert decltype restrict register '
    character(len=*), parameter :: C_TYPES = ' void int char float double long short &
        &unsigned signed bool size_t ptrdiff_t int8_t int16_t int32_t int64_t uint8_t &
        &uint16_t uint32_t uint64_t std string vector '
    character(len=*), parameter :: JS_KEYWORDS = ' function var let const return if else &
        &for while do new class import export from async await this null true false &
        &undefined typeof instanceof in of switch case default break continue try catch &
        &finally throw yield extends super static get set delete void '
    character(len=*), parameter :: PY_KEYWORDS = ' def class return if elif else for while &
        &in import from as with try except finally raise pass break continue lambda yield &
        &None True False and or not is global nonlocal assert del async await '
    character(len=*), parameter :: SH_KEYWORDS = ' if then else elif fi for in do done &
        &while until case esac function '
    !! Builtins count only as commands, not as arguments (fo test).
    character(len=*), parameter :: SH_BUILTINS = ' return local export set unset echo exit &
        &shift read cd test trap source eval exec '
    character(len=*), parameter :: LITERALS = ' true false yes no on off null '

contains

    integer function lang_of(path) result(lang)
        !! The language of a file, from its name.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: name, ext
        integer :: dot

        name = lower(path(index(path, '/', back=.true.) + 1:))
        dot = index(name, '.', back=.true.)
        ext = ''
        if (dot > 0) ext = name(dot + 1:)
        select case (ext)
        case ('f90', 'f95', 'f03', 'f08', 'f18', 'f', 'for', 'fpp', 'inc', 'fi')
            lang = LANG_FORTRAN
        case ('c', 'h', 'cc', 'cpp', 'cxx', 'hpp', 'hh', 'cu', 'cuh', 'm')
            lang = LANG_C
        case ('sh', 'bash', 'zsh')
            lang = LANG_SHELL
        case ('py')
            lang = LANG_PYTHON
        case ('toml', 'ini', 'cfg')
            lang = LANG_TOML
        case ('yml', 'yaml')
            lang = LANG_YAML
        case ('cmake')
            lang = LANG_CMAKE
        case ('md', 'markdown')
            lang = LANG_MARKDOWN
        case ('js', 'mjs', 'ts', 'json')
            lang = LANG_JS
        case default
            lang = LANG_NONE
            if (name == 'cmakelists.txt') lang = LANG_CMAKE
            if (name == 'makefile' .or. name == 'dockerfile') lang = LANG_SHELL
        end select
    end function lang_of

    subroutine highlight(line, lang, state, ends, tones)
        !! Runs of `line` (bytes) in language `lang`.
        character(len=*), intent(in) :: line
        integer, intent(in) :: lang
        integer, intent(inout) :: state
        integer, allocatable, intent(out) :: ends(:), tones(:)
        integer, allocatable :: tone(:)
        integer :: n, i, j, k

        n = len(line)
        allocate (tone(max(n, 1)))
        tone = 0
        select case (lang)
        case (LANG_FORTRAN)
            call fortran_line()
        case (LANG_C, LANG_JS)
            call c_line()
        case (LANG_SHELL, LANG_PYTHON, LANG_TOML, LANG_YAML, LANG_CMAKE)
            call hash_line()
        case (LANG_MARKDOWN)
            call markdown_line()
        end select
        ! Merge equal neighbours into runs.
        allocate (ends(0), tones(0))
        i = 1
        do while (i <= n)
            j = i
            do while (j < n)
                if (tone(j + 1) /= tone(i)) exit
                j = j + 1
            end do
            ends = [ends, j]
            tones = [tones, tone(i)]
            i = j + 1
        end do

    contains

        subroutine fortran_line()
            character :: q
            i = verify(line, ' ')
            if (i > 0) then
                if (line(i:i) == '#') then
                    tone = UI_TONE_PREPROC
                    return
                end if
            end if
            i = 1
            do while (i <= n)
                select case (line(i:i))
                case ('!')
                    tone(i:n) = UI_TONE_COMMENT
                    return
                case ("'", '"')
                    q = line(i:i)
                    j = i + 1
                    do while (j <= n)
                        if (line(j:j) == q) then
                            if (j < n) then
                                if (line(j + 1:j + 1) == q) then
                                    j = j + 2
                                    cycle
                                end if
                            end if
                            exit
                        end if
                        j = j + 1
                    end do
                    tone(i:min(j, n)) = UI_TONE_STRING
                    i = j + 1
                case ('.')
                    ! .true., .and. and the other dotted operators.
                    k = index(line(i + 1:), '.')
                    if (k > 1 .and. k <= 6) then
                        if (verify(lower(line(i + 1:i + k - 1)), &
                            'abcdefghijklmnopqrstuvwxyz') == 0) then
                            if (index(' true false ', ' '//lower(line(i + 1:i + k - 1))// &
                                ' ') > 0) then
                                tone(i:i + k) = UI_TONE_NUMBER
                            else
                                tone(i:i + k) = UI_TONE_KEYWORD
                            end if
                            i = i + k + 1
                            cycle
                        end if
                    end if
                    call number_at(i)
                case default
                    if (is_digit(line(i:i))) then
                        call number_at(i)
                    else if (is_word_start(line(i:i))) then
                        j = word_end(i)
                        call classify(lower(line(i:j)), F_KEYWORDS, F_TYPES, i, j)
                        i = j + 1
                    else
                        i = i + 1
                    end if
                end select
            end do
        end subroutine fortran_line

        subroutine c_line()
            character :: q
            i = verify(line, ' ')
            if (i > 0 .and. state == IN_CODE .and. lang == LANG_C) then
                if (line(i:i) == '#') then
                    tone = UI_TONE_PREPROC
                    return
                end if
            end if
            i = 1
            if (state == IN_BLOCK_COMMENT) then
                k = index(line, '*/')
                if (k == 0) then
                    tone = UI_TONE_COMMENT
                    return
                end if
                tone(1:k + 1) = UI_TONE_COMMENT
                state = IN_CODE
                i = k + 2
            end if
            do while (i <= n)
                if (line(i:min(i + 1, n)) == '//') then
                    tone(i:n) = UI_TONE_COMMENT
                    return
                else if (line(i:min(i + 1, n)) == '/*') then
                    k = index(line(i + 2:), '*/')
                    if (k == 0) then
                        tone(i:n) = UI_TONE_COMMENT
                        state = IN_BLOCK_COMMENT
                        return
                    end if
                    tone(i:i + k + 2) = UI_TONE_COMMENT
                    i = i + k + 3
                else if (line(i:i) == '"' .or. line(i:i) == "'" .or. line(i:i) == '`') then
                    q = line(i:i)
                    j = i + 1
                    do while (j <= n)
                        if (line(j:j) == '\') then
                            j = j + 2
                            cycle
                        end if
                        if (line(j:j) == q) exit
                        j = j + 1
                    end do
                    tone(i:min(j, n)) = UI_TONE_STRING
                    i = j + 1
                else if (is_digit(line(i:i))) then
                    call number_at(i)
                else if (is_word_start(line(i:i))) then
                    j = word_end(i)
                    if (lang == LANG_JS) then
                        call classify(line(i:j), JS_KEYWORDS, ' ', i, j)
                    else
                        call classify(line(i:j), C_KEYWORDS, C_TYPES, i, j)
                    end if
                    i = j + 1
                else
                    i = i + 1
                end if
            end do
        end subroutine c_line

        subroutine hash_line()
            !! Languages with # comments: shell, Python, TOML, YAML, CMake.
            character :: q
            integer :: first
            logical :: key_done

            i = 1
            if (state == IN_TRIPLE) then
                k = index(line, '"""')
                if (k == 0) then
                    tone = UI_TONE_STRING
                    return
                end if
                tone(1:k + 2) = UI_TONE_STRING
                state = IN_CODE
                i = k + 3
            end if
            first = verify(line, ' ')
            key_done = .false.
            if (lang == LANG_TOML .and. first > 0) then
                if (line(first:first) == '[') then
                    tone(first:n) = UI_TONE_STRONG
                    return
                end if
            end if
            do while (i <= n)
                select case (line(i:i))
                case ('#')
                    tone(i:n) = UI_TONE_COMMENT
                    return
                case ('"', "'")
                    q = line(i:i)
                    if (lang == LANG_PYTHON .and. line(i:min(i + 2, n)) == '"""') then
                        k = index(line(i + 3:), '"""')
                        if (k == 0) then
                            tone(i:n) = UI_TONE_STRING
                            state = IN_TRIPLE
                            return
                        end if
                        tone(i:i + k + 4) = UI_TONE_STRING
                        i = i + k + 5
                        cycle
                    end if
                    j = i + 1
                    do while (j <= n)
                        if (line(j:j) == '\' .and. q == '"') then
                            j = j + 2
                            cycle
                        end if
                        if (line(j:j) == q) exit
                        j = j + 1
                    end do
                    tone(i:min(j, n)) = UI_TONE_STRING
                    i = j + 1
                case ('$')
                    if (lang == LANG_SHELL .or. lang == LANG_CMAKE) then
                        j = i + 1
                        if (j <= n) then
                            if (line(j:j) == '{') then
                                k = index(line(j:), '}')
                                if (k > 0) j = j + k - 1
                            else
                                do while (j < n)
                                    if (.not. is_word_char(line(j + 1:j + 1))) exit
                                    j = j + 1
                                end do
                            end if
                        end if
                        tone(i:min(j, n)) = UI_TONE_TYPE
                        i = j + 1
                    else
                        i = i + 1
                    end if
                case default
                    if (is_digit(line(i:i))) then
                        call number_at(i)
                    else if (is_word_start(line(i:i))) then
                        j = word_end(i)
                        if ((lang == LANG_TOML .or. lang == LANG_YAML) .and. .not. key_done) then
                            ! The key of "key = value" or "key: value".
                            k = verify(line(j + 1:), ' ')
                            if (k > 0) then
                                if (line(j + k:j + k) == '=' .or. line(j + k:j + k) == ':') &
                                    tone(i:j) = UI_TONE_KEYWORD
                            end if
                            key_done = .true.
                            if (index(LITERALS, ' '//lower(line(i:j))//' ') > 0) &
                                tone(i:j) = UI_TONE_NUMBER
                        else if (lang == LANG_CMAKE) then
                            k = verify(line(j + 1:), ' ')
                            if (k > 0) then
                                if (line(j + k:j + k) == '(') tone(i:j) = UI_TONE_KEYWORD
                            end if
                        else if (lang == LANG_PYTHON) then
                            call classify(line(i:j), PY_KEYWORDS, ' ', i, j)
                        else if (lang == LANG_SHELL) then
                            call classify(line(i:j), SH_KEYWORDS, ' ', i, j)
                            if (command_at(i)) call classify(line(i:j), SH_BUILTINS, ' ', i, j)
                        else if (index(LITERALS, ' '//lower(line(i:j))//' ') > 0) then
                            tone(i:j) = UI_TONE_NUMBER
                        end if
                        i = j + 1
                    else
                        i = i + 1
                    end if
                end select
            end do
        end subroutine hash_line

        subroutine markdown_line()
            i = verify(line, ' ')
            if (i == 0) return
            if (line(i:i) == '#') then
                tone = UI_TONE_STRONG
                return
            end if
            if (line(i:min(i + 2, n)) == '```') then
                tone = UI_TONE_STRING
                return
            end if
            if (line(i:i) == '-' .or. line(i:i) == '*' .or. line(i:i) == '>') &
                tone(i:i) = UI_TONE_KEYWORD
            i = 1
            do while (i <= n)
                if (line(i:i) == '`') then
                    k = index(line(i + 1:), '`')
                    if (k == 0) exit
                    tone(i:i + k) = UI_TONE_STRING
                    i = i + k + 1
                else
                    i = i + 1
                end if
            end do
        end subroutine markdown_line

        logical function command_at(a)
            !! Whether a word at `a` is in command position.
            integer, intent(in) :: a
            integer :: p
            p = verify(line(1:a - 1), ' ', back=.true.)
            command_at = p == 0
            if (p > 0) command_at = index(';|&(', line(p:p)) > 0 .or. &
                line(max(1, p - 3):p) == 'then' .or. line(max(1, p - 1):p) == 'do'
        end function command_at

        subroutine classify(word, keywords, types, a, b)
            character(len=*), intent(in) :: word, keywords, types
            integer, intent(in) :: a, b
            if (index(keywords, ' '//word//' ') > 0) then
                tone(a:b) = UI_TONE_KEYWORD
            else if (index(types, ' '//word//' ') > 0) then
                tone(a:b) = UI_TONE_TYPE
            end if
        end subroutine classify

        subroutine number_at(start)
            !! A number with fraction, exponent and kind suffix (1.5d-3, 2_dp, 0x1F).
            integer, intent(inout) :: start
            integer :: e
            e = start
            do while (e < n)
                if (.not. (is_word_char(line(e + 1:e + 1)) .or. line(e + 1:e + 1) == '.' .or. &
                    ((line(e + 1:e + 1) == '-' .or. line(e + 1:e + 1) == '+') .and. &
                    index('eEdD', line(e:e)) > 0))) exit
                ! "1.and." is 1 followed by .and.
                if (line(e + 1:e + 1) == '.' .and. e + 2 <= n) then
                    if (is_word_start(line(e + 2:e + 2)) .and. index('eEdD', line(e + 2:e + 2)) &
                        == 0) exit
                end if
                e = e + 1
            end do
            tone(start:e) = UI_TONE_NUMBER
            start = e + 1
        end subroutine number_at

        integer function word_end(start)
            integer, intent(in) :: start
            word_end = start
            do while (word_end < n)
                if (.not. is_word_char(line(word_end + 1:word_end + 1))) exit
                word_end = word_end + 1
            end do
        end function word_end

    end subroutine highlight

    pure logical function is_digit(c)
        character, intent(in) :: c
        is_digit = c >= '0' .and. c <= '9'
    end function is_digit

    pure logical function is_word_start(c)
        character, intent(in) :: c
        is_word_start = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. c == '_'
    end function is_word_start

    pure logical function is_word_char(c)
        character, intent(in) :: c
        is_word_char = is_word_start(c) .or. is_digit(c)
    end function is_word_char

    pure function lower(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: i
        r = s
        do i = 1, len(r)
            if (r(i:i) >= 'A' .and. r(i:i) <= 'Z') r(i:i) = achar(iachar(r(i:i)) + 32)
        end do
    end function lower

end module fy_highlight
