program test_highlight
    use fortui, only: UI_TONE_KEYWORD, UI_TONE_STRING, UI_TONE_COMMENT, UI_TONE_NUMBER, &
        UI_TONE_TYPE, UI_TONE_PREPROC, UI_TONE_STRONG
    use fy_highlight, only: highlight, lang_of, LANG_FORTRAN, LANG_C, LANG_PYTHON, &
        LANG_TOML, LANG_SHELL, LANG_NONE, LANG_CMAKE
    implicit none

    integer :: state

    call check(lang_of('src/a.F90') == LANG_FORTRAN .and. lang_of('x/CMakeLists.txt') == &
        LANG_CMAKE .and. lang_of('fpm.toml') == LANG_TOML .and. lang_of('README') == LANG_NONE, &
        'languages from file names')

    state = 0
    call expect('    real(dp) :: x = 1.5d-3_dp  ! it''s', LANG_FORTRAN, &
        [character(len=12) :: 'real', 'dp', '1.5d-3_dp', '! it''s'], &
        [UI_TONE_TYPE, 0, UI_TONE_NUMBER, UI_TONE_COMMENT])
    call expect('if (a .and. b == .true.) call f(''don''''t'')', LANG_FORTRAN, &
        [character(len=12) :: 'if', '.and.', '.true.', 'call', '''don''''t'''], &
        [UI_TONE_KEYWORD, UI_TONE_KEYWORD, UI_TONE_NUMBER, UI_TONE_KEYWORD, UI_TONE_STRING])
    call expect('END SUBROUTINE solve', LANG_FORTRAN, [character(len=12) :: 'END', &
        'SUBROUTINE', 'solve'], [UI_TONE_KEYWORD, UI_TONE_KEYWORD, 0])
    call expect('#ifdef USE_MPI', LANG_FORTRAN, [character(len=14) :: '#ifdef USE_MPI'], &
        [UI_TONE_PREPROC])

    call expect('int n = 3; /* start', LANG_C, [character(len=12) :: 'int', '3', &
        '/* start'], [UI_TONE_TYPE, UI_TONE_NUMBER, UI_TONE_COMMENT])
    call check(state == 1, 'a C block comment continues on the next line')
    call expect('still */ return 0;', LANG_C, [character(len=12) :: 'still */', 'return', &
        '0'], [UI_TONE_COMMENT, UI_TONE_KEYWORD, UI_TONE_NUMBER])
    call check(state == 0, 'and ends there')

    call expect('x = """doc', LANG_PYTHON, [character(len=8) :: '"""doc'], [UI_TONE_STRING])
    call expect('more""" if', LANG_PYTHON, [character(len=8) :: 'more"""', 'if'], &
        [UI_TONE_STRING, UI_TONE_KEYWORD])
    call expect('[dependencies]', LANG_TOML, [character(len=14) :: '[dependencies]'], &
        [UI_TONE_STRONG])
    call expect('fx = { git = "u" }  # pin', LANG_TOML, [character(len=8) :: 'fx', '"u"', &
        '# pin'], [UI_TONE_KEYWORD, UI_TONE_STRING, UI_TONE_COMMENT])
    call expect('for f in $FILES; do echo "${f}"; done', LANG_SHELL, [character(len=8) :: &
        'for', 'in', '$FILES', 'do', 'echo', '"${f}"', 'done'], [UI_TONE_KEYWORD, &
        UI_TONE_KEYWORD, UI_TONE_TYPE, UI_TONE_KEYWORD, UI_TONE_KEYWORD, UI_TONE_STRING, &
        UI_TONE_KEYWORD])

contains

    subroutine expect(line, lang, tokens, want)
        !! Each token appears in the line (first occurrence) entirely in its tone,
        !! and the runs cover the whole line.
        character(len=*), intent(in) :: line, tokens(:)
        integer, intent(in) :: lang, want(:)
        integer, allocatable :: ends(:), tones(:), byte_tone(:)
        integer :: k, at, b, r

        call highlight(line, lang, state, ends, tones)
        allocate (byte_tone(len(line)))
        at = 1
        do r = 1, size(ends)
            byte_tone(at:ends(r)) = tones(r)
            at = ends(r) + 1
        end do
        call check(size(ends) > 0, 'runs for: '//line)
        call check(ends(size(ends)) == len(line), 'runs cover: '//line)
        at = 1
        do k = 1, size(tokens)
            b = index(line(at:), trim(tokens(k))) + at - 1
            call check(b >= at, 'token found: '//trim(tokens(k)))
            call check(all(byte_tone(b:b + len_trim(tokens(k)) - 1) == want(k)), &
                trim(tokens(k))//' in its tone in: '//line)
            at = b + len_trim(tokens(k))
        end do

    end subroutine expect

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_highlight
