module fy_land
    !! Landing: a branch of a primary repository becomes one squashed commit on
    !! `main`, after the repository's check passed on exactly that commit; `main`
    !! then moves forward to it and is pushed to the repository's mirror.
    use fy_proc, only: proc_run, proc_find
    implicit none
    private

    public :: land_branch, land_hook_install

    integer, parameter :: LOG_BYTES = 65536

contains

    subroutine land_hook_install(repo, stat)
        !! A pre-receive hook in the bare repository `repo`: pushes may create and
        !! update any branch except `main`, which only landing changes.
        character(len=*), intent(in) :: repo
        integer, intent(out) :: stat
        character(len=:), allocatable :: out
        integer :: u, ios

        open (newunit=u, file=repo//'/hooks/pre-receive', status='replace', action='write', &
            iostat=ios)
        stat = ios
        if (ios /= 0) return
        write (u, '(a)') '#!/bin/sh', &
            '# Installed by fortyard: main changes only by landing (fy land).', &
            '[ -n "$FY_LANDING" ] && exit 0', &
            'while read old new ref; do', &
            '    if [ "$ref" = refs/heads/main ]; then', &
            '        echo "main changes only by landing: push a branch, then fy land" >&2', &
            '        exit 1', &
            '    fi', &
            'done', &
            'exit 0'
        close (u)
        call proc_run([character(len=1024) :: 'chmod', '755', repo//'/hooks/pre-receive'], &
            out, stat, 1024)
    end subroutine land_hook_install

    subroutine land_branch(repo, work, name, branch, number, base_url, upstream, commit, log, ok)
        !! Lands `branch` of the bare repository `repo` (called `name` in URLs) as
        !! landing `number`, using the scratch directory `work`. On success `commit`
        !! is the new `main`; `log` tells what happened either way.
        character(len=*), intent(in) :: repo, work, name, branch, base_url, upstream
        integer, intent(in) :: number
        character(len=:), allocatable, intent(out) :: commit, log
        logical, intent(out) :: ok
        character(len=:), allocatable :: out, subjects, message, num
        ! Fixed length: gfortran overflows typed array constructors around
        ! deferred-length strings.
        character(len=1024) :: range, author
        integer :: status, k, n
        logical :: has_main

        ok = .false.
        commit = ''
        log = ''
        num = itoa(number)
        call run([character(len=1024) :: 'rm', '-rf', work], '')
        call run([character(len=1024) :: 'git', 'clone', '--quiet', '--shared', repo, work], '')
        if (status /= 0) return
        ! A landing interrupted by a restart runs again; if it already moved main,
        ! its commit (made by fortyard, numbered like it) is there and it is done.
        call git([character(len=1024) :: 'log', '-1', '--format=%H', '--fixed-strings', &
            '--committer=fortyard@localhost', '--grep= (#'//num//')', 'origin/main'])
        if (status == 0 .and. len_trim(out) >= 40) then
            commit = first_line(out)
            log = 'Landed as '//commit//' before a restart.'
            ok = .true.
            return
        end if
        call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', 'origin/'//branch])
        if (status /= 0) then
            log = 'There is no branch '//branch//'.'
            return
        end if
        call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', 'origin/main'])
        has_main = status == 0

        ! One squashed commit on top of main (or the first commit of main).
        if (has_main) then
            range = 'origin/main..origin/'//branch
            call git([character(len=1024) :: 'checkout', '--quiet', '-B', 'landing', &
                'origin/main'])
            call git([character(len=1024) :: 'merge', '--squash', '--quiet', 'origin/'//branch])
            if (status /= 0) then
                log = 'The branch does not merge cleanly with main; rebase it onto main.'// &
                    new_line('a')//out
                return
            end if
        else
            range = 'origin/'//branch
            call git([character(len=1024) :: 'checkout', '--quiet', '--orphan', 'landing', &
                'origin/'//branch])
        end if
        call git([character(len=1024) :: 'log', '--reverse', '--format=%s', trim(range)])
        subjects = out
        n = count_lines(subjects)
        if (n == 0) then
            log = 'The branch has nothing that main does not have.'
            return
        end if
        if (n == 1) then
            call git([character(len=1024) :: 'log', '-1', '--format=%B', 'origin/'//branch])
            message = trim_trailing(out)
        else
            message = first_line(subjects)//new_line('a')//new_line('a')// &
                'Squashed commits:'//new_line('a')//bulleted(subjects)
        end if
        k = index(message, new_line('a'))
        if (k == 0) k = len(message) + 1
        message = message(1:k - 1)//' (#'//num//')'//message(k:)
        if (len(base_url) > 0) message = message//new_line('a')//new_line('a')// &
            'Change: '//base_url//'/'//name//'/-/changes/'//num
        call git([character(len=1024) :: 'log', '--reverse', '--format=%an <%ae>', trim(range)])
        author = first_line(out)
        call write_file(work//'/.git/LANDING_MSG', message//new_line('a'))
        call git([character(len=1024) :: '-c', 'user.name=fortyard', '-c', &
            'user.email=fortyard@localhost', 'commit', '--quiet', '--no-verify', &
            '--author='//trim(author), '-F', '.git/LANDING_MSG'])
        if (status /= 0) then
            log = 'The squashed commit could not be made.'//new_line('a')//out
            return
        end if

        ! The check runs on exactly the commit that will become main.
        call check(status)
        log = out
        if (status /= 0) then
            log = 'The check failed (exit status '//itoa(status)//').'//new_line('a')//out
            return
        end if
        call run([character(len=1024) :: 'env', 'FY_LANDING=1', 'git', 'push', '--quiet', &
            'origin', 'landing:main', ':'//branch], work)
        if (status /= 0) then
            log = log//new_line('a')//'main could not be moved forward.'//new_line('a')//out
            return
        end if
        call git([character(len=1024) :: 'rev-parse', 'HEAD'])
        commit = first_line(out)
        ok = .true.
        log = log//new_line('a')//'Landed as '//commit//'.'
        if (len(upstream) > 0) then
            call run([character(len=1024) :: 'env', 'GIT_TERMINAL_PROMPT=0', 'timeout', '300', &
                'git', '--git-dir', repo, 'push', '--quiet', upstream, &
                'refs/heads/main:refs/heads/main'], '')
            if (status == 0) then
                log = log//new_line('a')//'Pushed to '//upstream//'.'
            else
                log = log//new_line('a')//'Pushing to '//upstream//' failed:'// &
                    new_line('a')//out
            end if
        end if
        call run([character(len=1024) :: 'rm', '-rf', work], '')

    contains

        subroutine git(args)
            character(len=*), intent(in) :: args(:)
            call run([character(len=1024) :: 'git', args], work)
        end subroutine git

        subroutine run(argv, dir)
            character(len=*), intent(in) :: argv(:), dir
            if (len(dir) > 0) then
                call proc_run(argv, out, status, LOG_BYTES, dir=dir)
            else
                call proc_run(argv, out, status, LOG_BYTES)
            end if
        end subroutine run

        subroutine check(code)
            !! fo test for an fpm project, else an executable .fortyard/check;
            !! under a memory cap when systemd can give one.
            integer, intent(out) :: code
            character(len=1024), allocatable :: cmd(:)
            character(len=256) :: runtime
            integer :: rn, rs
            logical :: exists

            inquire (file=work//'/fpm.toml', exist=exists)
            if (exists) then
                cmd = [character(len=1024) :: 'fo', 'test']
            else
                inquire (file=work//'/.fortyard/check', exist=exists)
                if (.not. exists) then
                    out = 'No check is configured (fpm.toml or .fortyard/check).'
                    code = 0
                    return
                end if
                cmd = [character(len=1024) :: './.fortyard/check']
            end if
            cmd = [character(len=1024) :: 'timeout', '3600', cmd]
            call get_environment_variable('XDG_RUNTIME_DIR', runtime, rn, rs)
            if (rs == 0 .and. len(proc_find('systemd-run')) > 0) cmd = [character(len=1024) :: &
                'systemd-run', '--user', '--scope', '--quiet', '-p', 'MemoryMax=8G', '-p', &
                'MemorySwapMax=0', cmd]
            call proc_run([character(len=1024) :: 'sh', '-c', 'exec "$@" 2>&1', 'check', &
                cmd], out, code, LOG_BYTES, dir=work)
        end subroutine check

    end subroutine land_branch

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: u
        open (newunit=u, file=path, status='replace', access='stream', form='unformatted')
        write (u) text
        close (u)
    end subroutine write_file

    integer function count_lines(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k
        n = 0
        do k = 1, len(s)
            if (s(k:k) == new_line('a')) n = n + 1
        end do
        if (len(s) > 0) then
            if (s(len(s):) /= new_line('a')) n = n + 1
        end if
    end function count_lines

    function first_line(s) result(l)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: l
        integer :: k
        k = index(s, new_line('a'))
        if (k == 0) then
            l = s
        else
            l = s(1:k - 1)
        end if
    end function first_line

    function bulleted(s) result(b)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: b
        integer :: pos, k
        b = ''
        pos = 1
        do while (pos <= len(s))
            k = index(s(pos:), new_line('a'))
            if (k == 0) k = len(s) - pos + 2
            if (k > 1) b = b//'- '//s(pos:pos + k - 2)//new_line('a')
            pos = pos + k
        end do
        b = trim_trailing(b)
    end function bulleted

    function trim_trailing(s) result(t)
        !! Without trailing newlines.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: n
        n = len(s)
        do while (n > 0)
            if (s(n:n) /= new_line('a')) exit
            n = n - 1
        end do
        t = s(1:n)
    end function trim_trailing

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_land
