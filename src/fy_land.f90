module fy_land
    !! Landing: a branch of a primary repository becomes one squashed commit on
    !! `main`, after the repository's check passed on exactly that commit; `main`
    !! then moves forward to it and is pushed to the repository's mirror. A
    !! branch that is `main` plus one merge commit lands as that merge, so the
    !! merged history (such as an upstream's) stays an ancestor of `main`.
    use fy_proc, only: proc_run, proc_find
    implicit none
    private

    public :: land_branch, land_hook_install, land_check, check_runner_i

    abstract interface
        subroutine check_runner_i(repo, ref, passed, log)
            !! Has a runner check `ref` of repository `repo` and waits for it.
            character(len=*), intent(in) :: repo, ref
            logical, intent(out) :: passed
            character(len=:), allocatable, intent(out) :: log
        end subroutine check_runner_i
    end interface

    integer, parameter :: LOG_BYTES = 65536
    !! git's empty tree, the base of a first landing.
    character(len=*), parameter :: EMPTY_TREE = '4b825dc642cb6eb9a060e54bf8d69288fbee4904'
    !! Prints each file the landing adds or changes that is over 1 MiB and not
    !! matched by a pattern in .fortyard/allow-large (one shell pattern a line).
    character(len=*), parameter :: SIZE_CHECK = 'git diff --name-only --diff-filter=AMR "$1" HEAD '// &
        '| while IFS= read -r f; do s=$(git cat-file -s "HEAD:$f"); '// &
        '[ "$s" -gt 1048576 ] || continue; ok=; if [ -f .fortyard/allow-large ]; then '// &
        'while IFS= read -r p; do case "$f" in $p) ok=1;; esac; done < .fortyard/allow-large; '// &
        'fi; [ -n "$ok" ] || echo "$f ($s bytes)"; done'
    !! fo fmt --check on the Fortran files the landing adds or changes, except
    !! those matched by a pattern in .fortyard/format-exclude (one shell pattern
    !! a line), for code that follows another project's style.
    character(len=*), parameter :: FORMAT_CHECK = 'files=$(git diff --name-only '// &
        '--diff-filter=AMR "$1" HEAD -- "*.f90" "*.F90" "*.f" "*.F" | while IFS= read -r f; do '// &
        'if [ -f .fortyard/format-exclude ]; then while IFS= read -r p; do '// &
        'case "$f" in $p) continue 2;; esac; done < .fortyard/format-exclude; fi; echo "$f"; '// &
        'done); [ -n "$files" ] || exit 0; '// &
        '{ [ -f fpm.toml ] || [ -f CMakeLists.txt ]; } || exit 0; fo fmt --check $files 2>&1'

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

    subroutine land_branch(repo, work, name, branch, ident, base_url, upstream, commit, log, ok, &
            runner, number, message_in, min_number)
        !! Lands `branch` of the bare repository `repo` (called `name` in URLs),
        !! using the scratch directory `work`. `ident` names this landing uniquely
        !! and goes into the commit as a `Landing:` trailer. The commit is numbered
        !! after the highest `(#N)` on main. On success `commit` is the new `main`;
        !! `log` tells what happened either way.
        character(len=*), intent(in) :: repo, work, name, branch, ident, base_url, upstream
        character(len=:), allocatable, intent(out) :: commit, log
        logical, intent(out) :: ok
        procedure(check_runner_i), optional :: runner
        !! A change's number and commit message (with its trailers), and the
        !! lowest number a landing without a change may take (numbers of open
        !! changes are taken).
        integer, intent(in), optional :: number, min_number
        character(len=*), intent(in), optional :: message_in
        logical :: passed
        character(len=:), allocatable :: runner_log
        character(len=:), allocatable :: out, subjects, message, num
        ! Fixed length: gfortran overflows typed array constructors around
        ! deferred-length strings.
        character(len=1024) :: range, author
        character(len=40) :: base
        character(len=:), allocatable :: merged
        integer :: status, k, n
        logical :: has_main

        ok = .false.
        commit = ''
        log = ''
        call run([character(len=1024) :: 'rm', '-rf', work], '')
        call run([character(len=1024) :: 'git', 'clone', '--quiet', '--shared', repo, work], '')
        if (status /= 0) return
        ! A landing interrupted by a restart runs again; if it already moved main,
        ! its commit (made by fortyard, with its trailer) is there and it is done.
        call git([character(len=1024) :: 'log', '-1', '--format=%H', '--fixed-strings', &
            '--committer=fortyard@localhost', '--grep=Landing: '//ident, 'origin/main'])
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
        if (present(number)) then
            num = itoa(number)
        else
            k = next_number()
            if (present(min_number)) k = max(k, min_number)
            num = itoa(k)
        end if

        ! A branch that is main plus one merge commit lands as that merge.
        merged = ''
        if (has_main) then
            call merge_parent(merged)
            if (status /= 0) return
        end if
        ! Else one squashed commit on top of main (or the first commit of main).
        if (has_main) then
            range = 'origin/main..origin/'//branch
            call git([character(len=1024) :: 'checkout', '--quiet', '-B', 'landing', &
                'origin/main'])
            if (len(merged) == 0) then
                ! A three-way merge needs an identity, though it commits nothing.
                call git([character(len=1024) :: '-c', 'user.name=fortyard', '-c', &
                    'user.email=fortyard@localhost', 'merge', '--squash', '--quiet', &
                    'origin/'//branch])
                if (status /= 0) then
                    log = 'The branch does not merge cleanly with main; rebase it onto main.'// &
                        new_line('a')//out
                    return
                end if
            end if
        else
            range = 'origin/'//branch
            call git([character(len=1024) :: 'checkout', '--quiet', '--orphan', 'landing', &
                'origin/'//branch])
        end if
        ! Commits whose patch main already has (a branch not rebased after an
        ! earlier landing of part of it) do not name the landing.
        if (has_main) then
            call git([character(len=1024) :: 'log', '--reverse', '--format=%s', '--no-merges', &
                '--cherry-pick', '--right-only', 'origin/main...origin/'//branch])
        else
            call git([character(len=1024) :: 'log', '--reverse', '--format=%s', trim(range)])
        end if
        subjects = out
        n = count_lines(subjects)
        if (n == 0) then
            log = 'The branch has nothing that main does not have.'
            return
        end if
        if (n == 1 .or. len(merged) > 0) then
            call git([character(len=1024) :: 'log', '-1', '--format=%B', 'origin/'//branch])
            message = trim_trailing(out)
        else
            message = first_line(subjects)//new_line('a')//new_line('a')// &
                'Squashed commits:'//new_line('a')//bulleted(subjects)
        end if
        if (present(message_in)) then
            message = message_in//new_line('a')//'Landing: '//ident
        else
            k = index(message, new_line('a'))
            if (k == 0) k = len(message) + 1
            message = message(1:k - 1)//' (#'//num//')'//message(k:)
            if (len(base_url) > 0) message = message//new_line('a')//new_line('a')// &
                'Change: '//base_url//'/'//name//'/-/changes/'//num
            if (len(base_url) == 0) message = message//new_line('a')
            message = message//new_line('a')//'Landing: '//ident
        end if
        call git([character(len=1024) :: 'log', '--reverse', '--format=%an <%ae>', trim(range)])
        author = first_line(out)
        call write_file(work//'/.git/LANDING_MSG', message//new_line('a'))
        if (len(merged) > 0) then
            ! The merge as it was resolved, on main and the merged commit, by the
            ! merge's author.
            call git([character(len=1024) :: 'log', '-1', '--format=%an%n%ae', 'origin/'//branch])
            k = index(out, new_line('a'))
            call run([character(len=1024) :: 'env', 'GIT_AUTHOR_NAME='//out(1:k - 1), &
                'GIT_AUTHOR_EMAIL='//first_line(out(k + 1:)), 'git', '-c', 'user.name=fortyard', &
                '-c', 'user.email=fortyard@localhost', 'commit-tree', 'origin/'//branch//'^{tree}', &
                '-p', 'origin/main', '-p', merged, '-F', '.git/LANDING_MSG'], work)
            if (status == 0) call git([character(len=1024) :: 'checkout', '--quiet', '-B', &
                'landing', first_line(out)])
        else
            call git([character(len=1024) :: '-c', 'user.name=fortyard', '-c', &
                'user.email=fortyard@localhost', 'commit', '--quiet', '--no-verify', &
                '--author='//trim(author), '-F', '.git/LANDING_MSG'])
        end if
        if (status /= 0) then
            log = 'The landing commit could not be made.'//new_line('a')//out
            return
        end if

        ! A merge is checked for what differs from the merged history only.
        base = EMPTY_TREE
        if (has_main) base = 'HEAD~1'
        if (len(merged) > 0) base = merged
        ! What local pre-commit hooks check, checked again on the landing commit,
        ! so nothing that skipped them reaches main.
        call run([character(len=1024) :: 'sh', '-c', SIZE_CHECK, 'size-check', &
            base], work)
        if (status /= 0 .or. len_trim(out) > 0) then
            log = 'Files over 1 MiB (allow them in .fortyard/allow-large):'//new_line('a')//out
            return
        end if
        call run([character(len=1024) :: 'sh', '-c', FORMAT_CHECK, 'format-check', &
            base], work)
        if (status /= 0) then
            log = 'Changed Fortran files need formatting (run fo fmt):'//new_line('a')//out
            return
        end if

        ! The check runs on exactly the commit that will become main: by a runner
        ! (its own container and limits) when there is one, else here.
        if (present(runner)) then
            call run([character(len=1024) :: 'env', 'FY_LANDING=1', 'git', 'push', '--quiet', &
                'origin', 'HEAD:'//check_ref(ident)], work)
            call runner(name, check_ref(ident), passed, runner_log)
            call run([character(len=1024) :: 'env', 'FY_LANDING=1', 'git', 'push', '--quiet', &
                'origin', ':'//check_ref(ident)], work)
            ! Set after the cleanup above, which reuses status and out.
            out = runner_log
            if (passed) out = 'Checked by a runner.'//new_line('a')//runner_log
            status = merge(0, 1, passed)
        else
            call land_check(work, out, status)
        end if
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

        subroutine merge_parent(parent)
            !! The second parent of the branch's tip when the tip is a merge whose
            !! first parent is main, else ''. A merge onto an older main fails
            !! (status 1): squashing it would drop the merged history.
            character(len=:), allocatable, intent(out) :: parent
            character(len=:), allocatable :: second, first, main

            parent = ''
            call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', &
                'origin/'//branch//'^2'])
            if (status /= 0) then
                status = 0
                return
            end if
            second = first_line(out)
            call git([character(len=1024) :: 'rev-parse', 'origin/'//branch//'^1', 'origin/main'])
            first = first_line(out)
            main = out(len(first) + 2:)
            main = first_line(main)
            if (first == main) then
                parent = second
                return
            end if
            call git([character(len=1024) :: 'merge-base', '--is-ancestor', first, 'origin/main'])
            if (status == 0) then
                log = 'The branch is a merge onto an older main; merge again on the current main.'
                status = 1
                return
            end if
            status = 0
        end subroutine merge_parent

        integer function next_number() result(m)
            !! One more than the highest "(#N)" ending a subject on main, so numbers
            !! continue those of earlier landings and of pull requests before.
            integer :: pos, e, s, v, ios
            m = 1
            if (.not. has_main) return
            call git([character(len=1024) :: 'log', '--format=%s', 'origin/main'])
            pos = 1
            do while (pos <= len(out))
                e = index(out(pos:), new_line('a'))
                if (e == 0) e = len(out) - pos + 2
                e = pos + e - 2
                if (e - pos >= 3) then
                    if (out(e:e) == ')') then
                        s = index(out(pos:e), '(#', back=.true.)
                        if (s > 0) then
                            s = pos + s + 1
                            if (e - 1 >= s .and. verify(out(s:e - 1), '0123456789') == 0) then
                                read (out(s:e - 1), *, iostat=ios) v
                                if (ios == 0) m = max(m, v + 1)
                            end if
                        end if
                    end if
                end if
                pos = e + 2
            end do
        end function next_number

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

    end subroutine land_branch

    subroutine land_check(work, out, code, verify)
        !! The check of a landing commit checked out in `work`: fo build and fo
        !! test for an fpm project (applications built, so tests may run them),
        !! else an executable .fortyard/check; under a memory cap when systemd can
        !! give one. `out` is its output, `code` its exit status. With `verify`,
        !! the verification of main instead (fy_verify): .fortyard/verify, the
        !! repository's full suite, else for an fpm project every test run
        !! again, without reusing earlier passes.
        character(len=*), intent(in) :: work
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: code
        logical, intent(in), optional :: verify
        character(len=1024), allocatable :: cmd(:)
        character(len=256) :: runtime
        character(len=8) :: limit
        integer :: rn, rs
        logical :: exists, full

        full = .false.
        if (present(verify)) full = verify
        limit = '3600'
        if (full) then
            ! The full suite of a large code may take hours.
            limit = '21600'
            inquire (file=work//'/.fortyard/verify', exist=exists)
            if (exists) then
                cmd = [character(len=1024) :: './.fortyard/verify']
            else
                inquire (file=work//'/fpm.toml', exist=exists)
                if (.not. exists) then
                    out = 'No verification is configured (.fortyard/verify or fpm.toml).'
                    code = 0
                    return
                end if
                cmd = [character(len=1024) :: 'sh', '-c', 'fo build && FO_TEST_RERUN=1 fo test']
            end if
        else
            inquire (file=work//'/fpm.toml', exist=exists)
            if (exists) then
                cmd = [character(len=1024) :: 'sh', '-c', 'fo build && fo test']
            else
                inquire (file=work//'/.fortyard/check', exist=exists)
                if (.not. exists) then
                    out = 'No check is configured (fpm.toml or .fortyard/check).'
                    code = 0
                    return
                end if
                cmd = [character(len=1024) :: './.fortyard/check']
            end if
        end if
        cmd = [character(len=1024) :: 'timeout', trim(limit), cmd]
        call get_environment_variable('XDG_RUNTIME_DIR', runtime, rn, rs)
        if (rs == 0 .and. len(proc_find('systemd-run')) > 0) cmd = [character(len=1024) :: &
            'systemd-run', '--user', '--scope', '--quiet', '-p', 'MemoryMax=8G', '-p', &
            'MemorySwapMax=0', cmd]
        call proc_run([character(len=1024) :: 'sh', '-c', 'exec "$@" 2>&1', 'check', cmd], out, &
            code, LOG_BYTES, dir=work)
    end subroutine land_check

    function check_ref(ident) result(ref)
        !! Where a landing commit waits for its runner: refs/fortyard/checks/N.
        character(len=*), intent(in) :: ident
        character(len=:), allocatable :: ref
        integer :: at
        at = index(ident, '@')
        if (at == 0) at = len(ident) + 1
        ref = 'refs/fortyard/checks/'//ident(1:at - 1)
    end function check_ref

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
