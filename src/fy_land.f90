module fy_land
    !! Landing: a branch of a primary repository becomes one squashed commit on
    !! `main`, after the repository's check passed on exactly that commit; `main`
    !! then moves forward to it and is pushed to the repository's mirror. A
    !! branch that is `main` plus one merge commit lands as that merge, so the
    !! merged history (such as an upstream's) stays an ancestor of `main`. The
    !! commit is made in the bare repository itself, from git's three-way merge
    !! of trees; files are written only for the checks that read them.
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fy_proc, only: &
        proc_find, &
        proc_run
    implicit none
    private

    public :: &
        check_runner_i, &
        land_branch, &
        land_candidate, &
        land_check, &
        land_git_check, &
        land_git_new_enough, &
        land_hook_install, &
        land_push_check

    abstract interface
        subroutine check_runner_i(repo, ref, passed, log)
            !! Has a runner check `ref` of repository `repo` and waits for it.
            character(len=*), intent(in) :: repo, ref
            logical, intent(out) :: passed
            character(len=:), allocatable, intent(out) :: log
        end subroutine check_runner_i
    end interface

    integer, parameter :: LOG_BYTES = 65536
    character(len=*), parameter :: NO_CHECK = 'Not checked: the policy of the repository sets tests=none.'
    !! git's empty tree, the base of a first landing.
    character(len=*), parameter :: EMPTY_TREE = '4b825dc642cb6eb9a060e54bf8d69288fbee4904'
    !! The value of a ref that does not exist yet.
    character(len=*), parameter :: NO_COMMIT = '0000000000000000000000000000000000000000'
    !! git merge-tree takes --merge-base from this version on.
    character(len=*), parameter :: GIT_NEEDED = '2.40'
    !! The repository's size limit in bytes, read into $lim from the text of
    !! .fortyard/size-limit: 1 MiB unless that is a single number.
    character(len=*), parameter :: SIZE_LIMIT = 'case "$lim" in ""|*[!0-9]*) lim=1048576;; esac; '
    !! Shell: git in the bare repository $3, the landing commit $2 and what
    !! it is compared with, $1.
    character(len=*), parameter :: IN_REPO = 'r=$3; g() { git --git-dir="$r" "$@"; }; '
    !! Prints each file the landing adds or changes that is over the size limit
    !! and not matched by a pattern in .fortyard/allow-large (one shell pattern
    !! a line). Sizes and both files are read from the object store.
    character(len=*), parameter :: SIZE_CHECK = IN_REPO// &
        'lim=$(g cat-file -p "$2:.fortyard/size-limit" 2>/dev/null); '//SIZE_LIMIT// &
        'allow=$(g cat-file -p "$2:.fortyard/allow-large" 2>/dev/null); '// &
        'g -c core.quotePath=false diff --name-only --diff-filter=AMR "$1" "$2" '// &
        '| while IFS= read -r f; do s=$(g cat-file -s "$2:$f"); '// &
        '[ "$s" -gt "$lim" ] || continue; '// &
        'printf "%s\n" "$allow" | { while IFS= read -r p; do [ -n "$p" ] || continue; '// &
        'case "$f" in $p) exit 0;; esac; done; exit 1; } || echo "$f ($s bytes)"; done'
    !! A shell function for a push: reads the pushed commits and prints each
    !! file one of them adds or changes that is over the size limit and matched
    !! by no pattern in .fortyard/allow-large, both taken from the pushed commit
    !! $1. Every pushed commit counts, so a large file cannot enter history and
    !! leave again, and every path does, so a copy under an allowed path does
    !! not carry the file in. A merge counts for what differs from all it merges.
    character(len=*), parameter :: SIZE_RECEIVE = 'large() { '// &
        'allow=$(git cat-file -p "$1:.fortyard/allow-large" 2>/dev/null); '// &
        'lim=$(git cat-file -p "$1:.fortyard/size-limit" 2>/dev/null); '//SIZE_LIMIT// &
        'git -c core.quotePath=false diff-tree --stdin -r -c --root --no-abbrev | '// &
        'awk -F"\t" ''/^:/ { n = split($1, a, " "); print a[n - 1], $2 }'' | sort -u | '// &
        'git cat-file --batch-check="%(objecttype) %(objectsize) %(rest)" | '// &
        'while read -r t s f; do [ "$t" = blob ] && [ "$s" -gt "$lim" ] || continue; '// &
        'printf "%s\n" "$allow" | { while IFS= read -r p; do [ -n "$p" ] || continue; '// &
        'case "$f" in $p) exit 0;; esac; done; exit 1; } || echo "$f ($s bytes)"; done; }'
    !! What a push of HEAD to the remote $1 sends: the commits HEAD has beyond
    !! the remote's refs. A ref whose commit is not here cannot be left out, so
    !! before a refusal those are fetched and the push is looked at again:
    !! history the remote already has is never held against the push. Nothing
    !! when the remote does not answer; the push then fails by itself.
    character(len=*), parameter :: PUSH_CHECK = SIZE_RECEIVE//'; '// &
        'refs=$(git ls-remote "$1" 2>/dev/null) || exit 0; '// &
        'sent() { { echo HEAD; printf "%s\n" "$refs" | while read -r h r; do '// &
        'git cat-file -e "$h^{commit}" 2>/dev/null && echo "^$h"; done; } | '// &
        'git rev-list --stdin | large HEAD; }; '// &
        'out=$(sent); [ -n "$out" ] || exit 0; '// &
        'miss=$(printf "%s\n" "$refs" | while read -r h r; do case "$r" in '// &
        'refs/*"^{}") ;; refs/*) git cat-file -e "$h^{commit}" 2>/dev/null || echo "$r";; '// &
        'esac; done); if [ -n "$miss" ]; then '// &
        'git fetch -q --no-tags --no-write-fetch-head "$1" $miss 2>/dev/null || exit 0; '// &
        'out=$(sent); fi; [ -z "$out" ] || printf "%s\n" "$out"'
    !! fo fmt --check on the Fortran files the landing adds or changes, except
    !! those matched by a pattern in .fortyard/format-exclude (one shell pattern
    !! a line), for code that follows another project's style. The formatter
    !! reads files, so these and the project files it looks for are written
    !! from the object store into the empty current directory, through an
    !! index of their own that leaves the bare repository as it is. Names are
    !! split at line ends only, so one with a space is one file.
    character(len=*), parameter :: FORMAT_CHECK = IN_REPO// &
        'ex=$(g cat-file -p "$2:.fortyard/format-exclude" 2>/dev/null); '// &
        'files=$(g -c core.quotePath=false diff --name-only --diff-filter=AMR "$1" "$2" -- '// &
        '"*.f90" "*.F90" "*.f" "*.F" | while IFS= read -r f; do '// &
        'printf "%s\n" "$ex" | { while IFS= read -r p; do [ -n "$p" ] || continue; '// &
        'case "$f" in $p) exit 0;; esac; done; exit 1; } || echo "$f"; '// &
        'done); [ -n "$files" ] || exit 0; '// &
        'cfg=$(g ls-tree --name-only "$2" -- fpm.toml CMakeLists.txt .fprettify .fprettify.rc); '// &
        'case "$cfg" in *fpm.toml*|*CMakeLists.txt*) ;; *) exit 0;; esac; '// &
        'export GIT_INDEX_FILE="$PWD/.fy-index"; g read-tree "$2" && '// &
        'printf "%s\n" "$files" "$cfg" | tr "\n" "\0" | g --work-tree="$PWD" checkout-index -z '// &
        '--stdin --prefix="$PWD/" 2>&1 || exit 1; unset GIT_INDEX_FILE; rm -f .fy-index; '// &
        'IFS="'//new_line('a')//'"; set -f; fo fmt --check $files 2>&1'

contains

    subroutine land_hook_install(repo, stat)
        !! A pre-receive hook in the bare repository `repo`: pushes may create and
        !! update any branch except `main`, which only landing changes, and bring
        !! no file over the size limit that .fortyard/allow-large does not allow.
        !! refs/fortyard/ holds what the server recorded and what it checks and
        !! lands, so no push moves it; tags and other refs move only for a push
        !! with the admin scope (FY_PUSH_ADMIN, set by the server).
        character(len=*), intent(in) :: repo
        integer, intent(out) :: stat
        character(len=:), allocatable :: out
        integer :: u, ios

        ! Written aside and renamed, so a push in progress reads a whole hook.
        open (newunit=u, file=repo//'/hooks/pre-receive.new', status='replace', action='write', &
            iostat=ios)
        stat = ios
        if (ios /= 0) return
        write (u, '(a)') '#!/bin/sh', &
            '# Installed by fortyard: main changes only by landing (fy land), and a', &
            '# push brings no file over the size limit outside .fortyard/allow-large.', &
            '[ -n "$FY_LANDING" ] && exit 0', &
            SIZE_RECEIVE, &
            'found=', &
            'while read old new ref; do', &
            '    if [ "$ref" = refs/heads/main ]; then', &
            '        echo "main changes only by landing: push a branch, then fy land" >&2', &
            '        exit 1', &
            '    fi', &
            '    case "$ref" in', &
            '    refs/fortyard/*)', &
            '        echo "$ref is kept by fortyard and takes no push" >&2', &
            '        exit 1 ;;', &
            '    refs/heads/*) ;;', &
            '    *)', &
            '        if [ -z "$FY_PUSH_ADMIN" ]; then', &
            '            echo "pushing $ref needs the admin scope; the write scope pushes branches" >&2', &
            '            exit 1', &
            '        fi ;;', &
            '    esac', &
            '    # A deleted ref brings nothing.', &
            '    case "$new" in *[!0]*) ;; *) continue ;; esac', &
            '    out=$(git rev-list "$new" --not --all </dev/null | large "$new")', &
            '    [ -n "$out" ] || continue', &
            '    [ -n "$found" ] || echo "Files over the size limit (allow them in .fortyard/allow-large):" >&2', &
            '    echo "$out" >&2', &
            '    found=1', &
            'done', &
            '[ -z "$found" ] || exit 1', &
            'exit 0'
        close (u)
        call proc_run([character(len=1024) :: 'chmod', '755', repo//'/hooks/pre-receive.new'], &
            out, stat, 1024)
        if (stat == 0) call proc_run([character(len=1024) :: 'mv', '-f', &
            repo//'/hooks/pre-receive.new', repo//'/hooks/pre-receive'], out, stat, 1024)
    end subroutine land_hook_install

    subroutine land_push_check(remote, large)
        !! What the receiving repository would refuse of a push of HEAD of the
        !! checkout in the current directory to the git remote `remote`: `large`
        !! lists the files over the size limit that .fortyard/allow-large does not allow,
        !! and is empty when there are none.
        character(len=*), intent(in) :: remote
        character(len=:), allocatable, intent(out) :: large
        integer :: status

        call proc_run([character(len=2048) :: 'sh', '-c', PUSH_CHECK, 'push-check', remote], &
            large, status, LOG_BYTES)
    end subroutine land_push_check

    subroutine land_branch(repo, work, name, branch, ident, base_url, upstream, commit, log, ok, &
            runner, number, message_in, min_number, onto, hold, alone, check, expect, pinned)
        !! Lands `branch` of the bare repository `repo` (called `name` in URLs).
        !! The landing commit is made in `repo` itself, without a work tree; the
        !! scratch directory `work` holds what needs files. `ident` names this
        !! landing uniquely and goes into the commit as a `Landing:` trailer. The
        !! commit is numbered after the highest `(#N)` on main. On success
        !! `commit` is the new `main`; `log` tells what happened either way.
        character(len=*), intent(in) :: repo, work, name, branch, ident, base_url, upstream
        character(len=:), allocatable, intent(out) :: commit, log
        logical, intent(out) :: ok
        procedure(check_runner_i), optional :: runner
        !! A change's number and commit message (with its trailers), and the
        !! lowest number a landing without a change may take (numbers of open
        !! changes are taken).
        integer, intent(in), optional :: number, min_number
        character(len=*), intent(in), optional :: message_in
        !! For the merge queue: with `hold`, the landing commit is made on the ref
        !! `onto` (main when empty), kept at the ref `hold` and neither checked
        !! nor landed. A branch that cannot share a candidate (a merge of another
        !! history into main, or the first commit of main) sets `alone` instead
        !! and is left untouched.
        character(len=*), intent(in), optional :: onto, hold
        logical, intent(out), optional :: alone
        !! False when the policy of the repository asks for no check (tests=none).
        logical, intent(in), optional :: check
        !! The commit the branch has to be at, when not empty: the version of
        !! its change that was reviewed (review=before).
        character(len=*), intent(in), optional :: expect
        !! The commit of the branch that the landing commit was made from.
        character(len=40), intent(out), optional :: pinned
        logical :: passed
        character(len=:), allocatable :: runner_log
        character(len=:), allocatable :: out, subjects, message, num
        ! Fixed length: gfortran overflows typed array constructors around
        ! deferred-length strings.
        character(len=1024) :: range, author_name, author_mail
        ! The branch, main, what the landing commit stands on (main, or the
        ! candidate before it), the merged history of an upstream merge, and
        ! the landing commit with its tree and what it is checked against.
        character(len=40) :: tip, main, base, merged, tree, made, against
        integer :: status, k, n
        logical :: has_main, checked

        ok = .false.
        commit = ''
        checked = .true.
        if (present(check)) checked = check
        log = ''
        if (present(alone)) alone = .false.
        if (present(pinned)) pinned = ''
        call run([character(len=1024) :: 'rm', '-rf', work], '')
        call run([character(len=1024) :: 'mkdir', '-p', work//'/files'], '')
        if (status /= 0) return
        ! A landing interrupted by a restart runs again; if it already moved main,
        ! its commit (made by fortyard, with its trailer) is there and it is done.
        call git([character(len=1024) :: 'log', '-1', '--format=%H', '--fixed-strings', &
            '--committer=fortyard@localhost', '--grep=Landing: '//ident, 'refs/heads/main'])
        if (status == 0 .and. len_trim(out) >= 40) then
            commit = first_line(out)
            log = 'Landed as '//commit//' before a restart.'
            ok = .true.
            return
        end if
        call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', &
            'refs/heads/'//branch//'^{commit}'])
        if (status /= 0) then
            log = 'There is no branch '//branch//'.'
            return
        end if
        tip = first_line(out)
        if (present(pinned)) pinned = tip
        call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', &
            'refs/heads/main^{commit}'])
        has_main = status == 0
        main = ''
        if (has_main) main = first_line(out)
        base = main
        if (present(expect)) then
            if (len(expect) > 0 .and. tip /= expect) then
                log = 'The branch '//branch//' is at '//tip//', which is not the reviewed'// &
                    ' version '//expect//' of its change, and the policy of the repository'// &
                    ' asks for a review before landing (review=before). Have the newest'// &
                    ' version reviewed, or push the reviewed one again.'
                return
            end if
        end if
        merged = ''
        if (present(hold)) then
            if (.not. has_main) then
                if (present(alone)) alone = .true.
                return
            end if
            ! A merge on main, or on an older main, is no member of a candidate:
            ! it lands as that merge or is refused, by itself. Any other merge
            ! is squashed with the branch's other commits.
            call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', tip//'^2'])
            if (status == 0) then
                call git([character(len=1024) :: 'merge-base', '--is-ancestor', tip//'^1', main])
                if (status == 0) then
                    if (present(alone)) alone = .true.
                    return
                end if
            end if
            if (present(onto)) then
                if (len(onto) > 0) then
                    ! The candidate stands in for main in everything below.
                    call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', &
                        onto//'^{commit}'])
                    if (status /= 0) then
                        log = 'The queue candidate could not be fetched.'//new_line('a')//out
                        return
                    end if
                    base = first_line(out)
                end if
            end if
        else if (has_main) then
            ! A branch that is main plus one merge commit lands as that merge.
            call merge_parent(merged)
            if (status /= 0) return
        end if
        if (present(number)) then
            num = itoa(number)
        else
            k = next_number()
            if (present(min_number)) k = max(k, min_number)
            num = itoa(k)
        end if

        ! Else one squashed commit on top of main (or the first commit of main):
        ! what the branch changed since it left main, merged three-way onto
        ! the newest state by git, in the object store alone.
        tree = ''
        if (has_main .and. len_trim(merged) == 0) then
            range = base//'..'//tip
            call git([character(len=1024) :: 'merge-base', '--all', base, tip])
            if (status /= 0) then
                log = 'The branch does not merge cleanly with main; rebase it onto main.'// &
                    new_line('a')//'It shares no history with main.'
                return
            end if
            ! Merge attributes are those of the side landed on, as in a checkout
            ! of it. Several merge bases (a history both sides merged) are left
            ! to git, which merges them first as git merge does.
            if (count_lines(out) > 1) then
                call git([character(len=1024) :: '-c', 'core.quotePath=false', &
                    '--attr-source='//base, 'merge-tree', '--write-tree', '--name-only', base, tip])
            else
                call git([character(len=1024) :: '-c', 'core.quotePath=false', &
                    '--attr-source='//base, 'merge-tree', '--write-tree', '--name-only', &
                    '--merge-base='//first_line(out), base, tip])
            end if
            if (status == 1) then
                ! After the tree: the conflicting paths, an empty line and what
                ! git says about each.
                k = max(1, index(out, new_line('a')))
                log = 'The branch does not merge cleanly with main; rebase it onto main.'// &
                    new_line('a')//'Conflicting paths:'//trim_trailing(out(k:))
                return
            else if (status /= 0) then
                log = 'The branch could not be merged with main.'//new_line('a')//out
                return
            end if
            tree = first_line(out)
            call git([character(len=1024) :: 'rev-parse', base//'^{tree}'])
            if (status == 0 .and. first_line(out) == tree) then
                log = 'The branch has nothing that main does not have.'
                return
            end if
        else
            range = tip
            if (has_main) range = base//'..'//tip
            call git([character(len=1024) :: 'rev-parse', tip//'^{tree}'])
            if (status == 0) tree = first_line(out)
        end if
        ! Commits whose patch main already has (a branch not rebased after an
        ! earlier landing of part of it) do not name the landing.
        if (has_main) then
            call git([character(len=1024) :: 'log', '--reverse', '--format=%s', '--no-merges', &
                '--cherry-pick', '--right-only', base//'...'//tip])
        else
            call git([character(len=1024) :: 'log', '--reverse', '--format=%s', trim(range)])
        end if
        subjects = out
        n = count_lines(subjects)
        if (n == 0) then
            log = 'The branch has nothing that main does not have.'
            return
        end if
        if (len_trim(merged) > 0 .or. .not. has_main) then
            call git([character(len=1024) :: 'log', '-1', '--format=%B', tip])
            message = trim_trailing(out)
        else if (n == 1) then
            ! The message of that one commit, which a merge of main into the
            ! branch may have left below the tip.
            call git([character(len=1024) :: 'log', '-1', '--format=%B', '--no-merges', &
                '--cherry-pick', '--right-only', base//'...'//tip])
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
        call write_file(work//'/LANDING_MSG', message//new_line('a'))
        if (len_trim(merged) > 0) then
            ! The merge as it was resolved, on main and the merged commit, by the
            ! merge's author.
            call git([character(len=1024) :: 'log', '-1', '--format=%an%n%ae', tip])
        else
            call git([character(len=1024) :: 'log', '--reverse', '--format=%an%n%ae', trim(range)])
        end if
        k = max(1, index(out, new_line('a')))
        author_name = out(1:k - 1)
        author_mail = first_line(out(k + 1:))
        if (len_trim(merged) > 0) then
            call run([character(len=1024) :: 'env', 'GIT_AUTHOR_NAME='//trim(author_name), &
                'GIT_AUTHOR_EMAIL='//trim(author_mail), 'GIT_COMMITTER_NAME=fortyard', &
                'GIT_COMMITTER_EMAIL=fortyard@localhost', 'git', '--git-dir', repo, &
                'commit-tree', tree, '-p', main, '-p', merged, '-F', work//'/LANDING_MSG'], '')
        else
            ! The message is tidied as git commit does it.
            call run([character(len=1024) :: 'env', 'GIT_AUTHOR_NAME='//trim(author_name), &
                'GIT_AUTHOR_EMAIL='//trim(author_mail), 'GIT_COMMITTER_NAME=fortyard', &
                'GIT_COMMITTER_EMAIL=fortyard@localhost', 'sh', '-c', &
                'git stripspace < "$1" | git --git-dir "$2" commit-tree "$3" '// &
                '${4:+-p "$4"} -F - 2>&1', 'commit', work//'/LANDING_MSG', repo, tree, base], '')
        end if
        if (status /= 0 .or. len_trim(tree) == 0) then
            log = 'The landing commit could not be made.'//new_line('a')//out
            return
        end if
        made = first_line(out)

        ! A merge is checked for what differs from the merged history only.
        against = EMPTY_TREE
        if (has_main) against = base
        if (len_trim(merged) > 0) against = merged
        ! What local pre-commit hooks check, checked again on the landing commit,
        ! so nothing that skipped them reaches main.
        call run([character(len=1024) :: 'sh', '-c', SIZE_CHECK, 'size-check', against, made, &
            repo], '')
        if (status /= 0 .or. len_trim(out) > 0) then
            log = 'Files over the size limit (allow them in .fortyard/allow-large):'//new_line('a')//out
            return
        end if
        call run([character(len=1024) :: 'sh', '-c', FORMAT_CHECK, 'format-check', against, &
            made, absolute(repo)], work//'/files')
        if (status /= 0) then
            log = 'Changed Fortran files need formatting (run fo fmt):'//new_line('a')//out
            return
        end if

        if (present(hold)) then
            call git([character(len=1024) :: 'update-ref', hold, made])
            if (status /= 0) then
                log = 'The landing commit could not be queued.'//new_line('a')//out
                return
            end if
            commit = made
            ok = .true.
            call run([character(len=1024) :: 'rm', '-rf', work], '')
            return
        end if
        ! The check runs on exactly the commit that will become main: by a runner
        ! (its own container and limits) when there is one, else here.
        if (.not. checked) then
            out = NO_CHECK
            status = 0
        else if (present(runner)) then
            call git([character(len=1024) :: 'update-ref', check_ref(ident), made])
            call runner(name, check_ref(ident), passed, runner_log)
            call git([character(len=1024) :: 'update-ref', '-d', check_ref(ident)])
            ! Set after the cleanup above, which reuses status and out.
            out = runner_log
            if (passed) out = 'Checked by a runner.'//new_line('a')//runner_log
            status = merge(0, 1, passed)
        else
            call check_out(repo, work//'/check', made, out, status)
            if (status /= 0) then
                log = 'The landing commit could not be checked out.'//new_line('a')//out
                return
            end if
            call land_check(work//'/check', out, status)
        end if
        log = out
        if (status /= 0) then
            log = 'The check failed (exit status '//itoa(status)//').'//new_line('a')//out
            return
        end if
        ! main moves only from the commit this landing was made on.
        if (has_main) then
            call git([character(len=1024) :: 'update-ref', 'refs/heads/main', made, main])
        else
            call git([character(len=1024) :: 'update-ref', 'refs/heads/main', made, NO_COMMIT])
        end if
        if (status /= 0) then
            log = log//new_line('a')//'main could not be moved forward.'//new_line('a')//out
            return
        end if
        ! Commits pushed to the branch since then stay on it.
        call git([character(len=1024) :: 'update-ref', '-d', 'refs/heads/'//branch, tip])
        commit = made
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
            character(len=40), intent(out) :: parent
            character(len=40) :: second

            parent = ''
            call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', tip//'^2'])
            if (status /= 0) then
                status = 0
                return
            end if
            second = first_line(out)
            call git([character(len=1024) :: 'rev-parse', tip//'^1'])
            if (first_line(out) == main) then
                parent = second
                return
            end if
            call git([character(len=1024) :: 'merge-base', '--is-ancestor', tip//'^1', main])
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
            call git([character(len=1024) :: 'log', '--format=%s', base])
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
            call run([character(len=1024) :: 'git', '--git-dir', repo, args], '')
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

    subroutine land_candidate(repo, work, name, ref, branches, holds, upstream, log, ok, runner, &
            check, pinned)
        !! Checks the queue candidate at the ref `ref` of the bare repository
        !! `repo` once and, when it passes, moves main to it and removes the
        !! landed `branches`. The refs `holds` of the candidate's commits are
        !! removed either way. With `pinned`, the commit each branch was squashed
        !! from, a branch that moved on since then stays, with its later commits.
        character(len=*), intent(in) :: repo, work, name, ref, upstream
        character(len=*), intent(in) :: branches(:), holds(:)
        character(len=:), allocatable, intent(out) :: log
        logical, intent(out) :: ok
        procedure(check_runner_i), optional :: runner
        logical, intent(in), optional :: check
        character(len=*), intent(in), optional :: pinned(:)
        character(len=:), allocatable :: out, runner_log
        character(len=40) :: tip, main
        integer :: status, i
        logical :: passed, checked

        ok = .false.
        log = ''
        checked = .true.
        if (present(check)) checked = check
        call run([character(len=1024) :: 'rm', '-rf', work], '')
        call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', ref//'^{commit}'])
        if (status /= 0) then
            log = 'The queue candidate could not be checked out.'//new_line('a')//out
            call drop_holds()
            return
        end if
        tip = first_line(out)
        if (.not. checked) then
            out = NO_CHECK
            status = 0
        else if (present(runner)) then
            call runner(name, ref, passed, runner_log)
            out = runner_log
            if (passed) out = 'Checked by a runner.'//new_line('a')//runner_log
            status = merge(0, 1, passed)
        else
            call check_out(repo, work, tip, out, status)
            if (status /= 0) then
                log = 'The queue candidate could not be checked out.'//new_line('a')//out
                call drop_holds()
                return
            end if
            call land_check(work, out, status)
        end if
        log = out
        if (status /= 0) then
            log = 'The check failed (exit status '//itoa(status)//').'//new_line('a')//out
            call drop_holds()
            return
        end if
        ! main moves forward only: from a commit the candidate stands on.
        call git([character(len=1024) :: 'rev-parse', '--verify', '--quiet', &
            'refs/heads/main^{commit}'])
        main = first_line(out)
        if (status == 0) call git([character(len=1024) :: 'merge-base', '--is-ancestor', main, tip])
        if (status == 0) call git([character(len=1024) :: 'update-ref', 'refs/heads/main', tip, &
            main])
        if (status /= 0) then
            log = log//new_line('a')//'main could not be moved forward.'//new_line('a')//out
            call drop_holds()
            return
        end if
        do i = 1, size(branches)
            if (present(pinned)) then
                call git([character(len=1024) :: 'update-ref', '-d', &
                    'refs/heads/'//trim(branches(i)), trim(pinned(i))])
            else
                call git([character(len=1024) :: 'update-ref', '-d', 'refs/heads/'//trim(branches(i))])
            end if
        end do
        ok = .true.
        call drop_holds()
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

        subroutine drop_holds()
            integer :: k, st
            character(len=:), allocatable :: o

            do k = 1, size(holds)
                call proc_run([character(len=1024) :: 'git', '--git-dir', repo, 'update-ref', &
                    '-d', trim(holds(k))], o, st, LOG_BYTES)
            end do
        end subroutine drop_holds

        subroutine git(args)
            character(len=*), intent(in) :: args(:)
            call run([character(len=1024) :: 'git', '--git-dir', repo, args], '')
        end subroutine git

        subroutine run(argv, dir)
            character(len=*), intent(in) :: argv(:), dir
            if (len(dir) > 0) then
                call proc_run(argv, out, status, LOG_BYTES, dir=dir)
            else
                call proc_run(argv, out, status, LOG_BYTES)
            end if
        end subroutine run
    end subroutine land_candidate

    subroutine check_out(repo, dir, commit, out, status)
        !! A work tree of `commit` of the bare repository `repo` in `dir`, for a
        !! check that runs here: a clone that borrows the repository's objects.
        character(len=*), intent(in) :: repo, dir, commit
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: status

        call proc_run([character(len=1024) :: 'git', 'clone', '--quiet', '--shared', &
            '--no-checkout', repo, dir], out, status, LOG_BYTES)
        if (status == 0) call proc_run([character(len=1024) :: 'git', 'checkout', '--quiet', '-B', &
            'landing', commit], out, status, LOG_BYTES, dir=dir)
    end subroutine check_out

    subroutine land_git_check(ok, message)
        !! Whether the git on PATH can squash in a bare repository; `message`
        !! says what is missing when it cannot.
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: out
        integer :: status

        call proc_run([character(len=16) :: 'git', '--version'], out, status, 1024)
        ok = .false.
        message = 'fy: git is not installed; fy serve needs git '//GIT_NEEDED//' or newer'
        if (status /= 0) return
        ok = land_git_new_enough(out)
        message = ''
        if (.not. ok) message = 'fy: '//first_line(out)//' is too old: fy serve needs git '// &
            GIT_NEEDED//' or newer, as landings are squashed with git merge-tree --merge-base'
    end subroutine land_git_check

    pure logical function land_git_new_enough(version) result(enough)
        !! True when the answer of `git --version` ("git version 2.43.0", with
        !! whatever a vendor adds after it) names 2.40 or newer.
        character(len=*), intent(in) :: version
        integer :: s, e, major, minor, ios

        enough = .false.
        s = scan(version, '0123456789')
        if (s == 0) return
        e = s + verify(version(s:)//' ', '0123456789') - 2
        read (version(s:e), *, iostat=ios) major
        if (ios /= 0 .or. e + 2 > len(version)) return
        if (version(e + 1:e + 1) /= '.') return
        s = e + 2
        e = s + verify(version(s:)//' ', '0123456789') - 2
        if (e < s) return
        read (version(s:e), *, iostat=ios) minor
        if (ios /= 0) return
        enough = major > 2 .or. (major == 2 .and. minor >= 40)
    end function land_git_new_enough

    function absolute(path) result(full)
        !! `path` from the root, for a command that runs in another directory.
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

    subroutine land_check(work, out, code, verify, limit, timed_out, grace)
        !! The check of a landing commit checked out in `work`: fo build and fo
        !! test for an fpm project (applications built, so tests may run them),
        !! else an executable .fortyard/check; under a memory cap when systemd can
        !! give one. `out` is its output, `code` its exit status. With `verify`,
        !! the verification of main instead (fy_verify): .fortyard/verify, the
        !! repository's full suite, else for an fpm project every test run
        !! again, without reusing earlier passes. `limit` is the time limit in
        !! seconds (0: none; default an hour, six for a verification): the
        !! check runs in a process group of its own, which is sent SIGTERM at
        !! the limit and SIGKILL `grace` seconds later (default 30), so no
        !! compiler or test program it started survives; `timed_out` says
        !! that the limit ended it. A process it started that outlives it,
        !! also in a new session, is killed when it ends.
        character(len=*), intent(in) :: work
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: code
        logical, intent(in), optional :: verify
        integer, intent(in), optional :: limit, grace
        logical, intent(out), optional :: timed_out
        character(len=1024), allocatable :: cmd(:)
        character(len=256) :: runtime
        character(len=16) :: seconds, after
        integer(int64) :: started, ended, rate
        integer :: rn, rs, lim
        logical :: exists, full

        if (present(timed_out)) timed_out = .false.
        full = .false.
        if (present(verify)) full = verify
        lim = 3600
        ! The full suite of a large code may take hours.
        if (full) lim = 21600
        if (present(limit)) lim = limit
        write (seconds, '(i0)') lim
        after = '30'
        if (present(grace)) write (after, '(i0)') grace
        if (full) then
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
        ! A sandbox the host provides (FY_CHECK_WRAP, e.g. srt with its
        ! settings) takes the check's command as its arguments.
        call get_environment_variable('FY_CHECK_WRAP', runtime, rn, rs)
        if (rs == 0 .and. rn > 0) cmd = [words(trim(runtime)), cmd]
        inquire (file='/proc/self/status', exist=exists)
        call get_environment_variable('FY_CHECK_WATCHED', runtime, rn, rs)
        if (rs == 0 .and. rn > 0) exists = .false.
        if (.not. exists) then
            call watched_check()
            return
        end if
        ! timeout puts the check in a process group of its own and signals the
        ! whole group, so compilers and test programs end with it.
        if (lim > 0) cmd = [character(len=1024) :: 'timeout', '-k', trim(after), trim(seconds), cmd]
        call get_environment_variable('XDG_RUNTIME_DIR', runtime, rn, rs)
        if (rs == 0 .and. len(proc_find('systemd-run')) > 0) cmd = [character(len=1024) :: &
            'systemd-run', '--user', '--scope', '--quiet', '-p', 'MemoryMax=8G', '-p', &
            'MemorySwapMax=0', cmd]
        call system_clock(started, rate)
        ! A process the check started in a session of its own escapes the
        ! group's signals; it is found by a mark in its environment and
        ! killed once the check ended. The output goes through a file, so
        ! such a process cannot hold the pipe open past the limit.
        call proc_run([character(len=1024) :: 'sh', '-c', 'o=$(mktemp) || exit 125; '// &
            'FORTYARD_CHECK="${FORTYARD_CHECK-}/$$/" "$@" > "$o" 2>&1; s=$?; '// &
            'for i in 1 2 3 4 5; do p=$(grep -lzs -- "^FORTYARD_CHECK=.*/$$/" '// &
            '/proc/[0-9]*/environ); [ -n "$p" ] || break; for f in $p; do f=${f#/proc/}; '// &
            'kill -KILL "${f%/environ}" 2> /dev/null; done; done; '// &
            'cat "$o"; rm -f "$o"; exit $s', 'check', cmd], out, code, LOG_BYTES, dir=work)
        call system_clock(ended)
        ! 124: timeout ended it with SIGTERM; 137: with SIGKILL after the grace.
        if (present(timed_out)) timed_out = lim > 0 .and. (code == 124 .or. code == 137) .and. &
            ended - started >= int(lim, int64)*rate

    contains

        subroutine watched_check()
            !! Without /proc, timeout and systemd (macOS), or with
            !! FY_CHECK_WATCHED set: a shell watches the
            !! check's process tree every second. At the time limit it sends
            !! SIGTERM to the check's process group and SIGKILL after the grace;
            !! when the tree's resident memory passes FY_CHECK_MEMORY GiB
            !! (default 8) it kills the group at once. Every process seen in
            !! the tree, also one that left the group, is killed when the
            !! check ends.
            character(len=16) :: gib
            integer :: gn, gs, mem, ios

            mem = 8
            call get_environment_variable('FY_CHECK_MEMORY', gib, gn, gs)
            if (gs == 0 .and. gn > 0) then
                read (gib, *, iostat=ios) mem
                if (ios /= 0 .or. mem < 1) mem = 8
            end if
            write (gib, '(i0)') mem
            call system_clock(started, rate)
            ! The script is longer than the other arguments' 1024 characters.
            call proc_run([character(len=4096) :: 'sh', '-c', &
                'o=$(mktemp) && seen=$(mktemp) || exit 125; lim=$1 grace=$2 gib=$3; shift 3; '// &
            ! perl gives the check a process group of its own; sh's job
            ! control does not without a terminal (dash).
                'perl -e ''setpgrp(0, 0); exec { $ARGV[0] } @ARGV or die "$ARGV[0]: $!\n"'''// &
                ' "$@" > "$o" 2>&1 & p=$!; t=0 why=; '// &
                'while kill -0 "$p" 2> /dev/null; do sleep 1; t=$((t + 1)); '// &
                'set -- $(ps -A -o pid=,ppid=,rss= | awk -v r="$p" ''{ pp[$1] = $2; rs[$1] = $3 }'// &
                ' END { d[r] = 1; do { c = 0; for (x in pp) if (!(x in d) && (pp[x] in d)) {'// &
                ' d[x] = 1; c = 1 } } while (c); s = 0; l = ""; for (x in d) if (x in rs) {'// &
                ' s += rs[x]; l = l " " x }; print s l }''); kb=${1:-0}; shift; '// &
                'for q in "$@"; do echo "$q"; done >> "$seen"; '// &
                '[ $((t % 60)) -eq 0 ] && sort -u -o "$seen" "$seen"; '// &
                'if [ "$lim" -gt 0 ] && [ "$t" -ge "$lim" ]; then why=time; break; fi; '// &
                'if [ "$kb" -gt $((gib * 1048576)) ]; then why=memory; break; fi; done; '// &
                'if [ "$why" = time ]; then kill -TERM -"$p" 2> /dev/null; i=0; '// &
                'while kill -0 -"$p" 2> /dev/null && [ "$i" -lt "$grace" ]; do sleep 1; '// &
                'i=$((i + 1)); done; fi; kill -KILL -"$p" 2> /dev/null; wait "$p"; s=$?; '// &
            ! Orphans only, seen in the tree or working in the check's
            ! directory (one may escape before the first look): a number
            ! seen earlier may belong to another process by now, but not
            ! to one that launchd adopted.
                'w=$(pwd -P); for i in 1 2 3; do { ps -A -o pid=,ppid= | awk ''$2 == 1 '// &
                '{ print "o" $1 }''; sed "s/^/s/" "$seen"; lsof -a -d cwd -u "$(id -u)" -Fpn '// &
                '2> /dev/null; } | awk -v w="$w" ''/^o/ { o[substr($0, 2)] = 1; next } /^s/ '// &
                '{ s[substr($0, 2)] = 1; next } /^p/ { p = substr($0, 2); next } /^n/ { c = '// &
                'substr($0, 2); if (c == w || index(c, w "/") == 1) s[p] = 1 } END { for (x in o) '// &
                'if (x in s) print x }'' | xargs kill -KILL 2> /dev/null; done; cat "$o"; '// &
                'rm -f "$o" "$seen"; '// &
                'case $why in time) exit 124 ;; memory) echo "The check used more than '// &
                '$gib GiB of memory (FY_CHECK_MEMORY) and was ended."; exit 137 ;; esac; '// &
                'exit $s', 'check', trim(seconds), trim(after), trim(gib), cmd], out, code, &
                LOG_BYTES, dir=work)
            call system_clock(ended)
            if (present(timed_out)) timed_out = lim > 0 .and. code == 124 .and. &
                ended - started >= int(lim, int64)*rate
        end subroutine watched_check

    end subroutine land_check

    function words(text) result(w)
        !! The words of `text`, split at blanks.
        character(len=*), intent(in) :: text
        character(len=1024), allocatable :: w(:)
        integer :: i, j

        allocate (w(0))
        i = 1
        do while (i <= len(text))
            if (text(i:i) == ' ') then
                i = i + 1
                cycle
            end if
            j = index(text(i:), ' ')
            if (j == 0) then
                j = len(text) + 1
            else
                j = i + j - 1
            end if
            w = [w, text(i:j - 1)]
            i = j
        end do
    end function words

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
