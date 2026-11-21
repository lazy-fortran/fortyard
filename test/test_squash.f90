program test_squash
    !! The landing commit is made in the bare repository, without a work tree:
    !! a clean squash onto a main that moved on, a conflict with its paths, an
    !! empty squash, a merge of main inside a branch, an upstream merge, the
    !! candidate of several landings, merge attributes, two merge bases, the
    !! size and format checks, and the git version fy serve asks for.
    use fy_land, only: land_branch, land_candidate, land_git_new_enough
    implicit none

    character(len=*), parameter :: data = 'build/test_squash_data', work = 'build/test_squash_work'
    character(len=*), parameter :: repo = data//'/demo.git', scratch = data//'/land'
    character(len=*), parameter :: git = 'git -c user.name=T -c user.email=t@example.org '
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: env, in, bare, out, commit, log, main, first, fy
    character(len=4096) :: cwd
    ! What the runner stand-in does, and what it saw.
    character(len=8) :: probe
    ! The commits two members of a candidate were squashed from.
    character(len=40) :: pins(2)
    logical :: ok, alone, saw_commit, saw_work_tree
    integer :: stat

    call getcwd(cwd)
    env = 'export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 && '
    in = env//'cd '//work//' && '
    bare = env//'git --git-dir='//repo//' '
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call run(env// &
        'git init -q --bare '//repo//' && git init -q -b main '//work)
    call run(in//'printf "1\n2\n3\n4\n5\n" > README && printf "f\n" > F && '//git//'add -A && '// &
        git//'commit -q -m Start && git remote add yard '//trim(cwd)//'/'//repo// &
        ' && git push -q yard HEAD:start')
    call check(stat == 0, 'a first branch is pushed: '//out)
    call land_branch(repo, scratch, 'demo', 'start', '1@t', '', '', commit, log, ok)
    call run(bare//'log --format=%s main')
    call check(ok .and. out == 'Start (#1)'//nl, 'the first landing makes main: '//log//out)

    ! Two branches leave the same main; the second is squashed onto the first.
    call run(in//'git fetch -q yard main && git tag v1 FETCH_HEAD && '// &
        'git checkout -q -B other v1 && printf "one\n2\n3\n4\n5\n" > README && '// &
        'printf "h\n" > F && '//git//'commit -q -a -m "Change top" && '// &
        'git push -q yard HEAD:other && git checkout -q -B clean v1 && '// &
        'printf "1\n2\n3\n4\nfive\n" > README && printf "a\n" > A && '//git//'add -A && '// &
        git//'commit -q -m "Add A" -m "The body of A." && printf "b\n" > B && '//git// &
        'add -A && '//git//'commit -q -m "Add B" && git push -q yard HEAD:clean && '// &
        'git checkout -q -B conf v1 && printf "uno\n2\n3\n4\n5\n" > README && '// &
        'printf "g\n" > F && '//git//'commit -q -a -m "Another top" && '// &
        'git push -q yard HEAD:conf')
    call check(stat == 0, 'three branches are pushed: '//out)
    call land_branch(repo, scratch, 'demo', 'other', '2@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'the first of them lands: '//log)
    first = commit
    probe = 'look'
    call land_branch(repo, scratch, 'demo', 'clean', '3@t', '', '', commit, log, ok, runner=runner)
    call check(ok, 'a branch behind main lands: '//log)
    call check(saw_commit, 'the runner is given a commit with the squashed tree')
    call check(.not. saw_work_tree, 'the squash needs no clone or work tree')
    call run(bare//'log -1 --format="%an <%ae>|%cn <%ce>|%P|%H" main')
    call check(out == 'T <t@example.org>|fortyard <fortyard@localhost>|'//first//'|'//commit//nl, &
        'one commit by the author, committed by fortyard on the main before it: '//out)
    call run(bare//'log -1 --format=%B main')
    call check(out == 'Add A (#3)'//nl//nl//'Squashed commits:'//nl//'- Add A'//nl//'- Add B'// &
        nl//nl//'Landing: 3@t'//nl//nl, 'with the message of a squash: '//out)
    call run(bare//'show main:README main:A main:B')
    call check(out == 'one'//nl//'2'//nl//'3'//nl//'4'//nl//'five'//nl//'a'//nl//'b'//nl, &
        'main has both sides of the three-way merge: '//out)
    call run(bare//'for-each-ref --format="%(refname)"')
    call check(index(out, 'refs/heads/clean') == 0 .and. index(out, 'refs/fortyard') == 0, &
        'the landed branch and the ref of the check are gone: '//out)

    ! A conflict is found by git and names its paths; nothing moves.
    main = commit
    call land_branch(repo, scratch, 'demo', 'conf', '4@t', '', '', commit, log, ok, check=.false.)
    call check(.not. ok .and. index(log, 'does not merge cleanly with main') > 0 .and. &
        index(log, 'Conflicting paths:'//nl//'F'//nl//'README'//nl) > 0, &
        'a conflict lists the conflicting paths: '//log)
    call run(bare//'rev-parse main conf')
    call check(stat == 0 .and. index(out, main//nl) == 1, 'main and the branch stay: '//out)

    ! Commits that add up to nothing are refused before a commit is made.
    call run(in//'git fetch -q yard main && git checkout -q -B none FETCH_HEAD && '// &
        'printf "x\n" > X && '//git//'add -A && '//git//'commit -q -m "Add X" && '//git// &
        'rm -q X && '//git//'commit -q -m "Remove X" && git push -q yard HEAD:none')
    call land_branch(repo, scratch, 'demo', 'none', '5@t', '', '', commit, log, ok, check=.false.)
    call check(.not. ok .and. log == 'The branch has nothing that main does not have.', &
        'an empty squash is refused: '//log)

    ! A merge of main inside a branch is squashed with its other commits, also
    ! as a member of a candidate, where the next member stands on it.
    call run(in//'git checkout -q -B feat FETCH_HEAD && printf "g\n" > G && '//git// &
        'add -A && '//git//'commit -q -m "Add G" -m "Why G." && '// &
        'git checkout -q -B m2 FETCH_HEAD && printf "h\n" > H && '//git//'add -A && '// &
        git//'commit -q -m "Add H" && git push -q yard HEAD:m2')
    call land_branch(repo, scratch, 'demo', 'm2', '6@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'main moves on: '//log)
    main = commit
    call run(in//'git fetch -q yard main && git checkout -q feat && '//git// &
        'merge -q --no-edit -m "Merge main into feat" FETCH_HEAD && '// &
        'git push -q yard HEAD:feat && git checkout -q -B feat2 FETCH_HEAD && '// &
        'printf "k\n" > K && '//git//'add -A && '//git//'commit -q -m "Add K" && '// &
        'git push -q yard HEAD:feat2 && git rev-parse feat^2')
    call check(stat == 0 .and. out == main//nl, 'a branch ends in a merge of main: '//out)
    call land_branch(repo, scratch, 'demo', 'feat', '7@t', '', '', commit, log, ok, onto='', &
        hold='refs/fortyard/queue/7', alone=alone)
    call check(ok .and. .not. alone, 'it is a member of a candidate: '//log)
    first = commit
    call land_branch(repo, scratch, 'demo', 'feat2', '8@t', '', '', commit, log, ok, &
        onto='refs/fortyard/queue/7', hold='refs/fortyard/queue/8', alone=alone)
    call check(ok .and. .not. alone, 'the next member is squashed onto it: '//log)
    call run(bare//'log -2 --format="%P|%H|%s" refs/fortyard/queue/8 && '//bare//'rev-parse main')
    call check(out == first//'|'//commit//'|Add K (#6)'//nl//main//'|'//first//'|Add G (#5)'//nl// &
        main//nl, 'each member is one commit with one parent, main stays: '//out)
    call run(bare//'log -1 --format=%B '//first)
    call check(out == 'Add G (#5)'//nl//nl//'Why G.'//nl//nl//'Landing: 7@t'//nl//nl, &
        'the squash carries the message of the branch''s own commit: '//out)
    call land_candidate(repo, scratch, 'demo', 'refs/fortyard/queue/8', &
        [character(len=8) :: 'feat', 'feat2'], [character(len=32) :: 'refs/fortyard/queue/7', &
        'refs/fortyard/queue/8'], '', log, ok)
    call check(ok, 'the candidate lands: '//log)
    call run(bare//'for-each-ref --format="%(refname) %(objectname)"')
    call check(index(out, 'refs/heads/main '//commit//nl) > 0 .and. &
        index(out, 'refs/heads/feat') == 0 .and. index(out, 'refs/fortyard') == 0, &
        'main is its tip, its branches and queue refs are gone: '//out)
    call run(bare//'show main:G main:H main:K')
    call check(out == 'g'//nl//'h'//nl//'k'//nl, 'main has every member: '//out)

    ! A merge of another history into the current main lands as that merge and
    ! shares no candidate.
    call run(in//'git fetch -q yard main && git checkout -q -B side FETCH_HEAD && '// &
        'printf "u\n" > U && '//git//'add -A && '//git//'commit -q -m "Upstream change" && '// &
        'git checkout -q -B sync FETCH_HEAD && '//git//'merge -q --no-ff -m "Merge upstream" '// &
        'side && git push -q yard HEAD:sync && git rev-parse side')
    call check(stat == 0, 'an upstream merge is pushed: '//out)
    first = out(1:40)
    main = commit
    call land_branch(repo, scratch, 'demo', 'sync', '9@t', '', '', commit, log, ok, onto='', &
        hold='refs/fortyard/queue/9', alone=alone)
    call check(alone .and. .not. ok, 'an upstream merge is left out of a candidate: '//log)
    call land_branch(repo, scratch, 'demo', 'sync', '9@t', '', '', commit, log, ok, check=.false.)
    call run(bare//'log -1 --format="%P|%s" main')
    call check(ok .and. out == main//' '//first//'|Merge upstream (#7)'//nl, &
        'it lands as a merge of main and the merged history: '//log//out)

    ! Commits pushed while the landing was checked stay on the branch.
    call run(in//'git fetch -q yard main && git checkout -q -B late FETCH_HEAD && '// &
        'printf "l\n" > L && '//git//'add -A && '//git//'commit -q -m "Add L" && '// &
        'git push -q yard HEAD:late')
    probe = 'push'
    call land_branch(repo, scratch, 'demo', 'late', '10@t', '', '', commit, log, ok, runner=runner)
    call run(bare//'ls-tree --name-only main && '//bare//'ls-tree --name-only late')
    call check(ok .and. count_of(out, 'L'//nl) == 2 .and. count_of(out, 'M'//nl) == 1, &
        'main has what was queued, the branch keeps what came later: '//log//out)

    ! Merge attributes of main apply as in a checkout of it.
    call run(in//'git fetch -q yard main && git checkout -q -B attr FETCH_HEAD && '// &
        'printf "CHANGELOG merge=union\n" > .gitattributes && printf "a\n" > CHANGELOG && '// &
        'printf "1\n2\n3\n4\n5\n6\n7\n8\n9\n" > N && '//git//'add -A && '//git// &
        'commit -q -m "Add a changelog" && git push -q yard HEAD:attr')
    call land_branch(repo, scratch, 'demo', 'attr', '11@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'a changelog with a merge attribute lands: '//log)
    call run(in//'git fetch -q yard main && git checkout -q -B ux FETCH_HEAD && '// &
        'printf "a\nx\n" > CHANGELOG && '//git//'commit -q -a -m "Note x" && '// &
        'git push -q yard HEAD:ux && git checkout -q -B uy FETCH_HEAD && '// &
        'printf "a\ny\n" > CHANGELOG && '//git//'commit -q -a -m "Note y" && '// &
        'git push -q yard HEAD:uy')
    call land_branch(repo, scratch, 'demo', 'uy', '12@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'one side of the changelog lands: '//log)
    call land_branch(repo, scratch, 'demo', 'ux', '13@t', '', '', commit, log, ok, check=.false.)
    call run(bare//'show main:CHANGELOG')
    call check(ok .and. out == 'a'//nl//'y'//nl//'x'//nl, &
        'a path with merge=union keeps the lines of both sides: '//log//out)

    ! A history that the branch merged itself and main landed as an upstream
    ! merge gives two merge bases; git merges them first, as git merge does.
    call run(in//'git fetch -q yard main && git checkout -q -B side2 v1 && '// &
        'printf "1\n2\n3\n4\n5\n6\n7\n8\n9\n" > S && '//git//'add -A && '//git// &
        'commit -q -m "Side history" && git checkout -q -B cross FETCH_HEAD && '// &
        'printf "1\n2\n3\n4\n5\n6\n7\n8\nnine\n" > N && '//git//'commit -q -a -m "Own nine" && '// &
        git//'merge -q -m "Merge side into cross" side2 && '// &
        'printf "one\n2\n3\n4\n5\n6\n7\n8\n9\n" > S && '//git//'commit -q -a -m "Own S" && '// &
        'git push -q yard HEAD:cross && git checkout -q -B sync2 FETCH_HEAD && '//git// &
        'merge -q --no-ff -m "Merge side" side2 && git push -q yard HEAD:sync2')
    call check(stat == 0, 'a branch with a merged history is pushed: '//out)
    call land_branch(repo, scratch, 'demo', 'sync2', '14@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'main lands the same history as an upstream merge: '//log)
    call run(in//'git fetch -q yard main && git checkout -q -B mine FETCH_HEAD && '// &
        'printf "one\n2\n3\n4\n5\n6\n7\n8\n9\n" > N && '// &
        'printf "1\n2\n3\n4\n5\n6\n7\n8\nnine\n" > S && '//git// &
        'commit -q -a -m "Main side" && git push -q yard HEAD:mine')
    call land_branch(repo, scratch, 'demo', 'mine', '15@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'main changes the files of both histories: '//log)
    call run(bare//'merge-base --all main cross')
    call check(stat == 0 .and. len(out) == 82, 'main and the branch have two merge bases: '//out)
    call land_branch(repo, scratch, 'demo', 'cross', '16@t', '', '', commit, log, ok, check=.false.)
    call run(bare//'show main:N main:S && '//bare//'log -1 --format=%P main')
    call check(ok .and. index(out, 'one'//nl//'2') == 1 .and. count_of(out, 'one'//nl) == 2 &
        .and. count_of(out, 'nine'//nl) == 2 .and. count_of(out, ' ') == 0, &
        'the branch lands as one commit with both sides in both files: '//log//out)

    ! A member of a candidate that got a commit while the candidate was checked
    ! keeps its branch and that commit.
    call run(in//'git fetch -q yard main && git checkout -q -B q2 FETCH_HEAD && '// &
        'printf "q\n" > Q2 && '//git//'add -A && '//git//'commit -q -m "Add Q2" && '// &
        'git push -q yard HEAD:q2 && git checkout -q -B q1 FETCH_HEAD && '// &
        'printf "q\n" > Q1 && '//git//'add -A && '//git//'commit -q -m "Add Q1" && '// &
        'git push -q yard HEAD:q1')
    call land_branch(repo, scratch, 'demo', 'q1', '17@t', '', '', commit, log, ok, onto='', &
        hold='refs/fortyard/queue/17', alone=alone, pinned=pins(1))
    call check(ok, 'a member is held: '//log)
    call land_branch(repo, scratch, 'demo', 'q2', '18@t', '', '', commit, log, ok, &
        onto='refs/fortyard/queue/17', hold='refs/fortyard/queue/18', alone=alone, pinned=pins(2))
    call check(ok, 'the next member is held: '//log)
    call run(in//'git rev-parse HEAD && printf "q\n" > Q3 && '//git//'add -A && '//git// &
        'commit -q -m "Add Q3" && git push -q yard HEAD:q1')
    call check(stat == 0 .and. out == pins(1)//nl, 'the pinned commit is the tip that was held: '//out)
    call land_candidate(repo, scratch, 'demo', 'refs/fortyard/queue/18', &
        [character(len=8) :: 'q1', 'q2'], [character(len=32) :: 'refs/fortyard/queue/17', &
        'refs/fortyard/queue/18'], '', log, ok, check=.false., pinned=pins)
    call run(bare//'for-each-ref --format="%(refname)" refs/heads/q1 refs/heads/q2 && '//bare// &
        'ls-tree --name-only main && '//bare//'ls-tree --name-only q1')
    call check(ok .and. index(out, 'refs/heads/q1'//nl//'.gitattributes') == 1 .and. &
        count_of(out, 'Q1'//nl) == 2 .and. count_of(out, 'Q2'//nl) == 1 .and. &
        count_of(out, 'Q3'//nl) == 1, 'the branch that moved on stays, the other is gone: '//log//out)

    ! The size check reads sizes and the allowed patterns from the object
    ! store, also for a name that git would quote.
    call run(in//'git fetch -q yard main && git checkout -q -B big FETCH_HEAD && '// &
        'mkdir .fortyard && printf "100\n" > .fortyard/size-limit && '// &
        'head -c 200 /dev/zero > "$(printf "big \303\266.bin")" && '//git// &
        'add -A && '//git//'commit -q -m "Add a big file" && git push -q yard HEAD:big')
    call land_branch(repo, scratch, 'demo', 'big', '19@t', '', '', commit, log, ok, check=.false.)
    call check(.not. ok .and. index(log, 'Files over the size limit') == 1 .and. &
        index(log, nl//'big '//achar(195)//achar(182)//'.bin (200 bytes)') > 0, &
        'a file over the size limit of the branch is refused by name: '//log)
    call run(in//'printf "big*\n" > .fortyard/allow-large && '//git//'add -A && '//git// &
        'commit -q -m "Allow it" && git push -q yard HEAD:big')
    call land_branch(repo, scratch, 'demo', 'big', '19@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'a pattern in allow-large lets it land: '//log)

    ! The format check writes the changed Fortran files from the object store,
    ! whatever their names.
    call run(in//'git fetch -q yard main && git checkout -q -B fmt FETCH_HEAD && '// &
        'printf "name = \"demo\"\n" > fpm.toml && mkdir src && for f in "src/a b.f90" '// &
        '"$(printf "src/gr\303\266.f90")"; do '// &
        'printf "module m\n    implicit none\nend module m\n" > "$f"; done && '//git// &
        'add -A && '//git//'commit -q -m "Add modules" && git push -q yard HEAD:fmt')
    call land_branch(repo, scratch, 'demo', 'fmt', '20@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'formatted files with a space or an umlaut in their names land: '//log)
    call run(in//'git fetch -q yard main && git checkout -q -B bad FETCH_HEAD && '// &
        'printf "module  n\nimplicit none\n  end module n\n" > "src/bad one.f90" && '//git// &
        'add -A && '//git//'commit -q -m "Add an untidy module" && git push -q yard HEAD:bad')
    call land_branch(repo, scratch, 'demo', 'bad', '21@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'an unformatted file lands: '//log)
    call run(bare//'show "main:src/bad one.f90"')
    call check(out == 'module  n'//new_line('a')//'    implicit none'//new_line('a')// &
        'end module n'//new_line('a'), 'it lands as fo fmt formats it: '//out)
    call run(bare//'log -1 --format=%B main')
    call check(index(out, 'Formatted by fortyard (fo fmt):'//new_line('a')//'- src/bad one.f90') > 0 &
        .and. index(out, 'Formatted by fortyard') < index(out, 'Landing: '), &
        'the message names it above the trailers: '//out)
    call run(in//'git fetch -q yard main && git checkout -q -B odd FETCH_HEAD && '// &
        'printf "src/odd*\n" > .fortyard/format-exclude && '// &
        'printf "module  o\nimplicit none\n  end module o\n" > src/odd.f90 && '//git// &
        'add -A && '//git//'commit -q -m "Add an excluded module" && git push -q yard HEAD:odd')
    call land_branch(repo, scratch, 'demo', 'odd', '22@t', '', '', commit, log, ok, check=.false.)
    call check(ok, 'a file matched by format-exclude lands: '//log)
    call run(bare//'show main:src/odd.f90')
    call check(out == 'module  o'//new_line('a')//'implicit none'//new_line('a')// &
        '  end module o'//new_line('a'), 'unformatted: '//out)
    call run('find '//scratch//' 2>/dev/null')
    call check(len(out) == 0, 'the files written for the checks are removed: '//out)

    ! fy serve asks for a git that can do this.
    call check(land_git_new_enough('git version 2.40.0') .and. &
        land_git_new_enough('git version 2.56.0') .and. land_git_new_enough('git version 3.0') &
        .and. land_git_new_enough('git version 2.43.0 (Apple Git-154)'), &
        'git 2.40 and newer are enough')
    call check(.not. land_git_new_enough('git version 2.39.5') .and. &
        .not. land_git_new_enough('git version 1.99.0') .and. .not. land_git_new_enough('') &
        .and. .not. land_git_new_enough('git version 2'), 'older and unreadable versions are not')
    call run('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1); echo $PWD/$fy')
    fy = out(1:len(out) - 1)
    call run('mkdir -p '//data//'/old && printf "#!/bin/sh\necho git version 2.39.2\n" > '//data// &
        '/old/git && chmod +x '//data//'/old/git && PATH=$PWD/'//data//'/old:$PATH '//fy// &
        ' serve --data '//data//'/none')
    call check(stat /= 0 .and. index(out, 'git version 2.39.2 is too old') > 0 .and. &
        index(out, 'needs git 2.40 or newer') > 0, 'fy serve refuses an older git: '//out)

    print '(a)', 'squash: all checks passed'

contains

    subroutine runner(name, ref, passed, log)
        !! Stands in for a runner: looks at what it is given, or pushes to the
        !! branch while the check runs.
        character(len=*), intent(in) :: name, ref
        logical, intent(out) :: passed
        character(len=:), allocatable, intent(out) :: log

        passed = .true.
        log = 'Checked '//name//'.'
        if (probe == 'push') then
            call run(in//'printf "m\n" > M && '//git//'add -A && '//git// &
                'commit -q -m "Add M" && git push -q yard HEAD:late')
            return
        end if
        call run(bare//'ls-tree --name-only '//ref)
        saw_commit = out == 'A'//nl//'B'//nl//'F'//nl//'README'//nl
        call run('find '//scratch//' -name HEAD')
        saw_work_tree = len(out) > 0
    end subroutine runner

    integer function count_of(text, word) result(n)
        character(len=*), intent(in) :: text, word
        integer :: pos, k
        n = 0
        pos = 1
        do
            k = index(text(pos:), word)
            if (k == 0) exit
            n = n + 1
            pos = pos + k + len(word) - 1
        end do
    end function count_of

    subroutine run(command)
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: u, ios, n
        text = ''
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=ios) text
        close (u)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_squash
