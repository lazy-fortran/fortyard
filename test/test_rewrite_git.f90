program test_rewrite_git
    use fy_proc, only: proc_run, proc_setenv
    use fy_rewrite_git, only: rewrite_git_plan_t, rewrite_git_prepare, &
        rewrite_git_apply, rewrite_git_rollback, rewrite_git_recover, rewrite_git_completed
    implicit none

    character(len=*), parameter :: dir = 'build/test_rewrite_git'
    character(len=*), parameter :: live = dir//'/live.git', source = dir//'/new.git'
    character(len=*), parameter :: expected = dir//'/expected.refs'
    character(len=*), parameter :: bundle = dir//'/new.bundle', journal = dir//'/journal'
    character(len=*), parameter :: ident = ' -c user.name=T -c user.email=t@example.org '
    character(len=*), parameter :: nl = new_line('a')
    character(len=*), parameter :: many_refs = &
        '''BEGIN { for (i=1; i<=25000; i++) printf "%s refs/heads/%08d%0130d\n", oid, i, 0 }'' '
    type(rewrite_git_plan_t) :: plan, quoted_plan
    character(len=:), allocatable :: out, message, old, new, before, after, pack
    character(len=4096) :: cwd
    integer :: stat

    call getcwd(cwd)
    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call run('rm -rf '//dir//' && mkdir -p '//dir//' && '// &
        'git init -q --bare -b main '//live//' && git init -q --bare -b main '//source)
    call commit(live, 'old source', old)
    call commit(source, 'clean source', new)
    call run('git --git-dir '//live//' update-ref refs/fortyard/changes/1/1 '//old// &
        ' && git --git-dir '//live//' update-ref refs/notes/review '//old// &
        ' && git --git-dir '//live//' update-ref refs/pull/1/head '//old// &
        ' && git --git-dir '//live//ident//'tag -a v1 -m old '//old// &
        ' && git --git-dir '//source//' update-ref refs/fortyard/changes/1/1 '//new// &
        ' && git --git-dir '//source//' update-ref refs/notes/review '//new// &
        ' && git --git-dir '//source//ident//'tag -a v1 -m clean '//new// &
        ' && git --git-dir '//live//' config custom.sentinel retained && '// &
        'git --git-dir '//live//' config core.hooksPath '//trim(cwd)//'/'//live//'/hooks && '// &
        'printf "#!/bin/sh\nexit 0\n" > '//live//'/hooks/reference-transaction && '// &
        'chmod +x '//live//'/hooks/reference-transaction && '// &
        'git --git-dir '//source//' bundle create '//bundle//' --all && '// &
        'git --git-dir '//live//' show-ref > '//expected)
    call refs(live, before)
    call refs(source, after)

    call run('git --git-dir '//live//' update-ref --no-deref HEAD '//old)
    call rewrite_git_prepare(live, bundle, expected, dir//'/detached', plan, stat, message)
    call check(stat /= 0 .and. index(message, 'HEAD') > 0, 'a detached HEAD is refused')
    call run('git --git-dir '//live//' symbolic-ref HEAD refs/heads/other')
    call rewrite_git_prepare(live, bundle, expected, dir//'/wrong-head', plan, stat, message)
    call check(stat /= 0 .and. index(message, 'HEAD') > 0, 'a different symbolic HEAD is refused')
    call run('git --git-dir '//live//' symbolic-ref HEAD refs/heads/main')

    call run('printf "broken last line\n" > '//dir//'/invalid.refs')
    call rewrite_git_prepare(live, bundle, dir//'/invalid.refs', dir//'/invalid', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'Invalid') > 0, 'malformed last row is refused')
    call run('cat '//expected//' '//expected//' > '//dir//'/duplicate.refs')
    call rewrite_git_prepare(live, bundle, dir//'/duplicate.refs', dir//'/duplicate', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'duplicate') > 0, 'duplicate refs are refused')

    call run('git --git-dir '//source//' update-ref refs/pull/2/head '//new//' && '// &
        'git --git-dir '//source//' bundle create '//dir//'/bad.bundle --all')
    call rewrite_git_prepare(live, dir//'/bad.bundle', expected, dir//'/bad', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'disallowed') > 0, 'pull refs cannot enter clean history')
    call run('git --git-dir '//source//' update-ref -d refs/pull/2/head && '// &
        'git --git-dir '//source//' update-ref -d refs/fortyard/changes/1/1 && '// &
        'git --git-dir '//source//' bundle create '//dir//'/missing.bundle --all')
    call rewrite_git_prepare(live, dir//'/missing.bundle', expected, dir//'/missing', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'drop a repository ref') > 0, &
        'an operational change ref is preserved')
    call run('git --git-dir '//source//' update-ref refs/fortyard/changes/1/1 '//new)

    call run('git --git-dir '//source//' update-ref refs/custom/unexpected '//new//' && '// &
        'git --git-dir '//source//' bundle create '//dir//'/unexpected.bundle --all')
    call rewrite_git_prepare(live, dir//'/unexpected.bundle', expected, dir//'/unexpected', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'introduces a disallowed') > 0, &
        'new private namespaces require an existing ref')
    call run('git --git-dir '//source//' update-ref -d refs/custom/unexpected')

    call run('{ IFS= read -r header; printf "%s\n-'//new// &
        ' prerequisite\n" "$header"; cat; } < '//bundle//' > '//dir//'/partial.bundle')
    call rewrite_git_prepare(live, dir//'/partial.bundle', expected, dir//'/partial', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'self-contained') > 0, &
        'a bundle with prerequisites is refused before live refs change')
    call assert_refs(before, 'an incomplete bundle leaves the original refs')

    call run('{ IFS= read -r header; printf "%s\n" "$header"; '// &
        'awk -v oid='//new//' '//many_refs//'; cat; } < '//bundle//' > '//dir//'/oversized.bundle')
    call rewrite_git_prepare(live, dir//'/oversized.bundle', expected, dir//'/oversized', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'listing exceeds') > 0, &
        'a truncated replacement ref listing cannot become a partial plan')
    call run('git clone -q --mirror '//live//' '//dir//'/oversized.git && '// &
        'awk -v oid='//old//' '//many_refs//' > '//dir//'/oversized.git/packed-refs')
    call rewrite_git_prepare(dir//'/oversized.git', bundle, expected, dir//'/oversized-current', &
        plan, stat, message)
    call check(stat /= 0 .and. index(message, 'listing exceeds') > 0, &
        'a truncated live ref snapshot cannot pass a lease check')

    call run('git --git-dir '//live//' update-ref refs/heads/race '//old)
    call rewrite_git_prepare(live, bundle, expected, dir//'/stale', plan, stat, message)
    call check(stat /= 0 .and. index(message, 'changed since') > 0, 'stale initial lease is refused')
    call run('git --git-dir '//live//' update-ref -d refs/heads/race')

    call rewrite_git_prepare(live, bundle, expected, journal, plan, stat, message)
    call check(stat == 0, 'a complete clean replacement is prepared: '//message)
    call check(plan%old_main == old .and. plan%new_main == new, 'both main commits are recorded')
    call assert_refs(before, 'preparation leaves live refs unchanged')
    call rewrite_git_completed(live, journal, stat, message)
    call check(stat /= 0, 'prepared refs do not count as a committed rewrite')
    call run('git bundle list-heads '//journal//'/before.bundle > '//dir//'/backup.refs && '// &
        'test -s '//journal//'/before.refs && test -s '//journal//'/after.refs && '// &
        'test -s '//journal//'/apply.refs && test -s '//journal//'/rollback.refs && '// &
        'test -x '//journal//'/hooks/reference-transaction && '// &
        'cmp '//live//'/config '//journal//'/config')

    call capture('find '//journal//'/replacement.git/objects/pack -name "*.pack" -printf "%f"', pack)
    call run('rm '//live//'/objects/pack/'//pack//' && '// &
        'printf "existing object must not be overwritten\n" > '//live//'/objects/pack/'//pack)
    call rewrite_git_prepare(live, bundle, expected, dir//'/collision', quoted_plan, stat, message)
    call check(stat /= 0 .and. index(message, 'installed repository') > 0, &
        'an existing conflicting object is refused instead of overwritten')
    call capture('cat '//live//'/objects/pack/'//pack, out)
    call check(out == 'existing object must not be overwritten', 'original object bytes survive installation failure')
    call assert_refs(before, 'object installation failure leaves original refs intact')
    call run('rm '//live//'/objects/pack/'//pack//' && '// &
        'ln '//journal//'/replacement.git/objects/pack/'//pack//' '//live//'/objects/pack/'//pack)

    call proc_run([character(len=1024) :: 'git', 'clone', '--quiet', '--mirror', &
        live, dir//"/live 'space.git"], out, stat, 4096)
    call check(stat == 0, 'quoted path fixture')
    call rewrite_git_prepare(dir//"/live 'space.git", bundle, expected, &
        dir//"/journal 'space", quoted_plan, stat, message)
    call check(stat == 0, 'quoted paths are literal throughout preparation: '//message)
    call rewrite_git_apply(quoted_plan, stat, message)
    call check(stat == 0, 'quoted paths are literal during the ref transaction: '//message)
    call rewrite_git_completed(dir//"/live 'space.git", dir//"/journal 'space", stat, message)
    call check(stat == 0, 'a quoted-path rewrite completed')

    call run('git --git-dir '//live//' update-ref refs/heads/race '//old)
    call rewrite_git_apply(plan, stat, message)
    call check(stat /= 0 .and. index(message, 'changed while') > 0, 'lease is repeated at apply')
    call run('git --git-dir '//live//' update-ref -d refs/heads/race')
    call run('git --git-dir '//live//' update-ref --no-deref HEAD '//old)
    call rewrite_git_apply(plan, stat, message)
    call check(stat /= 0 .and. index(message, 'HEAD') > 0, 'detached HEAD drift is refused at apply')
    call assert_refs(before, 'a changed HEAD leaves all refs unchanged')
    call rewrite_git_recover(live, journal, stat, message)
    call check(stat /= 0 .and. index(message, 'HEAD') > 0, 'recovery refuses detached HEAD drift')
    call run('git --git-dir '//live//' symbolic-ref HEAD refs/heads/main')
    call run('printf "#!/bin/sh\n[ \"\0441\" != prepared ]\n" > '// &
        live//'/hooks/reference-transaction')
    call rewrite_git_apply(plan, stat, message)
    call check(stat /= 0, 'reference transaction hooks are honored')
    call assert_refs(before, 'a refusing hook leaves every ref unchanged')
    call run('printf "#!/bin/sh\nexit 0\n" > '//live//'/hooks/reference-transaction')
    call rewrite_git_apply(plan, stat, message)
    call check(stat == 0, 'the clean refs switch atomically: '//message)
    call assert_refs(after, 'all heads tags and operational refs switch; pull refs disappear')
    call rewrite_git_completed(live, journal, stat, message)
    call check(stat == 0, 'the exact committed ref snapshot is recognized')
    call run('test "$(git --git-dir '//live//' config custom.sentinel)" = retained && '// &
        'test -x '//live//'/hooks/reference-transaction && '// &
        'git --git-dir '//live//' fsck --full --strict > /dev/null')
    call rewrite_git_rollback(plan, stat, message)
    call check(stat == 0, 'rollback succeeds: '//message)
    call assert_refs(before, 'rollback restores all original refs including copied pull')
    call rewrite_git_recover(live, journal, stat, message)
    call check(stat == 0, 'recovery is safe when original refs already stand')
    call rewrite_git_apply(plan, stat, message)
    call check(stat == 0, 'a second application succeeds')
    call run('git --git-dir '//live//' update-ref refs/heads/race '//old)
    call rewrite_git_recover(live, journal, stat, message)
    call check(stat /= 0 .and. index(message, 'neither side') > 0, &
        'recovery refuses unrelated changes')
    call run('git --git-dir '//live//' update-ref -d refs/heads/race')
    call rewrite_git_recover(live, journal, stat, message)
    call check(stat == 0, 'recovery rolls the recorded replacement back')
    call assert_refs(before, 'recovery restores old refs')
    call run('rm -rf '//dir)

contains

    subroutine commit(repo, text, oid)
        character(len=*), intent(in) :: repo, text
        character(len=:), allocatable, intent(out) :: oid
        character(len=:), allocatable :: tree

        call capture('printf %s "'//text//'" | git --git-dir '//repo//' hash-object -w --stdin', tree)
        call run('printf "100644 blob '//trim(tree)//'\tREADME\n" > '//dir//'/tree.input')
        call capture('git --git-dir '//repo//' mktree < '//dir//'/tree.input', tree)
        call capture('printf "Initial\n" | git --git-dir '//repo//ident//'commit-tree '//trim(tree), oid)
        oid = trim(oid)
        call run('git --git-dir '//repo//' update-ref refs/heads/main '//oid)
    end subroutine commit

    subroutine refs(repo, text)
        character(len=*), intent(in) :: repo
        character(len=:), allocatable, intent(out) :: text
        integer :: rc

        call proc_run([character(len=1024) :: 'git', '--git-dir', repo, 'show-ref'], &
            text, rc, 65536)
        call check(rc == 0, 'read refs')
    end subroutine refs

    subroutine assert_refs(expected_text, what)
        character(len=*), intent(in) :: expected_text, what
        character(len=:), allocatable :: actual

        call refs(live, actual)
        call check(actual == expected_text, what)
    end subroutine assert_refs

    subroutine capture(command, text)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: text
        integer :: rc

        call proc_run([character(len=4096) :: 'sh', '-c', command], text, rc, 65536)
        call check(rc == 0, 'setup: '//command)
        if (len(text) > 0) then
            if (text(len(text):) == nl) text = text(:len(text) - 1)
        end if
    end subroutine capture

    subroutine run(command)
        character(len=*), intent(in) :: command
        integer :: rc

        call proc_run([character(len=4096) :: 'sh', '-c', command], out, rc, 65536)
        call check(rc == 0, 'setup: '//command//nl//out)
    end subroutine run

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (ok) return
        write (*, '(a)') 'FAIL: '//what
        error stop 1
    end subroutine check

end program test_rewrite_git
