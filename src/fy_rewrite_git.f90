module fy_rewrite_git
    use fy_proc, only: proc_run
    use fy_repo, only: repo_ref_ok
    implicit none
    private

    public :: rewrite_ref_t, rewrite_git_plan_t, rewrite_git_prepare, &
        rewrite_git_apply, rewrite_git_rollback, rewrite_git_recover, rewrite_git_completed

    integer, parameter :: MAX_REFS_BYTES = 4194304
    character(len=*), parameter :: HEX = '0123456789abcdef'

    type :: rewrite_ref_t
        character(len=40) :: oid = ''
        character(len=:), allocatable :: name
    end type rewrite_ref_t

    type :: rewrite_git_plan_t
        character(len=:), allocatable :: repo, work, old_main, new_main
        type(rewrite_ref_t), allocatable :: before(:), after(:)
    end type rewrite_git_plan_t

contains

    subroutine rewrite_git_prepare(repo, bundle, expected_refs, work, plan, status, message)
        character(len=*), intent(in) :: repo, bundle, expected_refs, work
        type(rewrite_git_plan_t), intent(out) :: plan
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        type(rewrite_ref_t), allocatable :: expected(:)
        character(len=:), allocatable :: text, out, stage
        integer :: k, found

        status = 1
        message = ''
        if (min(len(repo), len(bundle), len(expected_refs), len(work)) == 0 .or. &
            max(len(repo), len(bundle), len(expected_refs), len(work)) > 3000) then
            message = 'Rewrite paths must be nonempty and shorter than 3001 bytes.'
            return
        end if
        plan%repo = repo
        plan%work = work
        call read_text(expected_refs, text, status, message)
        if (status /= 0) return
        call parse_refs(text, expected, .false., status, message)
        if (status /= 0) return
        call current_refs(repo, plan%before, status, message)
        if (status /= 0) return
        if (.not. same_refs(plan%before, expected)) then
            call fail('The repository refs changed since the expected snapshot.')
            return
        end if
        found = find_ref(plan%before, 'refs/heads/main')
        if (found == 0) then
            call fail('The repository has no main branch.')
            return
        end if
        plan%old_main = plan%before(found)%oid
        do k = 1, size(plan%before)
            if (index(plan%before(k)%name, 'refs/replace/') /= 1) cycle
            call fail('Remove Git replacement refs before rewriting repository history.')
            return
        end do
        call proc_run([character(len=4096) :: 'git', 'bundle', 'list-heads', bundle], &
            out, status, MAX_REFS_BYTES)
        if (status /= 0) then
            message = 'The replacement bundle cannot be read: '//out
            return
        end if
        if (len(out) >= MAX_REFS_BYTES) then
            call fail('The replacement ref listing exceeds the supported size.')
            return
        end if
        call parse_refs(out, plan%after, .true., status, message)
        if (status /= 0) return
        found = find_ref(plan%after, 'refs/heads/main')
        if (found == 0) then
            call fail('The replacement bundle has no main branch.')
            return
        end if
        plan%new_main = plan%after(found)%oid
        do k = 1, size(plan%after)
            if (index(plan%after(k)%name, 'refs/pull/') == 1 .or. &
                index(plan%after(k)%name, 'refs/replace/') == 1) then
                call fail('The bundle contains a disallowed ref: '//plan%after(k)%name)
                return
            end if
            if (allowed_ref(plan%after(k)%name)) cycle
            if (find_ref(plan%before, plan%after(k)%name) /= 0) cycle
            call fail('The bundle introduces a disallowed ref: '//plan%after(k)%name)
            return
        end do
        do k = 1, size(plan%before)
            if (index(plan%before(k)%name, 'refs/pull/') == 1) cycle
            if (find_ref(plan%after, plan%before(k)%name) > 0) cycle
            call fail('The bundle would drop a repository ref: '//plan%before(k)%name)
            return
        end do
        call proc_run([character(len=4096) :: 'mkdir', '--', work], out, status, 4096)
        if (status /= 0) then
            message = 'The rewrite journal must be a new directory: '//out
            return
        end if
        stage = work//'/replacement.git'
        call proc_run([character(len=4096) :: 'git', 'init', '--quiet', '--bare', stage], &
            out, status, 4096)
        if (status /= 0) then
            message = 'Cannot create the replacement staging repository: '//out
            return
        end if
        ! Verification in an empty repository refuses bundles with prerequisites.
        call git(stage, [character(len=4096) :: 'bundle', 'verify', bundle], out, status)
        if (status /= 0) then
            message = 'The replacement must be a complete self-contained bundle: '//out
            return
        end if
        call git(stage, [character(len=4096) :: '-c', 'fetch.unpackLimit=0', &
            'fetch', '--quiet', '--no-tags', bundle, &
            '+refs/*:refs/*'], out, status)
        if (status /= 0) then
            message = 'Cannot import the replacement bundle: '//out
            return
        end if
        call git(stage, [character(len=128) :: 'fsck', '--full', '--strict'], out, status)
        if (status /= 0) then
            message = 'The replacement Git object graph is invalid: '//out
            return
        end if
        do k = 1, size(plan%after)
            if (index(plan%after(k)%name, 'refs/heads/') /= 1 .and. &
                index(plan%after(k)%name, 'refs/fortyard/') /= 1 .and. &
                index(plan%after(k)%name, 'refs/notes/') /= 1) cycle
            call git(stage, [character(len=128) :: 'cat-file', '-t', plan%after(k)%oid], &
                out, status)
            if (status /= 0 .or. out /= 'commit'//new_line('a')) then
                call fail('A replacement branch or operational ref does not point to a commit.')
                return
            end if
        end do
        call git(repo, [character(len=4096) :: 'bundle', 'create', work//'/before.bundle', &
            '--all'], out, status)
        if (status /= 0) then
            message = 'Cannot back up the original repository: '//out
            return
        end if
        call proc_run([character(len=4096) :: 'sh', '-c', &
            'for n in config hooks; do if [ -e "$1/$n" ]; then '// &
            'cp -a -- "$1/$n" "$2/$n" || exit; fi; done', 'backup', repo, work], &
            out, status, 4096)
        if (status /= 0) then
            message = 'Cannot back up the repository configuration: '//out
            return
        end if
        call write_refs(work//'/before.refs', plan%before, status)
        if (status == 0) call write_refs(work//'/after.refs', plan%after, status)
        if (status == 0) call write_transaction(work//'/apply.refs', &
            plan%before, plan%after, status)
        if (status == 0) call write_transaction(work//'/rollback.refs', &
            plan%after, plan%before, status)
        if (status /= 0) then
            message = 'Cannot write the durable rewrite ref journal.'
            return
        end if
        ! Hardlinks install whole objects without overwriting any original object.
        call proc_run([character(len=4096) :: 'sh', '-c', &
            'test ! -e "$1/objects/info/alternates" && '// &
            'cp -a --link --no-clobber -- "$1/objects/." "$2/objects/"', &
            'objects', stage, repo], &
            out, status, 4096)
        if (status /= 0) then
            message = 'Cannot install replacement objects; the journal must share the repository filesystem: '//out
            return
        end if
        call git(repo, [character(len=128) :: 'fsck', '--full', '--strict'], out, status)
        if (status /= 0) then
            message = 'The installed repository Git object graph is invalid: '//out
            return
        end if
        call proc_run([character(len=4096) :: 'sync', '-f', work], out, status, 4096)
        if (status /= 0) message = 'Cannot synchronize the rewrite journal and Git objects: '//out

    contains

        subroutine fail(why)
            character(len=*), intent(in) :: why
            status = 1
            message = why
        end subroutine fail
    end subroutine rewrite_git_prepare

    subroutine rewrite_git_apply(plan, status, message)
        type(rewrite_git_plan_t), intent(in) :: plan
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        type(rewrite_ref_t), allocatable :: now(:)

        call current_refs(plan%repo, now, status, message)
        if (status /= 0) return
        if (.not. same_refs(plan%before, now)) then
            status = 1
            message = 'The repository refs changed while preparing the rewrite.'
            return
        end if
        call run_transaction(plan%repo, plan%work//'/apply.refs', status, message)
    end subroutine rewrite_git_apply

    subroutine rewrite_git_rollback(plan, status, message)
        type(rewrite_git_plan_t), intent(in) :: plan
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message

        call run_transaction(plan%repo, plan%work//'/rollback.refs', status, message)
    end subroutine rewrite_git_rollback

    subroutine rewrite_git_recover(repo, work, status, message)
        character(len=*), intent(in) :: repo, work
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        type(rewrite_ref_t), allocatable :: before(:), after(:), now(:)
        character(len=:), allocatable :: text

        call read_text(work//'/before.refs', text, status, message)
        if (status /= 0) return
        call parse_refs(text, before, .false., status, message)
        if (status /= 0) return
        call read_text(work//'/after.refs', text, status, message)
        if (status /= 0) return
        call parse_refs(text, after, .false., status, message)
        if (status /= 0) return
        call current_refs(repo, now, status, message)
        if (status /= 0) return
        if (same_refs(now, before)) return
        if (.not. same_refs(now, after)) then
            status = 1
            message = 'Recovery refuses refs that match neither side of the recorded rewrite.'
            return
        end if
        call run_transaction(repo, work//'/rollback.refs', status, message)
    end subroutine rewrite_git_recover

    subroutine rewrite_git_completed(repo, work, status, message)
        character(len=*), intent(in) :: repo, work
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        type(rewrite_ref_t), allocatable :: after(:), now(:)
        character(len=:), allocatable :: text

        call read_text(work//'/after.refs', text, status, message)
        if (status /= 0) return
        call parse_refs(text, after, .false., status, message)
        if (status /= 0) return
        call current_refs(repo, now, status, message)
        if (status /= 0) return
        if (same_refs(now, after)) return
        status = 1
        message = 'The repository refs do not match the committed rewrite.'
    end subroutine rewrite_git_completed

    subroutine run_transaction(repo, file, status, message)
        character(len=*), intent(in) :: repo, file
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message

        call require_main_head(repo, status, message)
        if (status /= 0) return
        ! The caller creates its pending marker after preparation, on this filesystem.
        call proc_run([character(len=4096) :: 'sync', '-f', repo], message, status, 4096)
        if (status /= 0) then
            message = 'Cannot synchronize the pending rewrite before changing refs: '//message
            return
        end if
        call proc_run([character(len=4096) :: 'sh', '-c', &
            'exec git --git-dir "$1" update-ref --stdin < "$2" 2>&1', &
            'refs', repo, file], message, status, 65536)
        if (status /= 0) return
        call proc_run([character(len=4096) :: 'sync', '-f', repo], message, status, 4096)
        if (status /= 0) message = 'Refs changed but could not be synchronized; run rewrite recovery: '//message
    end subroutine run_transaction

    subroutine current_refs(repo, refs, status, message)
        character(len=*), intent(in) :: repo
        type(rewrite_ref_t), allocatable, intent(out) :: refs(:)
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: text

        call require_main_head(repo, status, message)
        if (status /= 0) return
        call git(repo, [character(len=128) :: 'for-each-ref', &
            '--format=%(objectname) %(refname)'], text, status)
        if (status /= 0) then
            message = 'Cannot read the repository refs: '//text
            return
        end if
        if (len(text) >= MAX_REFS_BYTES) then
            status = 1
            message = 'The repository ref listing exceeds the supported size.'
            return
        end if
        call parse_refs(text, refs, .false., status, message)
    end subroutine current_refs

    subroutine require_main_head(repo, status, message)
        character(len=*), intent(in) :: repo
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: target

        call git(repo, [character(len=128) :: 'symbolic-ref', '--quiet', &
            '--no-recurse', 'HEAD'], target, status)
        if (status == 0 .and. target == 'refs/heads/main'//new_line('a')) then
            message = ''
            return
        end if
        status = 1
        message = 'History rewrite requires HEAD to be symbolic refs/heads/main.'
    end subroutine require_main_head

    subroutine git(repo, args, out, status)
        character(len=*), intent(in) :: repo, args(:)
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: status

        call proc_run([character(len=4096) :: 'git', '--git-dir', repo, args], &
            out, status, MAX_REFS_BYTES)
    end subroutine git

    subroutine parse_refs(text, refs, skip_head, status, message)
        character(len=*), intent(in) :: text
        type(rewrite_ref_t), allocatable, intent(out) :: refs(:)
        logical, intent(in) :: skip_head
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        type(rewrite_ref_t) :: item
        character(len=:), allocatable :: line
        integer :: pos, fin

        allocate (refs(0))
        status = 1
        message = 'Invalid or duplicate reference in the rewrite input.'
        pos = 1
        do while (pos <= len(text))
            fin = index(text(pos:), new_line('a'))
            if (fin == 0) fin = len(text) - pos + 2
            line = trim(text(pos:pos + fin - 2))
            pos = pos + fin
            if (len(line) == 0) cycle
            if (len(line) < 42) return
            if (line(41:41) /= ' ') return
            if (verify(line(1:40), HEX) /= 0) return
            if (skip_head .and. line(42:) == 'HEAD') cycle
            if (.not. repo_ref_ok(line(42:))) return
            if (index(line(42:), 'refs/') /= 1) return
            if (find_ref(refs, line(42:)) /= 0) return
            item%oid = line(1:40)
            item%name = line(42:)
            refs = [refs, item]
        end do
        message = ''
        status = 0
    end subroutine parse_refs

    logical function allowed_ref(name)
        character(len=*), intent(in) :: name

        allowed_ref = index(name, 'refs/heads/') == 1 .or. &
            index(name, 'refs/tags/') == 1 .or. index(name, 'refs/fortyard/') == 1
    end function allowed_ref

    integer function find_ref(refs, name) result(k)
        type(rewrite_ref_t), intent(in) :: refs(:)
        character(len=*), intent(in) :: name
        integer :: i

        k = 0
        do i = 1, size(refs)
            if (refs(i)%name /= name) cycle
            k = i
            return
        end do
    end function find_ref

    logical function same_refs(a, b) result(same)
        type(rewrite_ref_t), intent(in) :: a(:), b(:)
        integer :: k, j

        same = .false.
        if (size(a) /= size(b)) return
        do k = 1, size(a)
            j = find_ref(b, a(k)%name)
            if (j == 0) return
            if (a(k)%oid /= b(j)%oid) return
        end do
        same = .true.
    end function same_refs

    subroutine write_refs(file, refs, status)
        character(len=*), intent(in) :: file
        type(rewrite_ref_t), intent(in) :: refs(:)
        integer, intent(out) :: status
        integer :: u, k, rc

        open (newunit=u, file=file, status='new', action='write', iostat=status)
        if (status /= 0) return
        do k = 1, size(refs)
            write (u, '(a)', iostat=status) refs(k)%oid//' '//refs(k)%name
            if (status /= 0) exit
        end do
        close (u, iostat=rc)
        if (status == 0) status = rc
    end subroutine write_refs

    subroutine write_transaction(file, before, after, status)
        character(len=*), intent(in) :: file
        type(rewrite_ref_t), intent(in) :: before(:), after(:)
        integer, intent(out) :: status
        integer :: u, k, j, rc
        character(len=40) :: old

        open (newunit=u, file=file, status='new', action='write', iostat=status)
        if (status /= 0) return
        write (u, '(a)', iostat=status) 'start'
        do k = 1, size(after)
            if (status /= 0) exit
            j = find_ref(before, after(k)%name)
            old = repeat('0', 40)
            if (j /= 0) old = before(j)%oid
            write (u, '(a)', iostat=status) 'update '//after(k)%name//' '//after(k)%oid//' '//old
        end do
        do k = 1, size(before)
            if (status /= 0) exit
            if (find_ref(after, before(k)%name) /= 0) cycle
            write (u, '(a)', iostat=status) 'delete '//before(k)%name//' '//before(k)%oid
        end do
        if (status == 0) write (u, '(a)', iostat=status) 'prepare'
        if (status == 0) write (u, '(a)', iostat=status) 'commit'
        close (u, iostat=rc)
        if (status == 0) status = rc
    end subroutine write_transaction

    subroutine read_text(file, text, status, message)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text, message
        integer, intent(out) :: status
        integer :: u, n

        message = ''
        text = ''
        open (newunit=u, file=file, status='old', access='stream', form='unformatted', &
            action='read', iostat=status)
        if (status /= 0) then
            message = 'Cannot read the expected ref snapshot.'
            return
        end if
        inquire (unit=u, size=n)
        if (n < 0 .or. n > MAX_REFS_BYTES) then
            close (u)
            status = 1
            message = 'The expected ref snapshot is too large.'
            return
        end if
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=status) text
        close (u)
        if (status /= 0) message = 'Cannot read the expected ref snapshot.'
    end subroutine read_text

end module fy_rewrite_git
