module fy_repo
    !! Mirrored git repositories: validation of names, refs and paths from
    !! requests, and read-only queries answered by git subprocesses. Each query
    !! returns JSON for the API.
    use fx_json_build, only: &
        json_escape_string
    use fy_proc, only: &
        proc_run
    implicit none
    private

    public :: &
        repo_blob, &
        repo_clone, &
        repo_commit_json, &
        repo_fetch, &
        repo_init, &
        repo_log_json, &
        repo_name_ok, &
        repo_path_ok, &
        repo_publish, &
        repo_published, &
        repo_push_tags, &
        repo_ref_ok, &
        repo_refs_json, &
        repo_tree_json, &
        repo_unpublish

    integer, parameter, public :: BLOB_MAX = 1048576
    integer, parameter, public :: DIFF_MAX = 524288
    character(len=1), parameter :: US = achar(31), RS = achar(30), NUL = achar(0)

contains

    pure logical function repo_name_ok(name)
        character(len=*), intent(in) :: name

        repo_name_ok = len(name) >= 1 .and. len(name) <= 64
        if (.not. repo_name_ok) return
        repo_name_ok = verify(name, 'abcdefghijklmnopqrstuvwxyz0123456789._-') == 0 &
            .and. name(1:1) /= '.' .and. name(1:1) /= '-'
    end function repo_name_ok

    pure logical function repo_ref_ok(ref)
        !! A branch, tag or commit name. Never starts with '-', so git cannot
        !! read it as an option.
        character(len=*), intent(in) :: ref

        repo_ref_ok = len(ref) >= 1 .and. len(ref) <= 200
        if (.not. repo_ref_ok) return
        repo_ref_ok = verify(ref, &
            'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._/-') == 0 &
            .and. ref(1:1) /= '-' .and. ref(1:1) /= '/' .and. index(ref, '..') == 0 &
            .and. index(ref, '//') == 0
    end function repo_ref_ok

    pure logical function repo_path_ok(path)
        !! A path inside the tree: relative, without '..' segments or control
        !! characters. Empty means the top directory.
        character(len=*), intent(in) :: path
        integer :: i

        repo_path_ok = len(path) <= 1000
        if (.not. repo_path_ok .or. len(path) == 0) return
        repo_path_ok = path(1:1) /= '/' .and. path(len(path):len(path)) /= '/'
        do i = 1, len(path)
            if (iachar(path(i:i)) < 32 .or. iachar(path(i:i)) == 127) repo_path_ok = .false.
        end do
        if (path == '..' .or. index(path, '../') == 1 .or. index(path, '/../') > 0 .or. &
            (len(path) >= 3 .and. path(max(1, len(path) - 2):) == '/..')) repo_path_ok = .false.
        if (index(path, '//') > 0) repo_path_ok = .false.
    end function repo_path_ok

    subroutine git(repo, args, out, status, max_bytes)
        character(len=*), intent(in) :: repo
        character(len=*), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: out
        integer, intent(out) :: status
        integer, intent(in) :: max_bytes
        character(len=1024) :: argv(size(args) + 5)

        argv(1) = 'git'
        argv(2) = '--git-dir'
        argv(3) = repo
        argv(4) = '-c'
        argv(5) = 'core.quotePath=false'
        argv(6:) = args
        call proc_run(argv, out, status, max_bytes)
    end subroutine git

    subroutine repo_log_json(repo, ref, limit, json, status)
        !! The last `limit` commits reachable from `ref`, newest first.
        character(len=*), intent(in) :: repo, ref
        integer, intent(in) :: limit
        character(len=:), allocatable, intent(out) :: json
        integer, intent(out) :: status
        character(len=:), allocatable :: out, rec
        character(len=16) :: n
        integer :: pos, nxt
        logical :: first

        write (n, '(i0)') max(1, min(limit, 500))
        call git(repo, [character(len=64) :: 'log', '--format=%H%x1f%an%x1f%aI%x1f%s%x1e', &
            '-n', n, ref, '--'], out, status, 4 * 1048576)
        json = '{"commits":['
        if (status /= 0) then
            json = json//']}'
            return
        end if
        first = .true.
        pos = 1
        do
            nxt = index(out(pos:), RS)
            if (nxt == 0) exit
            rec = lstrip(out(pos:pos + nxt - 2))
            pos = pos + nxt
            if (.not. first) json = json//','
            first = .false.
            json = json//'{"id":"'//field(rec, 1)//'","author":"'// &
                json_escape_string(field(rec, 2))//'","date":"'//field(rec, 3)// &
                '","subject":"'//json_escape_string(field(rec, 4))//'"}'
        end do
        json = json//']}'
    end subroutine repo_log_json

    subroutine repo_tree_json(repo, ref, path, json, status)
        !! Entries of the directory `path` at `ref`: directories first.
        character(len=*), intent(in) :: repo, ref, path
        character(len=:), allocatable, intent(out) :: json
        integer, intent(out) :: status
        character(len=:), allocatable :: out, rec, spec, dirs, files, entry, kind, size_
        integer :: pos, nxt, tab

        spec = ref
        if (len(path) > 0) spec = ref//':'//path
        call git(repo, [character(len=1024) :: 'ls-tree', '-z', '-l', spec], out, status, &
            4 * 1048576)
        json = '{"entries":['
        if (status /= 0) then
            json = json//']}'
            return
        end if
        dirs = ''
        files = ''
        pos = 1
        do
            nxt = index(out(pos:), NUL)
            if (nxt == 0) exit
            rec = out(pos:pos + nxt - 2)
            pos = pos + nxt
            ! "<mode> <type> <object> <size>\t<name>"
            tab = index(rec, achar(9))
            if (tab == 0) cycle
            kind = word(rec(1:tab - 1), 2)
            size_ = word(rec(1:tab - 1), 4)
            if (size_ == '-') size_ = '0'
            entry = '{"name":"'//json_escape_string(rec(tab + 1:))//'","type":"'//kind// &
                '","size":'//size_//'}'
            if (kind == 'tree') then
                if (len(dirs) > 0) dirs = dirs//','
                dirs = dirs//entry
            else
                if (len(files) > 0) files = files//','
                files = files//entry
            end if
        end do
        if (len(dirs) > 0 .and. len(files) > 0) dirs = dirs//','
        json = json//dirs//files//']}'
    end subroutine repo_tree_json

    subroutine repo_blob(repo, ref, path, content, status)
        !! The file `path` at `ref`, at most BLOB_MAX bytes; status 413 when larger.
        character(len=*), intent(in) :: repo, ref, path
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: status
        character(len=:), allocatable :: out
        integer :: ios, bytes

        call git(repo, [character(len=1024) :: 'cat-file', '-s', ref//':'//path], out, &
            status, 64)
        content = ''
        if (status /= 0) return
        read (out, *, iostat=ios) bytes
        if (ios /= 0) then
            status = 1
            return
        end if
        if (bytes > BLOB_MAX) then
            status = 413
            return
        end if
        call git(repo, [character(len=1024) :: 'cat-file', 'blob', ref//':'//path], content, &
            status, BLOB_MAX)
    end subroutine repo_blob

    subroutine repo_commit_json(repo, id, json, status)
        !! One commit with its message and its patch against the first parent;
        !! the patch is cut at DIFF_MAX bytes ("truncated":true).
        character(len=*), intent(in) :: repo, id
        character(len=:), allocatable, intent(out) :: json
        integer, intent(out) :: status
        character(len=:), allocatable :: out, head, patch
        integer :: sep

        call git(repo, [character(len=256) :: 'show', '--no-color', '--first-parent', &
            '--format=%H%x1f%P%x1f%an%x1f%aI%x1f%B%x1e', '--patch', id, '--'], out, status, &
            DIFF_MAX + 65536)
        json = ''
        if (status /= 0) return
        sep = index(out, RS)
        if (sep == 0) then
            status = 1
            return
        end if
        head = out(1:sep - 1)
        patch = out(sep + 1:)
        if (len(patch) > 0) then
            if (patch(1:1) == new_line('a')) patch = patch(2:)
        end if
        json = '{"id":"'//field(head, 1)//'","parents":"'//field(head, 2)// &
            '","author":"'//json_escape_string(field(head, 3))//'","date":"'// &
            field(head, 4)//'","message":"'//json_escape_string(trim_newlines(field(head, 5)))// &
            '","truncated":'
        if (len(out) >= DIFF_MAX + 65536) then
            json = json//'true'
        else
            json = json//'false'
        end if
        json = json//',"patch":"'//json_escape_string(patch)//'"}'
    end subroutine repo_commit_json

    subroutine repo_refs_json(repo, json, status)
        !! Branches with their commits; the default branch first.
        character(len=*), intent(in) :: repo
        character(len=:), allocatable, intent(out) :: json
        integer, intent(out) :: status
        character(len=:), allocatable :: out, head, rec
        integer :: pos, nxt
        logical :: first

        call git(repo, [character(len=64) :: 'symbolic-ref', '--short', 'HEAD'], head, &
            status, 256)
        if (status /= 0) head = ''
        head = trim_newlines(head)
        call git(repo, [character(len=128) :: 'for-each-ref', &
            '--format=%(refname:short)%1f%(objectname)%1e', 'refs/heads'], out, status, &
            1048576)
        json = '{"default":"'//json_escape_string(head)//'","branches":['
        if (status /= 0) then
            json = json//']}'
            return
        end if
        first = .true.
        pos = 1
        do
            nxt = index(out(pos:), RS)
            if (nxt == 0) exit
            rec = lstrip(out(pos:pos + nxt - 2))
            pos = pos + nxt
            if (.not. first) json = json//','
            first = .false.
            json = json//'{"name":"'//json_escape_string(field(rec, 1))//'","id":"'// &
                field(rec, 2)//'"}'
        end do
        json = json//']}'
    end subroutine repo_refs_json

    subroutine repo_clone(upstream, repo, status, keys)
        !! A bare mirror of `upstream` at `repo`. With `keys`, a directory that
        !! holds a private key named like the repository and a known_hosts file,
        !! git reaches an SSH upstream with that key, now and for later fetches
        !! and pushes; GitHub accepts a deploy key for one repository only.
        character(len=*), intent(in) :: upstream, repo
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: keys
        character(len=:), allocatable :: out, name, ssh
        logical :: exists
        integer :: slash

        ssh = ''
        exists = .false.
        if (present(keys)) exists = len(keys) > 0
        if (exists) then
            slash = index(repo, '/', back=.true.)
            name = repo(slash + 1:)
            if (len(name) > 4) then
                if (name(len(name) - 3:) == '.git') name = name(:len(name) - 4)
            end if
            inquire (file=keys//'/'//name, exist=exists)
            if (exists) ssh = 'ssh -i '//keys//'/'//name//' -o IdentitiesOnly=yes '// &
                '-o BatchMode=yes -o UserKnownHostsFile='//keys//'/known_hosts'
        end if
        if (len(ssh) == 0) then
            call proc_run([character(len=1024) :: 'env', 'GIT_TERMINAL_PROMPT=0', 'timeout', &
                '600', 'git', 'clone', '--quiet', '--mirror', upstream, repo], out, status, 65536)
            return
        end if
        call proc_run([character(len=1024) :: 'env', 'GIT_TERMINAL_PROMPT=0', &
            'GIT_SSH_COMMAND='//ssh, 'timeout', '600', 'git', 'clone', '--quiet', '--mirror', &
            upstream, repo], out, status, 65536)
        if (status /= 0) return
        call proc_run([character(len=1024) :: 'git', '--git-dir', repo, 'config', &
            'core.sshCommand', ssh], out, status, 65536)
    end subroutine repo_clone

    subroutine repo_init(repo, status)
        !! An empty bare repository at `repo` that accepts pushes to its branches.
        character(len=*), intent(in) :: repo
        integer, intent(out) :: status
        character(len=:), allocatable :: out

        call proc_run([character(len=1024) :: 'git', 'init', '--quiet', '--bare', &
            '--initial-branch=main', repo], out, status, 65536)
    end subroutine repo_init

    subroutine repo_fetch(repo, status)
        character(len=*), intent(in) :: repo
        integer, intent(out) :: status
        character(len=:), allocatable :: out

        call proc_run([character(len=1024) :: 'env', 'GIT_TERMINAL_PROMPT=0', 'timeout', &
            '300', 'git', '--git-dir', repo, 'fetch', '--quiet', '--prune'], out, status, 65536)
    end subroutine repo_fetch

    subroutine repo_push_tags(repo, upstream, status)
        !! Pushes the tags of the primary repository `repo` to its upstream when
        !! they changed since the last push that succeeded. A tag that moved is
        !! refused by the upstream, never forced. A failure is retried on the
        !! next call and reported once per set of tags.
        character(len=*), intent(in) :: repo, upstream
        integer, intent(out) :: status
        character(len=:), allocatable :: tags, out
        integer :: stat

        status = 0
        call proc_run([character(len=1024) :: 'git', '--git-dir', repo, 'for-each-ref', &
            '--format=%(objectname) %(refname)', 'refs/tags'], tags, stat, 4194304)
        if (stat /= 0) return
        if (tags == slurp(repo//'/fortyard-tags-pushed')) return
        call proc_run([character(len=1024) :: 'env', 'GIT_TERMINAL_PROMPT=0', 'timeout', &
            '300', 'git', '--git-dir', repo, 'push', '--quiet', upstream, &
            'refs/tags/*:refs/tags/*'], out, stat, 65536)
        if (stat == 0) then
            call spit(repo//'/fortyard-tags-pushed', tags)
        else if (tags /= slurp(repo//'/fortyard-tags-failed')) then
            call spit(repo//'/fortyard-tags-failed', tags)
            status = stat
        end if

    contains

        function slurp(path) result(s)
            character(len=*), intent(in) :: path
            character(len=:), allocatable :: s
            integer :: u, ios, n

            s = ''
            open (newunit=u, file=path, access='stream', form='unformatted', status='old', &
                action='read', iostat=ios)
            if (ios /= 0) return
            inquire (unit=u, size=n)
            deallocate (s)
            allocate (character(len=n) :: s)
            if (n > 0) read (u, iostat=ios) s
            close (u)
        end function slurp

        subroutine spit(path, s)
            character(len=*), intent(in) :: path, s
            integer :: u, ios

            open (newunit=u, file=path, access='stream', form='unformatted', status='replace', &
                action='write', iostat=ios)
            if (ios /= 0) return
            write (u, iostat=ios) s
            close (u)
        end subroutine spit
    end subroutine repo_push_tags

    subroutine repo_publish(repo, upstream, check, ref, branch, commit, out, status)
        !! Pushes the commit of `ref` in `repo` to `upstream` as `branch`, for
        !! the check numbered `check`, forced, since a check queued again is
        !! published again. It is recorded in fortyard-published/<check> of
        !! `repo` before the push, so that repo_unpublish deletes it even
        !! when the push broke off half-way.
        character(len=*), intent(in) :: repo, upstream, check, ref, branch
        character(len=:), allocatable, intent(out) :: commit, out
        integer, intent(out) :: status
        integer :: u, ios

        out = ''
        call proc_run([character(len=1024) :: 'git', '--git-dir', repo, 'rev-parse', '--verify', &
            '--quiet', ref//'^{commit}'], commit, status, 4096)
        commit = trim_newlines(commit)
        if (status /= 0 .or. len(commit) /= 40) then
            out = 'No commit for '//ref//'.'
            status = 1
            return
        end if
        call proc_run([character(len=1024) :: 'mkdir', '-p', repo//'/fortyard-published'], out, &
            status, 4096)
        open (newunit=u, file=repo//'/fortyard-published/'//check, status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            out = 'The publication could not be recorded.'
            status = 1
            return
        end if
        write (u, '(a)') branch
        close (u)
        call proc_run([character(len=1024) :: 'sh', '-c', 'exec "$@" 2>&1', 'push', 'env', &
            'GIT_TERMINAL_PROMPT=0', 'timeout', '300', 'git', '--git-dir', repo, 'push', '--quiet', &
            '--force', upstream, commit//':refs/heads/'//branch], out, status, 65536)
    end subroutine repo_publish

    subroutine repo_unpublish(repo, upstream, check, branch, out, status)
        !! Deletes the branch that repo_publish pushed for check `check` from
        !! `upstream`, and its record; `branch` is its name, empty when
        !! nothing of the check is published. A branch the upstream does
        !! not have counts as deleted. A minute at most: it runs in a
        !! request worker and in the mirror loop, and the sweep retries.
        character(len=*), intent(in) :: repo, upstream, check
        character(len=:), allocatable, intent(out) :: branch, out
        integer, intent(out) :: status
        character(len=:), allocatable :: heads
        character(len=1024) :: line
        integer :: u, ios, stat

        branch = ''
        out = ''
        status = 0
        open (newunit=u, file=repo//'/fortyard-published/'//check, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
        branch = trim(line)
        if (ios /= 0 .or. .not. repo_ref_ok(branch)) then
            branch = ''
            call proc_run([character(len=1024) :: 'rm', '-f', repo//'/fortyard-published/'//check], &
                out, status, 4096)
            return
        end if
        call proc_run([character(len=1024) :: 'sh', '-c', 'exec "$@" 2>&1', 'push', 'env', &
            'GIT_TERMINAL_PROMPT=0', 'timeout', '60', 'git', '--git-dir', repo, 'push', '--quiet', &
            upstream, ':refs/heads/'//branch], out, status, 65536)
        if (status /= 0) then
            call proc_run([character(len=1024) :: 'env', 'GIT_TERMINAL_PROMPT=0', 'timeout', &
                '60', 'git', '--git-dir', repo, 'ls-remote', '--heads', upstream, &
                'refs/heads/'//branch], heads, stat, 65536)
            if (stat /= 0 .or. len(trim_newlines(heads)) > 0) return
            status = 0
        end if
        call proc_run([character(len=1024) :: 'rm', '-f', repo//'/fortyard-published/'//check], &
            heads, stat, 4096)
    end subroutine repo_unpublish

    function repo_published(repo) result(checks)
        !! The numbers of the checks that have a branch published from
        !! `repo`, one per line.
        character(len=*), intent(in) :: repo
        character(len=:), allocatable :: checks
        integer :: status

        call proc_run([character(len=1024) :: 'sh', '-c', 'ls "$1" 2>/dev/null; true', 'ls', &
            repo//'/fortyard-published'], checks, status, 1048576)
    end function repo_published

    function field(rec, n) result(s)
        !! Field n of a record separated by US.
        character(len=*), intent(in) :: rec
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        integer :: i, start, stop_

        start = 1
        do i = 1, n - 1
            stop_ = index(rec(start:), US)
            if (stop_ == 0) then
                s = ''
                return
            end if
            start = start + stop_
        end do
        stop_ = index(rec(start:), US)
        if (stop_ == 0) then
            s = rec(start:)
        else
            s = rec(start:start + stop_ - 2)
        end if
    end function field

    function word(s, n) result(w)
        !! The n-th blank-separated word of s.
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: w
        character(len=:), allocatable :: rest
        integer :: i, sp

        rest = adjustl(s)
        do i = 1, n - 1
            sp = index(rest, ' ')
            if (sp == 0) then
                w = ''
                return
            end if
            rest = adjustl(rest(sp + 1:))
        end do
        sp = index(rest, ' ')
        if (sp == 0) then
            w = trim(rest)
        else
            w = rest(1:sp - 1)
        end if
    end function word

    function lstrip(s) result(t)
        !! Without the line breaks git puts between formatted records.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: b

        b = verify(s, ' '//new_line('a')//achar(13))
        if (b == 0) then
            t = ''
        else
            t = s(b:)
        end if
    end function lstrip

    function trim_newlines(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: e

        e = len(s)
        do while (e > 0)
            if (s(e:e) /= new_line('a') .and. s(e:e) /= achar(13)) exit
            e = e - 1
        end do
        t = s(1:e)
    end function trim_newlines

end module fy_repo
