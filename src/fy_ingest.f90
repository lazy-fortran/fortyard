module fy_ingest
    !! Only an explicitly approved GitHub head becomes a change. Import never
    !! checks out files or queues checks. Landing remains a separate action.
    use fx_json_build, only: json_escape_string
    use fy_audit, only: audit_append
    use fy_change, only: change_branch_tips, change_sync, change_by_branch, &
        change_comment, change_review_set, change_t
    use fy_proc, only: proc_run, proc_pid
    use fy_sqlite, only: db_t
    implicit none
    private
    public :: ingest_github

contains

    subroutine ingest_github(db, repo, git_dir, upstream, pr, head, approved, &
            account, session, code, body)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, git_dir, upstream, pr, head, account, session
        logical, intent(in) :: approved
        integer, intent(out) :: code
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable :: source, url, branch, staging, out, tips, detail
        character(len=24) :: pid, number
        integer :: status, ignored, slash
        logical :: found
        type(change_t) :: change

        code = 400
        body = '{"error":"ingest needs a positive PR number and full lower-case head SHA"}'
        if (len(pr) == 0 .or. len(pr) > 10) return
        if (verify(pr, '0123456789') /= 0) return
        if (pr(1:1) == '0') return
        if (len(head) /= 40) return
        if (verify(head, '0123456789abcdef') /= 0) return
        if (.not. approved) then
            code = 403
            body = '{"error":"external code needs explicit approve_execution for this head"}'
            return
        end if
        source = ''
        if (index(upstream, 'https://github.com/') == 1) source = upstream(20:)
        if (index(upstream, 'git@github.com:') == 1) source = upstream(16:)
        if (len(source) > 4) then
            if (source(len(source) - 3:) == '.git') source = source(:len(source) - 4)
        end if
        body = '{"error":"ingest needs a GitHub upstream"}'
        if (len(source) == 0) return
        if (verify(source, &
                'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-/') /= 0) return
        slash = index(source, '/')
        if (slash <= 1 .or. slash >= len(source)) return
        if (index(source(slash + 1:), '/') /= 0) return
        if (index(source, '..') > 0 .or. source(1:1) == '/' .or. &
                source(len(source):) == '/') return
        url = 'https://github.com/'//source//'.git'
        branch = 'external/github/'//pr//'/'//head
        write (pid, '(i0)') proc_pid()
        staging = 'refs/fortyard/ingest/'//trim(pid)//'/'//pr//'/'//head
        tips = change_branch_tips(git_dir)
        call git('fetch', '--no-tags', '--no-recurse-submodules', &
            '--no-write-fetch-head', url, '+refs/pull/'//pr//'/head:'//staging)
        if (status /= 0) then
            call refuse(502, 'GitHub PR could not be fetched')
            return
        end if
        call git('rev-parse', staging)
        if (status /= 0 .or. out /= head//new_line('a')) then
            call refuse(409, 'PR head changed. Review the new head and approve it separately')
            return
        end if
        call git('merge-base', 'main', head)
        if (status /= 0) then
            call refuse(409, 'PR has no common history with main')
            return
        end if
        call git('ls-remote', '--exit-code', url, 'refs/pull/'//pr//'/head')
        if (status /= 0 .or. len(out) < 41) then
            call refuse(502, 'GitHub PR head could not be confirmed')
            return
        end if
        if (out(:40) /= head) then
            call refuse(409, 'PR head changed. Review the new head and approve it separately')
            return
        end if
        detail = 'https://github.com/'//source//'/pull/'//pr//' head '//head
        call audit_append(db, account, 'external.execution-approved', repo, detail, status)
        if (status /= 0) then
            call refuse(500, 'approval could not be recorded. No checks were queued')
            return
        end if
        call change_by_branch(db, repo, branch, change, found)
        if (found) then
            code = 200
        else
            call git('update-ref', 'refs/heads/'//branch, head, &
                repeat('0', 40))
            if (status /= 0) then
                call refuse(409, 'import branch already exists. Inspect it before retrying')
                return
            end if
            call change_sync(db, repo, git_dir, account, session, tips)
            call change_by_branch(db, repo, branch, change, found)
            if (.not. found) then
                call git('update-ref', '-d', 'refs/heads/'//branch, head)
                call refuse(409, 'PR did not create an open change. It may already be on main')
                return
            end if
            code = 201
        end if
        call change_review_set(db, change%id, 'human-reviewed', .false., '', head)
        if (code == 201) call change_comment(db, change%id, account, &
            'External execution approved for '//detail//'. Imported without running checks.')
        write (number, '(i0)') change%number
        body = '{"change":'//trim(number)//',"branch":"'//branch// &
            '","head":"'//head//'","source":"https://github.com/'//source// &
            '/pull/'//pr//'","checks_queued":false}'
        call drop_staging()

    contains

        subroutine git(a, b, c, d, e, f)
            character(len=*), intent(in) :: a
            character(len=*), intent(in), optional :: b, c, d, e, f
            character(len=1024) :: argv(9)
            integer :: n
            argv(1) = 'git'
            argv(2) = '--git-dir'
            argv(3) = git_dir
            argv(4) = a
            n = 4
            if (present(b)) then
                n = n + 1
                argv(n) = b
            end if
            if (present(c)) then
                n = n + 1
                argv(n) = c
            end if
            if (present(d)) then
                n = n + 1
                argv(n) = d
            end if
            if (present(e)) then
                n = n + 1
                argv(n) = e
            end if
            if (present(f)) then
                n = n + 1
                argv(n) = f
            end if
            call proc_run(argv(:n), out, status, 65536)
        end subroutine git

        subroutine drop_staging()
            character(len=:), allocatable :: discarded
            character(len=1024) :: argv(6)
            argv(1) = 'git'
            argv(2) = '--git-dir'
            argv(3) = git_dir
            argv(4) = 'update-ref'
            argv(5) = '-d'
            argv(6) = staging
            call proc_run(argv, discarded, ignored, 1024)
        end subroutine drop_staging

        subroutine refuse(error_code, message)
            integer, intent(in) :: error_code
            character(len=*), intent(in) :: message
            code = error_code
            body = '{"error":"'//json_escape_string(message)//'"}'
            call drop_staging()
        end subroutine refuse

    end subroutine ingest_github

end module fy_ingest
