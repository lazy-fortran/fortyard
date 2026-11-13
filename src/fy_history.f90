module fy_history
    !! Offline, leased replacement of a repository and its source-navigation metadata.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_audit, only: audit_append, audit_verify
    use fy_instance_lock, only: instance_lock, instance_unlock, instance_pending
    use fy_proc, only: proc_pid, proc_run
    use fy_repo, only: repo_name_ok
    use fy_rewrite_git, only: rewrite_git_plan_t, rewrite_git_prepare, &
        rewrite_git_apply, rewrite_git_recover, rewrite_git_completed
    use fy_rewrite_store, only: rewrite_store
    use fy_sqlite, only: db_t, stmt_t, db_open, db_close, db_exec, db_prepare, &
        stmt_bind_text, stmt_step, stmt_text, stmt_finalize, SQLITE_ROW, SQLITE_DONE
    use fy_store, only: store_open, store_snapshot
    use fy_verify, only: verify_request
    implicit none
    private
    public :: history_rewrite, history_recover

contains

    subroutine history_rewrite(data_path, repo, bundle_path, expected_path, map_path, &
                               journal, status, message, check_only, journal_id)
        character(len=*), intent(in) :: data_path, repo, bundle_path, expected_path, map_path
        character(len=:), allocatable, intent(out) :: journal, message
        integer, intent(out) :: status
        logical, intent(in), optional :: check_only
        character(len=*), intent(in), optional :: journal_id
        character(len=:), allocatable :: data, bundle, expected, mapping
        integer :: fd
        logical :: preview

        journal = ''
        preview = .false.
        if (present(check_only)) preview = check_only
        if (present(journal_id)) then
            if (.not. valid_journal_id(journal_id)) then
                status = 1
                message = 'A journal ID must contain 1 to 96 lower-case hexadecimal digits or hyphens.'
                return
            end if
        end if
        call canonical(data_path, data, status, message)
        if (status /= 0) return
        if (.not. repo_name_ok(repo)) then
            status = 1
            message = 'Invalid repository name.'
            return
        end if
        call instance_lock(data, fd, status)
        if (status /= 0) then
            message = 'The instance is running or locked; stop only fortyard before maintenance.'
            return
        end if
        if (instance_pending(data)) then
            status = 1
            message = 'A rewrite needs recovery; use fy admin rewrite NAME --recover JOURNAL.'
        else
            call canonical(bundle_path, bundle, status, message)
            if (status == 0) call canonical(expected_path, expected, status, message)
            if (status == 0) call canonical(map_path, mapping, status, message)
            if (status == 0) call rewrite_locked(data, repo, bundle, expected, mapping, &
                                                 journal, status, message, preview, journal_id)
        end if
        call instance_unlock(fd)
    end subroutine history_rewrite

    subroutine rewrite_locked(data, repo, bundle, expected, mapping, journal, status, message, &
                              check_only, journal_id)
        character(len=*), intent(in) :: data, repo, bundle, expected, mapping
        character(len=:), allocatable, intent(out) :: journal, message
        integer, intent(out) :: status
        logical, intent(in) :: check_only
        character(len=*), intent(in), optional :: journal_id
        type(rewrite_git_plan_t) :: plan
        type(db_t) :: db
        character(len=40), allocatable :: old(:), new(:)
        character(len=:), allocatable :: out, why, head, detail
        character(len=48) :: unique
        integer(int64) :: clock, request, count, broken
        integer :: unit, k, found, rc, ignored
        logical :: opened, transaction, pending, committed, exists

        status = 1
        message = ''
        journal = ''
        opened = .false.
        transaction = .false.
        pending = .false.
        call load_map(mapping, old, new, status, message)
        if (status /= 0) return
        call proc_run([character(len=4096) :: 'mkdir', '-p', '-m', '700', '--', &
                      data//'/history-rewrites'], out, status, 4096)
        if (status /= 0) then
            message = 'Cannot create the private rewrite journal directory: '//out
            return
        end if
        if (present(journal_id)) then
            journal = data//'/history-rewrites/'//repo//'-'//journal_id
        else
            call system_clock(clock)
            write (unique, '(i0,a,i0)') proc_pid(), '-', clock
            journal = data//'/history-rewrites/'//repo//'-'//trim(unique)
        end if
        call rewrite_git_prepare(data//'/repos/'//repo//'.git', bundle, expected, &
                                 journal, plan, status, message)
        if (status /= 0) return
        call validate_targets(journal, new, status, message)
        if (status /= 0) return
        found = 0
        do k = 1, size(old)
            if (old(k) == plan%old_main) found = k
        end do
        if (found == 0) then
            status = 1
            message = 'The commit map does not include the previous main.'
            return
        end if
        if (new(found) /= plan%new_main) then
            status = 1
            message = 'The commit map and replacement main disagree.'
            return
        end if
        do
            if (check_only) then
                inquire (file=data//'/fortyard.db', exist=exists)
                if (.not. exists) then
                    status = 1
                    message = 'The preflight requires an existing instance database.'
                    exit
                end if
                call db_open(data//'/fortyard.db', db, status)
                if (status /= 0) message = 'Cannot open the existing instance database for preflight.'
            else
                call store_open(data//'/fortyard.db', db, status, message)
            end if
            if (status /= 0) exit
            opened = .true.
            call audit_verify(db, count, broken, why, head)
            if (broken /= 0) then
                status = 1
                message = 'The existing audit chain failed verification: '//why
                exit
            end if
            if (.not. primary(db, repo)) then
                status = 1
                message = 'Only a primary repository can be rewritten.'
                exit
            end if
            call store_snapshot(db, journal//'/fortyard-before.db', status)
            if (status /= 0) then
                message = 'Cannot back up the instance database.'
                exit
            end if
            call proc_run([character(len=4096) :: 'cp', '--', mapping, &
                          journal//'/commit-map'], out, status, 4096)
            if (status /= 0) then
                message = 'Cannot preserve the reviewed commit map: '//out
                exit
            end if
            open (newunit=unit, file=journal//'/instance', status='new', &
                  action='write', iostat=status)
            if (status /= 0) then
                message = 'Cannot record the rewrite instance.'
                exit
            end if
            detail = plan%old_main//' -> '//plan%new_main//'; journal '//journal
            write (unit, '(a)') data, repo, detail
            close (unit)
            call db_exec(db, 'BEGIN IMMEDIATE;', status)
            if (status /= 0) then
                message = 'Cannot start the administrative database transaction.'
                exit
            end if
            transaction = .true.
            call rewrite_store(db, repo, old, new, status, message)
            if (status /= 0) exit
            if (check_only) then
                call db_exec(db, 'ROLLBACK;', status)
                if (status /= 0) then
                    message = 'Cannot roll back the preflight database transaction.'
                    exit
                end if
                transaction = .false.
                message = 'History rewrite preflight passed; refs and source metadata are unchanged.'
                exit
            end if
            open (newunit=unit, file=data//'/history-rewrite.pending', status='new', &
                  action='write', iostat=status)
            if (status /= 0) then
                message = 'Cannot create the recovery marker.'
                exit
            end if
            write (unit, '(a)') journal
            close (unit)
            pending = .true.
            call rewrite_git_apply(plan, status, message)
            if (status /= 0) exit
            call db_exec(db, 'COMMIT;', status)
            if (status /= 0) then
                message = 'Cannot commit the rewritten source metadata.'
                exit
            end if
            transaction = .false.
            call audit_append(db, 'admin', 'history.rewrite', repo, detail, status)
            if (status /= 0) then
                message = 'Cannot record the administrative rewrite in the audit log.'
                exit
            end if
            request = verify_request(db, repo, plan%repo)
            if (request == 0) then
                status = 1
                message = 'Cannot queue a fresh verification of the rewritten main.'
                exit
            end if
            call audit_verify(db, count, broken, why, head)
            if (broken /= 0) then
                status = 1
                message = 'The audit chain failed verification: '//why
                exit
            end if
            call db_close(db)
            opened = .false.
            call remove_marker(data, status)
            if (status /= 0) then
                message = 'The rewrite succeeded but its recovery marker could not be removed.'
                exit
            end if
            pending = .false.
            status = 0
            message = 'History replaced; previous verification and review results were not inherited.'
            exit
        end do
        if (transaction) call db_exec(db, 'ROLLBACK;', ignored)
        if (opened) call db_close(db)
        if (pending) then
            why = message
            call recover_locked(data, repo, journal, rc, message, committed)
            if (rc == 0) then
                if (committed) then
                    message = 'History replaced; the committed rewrite was finalized from its journal.'
                    status = 0
                    return
                end if
                message = why//' The original refs and database were restored.'
            else
                message = why//' Recovery is still required: '//message
            end if
            status = 1
        end if
    end subroutine rewrite_locked

    subroutine history_recover(data_path, repo, journal_path, status, message)
        character(len=*), intent(in) :: data_path, repo, journal_path
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: data, journal
        integer :: fd

        call canonical(data_path, data, status, message)
        if (status /= 0) return
        call canonical(journal_path, journal, status, message)
        if (status /= 0) return
        call instance_lock(data, fd, status)
        if (status /= 0) then
            message = 'The instance is running or locked; stop only fortyard before recovery.'
            return
        end if
        call recover_locked(data, repo, journal, status, message)
        call instance_unlock(fd)
    end subroutine history_recover

    subroutine recover_locked(data, repo, journal, status, message, committed)
        character(len=*), intent(in) :: data, repo, journal
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        logical, intent(out), optional :: committed
        character(len=4096) :: recorded, instance, name, detail
        character(len=:), allocatable :: out, why, head
        type(db_t) :: db
        integer(int64) :: count, broken, request
        integer :: unit, lookup_status
        logical :: recorded_completion, marker_exists, exists

        status = 1
        if (present(committed)) committed = .false.
        message = 'The pending rewrite journal does not match this instance and repository.'
        if (.not. repo_name_ok(repo)) return
        inquire (file=data//'/history-rewrite.pending', exist=marker_exists)
        if (marker_exists) then
            open (newunit=unit, file=data//'/history-rewrite.pending', status='old', &
                  action='read', iostat=status)
            if (status /= 0) return
            read (unit, '(a)', iostat=status) recorded
            close (unit)
            if (status /= 0) return
            status = 1
            if (trim(recorded) /= journal) return
        end if
        open (newunit=unit, file=journal//'/instance', status='old', &
              action='read', iostat=status)
        if (status /= 0) return
        read (unit, '(a)', iostat=status) instance
        if (status == 0) read (unit, '(a)', iostat=status) name
        if (status == 0) read (unit, '(a)', iostat=status) detail
        close (unit)
        if (status /= 0) return
        status = 1
        if (trim(instance) /= data .or. trim(name) /= repo) return
        if (len_trim(detail) < len('; journal '//journal)) return
        if (index(trim(detail), '; journal '//journal, back=.true.) /= &
            len_trim(detail) - len('; journal '//journal) + 1) return
        if (marker_exists) then
            call store_open(data//'/fortyard.db', db, status, message)
        else
            inquire (file=data//'/fortyard.db', exist=exists)
            if (.not. exists) return
            call db_open(data//'/fortyard.db', db, status)
            if (status /= 0) message = 'Cannot open the existing instance database for recovery.'
        end if
        if (status /= 0) return
        recorded_completion = audit_complete(db, repo, trim(detail), lookup_status)
        if (lookup_status /= 0) then
            call db_close(db)
            status = 1
            message = 'Cannot determine whether the rewrite committed; no recovery state was changed.'
            return
        end if
        if (recorded_completion) then
            if (present(committed)) committed = .true.
            call rewrite_git_completed(data//'/repos/'//repo//'.git', journal, status, message)
            if (status == 0) then
                call audit_verify(db, count, broken, why, head)
                if (broken /= 0) then
                    status = 1
                    message = 'The committed rewrite audit chain failed verification: '//why
                end if
            end if
            if (status == 0) then
                request = verify_request(db, repo, data//'/repos/'//repo//'.git')
                if (request == 0) then
                    status = 1
                    message = 'The committed rewrite still needs a fresh verification request.'
                end if
            end if
            call db_close(db)
            if (status /= 0) return
            if (marker_exists) call remove_marker(data, status)
            if (status == 0) message = 'Committed rewrite finalized; the immutable audit log is preserved.'
            return
        end if
        call db_close(db)
        if (.not. marker_exists) then
            status = 1
            message = 'The journal has no committed audit event and no pending marker; nothing was changed.'
            return
        end if
        call rewrite_git_recover(data//'/repos/'//repo//'.git', journal, status, message)
        if (status /= 0) return
        call proc_run([character(len=4096) :: 'sh', '-c', &
            'test -f "$2/fortyard-before.db" && cp -- "$2/fortyard-before.db" "$1/fortyard.db.restore"'// &
            ' && rm -f -- "$1/fortyard.db-wal" "$1/fortyard.db-shm"'// &
            ' && mv -f -- "$1/fortyard.db.restore" "$1/fortyard.db"', &
            'restore-rewrite', data, journal], out, status, 4096)
        if (status /= 0) then
            message = 'Cannot restore the database; the recovery marker remains: '//out
            return
        end if
        call remove_marker(data, status)
        if (status /= 0) then
            message = 'Original state restored, but the recovery marker remains.'
            return
        end if
        message = 'Original refs and database restored; the immutable audit log is unchanged.'
    end subroutine recover_locked

    pure logical function valid_journal_id(value) result(valid)
        character(len=*), intent(in) :: value
        valid = len(value) >= 1 .and. len(value) <= 96
        if (valid) valid = verify(value, '0123456789abcdef-') == 0
    end function valid_journal_id

    subroutine canonical(path, resolved, status, message)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: resolved, message
        integer, intent(out) :: status
        character(len=:), allocatable :: out

        status = 1
        resolved = ''
        message = 'A maintenance path is empty, too long, or contains a control character.'
        if (len(path) == 0 .or. len(path) > 4000) return
        if (len_trim(path) /= len(path)) return
        if (index(path, achar(0)) > 0 .or. index(path, new_line('a')) > 0) return
        call proc_run([character(len=4096) :: 'realpath', '-e', '--', path], &
                      out, status, 4096)
        resolved = out
        if (len(resolved) > 0) then
            if (resolved(len(resolved):) == new_line('a')) resolved = resolved(:len(resolved) - 1)
        end if
        if (status == 0) then
            if (len(resolved) > 3000 .or. len_trim(resolved) /= len(resolved) .or. &
                index(resolved, achar(0)) > 0 .or. &
                index(resolved, new_line('a')) > 0) then
                status = 1
                message = 'The resolved maintenance path is too long or contains a control character.'
                return
            end if
        end if
        message = ''
        if (status /= 0) message = 'Cannot resolve an existing maintenance path: '//path
    end subroutine canonical

    subroutine load_map(path, old, new, status, message)
        character(len=*), intent(in) :: path
        character(len=40), allocatable, intent(out) :: old(:), new(:)
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        character(len=4096) :: line
        character(len=40) :: before, after
        integer :: unit, count, rc

        allocate (old(0), new(0))
        message = ''
        open (newunit=unit, file=path, status='old', action='read', iostat=status)
        if (status /= 0) then
            message = 'Cannot read the commit map.'
            return
        end if
        count = 0
        do
            read (unit, '(a)', iostat=rc) line
            if (rc < 0) exit
            if (rc /= 0) then
                status = 1
                exit
            end if
            if (len_trim(line) == 0) cycle
            if (line(1:4) == 'old ' .and. trim(adjustl(line(5:))) == 'new') cycle
            if (len_trim(line) /= 81) then
                status = 1
                exit
            end if
            before = line(1:40)
            after = line(42:81)
            if (line(41:41) /= ' ') then
                status = 1
                exit
            end if
            old = [old, before]
            new = [new, after]
            count = count + 1
        end do
        close (unit)
        if (status == 0 .and. count == 0) status = 1
        if (status /= 0) message = 'The commit map must contain old40 new40 rows.'
    end subroutine load_map

    subroutine validate_targets(journal, new, status, message)
        character(len=*), intent(in) :: journal, new(:)
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: out
        character(len=4096) :: line
        integer :: unit, i

        message = 'Every replacement commit must exist in the cleaned bundle.'
        open (newunit=unit, file=journal//'/map-targets', status='new', &
              action='write', iostat=status)
        if (status /= 0) return
        do i = 1, size(new)
            write (unit, '(a)', iostat=status) new(i)
            if (status /= 0) exit
        end do
        close (unit)
        if (status /= 0) return
        call proc_run([character(len=4096) :: 'sh', '-c', &
            'git --git-dir="$1/replacement.git" cat-file --batch-check'// &
            ' < "$1/map-targets" > "$1/map-types"', 'rewrite-targets', journal], &
            out, status, 4096)
        if (status /= 0) return
        open (newunit=unit, file=journal//'/map-types', status='old', &
              action='read', iostat=status)
        if (status /= 0) return
        do i = 1, size(new)
            read (unit, '(a)', iostat=status) line
            if (status /= 0) exit
            if (index(line, new(i)//' commit ') /= 1) then
                status = 1
                exit
            end if
        end do
        close (unit)
        if (status == 0) message = ''
    end subroutine validate_targets

    logical function primary(db, repo)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(stmt_t) :: statement
        integer :: status

        primary = .false.
        call db_prepare(db, 'SELECT mode FROM repos WHERE name=?1;', statement, status)
        if (status /= 0) return
        call stmt_bind_text(statement, 1, repo, status)
        if (stmt_step(statement) == SQLITE_ROW) primary = stmt_text(statement, 0) == 'primary'
        call stmt_finalize(statement)
    end function primary

    logical function audit_complete(db, repo, detail, status)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, detail
        type(stmt_t) :: statement
        integer, intent(out) :: status
        integer :: rc

        audit_complete = .false.
        call db_prepare(db, 'SELECT id FROM audit WHERE event=''history.rewrite'' '// &
            'AND subject=?1 AND detail=?2;', statement, status)
        if (status /= 0) return
        call stmt_bind_text(statement, 1, repo, status)
        call stmt_bind_text(statement, 2, detail, status)
        if (status /= 0) then
            call stmt_finalize(statement)
            return
        end if
        rc = stmt_step(statement)
        audit_complete = rc == SQLITE_ROW
        if (rc /= SQLITE_ROW .and. rc /= SQLITE_DONE) status = rc
        call stmt_finalize(statement)
    end function audit_complete

    subroutine remove_marker(data, status)
        character(len=*), intent(in) :: data
        integer, intent(out) :: status
        character(len=:), allocatable :: out

        call proc_run([character(len=4096) :: 'sync', '-f', data], out, status, 4096)
        if (status /= 0) return
        call proc_run([character(len=4096) :: 'rm', '--', data//'/history-rewrite.pending'], &
                      out, status, 4096)
        if (status /= 0) return
        call proc_run([character(len=4096) :: 'sync', '-f', data], out, status, 4096)
    end subroutine remove_marker

end module fy_history
