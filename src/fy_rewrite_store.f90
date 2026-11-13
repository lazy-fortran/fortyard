module fy_rewrite_store
    use fy_sqlite, only: &
        SQLITE_DONE, SQLITE_OK, SQLITE_ROW, db_errmsg, db_exec, db_prepare, db_t, &
        stmt_bind_text, stmt_finalize, stmt_int, stmt_step, stmt_t, stmt_text
    implicit none
    private
    public :: rewrite_store, history_rewrite_lookup

contains

    subroutine rewrite_store(db, repo, old, new, status, message)
        ! The caller owns the administrative transaction.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, old(:), new(:)
        integer, intent(out) :: status
        character(len=:), allocatable, intent(out) :: message
        type(stmt_t) :: st
        integer :: i, j, rc

        status = 1
        message = ''
        if (size(old) /= size(new)) then
            message = 'the old and new commit lists have different lengths'
            return
        end if
        do i = 1, size(old)
            if (.not. commit_id(old(i)) .or. .not. commit_id(new(i))) then
                message = 'a commit map entry is not a nonzero full lower-case Git SHA-1'
                return
            end if
            do j = 1, i - 1
                if (old(i) == old(j)) then
                    message = 'the commit map repeats an old commit'
                    return
                end if
            end do
        end do
        call bound_query("SELECT count(*) FROM landings WHERE repo = ?1"// &
            " AND state IN ('queued', 'running');", i)
        if (status /= SQLITE_OK) return
        if (i > 0) then
            call refuse('the repository has queued or running jobs')
            return
        end if
        call bound_query("SELECT count(*) FROM checks WHERE repo = ?1"// &
            " AND state IN ('queued', 'running');", i)
        if (status /= SQLITE_OK) return
        if (i > 0) then
            call refuse('the repository has queued or running checks')
            return
        end if
        call db_exec(db, 'DROP TABLE IF EXISTS temp.history_rewrite_map;'// &
            'CREATE TEMP TABLE history_rewrite_map (old_id TEXT PRIMARY KEY, new_id TEXT NOT NULL);', &
            status)
        if (status /= SQLITE_OK) then
            message = db_errmsg(db)
            return
        end if
        do i = 1, size(old)
            call db_prepare(db, 'INSERT INTO temp.history_rewrite_map VALUES (?1, ?2);', st, status)
            if (status /= SQLITE_OK) exit
            call stmt_bind_text(st, 1, trim(old(i)), status)
            if (status == SQLITE_OK) call stmt_bind_text(st, 2, trim(new(i)), status)
            if (status == SQLITE_OK) then
                rc = stmt_step(st)
                if (rc /= SQLITE_DONE) status = rc
            end if
            call stmt_finalize(st)
            if (status /= SQLITE_OK) exit
        end do
        if (status /= SQLITE_OK) then
            message = db_errmsg(db)
            return
        end if
        call bound_query('SELECT count(*) FROM history_rewrites h'// &
            ' JOIN temp.history_rewrite_map m ON h.old_id = m.old_id'// &
            ' WHERE h.repo = ?1 AND h.new_id <> m.new_id;', i)
        if (status /= SQLITE_OK) return
        if (i > 0) then
            call refuse('an old commit already has a different recorded replacement')
            return
        end if
        call bound_query('WITH pointers(id) AS (SELECT commit_id FROM changes WHERE repo = ?1'// &
            ' UNION ALL SELECT v.head FROM change_versions v JOIN changes c ON c.id = v.change'// &
            ' WHERE c.repo = ?1 UNION ALL SELECT v.base FROM change_versions v'// &
            ' JOIN changes c ON c.id = v.change WHERE c.repo = ?1)'// &
            " SELECT count(*) FROM pointers p WHERE coalesce(p.id, '') <> '' AND NOT EXISTS"// &
            ' (SELECT 1 FROM temp.history_rewrite_map m WHERE m.old_id = p.id OR m.new_id = p.id);', i)
        if (status /= SQLITE_OK) return
        if (i > 0) then
            call refuse('the commit map omits a stored source pointer')
            return
        end if
        call db_exec(db, 'DELETE FROM temp.history_rewrite_map WHERE old_id = new_id;', status)
        if (status /= SQLITE_OK) then
            message = db_errmsg(db)
            return
        end if
        call bound_query('WITH RECURSIVE edges(old_id, new_id) AS ('// &
            ' SELECT old_id, new_id FROM history_rewrites WHERE repo = ?1'// &
            ' UNION SELECT old_id, new_id FROM temp.history_rewrite_map),'// &
            ' chain(old_id, new_id) AS (SELECT old_id, new_id FROM edges'// &
            ' UNION SELECT c.old_id, e.new_id FROM chain c JOIN edges e ON e.old_id = c.new_id)'// &
            ' SELECT count(*) FROM chain WHERE old_id = new_id;', i)
        if (status /= SQLITE_OK) return
        if (i > 0) then
            call refuse('the commit map creates a replacement cycle')
            return
        end if

        ! Completed checks and releases keep the identities of the source they read.
        call bound_exec("UPDATE changes SET review_state = 'requested' WHERE repo = ?1 AND ("// &
            ' commit_id IN (SELECT old_id FROM temp.history_rewrite_map) OR id IN'// &
            ' (SELECT change FROM change_versions WHERE head IN'// &
            ' (SELECT old_id FROM temp.history_rewrite_map) OR base IN'// &
            ' (SELECT old_id FROM temp.history_rewrite_map)));')
        if (status /= SQLITE_OK) return
        call bound_exec('UPDATE changes SET commit_id = (SELECT new_id FROM'// &
            ' temp.history_rewrite_map WHERE old_id = changes.commit_id)'// &
            ' WHERE repo = ?1 AND commit_id IN (SELECT old_id FROM temp.history_rewrite_map);')
        if (status /= SQLITE_OK) return
        call bound_exec('UPDATE change_versions SET head = coalesce((SELECT new_id FROM'// &
            ' temp.history_rewrite_map WHERE old_id = change_versions.head), head),'// &
            ' base = coalesce((SELECT new_id FROM temp.history_rewrite_map'// &
            ' WHERE old_id = change_versions.base), base)'// &
            ' WHERE change IN (SELECT id FROM changes WHERE repo = ?1);')
        if (status /= SQLITE_OK) return
        call bound_exec('INSERT OR IGNORE INTO history_rewrites (repo, old_id, new_id, rewritten_at)'// &
            " SELECT ?1, old_id, new_id, strftime('%Y-%m-%dT%H:%M:%SZ','now')"// &
            ' FROM temp.history_rewrite_map;')
        if (status /= SQLITE_OK) return
        call db_exec(db, 'DROP TABLE temp.history_rewrite_map;', status)
        if (status /= SQLITE_OK) message = db_errmsg(db)

    contains

        subroutine refuse(why)
            character(len=*), intent(in) :: why
            status = 1
            message = why
        end subroutine refuse

        subroutine bound_query(sql, value)
            character(len=*), intent(in) :: sql
            integer, intent(out) :: value
            type(stmt_t) :: query
            integer :: step

            value = 0
            call db_prepare(db, sql, query, status)
            if (status == SQLITE_OK) call stmt_bind_text(query, 1, repo, status)
            if (status == SQLITE_OK) then
                step = stmt_step(query)
                if (step == SQLITE_ROW) then
                    value = int(stmt_int(query, 0))
                else
                    status = step
                end if
            end if
            call stmt_finalize(query)
            if (status /= SQLITE_OK) message = db_errmsg(db)
        end subroutine bound_query

        subroutine bound_exec(sql)
            character(len=*), intent(in) :: sql
            type(stmt_t) :: query
            integer :: step

            call db_prepare(db, sql, query, status)
            if (status == SQLITE_OK) call stmt_bind_text(query, 1, repo, status)
            if (status == SQLITE_OK) then
                step = stmt_step(query)
                if (step /= SQLITE_DONE) status = step
            end if
            call stmt_finalize(query)
            if (status /= SQLITE_OK) message = db_errmsg(db)
        end subroutine bound_exec

    end subroutine rewrite_store

    subroutine history_rewrite_lookup(db, repo, old, new, found, status)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, old
        character(len=:), allocatable, intent(out) :: new
        logical, intent(out) :: found
        integer, intent(out), optional :: status
        type(stmt_t) :: st
        character(len=40) :: seen(257)
        character(len=:), allocatable :: next
        integer :: i, k, rc, step

        found = .false.
        new = old
        if (present(status)) status = 0
        if (.not. commit_id(old)) return
        do i = 1, size(seen)
            seen(i) = new
            call db_prepare(db, 'SELECT new_id FROM history_rewrites WHERE repo = ?1'// &
                ' AND old_id = ?2;', st, rc)
            if (rc == SQLITE_OK) call stmt_bind_text(st, 1, repo, rc)
            if (rc == SQLITE_OK) call stmt_bind_text(st, 2, new, rc)
            if (rc == SQLITE_OK) then
                step = stmt_step(st)
                if (step == SQLITE_DONE) then
                    call stmt_finalize(st)
                    found = i > 1
                    return
                else if (step == SQLITE_ROW) then
                    next = stmt_text(st, 0)
                else
                    rc = step
                end if
            end if
            call stmt_finalize(st)
            if (rc /= SQLITE_OK) exit
            if (.not. commit_id(next)) exit
            do k = 1, i
                if (next == seen(k)) exit
            end do
            if (k <= i) exit
            if (i == size(seen)) exit
            new = next
        end do
        ! Broken metadata never turns an old source link into an arbitrary target.
        new = old
        if (present(status)) status = 1
    end subroutine history_rewrite_lookup

    pure logical function commit_id(value)
        character(len=*), intent(in) :: value
        commit_id = len_trim(value) == 40
        if (.not. commit_id) return
        commit_id = verify(trim(value), '0123456789abcdef') == 0
        if (value == repeat('0', 40)) commit_id = .false.
    end function commit_id

end module fy_rewrite_store
