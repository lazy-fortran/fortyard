program test_history
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_audit, only: audit_append, audit_verify
    use fy_history, only: history_rewrite, history_recover
    use fy_instance_lock, only: instance_lock, instance_unlock, instance_pending
    use fy_proc, only: proc_run, proc_setenv
    use fy_rewrite_git, only: rewrite_git_plan_t, rewrite_git_prepare, rewrite_git_apply
    use fy_rewrite_store, only: history_rewrite_lookup
    use fy_sqlite, only: db_t, stmt_t, db_close, db_exec, db_prepare, &
        stmt_step, stmt_text, stmt_finalize, SQLITE_ROW
    use fy_store, only: store_open, store_repo_add, store_snapshot
    implicit none

    character(len=*), parameter :: root = 'build/test_history'
    character(len=*), parameter :: data_relative = root//'/instance with spaces'
    character(len=*), parameter :: ident = ' -c user.name=T -c user.email=t@example.org '
    type(db_t) :: db
    type(rewrite_git_plan_t) :: plan
    character(len=:), allocatable :: data, work, original, replacement, third, out
    character(len=:), allocatable :: journal, message, initial_audit, rewritten_audit, why, head
    character(len=:), allocatable :: preview_journal, before_metadata, before_refs, completed_journal
    character(len=:), allocatable :: resolved
    character(len=4096) :: cwd
    integer :: status, fd, unit
    integer(int64) :: count, broken
    logical :: found

    call proc_setenv('GIT_CONFIG_GLOBAL', '/dev/null')
    call proc_setenv('GIT_CONFIG_NOSYSTEM', '1')
    call getcwd(cwd)
    data = trim(cwd)//'/'//data_relative
    work = trim(cwd)//'/'//root
    call run('rm -rf '//root//' && mkdir -p "'//data//'/repos" && '// &
        'git init -q -b main '//root//'/source && '// &
        'printf keep > '//root//'/source/source.f90 && '// &
        'printf obsolete > '//root//'/source/old-output && '// &
        'git -C '//root//'/source add . && git -C '//root//'/source'//ident// &
        'commit -q -m original && git clone -q --bare '//root//'/source "'//data//'/repos/demo.git"')
    call run('git -C '//root//'/source rev-parse HEAD')
    original = chomp(out)
    call run('git -C '//root//'/source checkout -q --orphan clean && '// &
        'git -C '//root//'/source rm -q -rf . && printf keep > '//root//'/source/source.f90 && '// &
        'git -C '//root//'/source add . && git -C '//root//'/source'//ident// &
        'commit -q -m clean && git -C '//root//'/source branch -M main && '// &
        'git -C '//root//'/source bundle create "'//work//'/clean.bundle" --all && '// &
        'git --git-dir="'//data//'/repos/demo.git" show-ref > '//root//'/expected.refs')
    call run('git -C '//root//'/source rev-parse HEAD')
    replacement = chomp(out)
    call write_map(original, replacement)
    call store_open(data//'/fortyard.db', db, status)
    call check(status == 0, 'instance store opens')
    call store_repo_add(db, 'demo', '', status, mode='primary')
    call check(status == 0, 'primary repository recorded')
    call audit_append(db, 'admin', 'test.before', 'demo', 'original evidence', status)
    call check(status == 0, 'initial audit event')
    initial_audit = scalar('SELECT hash FROM audit_head;')
    call sql("INSERT INTO landings(repo,branch,account,state,created_at,commit_id,kind)"// &
        " VALUES ('demo','main','tester','passed','now','"//original//"','verify');")
    call sql("INSERT INTO changes(id,repo,number,change_key,branch,title,description,author,"// &
        "state,created_at,updated_at,commit_id,review_state) VALUES"// &
        " (1,'demo',1,'test','fix','test','test','tester','open','now','now','"//original// &
        "','human-reviewed');")
    call sql("INSERT INTO change_versions(change,version,head,base,created_at) VALUES"// &
        " (1,1,'"//original//"','"//original//"','now');")
    before_metadata = metadata()
    call db_close(db)

    call instance_lock(data, fd, status)
    call check(status == 0, 'simulated server owns instance')
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message)
    call check(status /= 0 .and. index(message, 'running or locked') > 0, &
        'a live instance cannot be rewritten')
    call instance_unlock(fd)
    call check(.not. instance_pending(data), 'failed preflight creates no pending marker')
    call history_rewrite(data//' ', 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message)
    call check(status /= 0, 'trailing blanks cannot select a different data directory')
    call proc_run([character(len=4096) :: 'mkdir', work//'/resolved'//new_line('a')//'path'], &
        out, status, 4096)
    call check(status == 0, 'resolved-path fixture directory')
    call proc_run([character(len=4096) :: 'ln', '-s', work//'/resolved'//new_line('a')//'path', &
        work//'/short-link'], out, status, 4096)
    call check(status == 0, 'resolved-path fixture symlink')
    call history_rewrite(work//'/short-link', 'demo', work//'/clean.bundle', &
        work//'/expected.refs', work//'/commit-map', journal, status, message)
    call check(status /= 0 .and. index(message, 'resolved maintenance path') > 0, &
        'a symlink cannot hide a newline in the instance path')

    open (newunit=unit, file=work//'/commit-map', position='append', status='old')
    write (unit, '(a)') repeat('c', 40)//' '//repeat('d', 40)
    close (unit)
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message)
    call check(status /= 0 .and. index(message, 'cleaned bundle') > 0, &
        'mapped destinations must exist as commits in the cleaned bundle')
    call check(.not. instance_pending(data), 'invalid commit map leaves no pending operation')
    call write_map(original, replacement)

    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true., journal_id='')
    call check(status /= 0, 'an explicitly empty journal ID refuses')
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true., journal_id='../bad')
    call check(status /= 0, 'journal IDs cannot traverse paths')
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true., journal_id=repeat('a', 97))
    call check(status /= 0, 'journal IDs cannot exceed 96 characters')
    call run('git --git-dir="'//data//'/repos/demo.git" show-ref')
    before_refs = out
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true., journal_id='deadbeef-01')
    call check(status == 0 .and. index(message, 'preflight passed') > 0, 'complete preview: '//message)
    call check(journal == data//'/history-rewrites/demo-deadbeef-01', 'chosen preview ID is exact')
    preview_journal = journal
    call check(.not. instance_pending(data), 'preview creates no pending marker')
    call run('git --git-dir="'//data//'/repos/demo.git" show-ref')
    call check(out == before_refs, 'preview preserves every repository ref')
    call store_open(data//'/fortyard.db', db, status)
    call check(metadata() == before_metadata, 'preview rolls back metadata, audit and verify requests')
    call sql("UPDATE change_versions SET base='"//repeat('c', 40)//"' WHERE change=1;")
    call db_close(db)
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true.)
    call check(status /= 0 .and. index(message, 'omits') > 0, 'preview checks all stored source pointers')
    call check(.not. instance_pending(data), 'a refused preview creates no marker')
    call store_open(data//'/fortyard.db', db, status)
    call sql("UPDATE change_versions SET base='"//original//"' WHERE change=1;")
    call check(metadata() == before_metadata, 'refused preview changes no source metadata')
    call db_close(db)
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true., journal_id='deadbeef-01')
    call check(status /= 0, 'an existing chosen journal cannot be reused')
    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, check_only=.true., journal_id=repeat('a', 96))
    call check(status == 0, 'the maximum journal ID length is accepted: '//message)
    call run('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1); "./$fy"'// &
        ' admin rewrite demo --data "'//data//'" --bundle "'//work//'/clean.bundle"'// &
        ' --expect-refs "'//work//'/expected.refs" --commit-map "'//work//'/commit-map"'// &
        ' --check --journal-id face-03')
    call check(index(out, 'preflight passed') > 0 .and. &
        index(out, data//'/history-rewrites/demo-face-03') > 0, 'the CLI forwards both preflight options')

    call history_rewrite(data, 'demo', work//'/clean.bundle', work//'/expected.refs', &
        work//'/commit-map', journal, status, message, journal_id='c0ffee-02')
    call check(status == 0, 'full leased rewrite: '//message)
    call check(journal == data//'/history-rewrites/demo-c0ffee-02', 'chosen rewrite journal ID is exact')
    completed_journal = journal
    call check(.not. instance_pending(data), 'successful rewrite clears recovery marker')
    call run('git --git-dir="'//data//'/repos/demo.git" rev-parse refs/heads/main')
    call check(chomp(out) == replacement, 'main uses cleaned history')
    call run('git --git-dir="'//data//'/repos/demo.git" ls-tree -r --name-only main')
    call check(index(out, 'old-output') == 0, 'obsolete file absent from cleaned source')
    call store_open(data//'/fortyard.db', db, status)
    call check(status == 0, 'rewritten store opens')
    call check(scalar("SELECT commit_id FROM landings WHERE state='passed';") == original, &
        'old passing evidence retains the exact original commit')
    call check(scalar("SELECT commit_id FROM landings WHERE state='queued';") == replacement, &
        'rewritten main queues its own verification')
    call check(scalar('SELECT hash FROM audit WHERE id=1;') == initial_audit, &
        'existing immutable audit event is untouched')
    call audit_verify(db, count, broken, why, head)
    call check(broken == 0 .and. count == 2, 'rewrite appends one valid audit event')
    rewritten_audit = head
    before_metadata = metadata()
    call db_close(db)

    call history_recover(data, 'demo', completed_journal, status, message)
    call check(status == 0, 'a completed journal finalizes without its cleared marker: '//message)
    call check(.not. instance_pending(data), 'marker-free completion creates no marker')
    call store_open(data//'/fortyard.db', db, status)
    call check(metadata() == before_metadata, 'marker-free recovery preserves audit and reuses verification')
    call db_close(db)
    call proc_run([character(len=16384) :: 'sh', '-c', &
        'fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1); "./$fy"'// &
        ' admin rewrite demo --data "'//data//'" --recover "'//completed_journal//'" --check 2>&1'], &
        out, status, 65536)
    call check(status /= 0 .and. index(out, 'cannot be combined') > 0, &
        'the CLI never performs recovery when a dry-run flag is also present')
    call history_recover(data, 'demo', preview_journal, status, message)
    call check(status /= 0, 'a journal without an audit event or marker cannot restore anything')
    call run('cp -a "'//completed_journal//'" "'//work//'/copied-journal"')
    call history_recover(data, 'demo', work//'/copied-journal', status, message)
    call check(status /= 0, 'a copied journal cannot borrow the completed journal audit event')
    call run('git --git-dir="'//data//'/repos/demo.git" update-ref refs/test/extra '//replacement)
    call history_recover(data, 'demo', completed_journal, status, message)
    call check(status /= 0, 'marker-free completion refuses a changed ref set')
    call run('git --git-dir="'//data//'/repos/demo.git" update-ref -d refs/test/extra')
    call store_open(data//'/fortyard.db', db, status)
    call check(metadata() == before_metadata, 'all marker-free refusals leave database state unchanged')
    call db_close(db)

    ! A crash after the audit commit must finalize, never truncate that audit event.
    call marker(journal)
    call history_recover(data, 'demo', journal, status, message)
    call check(status == 0, 'committed rewrite recovery: '//message)
    call store_open(data//'/fortyard.db', db, status)
    call audit_verify(db, count, broken, why, head)
    call check(broken == 0 .and. count == 2 .and. head == rewritten_audit, &
        'completion recovery retains the entire audit chain')
    call sql("UPDATE landings SET state='cancelled' WHERE state='queued';")
    call db_close(db)

    ! A crash before the audit commit must restore source and operational metadata.
    call run('printf second > '//root//'/source/source.f90 && '// &
        'git -C '//root//'/source add . && git -C '//root//'/source'//ident// &
        'commit -q -m second && git -C '//root//'/source bundle create "'//work// &
        '/second.bundle" --all && git --git-dir="'//data// &
        '/repos/demo.git" show-ref > '//root//'/second.refs')
    call run('git -C '//root//'/source rev-parse HEAD')
    third = chomp(out)
    call rewrite_git_prepare(data//'/repos/demo.git', work//'/second.bundle', &
        work//'/second.refs', work//'/interrupted', plan, status, message)
    call check(status == 0, 'interrupted fixture prepares: '//message)
    call store_open(data//'/fortyard.db', db, status)
    call store_snapshot(db, work//'/interrupted/fortyard-before.db', status)
    call check(status == 0, 'recovery snapshot')
    call sql("UPDATE repos SET upstream='interrupted' WHERE name='demo';")
    call db_close(db)
    open (newunit=unit, file=work//'/interrupted/instance', status='new')
    write (unit, '(a)') data, 'demo', replacement//' -> '//third//'; journal '//work//'/interrupted'
    close (unit)
    call marker(work//'/interrupted')
    call rewrite_git_apply(plan, status, message)
    call check(status == 0, 'interrupted fixture applies refs')
    call history_recover(data, 'demo', work//'/interrupted', status, message)
    call check(status == 0, 'uncommitted recovery: '//message)
    call run('git --git-dir="'//data//'/repos/demo.git" rev-parse refs/heads/main')
    call check(chomp(out) == replacement, 'uncommitted recovery restores original refs')
    call store_open(data//'/fortyard.db', db, status)
    call check(scalar("SELECT upstream FROM repos WHERE name='demo';") == '', &
        'uncommitted recovery restores operational metadata')
    call audit_verify(db, count, broken, why, head)
    call check(broken == 0 .and. count == 2 .and. head == rewritten_audit, &
        'uncommitted recovery also preserves the audit chain')
    call db_close(db)

    call write_map(replacement, third)
    call history_rewrite(data, 'demo', work//'/second.bundle', work//'/second.refs', &
        work//'/commit-map', journal, status, message, journal_id='c0ffee-04')
    call check(status == 0, 'a second full rewrite succeeds: '//message)
    call check(.not. instance_pending(data), 'the second rewrite also clears its marker')
    call run('git --git-dir="'//data//'/repos/demo.git" rev-parse refs/heads/main')
    call check(chomp(out) == third, 'the second replacement becomes main')
    call store_open(data//'/fortyard.db', db, status)
    call check(scalar("SELECT commit_id FROM landings WHERE state='passed';") == original, &
        'two rewrites keep the original passing evidence attached to its exact source')
    call check(scalar("SELECT commit_id FROM landings WHERE state='queued';") == third, &
        'the second replacement queues its own fresh verification')
    call check(scalar('SELECT hash FROM audit WHERE id=1;') == initial_audit .and. &
        scalar('SELECT hash FROM audit WHERE id=2;') == rewritten_audit, &
        'the second rewrite retains both earlier immutable audit events')
    call audit_verify(db, count, broken, why, head)
    call check(broken == 0 .and. count == 3, 'both rewrites form one valid audit chain')
    call history_rewrite_lookup(db, 'demo', original, resolved, found, status)
    call check(status == 0 .and. found .and. resolved == third, &
        'the original source identity follows both committed rewrites')
    call db_close(db)
    call run('rm -rf '//root)

contains

    function metadata() result(value)
        character(len=:), allocatable :: value
        value = scalar("SELECT commit_id || ':' || review_state || ':' ||"// &
            " (SELECT head || ':' || base FROM change_versions WHERE change=1) || ':' ||"// &
            " (SELECT count(*) FROM landings) || ':' || (SELECT count(*) FROM history_rewrites) ||"// &
            " ':' || (SELECT hash FROM audit_head) FROM changes WHERE id=1;")
    end function metadata

    subroutine run(command)
        character(len=*), intent(in) :: command
        call proc_run([character(len=16384) :: 'sh', '-c', command], out, status, 65536)
        call check(status == 0, 'fixture command: '//out)
    end subroutine run

    function chomp(text) result(value)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: value
        value = text
        if (len(value) > 0) then
            if (value(len(value):) == new_line('a')) value = value(:len(value) - 1)
        end if
    end function chomp

    subroutine check(ok, text)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: text
        if (ok) return
        print '(a)', text
        stop 1
    end subroutine check

    subroutine write_map(before, after)
        character(len=*), intent(in) :: before, after
        open (newunit=unit, file=work//'/commit-map', status='replace')
        write (unit, '(a)') 'old                                      new'
        write (unit, '(a)') before//' '//after
        close (unit)
    end subroutine write_map

    subroutine marker(path)
        character(len=*), intent(in) :: path
        open (newunit=unit, file=data//'/history-rewrite.pending', status='new')
        write (unit, '(a)') path
        close (unit)
    end subroutine marker

    subroutine sql(statement)
        character(len=*), intent(in) :: statement
        call db_exec(db, statement, status)
        call check(status == 0, 'fixture database update')
    end subroutine sql

    function scalar(query) result(value)
        character(len=*), intent(in) :: query
        character(len=:), allocatable :: value
        type(stmt_t) :: statement
        call db_prepare(db, query, statement, status)
        call check(status == 0, 'fixture query prepares')
        call check(stmt_step(statement) == SQLITE_ROW, 'fixture query has a row')
        value = stmt_text(statement, 0)
        call stmt_finalize(statement)
    end function scalar

end program test_history
