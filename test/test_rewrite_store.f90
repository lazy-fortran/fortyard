program test_rewrite_store
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_audit, only: audit_append, audit_verify
    use fy_rewrite_store, only: history_rewrite_lookup, rewrite_store
    use fy_sqlite, only: &
        SQLITE_ROW, db_close, db_exec, db_prepare, db_t, stmt_finalize, stmt_int, &
        stmt_step, stmt_t, stmt_text
    use fy_store, only: store_open
    implicit none

    character(len=*), parameter :: path = 'build/test_rewrite_store.db'
    character(len=40), parameter :: a = repeat('a', 40), b = repeat('b', 40), &
        c = repeat('c', 40), d = repeat('d', 40), e = repeat('e', 40), f = repeat('f', 40)
    type(db_t) :: db
    character(len=:), allocatable :: message, new, before, after, reason
    character(len=:), allocatable :: audit_head
    integer :: stat, ignored, i
    integer(int64) :: audited, broken
    logical :: found

    call execute_command_line('mkdir -p build; rm -f '//path//' '//path//'-wal '//path//'-shm')
    call store_open(path, db, stat)
    call check(stat == 0, 'fresh store migrates')
    call sql("INSERT INTO changes (id,repo,number,change_key,branch,title,description,author,"// &
        "state,created_at,updated_at,commit_id,review_state,reviewed_head) VALUES"// &
        " (1,'alpha',1,'one','fix','title','description','author','landed','now','now','"//a// &
        "','human-reviewed','"//b//"'),"// &
        " (2,'beta',1,'two','fix','title','description','author','landed','now','now','"//a// &
        "','human-reviewed','"//b//"'),"// &
        " (3,'alpha',2,'three','legacy','title','description','author','landed','now','now','"//a// &
        "','ai-reviewed',NULL),"// &
        " (4,'alpha',3,'four','base','title','description','author','landed','now','now','"//c// &
        "','human-reviewed','"//c//"');")
    call sql("INSERT INTO change_versions (change,version,head,base,pusher,created_at) VALUES"// &
        " (1,1,'"//a//"','"//b//"','author','now'),"// &
        " (2,1,'"//a//"','"//b//"','author','now'),"// &
        " (3,1,'"//a//"','"//b//"','author','now'),"// &
        " (4,1,'"//c//"','"//b//"','author','now');")
    call sql("INSERT INTO landings (repo,branch,account,state,created_at,commit_id,kind,log)"// &
        " VALUES ('alpha','main','author','passed','now','"//a//"','verify','original log');")
    call sql("INSERT INTO checks (repo,ref,state,created_at,log) VALUES"// &
        " ('alpha','"//a//"','passed','now','original check');")
    call sql("INSERT INTO verify_reports (repo,culprit,issue,created_at) VALUES"// &
        " ('alpha','"//a//"',1,'now');")
    call sql("INSERT INTO releases (repo,version,commit_id,notes,uploader,created_at,state) VALUES"// &
        " ('alpha','v1','"//a//"','original notes','author','now','published');")
    call audit_append(db, 'author', 'test.record', 'alpha', 'source '//a, stat)
    call check(stat == 0, 'audit evidence recorded')
    before = text('SELECT at || actor || event || subject || detail || prev || hash FROM audit;')

    call sql("INSERT INTO changes (id,repo,number,change_key,branch,title,description,author,"// &
        "state,created_at,updated_at,commit_id,review_state) VALUES"// &
        " (5,'partial',1,'partial','fix','title','description','author','open','now','now','"//a// &
        "','human-reviewed'),"// &
        " (6,'empty',1,'empty','fix','title','description','author','open','now','now',NULL,NULL);")
    call sql("INSERT INTO change_versions (change,version,head,base,pusher,created_at) VALUES"// &
        " (5,1,'"//b//"','"//c//"','author','now'), (6,1,'','','author','now');")
    call partial([b, c], [e, f], 'an omitted change commit refuses')
    call partial([a, c], [d, f], 'an omitted version head refuses')
    call partial([a, b], [d, e], 'an omitted version base refuses')
    call sql("UPDATE changes SET commit_id='"//d//"' WHERE id=5;")
    call sql("UPDATE change_versions SET head='"//d//"',base='"//e//"' WHERE change=5;")
    call sql('BEGIN IMMEDIATE;')
    call rewrite_store(db, 'partial', [a, b], [d, e], stat, message)
    call check(stat == 0, 'pointers already at map destinations are covered: '//message)
    call sql('COMMIT;')
    call check(text('SELECT review_state FROM changes WHERE id=5;') == 'human-reviewed', &
        'already rewritten source keeps its current review state')
    call sql('BEGIN IMMEDIATE;')
    call rewrite_store(db, 'empty', [a], [d], stat, message)
    call check(stat == 0, 'empty stored pointers need no map entry: '//message)
    call sql('COMMIT;')

    call sql('BEGIN IMMEDIATE;')
    call rewrite_store(db, 'alpha', [a, b, c], [d, e, c], stat, message)
    call check(stat == 0, 'source records remap: '//message)
    call sql('COMMIT;')
    call check(text('SELECT commit_id FROM changes WHERE id=1;') == d, 'landed source maps')
    call check(text('SELECT head FROM change_versions WHERE change=1;') == d, 'version head maps')
    call check(text('SELECT base FROM change_versions WHERE change=1;') == e, 'version base maps')
    call check(text('SELECT reviewed_head FROM changes WHERE id=1;') == b, 'review source stays old')
    call check(text('SELECT review_state FROM changes WHERE id=1;') == 'requested', &
        'new source requests a new review')
    call check(text('SELECT review_state FROM changes WHERE id=3;') == 'requested', &
        'legacy review without a source identity does not approve rewritten code')
    call check(text('SELECT review_state FROM changes WHERE id=4;') == 'requested' .and. &
        text('SELECT base FROM change_versions WHERE change=4;') == e, &
        'a rewritten diff base also needs a fresh review')
    call check(text('SELECT head FROM change_versions WHERE change=2;') == a .and. &
        text('SELECT commit_id FROM changes WHERE id=2;') == a, 'another repository stays untouched')
    call check(text('SELECT commit_id FROM landings;') == a .and. &
        text('SELECT state FROM landings;') == 'passed', 'completed verification retains its source')
    call check(text('SELECT ref FROM checks;') == a .and. text('SELECT state FROM checks;') == 'passed', &
        'completed check retains its source')
    call check(text('SELECT culprit FROM verify_reports;') == a, 'reported defect retains its source')
    call check(text('SELECT commit_id FROM releases;') == a, 'release retains its archived source')
    after = text('SELECT at || actor || event || subject || detail || prev || hash FROM audit;')
    call check(before == after, 'all immutable audit fields stay byte-identical')
    call audit_verify(db, audited, broken, reason, audit_head)
    call check(broken == 0_int64 .and. audited == 1_int64, 'audit chain stays valid')
    call history_rewrite_lookup(db, 'alpha', a, new, found, stat)
    call check(found .and. stat == 0 .and. new == d, 'old source link resolves')
    call history_rewrite_lookup(db, 'beta', a, new, found, stat)
    call check(.not. found .and. stat == 0 .and. new == a, 'aliases belong to one repository')

    call sql('BEGIN IMMEDIATE;')
    call rewrite_store(db, 'alpha', [d, e, c], [f, e, c], stat, message)
    call check(stat == 0, 'second rewrite: '//message)
    call sql('COMMIT;')
    call history_rewrite_lookup(db, 'alpha', a, new, found, stat)
    call check(found .and. stat == 0 .and. new == f, 'old link follows later rewrites')
    call check(text('SELECT commit_id FROM changes WHERE id=1;') == f, 'second source rewrite maps')
    call check(count('SELECT count(*) FROM landings WHERE commit_id='''//f//''';') == 0, &
        'the rewritten source has no inherited verification result')

    call rejected([a, a], [c, c], 'repeats', 'duplicate old commits refuse')
    call rejected([a], [c], 'different', 'conflicting replacements refuse')
    call rejected([a], [a], 'different', 'an identity cannot erase an earlier alias')
    call rejected([f, e, c], [a, e, c], 'cycle', 'combined replacement cycle refuses')
    call rejected([repeat('0', 40)], [c], 'SHA-1', 'pruned targets refuse')
    call rejected([a], [repeat('A', 40)], 'SHA-1', 'malformed commit ids refuse')
    call sql("INSERT INTO landings (repo,branch,account,state,created_at)"// &
        " VALUES ('alpha','fix','author','queued','now');")
    call rejected([f], [c], 'jobs', 'queued jobs prevent rewrite')
    call sql("UPDATE landings SET state='cancelled' WHERE state='queued';")
    call sql("INSERT INTO checks (repo,ref,state,created_at)"// &
        " VALUES ('alpha','refs/a','running','now');")
    call rejected([f], [c], 'checks', 'running checks prevent rewrite')
    call sql("UPDATE checks SET state='cancelled' WHERE state='running';")

    ! A corrupt table is rejected rather than hanging a source request.
    call sql("INSERT INTO history_rewrites VALUES ('broken','"//a//"','"//b//"','now'),"// &
        " ('broken','"//b//"','"//a//"','now');")
    call history_rewrite_lookup(db, 'broken', a, new, found, stat)
    call check(.not. found .and. stat /= 0 .and. new == a, 'lookup refuses corrupt cycles')
    call history_rewrite_lookup(db, 'alpha', 'main', new, found, stat)
    call check(.not. found .and. stat == 0 .and. new == 'main', 'ordinary refs have no alias')
    call sql('BEGIN IMMEDIATE;')
    do i = 1, 256
        call sql("INSERT INTO history_rewrites VALUES ('long','"//hex(i)//"','"// &
            hex(i + 1)//"','now');")
    end do
    call sql('COMMIT;')
    call history_rewrite_lookup(db, 'long', hex(1), new, found, stat)
    call check(found .and. stat == 0 .and. new == hex(257), '256 rewrites resolve')
    call sql("INSERT INTO history_rewrites VALUES ('long','"//hex(257)//"','"// &
        hex(258)//"','now');")
    call history_rewrite_lookup(db, 'long', hex(1), new, found, stat)
    call check(.not. found .and. stat /= 0 .and. new == hex(1), 'lookup bounds longer chains')
    call db_close(db)
    print '(a)', 'history rewrite metadata checks passed'

contains

    subroutine partial(old, new, label)
        character(len=*), intent(in) :: old(:), new(:), label
        integer :: prior
        prior = count('SELECT count(*) FROM history_rewrites;')
        call sql('BEGIN IMMEDIATE;')
        call rewrite_store(db, 'partial', old, new, stat, message)
        call check(stat /= 0 .and. index(message, 'omits') > 0, label//': '//message)
        call check(text('SELECT commit_id FROM changes WHERE id=5;') == a .and. &
            text('SELECT head FROM change_versions WHERE change=5;') == b .and. &
            text('SELECT base FROM change_versions WHERE change=5;') == c .and. &
            text('SELECT review_state FROM changes WHERE id=5;') == 'human-reviewed', &
            'partial-map refusal leaves all source and review fields unchanged before rollback')
        call check(count('SELECT count(*) FROM history_rewrites;') == prior, &
            'partial-map refusal leaves mappings unchanged before rollback')
        call db_exec(db, 'ROLLBACK;', ignored)
    end subroutine partial

    function hex(number) result(value)
        integer, intent(in) :: number
        character(len=40) :: value
        write (value, '(i40.40)') number
    end function hex

    subroutine rejected(old, new, expected, label)
        character(len=*), intent(in) :: old(:), new(:), expected, label
        integer :: prior
        prior = count('SELECT count(*) FROM history_rewrites;')
        call sql('BEGIN IMMEDIATE;')
        call rewrite_store(db, 'alpha', old, new, stat, message)
        call check(stat /= 0 .and. index(message, expected) > 0, label//': '//message)
        call check(count('SELECT count(*) FROM history_rewrites;') == prior, &
            'refusal leaves mappings unchanged before rollback')
        call check(text('SELECT commit_id FROM changes WHERE id=1;') == f, &
            'refusal leaves source unchanged before rollback')
        call db_exec(db, 'ROLLBACK;', ignored)
    end subroutine rejected

    subroutine sql(statement)
        character(len=*), intent(in) :: statement
        integer :: rc
        call db_exec(db, statement, rc)
        call check(rc == 0, 'test SQL succeeds')
    end subroutine sql

    function text(statement) result(value)
        character(len=*), intent(in) :: statement
        character(len=:), allocatable :: value
        type(stmt_t) :: st
        integer :: rc
        value = ''
        call db_prepare(db, statement, st, rc)
        call check(rc == 0, 'query prepares')
        call check(stmt_step(st) == SQLITE_ROW, 'query returns a row')
        value = stmt_text(st, 0)
        call stmt_finalize(st)
    end function text

    integer function count(statement) result(value)
        character(len=*), intent(in) :: statement
        type(stmt_t) :: st
        integer :: rc
        call db_prepare(db, statement, st, rc)
        call check(rc == 0, 'count prepares')
        call check(stmt_step(st) == SQLITE_ROW, 'count returns a row')
        value = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function count

    subroutine check(ok, label)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: label
        if (.not. ok) then
            print '(a)', 'FAIL: '//label
            error stop 1
        end if
    end subroutine check

end program test_rewrite_store
