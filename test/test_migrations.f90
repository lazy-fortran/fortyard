program test_migrations
    !! Named migrations: applied once and in the order of the registry, each
    !! with its record; a registry that contradicts the database is refused
    !! before anything is applied.
    use fx_hash, only: sha256_string
    use fy_migrations, only: migration_t, migration_add, migrations_registry
    use fy_proc, only: proc_fork, proc_exit, proc_wait, proc_kill, proc_sleep_ms, SIGTERM
    use fy_sqlite, only: db_t, stmt_t, db_open, db_close, db_exec, db_prepare, stmt_step, &
        stmt_int, stmt_finalize, SQLITE_ROW
    use fy_store, only: store_open, store_migrate_numbered, store_migrate_named, &
        store_schema_version, store_bootstrap, store_token_lookup, token_identity_t, &
        SCHEMA_VERSION
    implicit none

    character(len=*), parameter :: path = 'build/test_migrations.db'
    character(len=*), parameter :: locked = 'build/test_migrations.locked'
    integer, parameter :: procs = 8, rounds = 20
    type(db_t) :: db
    type(token_identity_t) :: who
    type(migration_t), allocatable :: registry(:), list(:), other(:)
    character(len=:), allocatable :: token, message
    integer :: stat, i, round, pid(procs), holder, status, applied, unit
    logical :: there

    call migrations_registry(registry)

    ! A fresh database gets the numbered migrations and the registry.
    call fresh(path)
    call store_open(path, db, stat)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION, &
        'a fresh database opens at the frozen version')
    call check(count('SELECT count(*) FROM migrations;') == size(registry), &
        'a fresh database has every migration of the registry')
    do i = 1, size(registry)
        call check(count("SELECT count(*) FROM migrations WHERE name = '"//registry(i)%name// &
            "' AND hash = '"//sha256_string(registry(i)%sql)//"';") == 1, &
            'recorded with the hash of its SQL: '//registry(i)%name)
    end do
    call db_close(db)

    ! A database as the release before named migrations left it: the numbered
    ! migrations, no record of names, and data.
    call fresh(path)
    call db_open(path, db, stat)
    call store_migrate_numbered(db, stat)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION, 'numbered migrations')
    call check(count("SELECT count(*) FROM sqlite_master WHERE name = 'migrations';") == 0, &
        'the numbered migrations record no names')
    call store_bootstrap(db, token, stat)
    call db_close(db)
    call store_open(path, db, stat)
    call check(stat == 0 .and. count('SELECT count(*) FROM migrations;') == size(registry), &
        'a database at the frozen version gets the named migrations')
    call store_token_lookup(db, token, who)
    call check(who%found .and. store_schema_version(db) == SCHEMA_VERSION, &
        'and keeps its data and its version')

    ! The order of the list is the order of application: the second needs
    ! the table of the first.
    allocate (list(0))
    call migration_add(list, 'test-notes', &
        'CREATE TABLE test_mig_notes (id INTEGER PRIMARY KEY); INSERT INTO test_mig_notes VALUES (1);')
    call migration_add(list, 'test-notes-text', 'ALTER TABLE test_mig_notes ADD COLUMN text TEXT;')
    call store_migrate_named(db, list, stat, message)
    call check(stat == 0 .and. len(message) == 0, 'migrations of a list apply')
    call check(count('SELECT count(text) FROM test_mig_notes;') == 0 .and. &
        count("SELECT (SELECT rowid FROM migrations WHERE name = 'test-notes') <"// &
        " (SELECT rowid FROM migrations WHERE name = 'test-notes-text');") == 1, &
        'in the order of the list')
    applied = count('SELECT count(*) FROM migrations;')

    ! Applied twice, nothing runs again: the INSERT would add a row and the
    ! CREATE TABLE would fail.
    call store_migrate_named(db, list, stat, message)
    call check(stat == 0 .and. count('SELECT count(*) FROM test_mig_notes;') == 1 .and. &
        count('SELECT count(*) FROM migrations;') == applied, 'applying twice changes nothing')
    call db_close(db)
    call store_open(path, db, stat)
    call check(stat == 0 .and. count('SELECT count(*) FROM migrations;') == applied, &
        'reopening changes nothing')

    ! A name that is in the list twice refuses, also for the entries before it.
    other = list
    call migration_add(other, 'test-first', 'CREATE TABLE test_mig_first (id INTEGER);')
    call migration_add(other, 'test-twice', 'CREATE TABLE test_mig_twice (id INTEGER);')
    call migration_add(other, 'test-twice', 'CREATE TABLE test_mig_twice_again (id INTEGER);')
    call store_migrate_named(db, other, stat, message)
    call check(stat /= 0 .and. index(message, '"test-twice" is defined twice') > 0, &
        'a duplicate name refuses to migrate: '//message)
    call check(count("SELECT count(*) FROM sqlite_master WHERE name IN ('test_mig_first', 'test_mig_twice',"// &
        " 'test_mig_twice_again');") == 0 .and. count('SELECT count(*) FROM migrations;') == applied, &
        'and leaves the database as it was')

    ! An applied migration whose SQL has changed refuses, also for the new
    ! entries before it.
    other = [migration_t ::]
    call migration_add(other, 'test-first', 'CREATE TABLE test_mig_first (id INTEGER);')
    call migration_add(other, 'test-notes', &
        'CREATE TABLE test_mig_notes (id INTEGER PRIMARY KEY);  INSERT INTO test_mig_notes VALUES (1);')
    call store_migrate_named(db, other, stat, message)
    call check(stat /= 0 .and. index(message, '"test-notes" was applied') > 0, &
        'a changed body of an applied migration refuses to migrate: '//message)
    call check(count("SELECT count(*) FROM sqlite_master WHERE name = 'test_mig_first';") == 0 .and. &
        count('SELECT count(*) FROM migrations;') == applied .and. &
        count("SELECT count(*) FROM migrations WHERE name = 'test-notes' AND hash = '"// &
        sha256_string(list(1)%sql)//"';") == 1, 'and leaves the database as it was')

    ! A migration that fails takes its earlier statements and its record
    ! with it; the one before it stays applied.
    other = [migration_t ::]
    call migration_add(other, 'test-first', 'CREATE TABLE test_mig_first (id INTEGER);')
    call migration_add(other, 'test-broken', &
        'CREATE TABLE test_mig_half (id INTEGER); ALTER TABLE no_such ADD COLUMN x TEXT;')
    call store_migrate_named(db, other, stat, message)
    call check(stat /= 0 .and. index(message, '"test-broken"') > 0, &
        'a migration that fails is reported by name: '//message)
    call check(count("SELECT count(*) FROM sqlite_master WHERE name = 'test_mig_half';") == 0 .and. &
        count("SELECT count(*) FROM migrations WHERE name = 'test-broken';") == 0, &
        'and is rolled back as a whole')
    call check(count("SELECT count(*) FROM sqlite_master WHERE name = 'test_mig_first';") == 1 .and. &
        count("SELECT count(*) FROM migrations WHERE name = 'test-first';") == 1, &
        'the migration before it stays applied')

    ! SQL that ends the transaction would be applied without its record.
    other = [migration_t ::]
    call migration_add(other, 'test-commit', &
        'CREATE TABLE test_mig_commit (id INTEGER); COMMIT;')
    call store_migrate_named(db, other, stat, message)
    call check(stat /= 0 .and. index(message, '"test-commit"') > 0 .and. &
        index(message, 'COMMIT') > 0, 'a migration with COMMIT is refused: '//message)
    call check(count("SELECT count(*) FROM sqlite_master WHERE name = 'test_mig_commit';") == 0 &
        .and. count("SELECT count(*) FROM migrations WHERE name = 'test-commit';") == 0, &
        'and neither applied nor recorded')

    ! The BEGIN and END of a trigger are not a transaction.
    other = [migration_t ::]
    call migration_add(other, 'test-trigger', &
        'CREATE TABLE test_mig_log (id INTEGER);'// &
        'CREATE TRIGGER test_mig_trigger AFTER INSERT ON test_mig_notes'// &
        ' BEGIN INSERT INTO test_mig_log VALUES (new.id); END;')
    call store_migrate_named(db, other, stat, message)
    call check(stat == 0 .and. &
        count("SELECT count(*) FROM migrations WHERE name = 'test-trigger';") == 1, &
        'a migration with a trigger applies: '//message)
    call db_close(db)

    ! Names the registry does not know were applied by a newer release that
    ! was rolled back; the store opens and leaves them.
    call store_open(path, db, stat)
    call check(stat == 0 .and. &
        count("SELECT count(*) FROM migrations WHERE name LIKE 'test-%';") == 4, &
        'a database with unknown applied names opens')
    call db_close(db)

    ! Processes that start on one fresh database at the same moment, as the
    ! workers of a server do, all open it, and each migration ran once.
    ! Several rounds: the races this guards against are lost only now and then.
    do round = 1, rounds
        call fresh(path)
        do i = 1, procs
            pid(i) = proc_fork()
            if (pid(i) == 0) then
                call store_open(path, db, stat)
                if (stat == 0) call store_migrate_named(db, list, stat, message)
                call db_close(db)
                call proc_exit(merge(0, 1, stat == 0))
            end if
        end do
        do i = 1, procs
            call proc_wait(pid(i), status)
            call check(status == 0, 'every process opens the fresh database')
        end do
        call store_open(path, db, stat)
        call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION .and. &
            count('SELECT count(*) FROM schema_version;') == 1, 'numbered migrations ran once')
        call check(count('SELECT count(*) FROM test_mig_notes;') == 1 .and. &
            count('SELECT count(*) FROM migrations;') == size(registry) + size(list), &
            'named migrations ran once')
        call db_close(db)
    end do

    ! A lock that another process keeps is reported when the busy handler
    ! gives up. The holder lets go after a minute, so waiting it out shows
    ! as a store that opened.
    call fresh(path)
    call execute_command_line('rm -f '//locked)
    holder = proc_fork()
    if (holder == 0) then
        call db_open(path, db, stat)
        call db_exec(db, 'BEGIN EXCLUSIVE;', stat)
        open (newunit=unit, file=locked, status='replace')
        close (unit)
        call proc_sleep_ms(60000)
        call proc_exit(0)
    end if
    there = .false.
    do while (.not. there)
        call proc_sleep_ms(10)
        inquire (file=locked, exist=there)
    end do
    call store_open(path, db, stat)
    call proc_kill(holder, SIGTERM)
    call proc_wait(holder, status)
    call check(stat /= 0, 'a database that stays locked fails to open')
    call db_close(db)
    call execute_command_line('rm -f '//locked)

contains

    subroutine fresh(file)
        character(len=*), intent(in) :: file

        call execute_command_line('rm -f '//file//' '//file//'-wal '//file//'-shm')
    end subroutine fresh

    function count(sql) result(n)
        !! The integer a query returns, or -1.
        character(len=*), intent(in) :: sql
        integer :: n
        type(stmt_t) :: st
        integer :: rc

        n = -1
        call db_prepare(db, sql, st, rc)
        if (rc /= 0) return
        if (stmt_step(st) == SQLITE_ROW) n = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function count

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_migrations
