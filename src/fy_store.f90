module fy_store
    !! Schema, accounts and API tokens. Tokens are stored only as SHA-256
    !! hashes; the plaintext is shown once when created.
    use, intrinsic :: iso_fortran_env, only: &
        int64, &
        int8
    use fx_hash, only: &
        sha256_string
    use fy_migrations, only: &
        migration_t, &
        migrations_registry
    use fy_proc, only: &
        proc_sleep_ms
    use fy_sqlite, only: &
        SQLITE_AUTH, &
        SQLITE_BUSY, &
        SQLITE_DONE, &
        SQLITE_OK, &
        SQLITE_ROW, &
        db_deny_transaction, &
        db_errmsg, &
        db_exec, &
        db_open, &
        db_prepare, &
        db_t, &
        stmt_bind_int, &
        stmt_bind_text, &
        stmt_finalize, &
        stmt_int, &
        stmt_step, &
        stmt_t, &
        stmt_text
    implicit none
    private

    public :: &
        check_t, &
        landing_t, &
        random_token, &
        release_t, &
        repo_info_t, &
        store_account_find, &
        store_bootstrap, &
        store_check_add, &
        store_check_claim, &
        store_check_error, &
        store_check_finish, &
        store_check_get, &
        store_check_release, &
        store_job, &
        store_job_checks, &
        store_job_counts, &
        store_job_next, &
        store_jobs, &
        store_jobs_state, &
        store_landing_add, &
        store_landing_check, &
        store_landing_finish, &
        store_landing_list, &
        store_landing_next, &
        store_landing_requeue, &
        store_landing_take, &
        store_migrate, &
        store_migrate_named, &
        store_migrate_numbered, &
        store_open, &
        store_release_add, &
        store_release_list, &
        store_release_ready, &
        store_repo_add, &
        store_repo_fetched, &
        store_repo_list, &
        store_repo_set_mode, &
        store_repo_set_number, &
        store_schema_version, &
        store_snapshot, &
        store_token_create, &
        store_token_list, &
        store_token_lookup, &
        store_token_revoke, &
        store_token_revoke_hash, &
        token_identity_t, &
        token_info_t

    !! The last numbered migration. Frozen: later migrations have names
    !! (fy_migrations) and leave the version alone, so a release from before
    !! them still finds the version it knows.
    integer, parameter, public :: SCHEMA_VERSION = 15

    type :: token_identity_t
        logical :: found = .false.
        integer(int64) :: account_id = 0
        integer(int64) :: token_id = 0
        character(len=:), allocatable :: account
        character(len=:), allocatable :: token_name
        !! The name lists show for what this token does: its own, or that of the
        !! token a browser session was opened with.
        character(len=:), allocatable :: session
        !! The token itself, or the one its browser session was opened with.
        integer(int64) :: origin_id = 0
        character(len=:), allocatable :: scopes
    end type token_identity_t

    type :: token_info_t
        integer(int64) :: id = 0
        !! The token a browser session was opened with; 0 for any other token.
        integer(int64) :: parent = 0
        character(len=:), allocatable :: name, scopes, created_at, expires_at
        logical :: revoked = .false., expired = .false.
    end type token_info_t

    type :: repo_info_t
        character(len=:), allocatable :: name, upstream, fetched_at
        !! "mirror" (fetched from upstream) or "primary" (pushed to fortyard).
        character(len=:), allocatable :: mode
        !! Exit status of the last clone or fetch; -1 before the first.
        integer :: fetch_status = -1
        !! Issues that are open, for the repository row of the pages.
        integer :: open_issues = 0
    end type repo_info_t

    type :: release_t
        !! A build of a commit, uploaded as one archive; "open" until its file
        !! arrived and was verified, then "ready".
        character(len=:), allocatable :: version, commit, notes, uploader, created, state
        character(len=:), allocatable :: sha256
        integer(int64) :: size = 0
    end type release_t

    type :: landing_t
        !! A branch queued to land on main as one squashed commit.
        integer(int64) :: id = 0
        character(len=:), allocatable :: repo, branch, account, state, created, finished
        character(len=:), allocatable :: commit, log, started
        !! The change it lands (fy_change), or 0.
        integer(int64) :: change = 0
        !! "landing", or "verify" for a verification of main (fy_verify).
        character(len=:), allocatable :: kind
        !! What a job names of its change: the number in its repository and
        !! the title, and which of the entries of that change it is (0: none).
        integer :: number = 0, entry = 0, entries = 0
        character(len=:), allocatable :: title
        !! The name of the token that queued it; empty when not recorded.
        character(len=:), allocatable :: session
    end type landing_t

    type :: check_t
        !! One run of a job's tree on a runner, as the job shows it. Seconds
        !! are -1 while the check has not got that far.
        integer(int64) :: id = 0
        integer :: wait = -1, run = -1
        character(len=:), allocatable :: state, runner, created, claimed, finished
    end type check_t

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    !! How long a check waits for the runner that checked its repository last.
    character(len=*), parameter :: AFFINITY_SECONDS = '20'
    !! A job with the number and title of its change and its place among the
    !! entries of that change (job_read).
    character(len=*), parameter :: JOB_COLS = 'SELECT l.id, l.repo, l.branch, l.account,'// &
        " l.state, l.created_at, coalesce(l.started_at, ''), coalesce(l.finished_at, ''),"// &
        " coalesce(l.commit_id, ''), l.kind, coalesce(c.number, 0), coalesce(c.title, ''),"// &
        " (SELECT count(*) FROM landings e WHERE e.change = l.change AND e.kind = 'landing'"// &
        ' AND e.id <= l.id), (SELECT count(*) FROM landings e WHERE e.change = l.change'// &
        " AND e.kind = 'landing'), coalesce(l.session, '')"
    character(len=*), parameter :: JOB_FROM = ' FROM landings l LEFT JOIN changes c ON'// &
        ' c.id = l.change'

contains

    subroutine store_open(path, db, stat, message)
        character(len=*), intent(in) :: path
        type(db_t), intent(out) :: db
        integer, intent(out) :: stat
        !! Why a migration was refused; empty when SQLite has the reason.
        character(len=:), allocatable, intent(out), optional :: message
        integer(int64) :: started, now, rate

        if (present(message)) message = ''
        call db_open(path, db, stat)
        if (stat /= SQLITE_OK) return
        ! Processes that start on a new database at the same moment all switch
        ! its journal mode, and SQLite refuses the ones that lose at once,
        ! without asking the busy handler. They ask again, for as long as the
        ! busy handler would wait: a lock that is held longer is reported.
        call system_clock(started, rate)
        do
            call db_exec(db, 'PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;', stat)
            if (stat /= SQLITE_BUSY) exit
            call system_clock(now)
            if (now - started > 5 * rate) exit
            call proc_sleep_ms(1)
        end do
        if (stat /= SQLITE_OK) return
        call store_migrate(db, stat, message)
    end subroutine store_open

    subroutine store_snapshot(db, path, stat)
        !! A consistent copy of the database at `path`, written while the server
        !! runs (VACUUM INTO), for file-level backups.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: path
        integer, intent(out) :: stat
        integer :: ignored

        if (index(path, "'") > 0) then
            stat = 1
            return
        end if
        call execute_command_line('rm -f '''//path//'.new''', exitstat=ignored)
        call db_exec(db, "VACUUM INTO '"//path//".new';", stat)
        if (stat /= SQLITE_OK) return
        call execute_command_line('mv -f '''//path//'.new'' '''//path//'''', exitstat=stat)
    end subroutine store_snapshot

    subroutine store_migrate(db, stat, message)
        !! Forward-only, additive migrations, so the previous release still runs:
        !! the numbered ones up to SCHEMA_VERSION, then the named ones of the
        !! registry (fy_migrations). `message` says why a migration was refused.
        type(db_t), intent(in) :: db
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out), optional :: message
        type(migration_t), allocatable :: list(:)
        character(len=:), allocatable :: why

        why = ''
        call store_migrate_numbered(db, stat)
        if (stat == SQLITE_OK) then
            call migrations_registry(list)
            call store_migrate_named(db, list, stat, why)
        end if
        if (present(message)) message = why
    end subroutine store_migrate

    subroutine store_migrate_numbered(db, stat)
        !! The migrations from before there were named ones. Nothing is added
        !! here: the version stays at SCHEMA_VERSION, which is what a release
        !! from before named migrations expects to find.
        type(db_t), intent(in) :: db
        integer, intent(out) :: stat

        stat = SQLITE_OK
        ! Every worker and thread opens the store; none of them waits for the
        ! write lock once the schema is there.
        if (store_schema_version(db) >= SCHEMA_VERSION) return
        call db_exec(db, 'CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL);', &
            stat)
        if (stat /= SQLITE_OK) return
        call numbered(db, 1, &
            'CREATE TABLE accounts (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL,'// &
            ' kind TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1,'// &
            ' created_at TEXT NOT NULL);'// &
            'CREATE TABLE tokens (id INTEGER PRIMARY KEY,'// &
            ' account_id INTEGER NOT NULL REFERENCES accounts(id),'// &
            ' name TEXT NOT NULL, hash TEXT UNIQUE NOT NULL, scopes TEXT NOT NULL,'// &
            ' created_at TEXT NOT NULL, expires_at TEXT,'// &
            ' revoked INTEGER NOT NULL DEFAULT 0);', stat)
        if (stat /= SQLITE_OK) return
        call numbered(db, 2, &
            'CREATE TABLE repos (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL,'// &
            ' upstream TEXT NOT NULL, created_at TEXT NOT NULL, fetched_at TEXT,'// &
            ' fetch_status INTEGER);', stat)
        if (stat /= SQLITE_OK) return
        ! "mirror": fetched from upstream; "primary": pushed to fortyard.
        call numbered(db, 3, &
            "ALTER TABLE repos ADD COLUMN mode TEXT NOT NULL DEFAULT 'mirror';", stat)
        if (stat /= SQLITE_OK) return
        call numbered(db, 4, &
            'CREATE TABLE landings (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
            ' branch TEXT NOT NULL, account TEXT NOT NULL,'// &
            " state TEXT NOT NULL DEFAULT 'queued', created_at TEXT NOT NULL,"// &
            ' finished_at TEXT, commit_id TEXT, log TEXT);', stat)
        if (stat /= SQLITE_OK) return
        call numbered(db, 5, &
            'CREATE TABLE releases (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
            ' version TEXT NOT NULL, commit_id TEXT NOT NULL, notes TEXT NOT NULL,'// &
            ' uploader TEXT NOT NULL, created_at TEXT NOT NULL,'// &
            " state TEXT NOT NULL DEFAULT 'open', sha256 TEXT, size INTEGER,"// &
            ' UNIQUE (repo, version));', stat)
        if (stat /= SQLITE_OK) return
        call numbered(db, 6, 'ALTER TABLE landings ADD COLUMN started_at TEXT;', stat)
        if (stat /= SQLITE_OK) return
        ! Checks a runner performs for a landing: queued, running, passed, failed.
        call numbered(db, 7, &
            'CREATE TABLE checks (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
            " ref TEXT NOT NULL, state TEXT NOT NULL DEFAULT 'queued', runner TEXT,"// &
            ' log TEXT, created_at TEXT NOT NULL, claimed_at TEXT, finished_at TEXT);', stat)
        if (stat /= SQLITE_OK) return
        ! The lander process (1, 2, ...) that runs a landing.
        call numbered(db, 8, 'ALTER TABLE landings ADD COLUMN lander INTEGER;', stat)
        if (stat /= SQLITE_OK) return
        ! Changes (fy_change): one per reviewed branch, with its versions and
        ! comments; a landing may land a change.
        call numbered(db, 9, &
            'CREATE TABLE changes (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
            ' number INTEGER NOT NULL, change_key TEXT NOT NULL, branch TEXT NOT NULL,'// &
            ' title TEXT NOT NULL, description TEXT NOT NULL, author TEXT NOT NULL,'// &
            " state TEXT NOT NULL DEFAULT 'open', created_at TEXT NOT NULL,"// &
            ' updated_at TEXT NOT NULL, commit_id TEXT, UNIQUE (repo, number));'// &
            'CREATE TABLE change_versions (change INTEGER NOT NULL, version INTEGER NOT NULL,'// &
            ' head TEXT NOT NULL, base TEXT NOT NULL, pusher TEXT, created_at TEXT NOT NULL,'// &
            ' PRIMARY KEY (change, version));'// &
            'CREATE TABLE change_comments (id INTEGER PRIMARY KEY, change INTEGER NOT NULL,'// &
            ' account TEXT NOT NULL, body TEXT NOT NULL, created_at TEXT NOT NULL);'// &
            'ALTER TABLE landings ADD COLUMN change INTEGER;', stat)
        if (stat /= SQLITE_OK) return
        ! Issues (fy_issue), numbered with the changes of their repository.
        call numbered(db, 10, &
            'CREATE TABLE issues (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
            ' number INTEGER NOT NULL, title TEXT NOT NULL, body TEXT NOT NULL,'// &
            " author TEXT NOT NULL, state TEXT NOT NULL DEFAULT 'open',"// &
            ' created_at TEXT NOT NULL, updated_at TEXT NOT NULL, UNIQUE (repo, number));'// &
            'CREATE TABLE issue_comments (id INTEGER PRIMARY KEY, issue INTEGER NOT NULL,'// &
            ' account TEXT NOT NULL, body TEXT NOT NULL, created_at TEXT NOT NULL);', stat)
        if (stat /= SQLITE_OK) return
        ! Verifications of main are jobs too (kind 'verify', fy_verify), and
        ! their runner checks run .fortyard/verify; a regression found by
        ! bisection is reported once per culprit commit.
        call numbered(db, 11, &
            "ALTER TABLE landings ADD COLUMN kind TEXT NOT NULL DEFAULT 'landing';"// &
            "ALTER TABLE checks ADD COLUMN kind TEXT NOT NULL DEFAULT 'check';"// &
            'CREATE TABLE verify_reports (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
            " culprit TEXT NOT NULL, issue INTEGER NOT NULL, state TEXT NOT NULL DEFAULT 'open',"// &
            ' created_at TEXT NOT NULL, UNIQUE (repo, culprit));', stat)
        if (stat /= SQLITE_OK) return
        ! A repository can count its changes and issues from a set number,
        ! so a fork does not continue the numbers in its upstream's history.
        call numbered(db, 12, 'ALTER TABLE repos ADD COLUMN number_next INTEGER;', stat)
        if (stat /= SQLITE_OK) return
        ! Review state of a change (fy_review): NULL until reviewed, then
        ! 'ai-reviewed' or 'human-reviewed'; 'requested' asks for a review
        ! and 'failed' records one that gave no usable answer. Changes
        ! landed before reviews existed are not reviewed.
        call numbered(db, 13, 'ALTER TABLE changes ADD COLUMN review_state TEXT;'// &
            'ALTER TABLE changes ADD COLUMN flagged INTEGER NOT NULL DEFAULT 0;'// &
            'ALTER TABLE changes ADD COLUMN flag_reason TEXT;'// &
            "UPDATE changes SET review_state = 'none' WHERE state <> 'open';", stat)
        if (stat /= SQLITE_OK) return
        ! The audit log (fy_audit): each row carries the hash of the row
        ! before it. The triggers keep fortyard itself from changing a row;
        ! whoever removes them is found by the chain. audit_head names the
        ! newest row, which has no successor to give its removal away.
        call numbered(db, 14, &
            'CREATE TABLE audit (id INTEGER PRIMARY KEY, at TEXT NOT NULL,'// &
            ' actor TEXT NOT NULL, event TEXT NOT NULL, subject TEXT NOT NULL,'// &
            ' detail TEXT NOT NULL, prev TEXT NOT NULL, hash TEXT NOT NULL);'// &
            'CREATE TRIGGER audit_keeps_rows BEFORE DELETE ON audit BEGIN'// &
            " SELECT raise(ABORT, 'the audit log is append-only'); END;"// &
            'CREATE TRIGGER audit_keeps_content BEFORE UPDATE ON audit BEGIN'// &
            " SELECT raise(ABORT, 'the audit log is append-only'); END;"// &
            'CREATE TABLE audit_head (one INTEGER PRIMARY KEY CHECK (one = 1),'// &
            ' id INTEGER NOT NULL, hash TEXT NOT NULL);', stat)
        if (stat /= SQLITE_OK) return
        ! The policy of a repository (fy_policy). The defaults are what
        ! every repository did before there were policies.
        ! changes.reviewed_head: the head of the version a review read.
        call numbered(db, 15, &
            "ALTER TABLE repos ADD COLUMN policy_tests TEXT NOT NULL DEFAULT 'full';"// &
            "ALTER TABLE repos ADD COLUMN policy_review TEXT NOT NULL DEFAULT 'after';"// &
            'ALTER TABLE repos ADD COLUMN policy_bisect INTEGER NOT NULL DEFAULT 1;'// &
            'ALTER TABLE repos ADD COLUMN policy_batch INTEGER NOT NULL DEFAULT 8;'// &
            'ALTER TABLE repos ADD COLUMN policy_unreviewed_limit INTEGER NOT NULL DEFAULT 0;'// &
            'ALTER TABLE changes ADD COLUMN reviewed_head TEXT;', stat)
        if (stat /= SQLITE_OK) return
    end subroutine store_migrate_numbered

    subroutine numbered(db, version, sql, stat)
        !! One numbered migration with its version, in one transaction.
        type(db_t), intent(in) :: db
        integer, intent(in) :: version
        character(len=*), intent(in) :: sql
        integer, intent(out) :: stat
        character(len=12) :: digits
        integer :: ignored

        ! IMMEDIATE, and the version is read under the lock: of several
        ! processes that start on one database, one applies the step and the
        ! others find it done.
        call db_exec(db, 'BEGIN IMMEDIATE;', stat)
        if (stat /= SQLITE_OK) return
        if (store_schema_version(db) < version) then
            write (digits, '(i0)') version
            call db_exec(db, sql//'DELETE FROM schema_version;'// &
                'INSERT INTO schema_version VALUES ('//trim(digits)//');', stat)
        end if
        if (stat == SQLITE_OK) call db_exec(db, 'COMMIT;', stat)
        if (stat /= SQLITE_OK) call db_exec(db, 'ROLLBACK;', ignored)
    end subroutine numbered

    subroutine store_migrate_named(db, list, stat, message)
        !! Applies the migrations of `list` that the database does not have yet,
        !! in the order of the list, each in one transaction with the record of
        !! its name and the SHA-256 of its SQL. Refuses, before anything is
        !! applied, a name that is in the list twice and an applied name whose
        !! SQL has changed since. Applied names that the list does not have are
        !! left alone: they come from a newer release that was rolled back.
        type(db_t), intent(in) :: db
        type(migration_t), intent(in) :: list(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: recorded
        character(len=64), allocatable :: hash(:)
        integer :: i, k, ignored
        logical :: found, pending

        message = ''
        stat = SQLITE_OK
        allocate (hash(size(list)))
        pending = .false.
        do i = 1, size(list)
            do k = 1, i - 1
                if (list(k)%name == list(i)%name) then
                    call refuse('migration "'//list(i)%name//'" is defined twice in the registry')
                    return
                end if
            end do
            hash(i) = sha256_string(list(i)%sql)
            call migration_applied(db, list(i)%name, recorded, found)
            if (found .and. recorded /= hash(i)) then
                call refuse(changed(list(i)%name))
                return
            end if
            if (.not. found) pending = .true.
        end do
        ! The usual start: nothing to apply, and nothing written.
        if (.not. pending) return

        call db_exec(db, 'CREATE TABLE IF NOT EXISTS migrations (name TEXT PRIMARY KEY,'// &
            ' hash TEXT NOT NULL, applied_at TEXT NOT NULL);', stat)
        if (stat /= SQLITE_OK) then
            message = db_errmsg(db)
            return
        end if
        do i = 1, size(list)
            ! Looked up again under the write lock: another process that
            ! starts at the same time may have applied it by now.
            call db_exec(db, 'BEGIN IMMEDIATE;', stat)
            if (stat /= SQLITE_OK) then
                message = db_errmsg(db)
                return
            end if
            call migration_applied(db, list(i)%name, recorded, found)
            if (found) then
                call db_exec(db, 'COMMIT;', ignored)
                if (recorded == hash(i)) cycle
                call refuse(changed(list(i)%name))
                return
            end if
            ! A COMMIT in the SQL would apply it without its record.
            call db_deny_transaction(db, .true.)
            call db_exec(db, list(i)%sql, stat)
            if (stat == SQLITE_AUTH) message = ' (a migration has no BEGIN, COMMIT or ROLLBACK)'
            if (stat /= SQLITE_OK) message = db_errmsg(db)//message
            call db_deny_transaction(db, .false.)
            if (stat == SQLITE_OK) then
                call migration_record(db, list(i)%name, hash(i), stat)
                if (stat == SQLITE_OK) call db_exec(db, 'COMMIT;', stat)
                if (stat /= SQLITE_OK) message = db_errmsg(db)
            end if
            if (stat /= SQLITE_OK) then
                message = 'migration "'//list(i)%name//'": '//message
                call db_exec(db, 'ROLLBACK;', ignored)
                return
            end if
        end do

    contains

        subroutine refuse(why)
            character(len=*), intent(in) :: why

            stat = 1
            message = why
        end subroutine refuse

        function changed(name) result(why)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: why

            why = 'migration "'//name//'" was applied to this database with other SQL'// &
                ' than the registry has now; an applied migration is never edited,'// &
                ' a change to the schema is a new migration'
        end function changed

    end subroutine store_migrate_named

    subroutine migration_applied(db, name, hash, found)
        !! The recorded hash of an applied migration. A database without the
        !! table has none applied.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: hash
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        hash = ''
        found = .false.
        call db_prepare(db, 'SELECT hash FROM migrations WHERE name = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            found = .true.
            hash = stmt_text(st, 0)
        end if
        call stmt_finalize(st)
    end subroutine migration_applied

    subroutine migration_record(db, name, hash, stat)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name, hash
        integer, intent(out) :: stat
        type(stmt_t) :: st

        call db_prepare(db, 'INSERT INTO migrations (name, hash, applied_at) VALUES'// &
            ' (?1, ?2, '//NOW//');', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        call stmt_bind_text(st, 2, hash, stat)
        stat = stmt_step(st)
        if (stat == SQLITE_DONE) stat = SQLITE_OK
        call stmt_finalize(st)
    end subroutine migration_record

    function store_schema_version(db) result(version)
        type(db_t), intent(in) :: db
        integer :: version
        type(stmt_t) :: st
        integer :: stat

        version = 0
        call db_prepare(db, 'SELECT max(version) FROM schema_version;', st, stat)
        if (stat /= SQLITE_OK) return
        if (stmt_step(st) == SQLITE_ROW) version = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function store_schema_version

    subroutine store_bootstrap(db, token, stat)
        !! Creates the administrator account and its first token. Fails if an
        !! administrator already exists, so the bootstrap token is shown once.
        type(db_t), intent(in) :: db
        character(len=:), allocatable, intent(out) :: token
        integer, intent(out) :: stat
        type(stmt_t) :: st
        integer(int64) :: account_id

        token = ''
        call db_prepare(db, "INSERT INTO accounts (name, kind, created_at) VALUES"// &
            " ('admin', 'admin', "//NOW//") RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        if (stmt_step(st) /= SQLITE_ROW) then
            stat = 1
            call stmt_finalize(st)
            return
        end if
        account_id = stmt_int(st, 0)
        call stmt_finalize(st)
        call store_token_create(db, account_id, 'bootstrap', 'admin', token, stat)
    end subroutine store_bootstrap

    subroutine store_account_find(db, name, account_id, kind, found)
        !! An active account by name.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name
        integer(int64), intent(out) :: account_id
        character(len=:), allocatable, intent(out) :: kind
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        account_id = 0
        kind = ''
        found = .false.
        call db_prepare(db, 'SELECT id, kind FROM accounts WHERE name = ?1 AND active = 1;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            found = .true.
            account_id = stmt_int(st, 0)
            kind = stmt_text(st, 1)
        end if
        call stmt_finalize(st)
    end subroutine store_account_find

    subroutine store_token_create(db, account_id, name, scopes, token, stat, days, parent, unique)
        !! A token that expires after `days`, or never when absent. A browser
        !! session names as `parent` the token it was opened with and is valid
        !! only as long as that one. With `unique`, stat is 2 and nothing is
        !! created when the account has a token of that name in any case of its
        !! letters, revoked or not.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: account_id
        character(len=*), intent(in) :: name, scopes
        character(len=:), allocatable, intent(out) :: token
        integer, intent(out) :: stat
        integer, intent(in), optional :: days
        integer(int64), intent(in), optional :: parent
        logical, intent(in), optional :: unique
        type(stmt_t) :: st
        integer(int64) :: lifetime
        character(len=:), allocatable :: column, value, guard
        integer :: step

        lifetime = -1
        if (present(days)) lifetime = days
        ! Named only for a session, so a token can be made before the named
        ! migrations ran.
        column = ''
        value = ''
        if (present(parent)) then
            column = ', parent'
            value = ', ?6'
        end if
        ! In the statement, so two workers creating the same name cannot both pass.
        guard = ''
        if (present(unique)) then
            if (unique) guard = ' WHERE NOT EXISTS (SELECT 1 FROM tokens'// &
                ' WHERE account_id = ?1 AND lower(name) = lower(?2))'
        end if
        token = random_token()
        call db_prepare(db, 'INSERT INTO tokens (account_id, name, hash, scopes, created_at,'// &
            ' expires_at'//column//') SELECT ?1, ?2, ?3, ?4, '//NOW//', CASE WHEN ?5 < 0 THEN NULL'// &
            " ELSE strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '+' || ?5 || ' days') END"// &
            value//guard//' RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, account_id, stat)
        call stmt_bind_text(st, 2, name, stat)
        call stmt_bind_text(st, 3, sha256_string(token), stat)
        call stmt_bind_text(st, 4, scopes, stat)
        call stmt_bind_int(st, 5, lifetime, stat)
        if (present(parent)) call stmt_bind_int(st, 6, parent, stat)
        step = stmt_step(st)
        stat = SQLITE_OK
        if (step == SQLITE_DONE) then
            stat = 2
        else if (step /= SQLITE_ROW) then
            stat = 1
        end if
        call stmt_finalize(st)
        if (stat /= SQLITE_OK) token = ''
    end subroutine store_token_create

    subroutine store_token_lookup(db, token, who)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: token
        type(token_identity_t), intent(out) :: who
        type(stmt_t) :: st
        integer :: stat

        who%account = ''
        who%token_name = ''
        who%scopes = ''
        who%session = ''
        if (len(token) < 8 .or. len(token) > 128) return
        call db_prepare(db, 'SELECT a.id, a.name, t.name, t.scopes, t.id,'// &
            ' coalesce(p.name, t.name), coalesce(t.parent, t.id) FROM tokens t'// &
            ' JOIN accounts a ON a.id = t.account_id LEFT JOIN tokens p ON p.id = t.parent'// &
            ' WHERE t.hash = ?1 AND t.revoked = 0 AND a.active = 1'// &
            ' AND (t.expires_at IS NULL OR t.expires_at > '//NOW//')'// &
        ! A browser session ends with the token it was opened with.
            ' AND (t.parent IS NULL OR (p.revoked = 0 AND'// &
            ' (p.expires_at IS NULL OR p.expires_at > '//NOW//')));', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, sha256_string(token), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            who%found = .true.
            who%account_id = stmt_int(st, 0)
            who%account = stmt_text(st, 1)
            who%token_name = stmt_text(st, 2)
            who%scopes = stmt_text(st, 3)
            who%token_id = stmt_int(st, 4)
            who%session = stmt_text(st, 5)
            who%origin_id = stmt_int(st, 6)
        end if
        call stmt_finalize(st)
    end subroutine store_token_lookup

    subroutine store_token_list(db, account_id, tokens, stat)
        !! Tokens of one account, newest first; never their secrets.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: account_id
        type(token_info_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        type(token_info_t) :: t

        allocate (tokens(0))
        call db_prepare(db, 'SELECT id, name, scopes, created_at, coalesce(expires_at, ''''),'// &
            ' revoked, coalesce(expires_at <= '//NOW//', 0), coalesce(parent, 0)'// &
            ' FROM tokens WHERE account_id = ?1'// &
            ' ORDER BY id DESC;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, account_id, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            t%id = stmt_int(st, 0)
            t%name = stmt_text(st, 1)
            t%scopes = stmt_text(st, 2)
            t%created_at = stmt_text(st, 3)
            t%expires_at = stmt_text(st, 4)
            t%revoked = stmt_int(st, 5) /= 0
            t%expired = stmt_int(st, 6) /= 0
            t%parent = stmt_int(st, 7)
            tokens = [tokens, t]
        end do
        call stmt_finalize(st)
        stat = SQLITE_OK
    end subroutine store_token_list

    subroutine store_token_revoke(db, account_id, token_id, found)
        !! Revokes a token of this account; found is false for any other token.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: account_id, token_id
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'UPDATE tokens SET revoked = 1 WHERE id = ?1 AND account_id = ?2'// &
            ' RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, token_id, stat)
        call stmt_bind_int(st, 2, account_id, stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine store_token_revoke

    subroutine store_token_revoke_hash(db, token)
        !! Revokes the token with this plaintext, if any.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: token
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, 'UPDATE tokens SET revoked = 1 WHERE hash = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, sha256_string(token), stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_token_revoke_hash


    subroutine store_repo_add(db, name, upstream, stat, mode)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name, upstream
        integer, intent(out) :: stat
        character(len=*), intent(in), optional :: mode
        type(stmt_t) :: st

        call db_prepare(db, 'INSERT INTO repos (name, upstream, created_at, mode) VALUES (?1, ?2, '// &
            NOW//', ?3);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        call stmt_bind_text(st, 2, upstream, stat)
        if (present(mode)) then
            call stmt_bind_text(st, 3, mode, stat)
        else
            call stmt_bind_text(st, 3, 'mirror', stat)
        end if
        stat = SQLITE_OK
        if (stmt_step(st) /= SQLITE_DONE) stat = 1
        call stmt_finalize(st)
    end subroutine store_repo_add

    subroutine store_repo_list(db, repos, stat)
        type(db_t), intent(in) :: db
        type(repo_info_t), allocatable, intent(out) :: repos(:)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        type(repo_info_t) :: r

        allocate (repos(0))
        call db_prepare(db, "SELECT name, upstream, coalesce(fetched_at, ''),"// &
            ' coalesce(fetch_status, -1), mode, (SELECT count(*) FROM issues'// &
            " WHERE issues.repo = repos.name AND issues.state = 'open')"// &
            ' FROM repos ORDER BY name;', st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            r%name = stmt_text(st, 0)
            r%upstream = stmt_text(st, 1)
            r%fetched_at = stmt_text(st, 2)
            r%fetch_status = int(stmt_int(st, 3))
            r%mode = stmt_text(st, 4)
            r%open_issues = int(stmt_int(st, 5))
            repos = [repos, r]
        end do
        call stmt_finalize(st)
        stat = SQLITE_OK
    end subroutine store_repo_list

    subroutine store_repo_set_mode(db, name, mode, found)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name, mode
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'UPDATE repos SET mode = ?2 WHERE name = ?1 RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        call stmt_bind_text(st, 2, mode, stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine store_repo_set_mode

    subroutine store_repo_set_number(db, name, next, found)
        !! The repository's next change or issue takes the number `next`.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name
        integer, intent(in) :: next
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'UPDATE repos SET number_next = ?2 WHERE name = ?1 RETURNING id;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        call stmt_bind_int(st, 2, int(next, int64), stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine store_repo_set_number

    subroutine store_landing_add(db, repo, branch, account, id, stat, change, session)
        !! Queues the landing of `branch` on `main` of `repo`, of `change` if given,
        !! by the token named `session`.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, branch, account
        integer(int64), intent(in), optional :: change
        character(len=*), intent(in), optional :: session
        integer(int64), intent(out) :: id
        integer, intent(out) :: stat
        type(stmt_t) :: st

        id = 0
        call db_prepare(db, 'INSERT INTO landings (repo, branch, account, created_at, change,'// &
            ' session) VALUES (?1, ?2, ?3, '//NOW//", nullif(?4, 0), nullif(?5, '')) RETURNING id;", &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, branch, stat)
        call stmt_bind_text(st, 3, account, stat)
        if (present(change)) then
            call stmt_bind_int(st, 4, change, stat)
        else
            call stmt_bind_int(st, 4, 0_int64, stat)
        end if
        if (present(session)) then
            call stmt_bind_text(st, 5, session, stat)
        else
            call stmt_bind_text(st, 5, '', stat)
        end if
        stat = 1
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            stat = SQLITE_OK
        end if
        call stmt_finalize(st)
        if (stat == SQLITE_OK) call change_moved(db, id)
    end subroutine store_landing_add

    subroutine store_landing_next(db, lander, l, found)
        !! Takes the oldest queued landing of a repository with no running one
        !! (main moves one landing at a time) and marks it running by `lander`.
        type(db_t), intent(in) :: db
        integer, intent(in) :: lander
        type(landing_t), intent(out) :: l
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, "UPDATE landings SET state = 'running', lander = ?1, started_at = "// &
            NOW//" WHERE id = (SELECT min(id) FROM landings WHERE state = 'queued' AND"// &
            " kind = 'landing' AND repo NOT IN (SELECT repo FROM landings WHERE state = 'running'"// &
            " AND kind = 'landing'))"// &
            ' RETURNING id, repo, branch, account, created_at, coalesce(change, 0);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, int(lander, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            l%id = stmt_int(st, 0)
            l%repo = stmt_text(st, 1)
            l%branch = stmt_text(st, 2)
            l%account = stmt_text(st, 3)
            l%created = stmt_text(st, 4)
            l%change = stmt_int(st, 5)
            l%state = 'running'
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_landing_next

    subroutine store_landing_take(db, lander, repo, l, found)
        !! Takes the oldest queued landing of `repo` for `lander`, which already
        !! runs a landing of that repository, so both can share a candidate.
        type(db_t), intent(in) :: db
        integer, intent(in) :: lander
        character(len=*), intent(in) :: repo
        type(landing_t), intent(out) :: l
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, "UPDATE landings SET state = 'running', lander = ?1, started_at = "// &
            NOW//" WHERE id = (SELECT min(id) FROM landings WHERE state = 'queued' AND"// &
            " kind = 'landing' AND repo = ?2)"// &
            ' RETURNING id, repo, branch, account, created_at, coalesce(change, 0);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, int(lander, int64), stat)
        call stmt_bind_text(st, 2, repo, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            l%id = stmt_int(st, 0)
            l%repo = stmt_text(st, 1)
            l%branch = stmt_text(st, 2)
            l%account = stmt_text(st, 3)
            l%created = stmt_text(st, 4)
            l%change = stmt_int(st, 5)
            l%state = 'running'
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_landing_take

    subroutine store_landing_requeue(db, lander)
        !! Landings that were running when `lander` stopped run again; lander 1
        !! also takes those of a release before there were several landers.
        type(db_t), intent(in) :: db
        integer, intent(in) :: lander
        type(stmt_t) :: st
        integer :: stat
        call db_prepare(db, "UPDATE landings SET state = 'queued' WHERE state = 'running'"// &
            " AND kind = 'landing' AND (lander = ?1 OR (lander IS NULL AND ?1 = 1));", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, int(lander, int64), stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_landing_requeue

    subroutine store_landing_finish(db, id, state, commit, log)
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: state, commit, log
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, 'UPDATE landings SET state = ?2, commit_id = ?3, log = ?4,'// &
            ' finished_at = '//NOW//' WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_text(st, 3, commit, stat)
        call stmt_bind_text(st, 4, log, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
        call change_moved(db, id)
    end subroutine store_landing_finish

    subroutine change_moved(db, landing)
        !! An entry that is queued or has ended is activity of its change: the
        !! lists by activity show it first.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: landing
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, 'UPDATE changes SET updated_at = '//NOW// &
            ' WHERE id = (SELECT change FROM landings WHERE id = ?1);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, landing, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine change_moved

    subroutine store_landing_list(db, repo, ls, stat)
        !! The newest 50 landings of `repo`, newest first.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(landing_t), allocatable, intent(out) :: ls(:)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        type(landing_t) :: l

        allocate (ls(0))
        call db_prepare(db, "SELECT id, branch, account, state, created_at,"// &
            " coalesce(finished_at, ''), coalesce(commit_id, ''), coalesce(log, ''),"// &
            " coalesce(session, '') FROM landings WHERE repo = ?1 ORDER BY id DESC LIMIT 50;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            l%id = stmt_int(st, 0)
            l%repo = repo
            l%branch = stmt_text(st, 1)
            l%account = stmt_text(st, 2)
            l%state = stmt_text(st, 3)
            l%created = stmt_text(st, 4)
            l%finished = stmt_text(st, 5)
            l%commit = stmt_text(st, 6)
            l%log = stmt_text(st, 7)
            l%session = stmt_text(st, 8)
            ls = [ls, l]
        end do
        call stmt_finalize(st)
        stat = SQLITE_OK
    end subroutine store_landing_list

    subroutine store_jobs(db, repo, ls, stat, kind, state)
        !! Running and queued jobs, then the 40 newest finished ones, of `repo`
        !! or of all repositories when it is empty; without logs. `kind`
        !! ("landing", "test", "verify") and `state` keep the jobs of that kind
        !! and in that state; empty, or state "all", keeps all.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(landing_t), allocatable, intent(out) :: ls(:)
        integer, intent(out) :: stat
        character(len=*), intent(in), optional :: kind, state
        type(stmt_t) :: st
        type(landing_t) :: l
        character(len=*), parameter :: WHERE = " WHERE (?1 = '' OR l.repo = ?1) AND"// &
            " (?2 = '' OR l.kind = ?2) AND (?3 = '' OR l.state = ?3)"
        character(len=:), allocatable :: only_kind, only_state
        integer :: pass

        allocate (ls(0))
        only_kind = ''
        if (present(kind)) only_kind = kind
        only_state = ''
        if (present(state)) only_state = state
        if (only_state == 'all') only_state = ''
        do pass = 1, 2
            if (pass == 1) then
                call db_prepare(db, JOB_COLS//JOB_FROM//WHERE// &
                    " AND l.state IN ('running', 'queued') ORDER BY l.state = 'queued', l.id;", &
                    st, stat)
            else
                call db_prepare(db, JOB_COLS//JOB_FROM//WHERE// &
                    " AND l.state NOT IN ('running', 'queued') ORDER BY l.id DESC LIMIT 40;", &
                    st, stat)
            end if
            if (stat /= SQLITE_OK) return
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_text(st, 2, only_kind, stat)
            call stmt_bind_text(st, 3, only_state, stat)
            do while (stmt_step(st) == SQLITE_ROW)
                call job_read(st, l)
                l%log = ''
                ls = [ls, l]
            end do
            call stmt_finalize(st)
        end do
        stat = SQLITE_OK
    end subroutine store_jobs

    subroutine store_job(db, id, l, found)
        !! One job (a landing or a verification) with its log.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        type(landing_t), intent(out) :: l
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, JOB_COLS//", coalesce(l.log, '')"//JOB_FROM//' WHERE l.id = ?1;', &
            st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            call job_read(st, l)
            l%log = stmt_text(st, 15)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_job

    subroutine job_read(st, l)
        !! A row of JOB_COLS.
        type(stmt_t), intent(in) :: st
        type(landing_t), intent(inout) :: l

        l%id = stmt_int(st, 0)
        l%repo = stmt_text(st, 1)
        l%branch = stmt_text(st, 2)
        l%account = stmt_text(st, 3)
        l%state = stmt_text(st, 4)
        l%created = stmt_text(st, 5)
        l%started = stmt_text(st, 6)
        l%finished = stmt_text(st, 7)
        l%commit = stmt_text(st, 8)
        l%kind = stmt_text(st, 9)
        l%number = int(stmt_int(st, 10))
        l%title = stmt_text(st, 11)
        l%entry = int(stmt_int(st, 12))
        l%entries = int(stmt_int(st, 13))
        l%session = stmt_text(st, 14)
    end subroutine job_read

    subroutine store_job_checks(db, l, checks)
        !! The checks of job `l`, oldest first: one per attempt. A check names
        !! its job through its ref, whose last part is the job's number; the
        !! member of a candidate that is not its last names the candidate's
        !! check through its check_id.
        type(db_t), intent(in) :: db
        type(landing_t), intent(in) :: l
        type(check_t), allocatable, intent(out) :: checks(:)
        type(stmt_t) :: st
        type(check_t) :: c
        integer :: stat

        allocate (checks(0))
        ! A UNION, not an OR: with an OR SQLite reads every check of the repository.
        call db_prepare(db, "SELECT id, state, coalesce(runner, ''), created_at,"// &
            " coalesce(claimed_at, ''), coalesce(finished_at, ''),"// &
            " coalesce(CAST(strftime('%s', claimed_at) - strftime('%s', created_at)"// &
            " AS INTEGER), -1), coalesce(CAST(strftime('%s', finished_at) -"// &
            " strftime('%s', claimed_at) AS INTEGER), -1) FROM checks WHERE id IN"// &
            ' (SELECT id FROM checks WHERE repo = ?1 AND ref IN'// &
            " ('refs/fortyard/checks/' || ?2, 'refs/fortyard/queue/' || ?2,"// &
            " 'refs/fortyard/verify/' || ?2)"// &
            ' UNION SELECT check_id FROM landings WHERE id = ?2) ORDER BY id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, l%repo, stat)
        call stmt_bind_int(st, 2, l%id, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            c%id = stmt_int(st, 0)
            c%state = stmt_text(st, 1)
            c%runner = stmt_text(st, 2)
            c%created = stmt_text(st, 3)
            c%claimed = stmt_text(st, 4)
            c%finished = stmt_text(st, 5)
            c%wait = int(stmt_int(st, 6))
            c%run = int(stmt_int(st, 7))
            checks = [checks, c]
        end do
        call stmt_finalize(st)
    end subroutine store_job_checks

    subroutine store_job_next(db, id, next, state)
        !! The entry that followed entry `id` for the same change, with its
        !! state; 0 when `id` is the newest entry of its change.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        integer(int64), intent(out) :: next
        character(len=:), allocatable, intent(out) :: state
        type(stmt_t) :: st
        integer :: stat

        next = 0
        state = ''
        call db_prepare(db, 'SELECT n.id, n.state FROM landings l JOIN landings n ON'// &
            " n.change = l.change AND n.kind = 'landing' AND n.id > l.id WHERE l.id = ?1"// &
            " AND l.kind = 'landing' ORDER BY n.id LIMIT 1;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            next = stmt_int(st, 0)
            state = stmt_text(st, 1)
        end if
        call stmt_finalize(st)
    end subroutine store_job_next

    subroutine store_job_counts(db, names, counts, stat)
        !! Per repository: running, queued, landed and failed in the last 24
        !! hours, and the runner checks it holds now.
        type(db_t), intent(in) :: db
        character(len=64), allocatable, intent(out) :: names(:)
        integer, allocatable, intent(out) :: counts(:, :)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        character(len=64) :: nm
        integer :: n, k

        allocate (names(0), counts(5, 0))
        call db_prepare(db, "SELECT repo, sum(state = 'running'), sum(state = 'queued'),"// &
            " sum(state = 'landed' AND finished_at >= strftime('%Y-%m-%dT%H:%M:%SZ', 'now',"// &
            " '-1 day')), sum(state = 'failed' AND finished_at >= strftime('%Y-%m-%dT%H:%M:%SZ',"// &
            " 'now', '-1 day')), (SELECT count(*) FROM checks c WHERE c.state = 'running' AND"// &
            ' c.repo = landings.repo) FROM landings GROUP BY repo ORDER BY repo;', st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            n = size(names) + 1
            nm = stmt_text(st, 0)
            names = [names, nm]
            counts = reshape([counts, [(int(stmt_int(st, k)), k = 1, 5)]], [5, n])
        end do
        call stmt_finalize(st)
        stat = SQLITE_OK
    end subroutine store_job_counts

    function store_jobs_state(db) result(s)
        !! Changes whenever a landing is queued, starts or finishes.
        type(db_t), intent(in) :: db
        character(len=:), allocatable :: s
        type(stmt_t) :: st
        integer :: stat

        s = ''
        ! One aggregate each, so each reads an index end instead of every row.
        call db_prepare(db, "SELECT (SELECT count(*) FROM landings) || '.' ||"// &
            " coalesce((SELECT max(id) FROM landings), 0) || '.' ||"// &
            " coalesce((SELECT max(started_at) FROM landings), '') || '.' ||"// &
            " coalesce((SELECT max(finished_at) FROM landings), '');", st, stat)
        if (stat /= SQLITE_OK) return
        if (stmt_step(st) == SQLITE_ROW) s = stmt_text(st, 0)
        call stmt_finalize(st)
    end function store_jobs_state

    subroutine store_check_add(db, repo, ref, id, stat, kind)
        !! Queues a check of `ref` in `repo` for a runner: of a landing ("check",
        !! the default) or a verification of main ("verify").
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, ref
        integer(int64), intent(out) :: id
        integer, intent(out) :: stat
        character(len=*), intent(in), optional :: kind
        type(stmt_t) :: st

        id = 0
        call db_prepare(db, 'INSERT INTO checks (repo, ref, created_at, kind) VALUES (?1, ?2, '// &
            NOW//', ?3) RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, ref, stat)
        if (present(kind)) then
            call stmt_bind_text(st, 3, kind, stat)
        else
            call stmt_bind_text(st, 3, 'check', stat)
        end if
        stat = 1
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            stat = SQLITE_OK
        end if
        call stmt_finalize(st)
    end subroutine store_check_add

    subroutine store_landing_check(db, ids, check)
        !! Check `check` decides the landings `ids`, the members of a candidate.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: ids(:), check
        type(stmt_t) :: st
        integer :: stat, k

        do k = 1, size(ids)
            call db_prepare(db, 'UPDATE landings SET check_id = ?1 WHERE id = ?2;', st, stat)
            if (stat /= SQLITE_OK) return
            call stmt_bind_int(st, 1, check, stat)
            call stmt_bind_int(st, 2, ids(k), stat)
            stat = stmt_step(st)
            call stmt_finalize(st)
        end do
    end subroutine store_landing_check

    subroutine store_check_claim(db, runner, id, repo, ref, found, kind, limit)
        !! Gives `runner` the queued check that shares the runners fairly:
        !! landing checks before verifications, then the check of the
        !! repository that holds the fewest running checks, then the oldest.
        !! A check queued less than AFFINITY_SECONDS ago is left to the runner
        !! that served its repository last while that runner is free, so the
        !! build tree warm for it is used; a busy warm runner reserves nothing,
        !! so no check waits behind another repository's check. One statement,
        !! so concurrent claims never take the same check. `limit` is the
        !! check's time limit in seconds (0: none), fixed at the claim: six
        !! hours for a verification, else the repository's check_timeout.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: repo, ref
        logical, intent(out) :: found
        character(len=:), allocatable, intent(out), optional :: kind
        integer, intent(out), optional :: limit
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        id = 0
        repo = ''
        ref = ''
        if (present(limit)) limit = 0
        call db_prepare(db, "UPDATE checks SET state = 'running', runner = ?1, claimed_at = "// &
            NOW//", time_limit = CASE WHEN kind = 'verify' THEN 21600 ELSE coalesce((SELECT"// &
            ' 60*policy_check_timeout FROM repos WHERE name = checks.repo), 3600) END'// &
            " WHERE id = (SELECT c.id FROM checks c WHERE c.state = 'queued' AND NOT"// &
            ' (c.created_at > strftime('//"'%Y-%m-%dT%H:%M:%SZ', 'now',"// &
            " '-"//AFFINITY_SECONDS//" seconds') AND EXISTS (SELECT 1 FROM checks p WHERE p.id ="// &
            ' (SELECT q.id FROM checks q WHERE q.repo = c.repo AND q.runner IS NOT NULL ORDER BY'// &
            ' q.id DESC LIMIT 1) AND p.runner <> ?1 AND NOT EXISTS (SELECT 1 FROM checks b WHERE'// &
            " b.state = 'running' AND b.runner = p.runner)))"// &
            " ORDER BY c.kind = 'verify', (SELECT count(*) FROM checks r WHERE r.state ="// &
            " 'running' AND r.repo = c.repo), c.id LIMIT 1) RETURNING id, repo, ref, kind,"// &
            ' time_limit;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            repo = stmt_text(st, 1)
            ref = stmt_text(st, 2)
            if (present(kind)) kind = stmt_text(st, 3)
            if (present(limit)) limit = int(stmt_int(st, 4))
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_check_claim

    subroutine store_check_release(db, runner, count)
        !! Queues the running checks of `runner` again; `count` of them.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner
        integer, intent(out) :: count
        type(stmt_t) :: st
        integer :: stat

        count = 0
        call db_prepare(db, "UPDATE checks SET state = 'queued', runner = NULL, claimed_at = NULL,"// &
            ' queued_at = '//NOW//" WHERE state = 'running' AND runner = ?1 RETURNING id;", st, &
            stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            count = count + 1
        end do
        call stmt_finalize(st)
    end subroutine store_check_release

    subroutine store_check_error(db, id, log, timeout, state, found, runner, lost_grace)
        !! A running check ended without a verdict: its runner failed or was
        !! lost, or with `timeout` the check reached its time limit. It is
        !! queued again, except at its second timeout, which ends it in the
        !! state "timeout". `log` is appended to its log; `state` is the new one.
        !! With `runner`, only while that runner holds the check, so a runner
        !! taken as lost cannot speak for a later claim. With `lost_grace`,
        !! only when the claim is older than twice its time limit (a failure
        !! is run again from an empty build directory) plus `lost_grace`
        !! seconds: its runner is lost.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: log
        logical, intent(in) :: timeout
        character(len=:), allocatable, intent(out) :: state
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: runner
        integer, intent(in), optional :: lost_grace
        character(len=:), allocatable :: only
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        state = ''
        only = ''
        if (present(runner)) only = ' AND runner = ?4'
        if (present(lost_grace)) only = only//' AND coalesce(time_limit, 3600) > 0 AND'// &
            " CAST(strftime('%s', 'now') - strftime('%s', claimed_at) AS INTEGER) >"// &
            ' 2*coalesce(time_limit, 3600) + ?5'
        ! SET reads the row as it was, so `timeouts` there is the count before.
        call db_prepare(db, 'UPDATE checks SET timeouts = timeouts + ?2, log ='// &
            " coalesce(log || char(10), '') || ?3, state = CASE WHEN ?2 = 1 AND timeouts >= 1"// &
            " THEN 'timeout' ELSE 'queued' END, runner = CASE WHEN ?2 = 1 AND timeouts >= 1 THEN"// &
            ' runner END, claimed_at = CASE WHEN ?2 = 1 AND timeouts >= 1 THEN claimed_at END,'// &
            ' finished_at = CASE WHEN ?2 = 1 AND timeouts >= 1 THEN '//NOW//' END, queued_at ='// &
            ' '//NOW//" WHERE id = ?1 AND state = 'running'"//only//' RETURNING state;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_int(st, 2, int(merge(1, 0, timeout), int64), stat)
        call stmt_bind_text(st, 3, log, stat)
        if (present(runner)) call stmt_bind_text(st, 4, runner, stat)
        if (present(lost_grace)) call stmt_bind_int(st, 5, int(lost_grace, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            state = stmt_text(st, 0)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_check_error

    subroutine store_check_finish(db, id, runner, state, log, found)
        !! Records the result of a check that `runner` holds: "passed" or
        !! "failed". A runner whose claim was taken from it as lost finds
        !! nothing. The log follows what earlier runner errors left in it.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: runner, state, log
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, "UPDATE checks SET state = ?2, log = coalesce(log || char(10), '')"// &
            ' || ?3, finished_at = '//NOW//" WHERE id = ?1 AND state = 'running' AND runner = ?4"// &
            ' RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_text(st, 3, log, stat)
        call stmt_bind_text(st, 4, runner, stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine store_check_finish

    subroutine store_check_get(db, id, state, log, age, limit)
        !! State and log of a check, and seconds since it was queued, queued
        !! again or claimed; `limit`, the time limit in seconds its last claim
        !! was given (0: none).
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: state, log
        integer, intent(out) :: age
        integer, intent(out), optional :: limit
        type(stmt_t) :: st
        integer :: stat

        state = ''
        log = ''
        age = 0
        if (present(limit)) limit = 0
        call db_prepare(db, "SELECT state, coalesce(log, ''), CAST(strftime('%s', 'now') -"// &
            " strftime('%s', coalesce(claimed_at, queued_at, created_at)) AS INTEGER),"// &
            ' coalesce(time_limit, 3600) FROM checks WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            state = stmt_text(st, 0)
            log = stmt_text(st, 1)
            age = int(stmt_int(st, 2))
            if (present(limit)) limit = int(stmt_int(st, 3))
        end if
        call stmt_finalize(st)
    end subroutine store_check_get

    subroutine store_release_add(db, repo, version, commit, notes, uploader, stat)
        !! Opens release `version` of `repo`; its file is uploaded next.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, version, commit, notes, uploader
        integer, intent(out) :: stat
        type(stmt_t) :: st

        call db_prepare(db, 'INSERT INTO releases (repo, version, commit_id, notes, uploader,'// &
            ' created_at) VALUES (?1, ?2, ?3, ?4, ?5, '//NOW//');', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, version, stat)
        call stmt_bind_text(st, 3, commit, stat)
        call stmt_bind_text(st, 4, notes, stat)
        call stmt_bind_text(st, 5, uploader, stat)
        stat = SQLITE_OK
        if (stmt_step(st) /= SQLITE_DONE) stat = 1
        call stmt_finalize(st)
    end subroutine store_release_add

    subroutine store_release_ready(db, repo, version, sha256, size, found)
        !! Records the verified file of an open release and makes it available.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, version, sha256
        integer(int64), intent(in) :: size
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, "UPDATE releases SET state = 'ready', sha256 = ?3, size = ?4"// &
            " WHERE repo = ?1 AND version = ?2 AND state = 'open' RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, version, stat)
        call stmt_bind_text(st, 3, sha256, stat)
        call stmt_bind_int(st, 4, size, stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine store_release_ready

    subroutine store_release_list(db, repo, rs, stat)
        !! The newest 30 releases of `repo`, newest first.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(release_t), allocatable, intent(out) :: rs(:)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        type(release_t) :: r

        allocate (rs(0))
        call db_prepare(db, "SELECT version, commit_id, notes, uploader, created_at, state,"// &
            " coalesce(sha256, ''), coalesce(size, 0) FROM releases WHERE repo = ?1"// &
            ' ORDER BY id DESC LIMIT 30;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            r%version = stmt_text(st, 0)
            r%commit = stmt_text(st, 1)
            r%notes = stmt_text(st, 2)
            r%uploader = stmt_text(st, 3)
            r%created = stmt_text(st, 4)
            r%state = stmt_text(st, 5)
            r%sha256 = stmt_text(st, 6)
            r%size = stmt_int(st, 7)
            rs = [rs, r]
        end do
        call stmt_finalize(st)
        stat = SQLITE_OK
    end subroutine store_release_list

    subroutine store_repo_fetched(db, name, status)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name
        integer, intent(in) :: status
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, 'UPDATE repos SET fetched_at = '//NOW//', fetch_status = ?2'// &
            ' WHERE name = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        call stmt_bind_int(st, 2, int(status, int64), stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_repo_fetched

    function random_token() result(token)
        !! 256 random bits from the kernel, hex-encoded with a recognizable prefix.
        character(len=:), allocatable :: token
        integer(int8) :: bytes(32)
        integer :: u, ios, i, b
        character(len=*), parameter :: hex = '0123456789abcdef'

        open (newunit=u, file='/dev/urandom', access='stream', form='unformatted', &
            action='read', status='old', iostat=ios)
        if (ios /= 0) error stop 'fy: cannot read /dev/urandom'
        read (u, iostat=ios) bytes
        close (u)
        if (ios /= 0) error stop 'fy: cannot read /dev/urandom'
        allocate (character(len=3 + 64) :: token)
        token(1:3) = 'fy_'
        do i = 1, 32
            b = iand(int(bytes(i)), 255)
            token(2 + 2*i:2 + 2*i) = hex(b/16 + 1:b/16 + 1)
            token(3 + 2*i:3 + 2*i) = hex(mod(b, 16) + 1:mod(b, 16) + 1)
        end do
    end function random_token

end module fy_store
