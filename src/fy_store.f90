module fy_store
    !! Schema, accounts and API tokens. Tokens are stored only as SHA-256
    !! hashes; the plaintext is shown once when created.
    use, intrinsic :: iso_fortran_env, only: &
        int64, &
        int8
    use fx_hash, only: &
        sha256_string
    use fy_dispatch, only: &
        DISPATCH_SEEN, &
        dispatch_backend, &
        dispatch_candidates, &
        dispatch_max_minutes, &
        dispatch_max_running
    use fy_live, only: &
        LIVE_EARLIER
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
        store_check_cancel, &
        store_check_raced, &
        store_check_claim, &
        store_check_error, &
        store_check_finish, &
        store_check_get, &
        store_check_holder, &
        store_check_info, &
        store_check_jobs, &
        store_check_release, &
        store_check_take, &
        store_job, &
        store_job_checks, &
        store_job_counts, &
        store_job_next, &
        store_jobs, &
        store_jobs_state, &
        store_landing_add, &
        store_landing_cancel, &
        store_landing_cancel_all, &
        store_landing_check, &
        store_landing_finish, &
        store_landing_left, &
        store_landing_list, &
        store_landing_next, &
        store_landing_requeue, &
        store_landing_seal, &
        store_landing_state, &
        store_landing_take, &
        store_landing_unseal, &
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
        store_repo_set_public, &
        store_schema_version, &
        store_snapshot, &
        store_time_valid, &
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
        !! Queued with --urgent: forms a candidate alone, before the others.
        logical :: urgent = .false.
    end type landing_t

    type :: check_t
        !! One run of a job's tree on a runner, as the job shows it. Seconds
        !! are -1 while the check has not got that far.
        integer(int64) :: id = 0
        integer :: wait = -1, run = -1
        character(len=:), allocatable :: state, runner, created, claimed, finished
    end type check_t

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    !! A check's log with the new text ?3 first, as the live log of its last
    !! attempt had it, and what earlier attempts left after it.
    character(len=*), parameter :: LOG_FIRST = "?3 || CASE WHEN coalesce(log, '') = '' THEN ''"// &
        " ELSE CASE WHEN ?3 = '' OR substr(?3, -1) = char(10) THEN '' ELSE char(10) END || '"// &
        LIVE_EARLIER//"' || char(10) || log END"
    !! How long a check waits for the runner that checked its repository last.
    character(len=*), parameter :: AFFINITY_SECONDS = '20'
    !! Takes check ?3 (or the subquery after it) for runner ?1 of class ?2.
    character(len=*), parameter :: TAKE = "UPDATE checks SET state = 'running', runner = ?1,"// &
        ' ran_class = ?2, claimed_at = '//NOW//', started_at = '//NOW//', time_limit = CASE'// &
        " WHEN kind IN ('verify', 'nightly') THEN 21600 ELSE coalesce((SELECT 60*policy_check_timeout FROM"// &
        ' repos WHERE name = checks.repo), 3600) END WHERE id = '
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

    subroutine store_repo_set_public(db, name, public, found, refused)
        !! Marks repository `name` public or private. A repository whose
        !! policy names backends stays public (`refused`): they would put a
        !! private repository's code on another system.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: name
        logical, intent(in) :: public
        logical, intent(out) :: found, refused
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        refused = .false.
        call db_prepare(db, "SELECT policy_backends <> '' FROM repos WHERE name = ?1;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            found = .true.
            refused = .not. public .and. stmt_int(st, 0) /= 0
        end if
        call stmt_finalize(st)
        if (.not. found .or. refused) return
        call db_prepare(db, 'UPDATE repos SET public = ?2 WHERE name = ?1 AND (?2 = 1 OR'// &
            " policy_backends = '') RETURNING id;", st, stat)
        found = .false.
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, name, stat)
        call stmt_bind_int(st, 2, int(merge(1, 0, public), int64), stat)
        found = stmt_step(st) == SQLITE_ROW
        refused = .not. found
        call stmt_finalize(st)
    end subroutine store_repo_set_public

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

    subroutine store_landing_add(db, repo, branch, account, id, stat, change, session, urgent)
        !! Queues the landing of `branch` on `main` of `repo`, of `change` if given,
        !! by the token named `session`; an `urgent` one goes first.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, branch, account
        integer(int64), intent(in), optional :: change
        character(len=*), intent(in), optional :: session
        logical, intent(in), optional :: urgent
        integer(int64), intent(out) :: id
        integer, intent(out) :: stat
        type(stmt_t) :: st
        integer(int64) :: first

        id = 0
        first = 0
        if (present(urgent)) first = merge(1_int64, 0_int64, urgent)
        call db_prepare(db, 'INSERT INTO landings (repo, branch, account, created_at, change,'// &
            ' session, urgent) VALUES (?1, ?2, ?3, '//NOW//", nullif(?4, 0), nullif(?5, ''), ?6)"// &
            ' RETURNING id;', st, stat)
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
        call stmt_bind_int(st, 6, first, stat)
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
        !! (main moves one landing at a time), an urgent one first, and marks it
        !! running by `lander`.
        type(db_t), intent(in) :: db
        integer, intent(in) :: lander
        type(landing_t), intent(out) :: l
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, "UPDATE landings SET state = 'running', lander = ?1, started_at = "// &
            NOW//", sealed = 0 WHERE id = (SELECT id FROM landings WHERE state = 'queued' AND"// &
            " kind = 'landing' AND repo NOT IN (SELECT repo FROM landings WHERE state = 'running'"// &
            " AND kind = 'landing') ORDER BY urgent DESC, id LIMIT 1)"// &
            ' RETURNING id, repo, branch, account, created_at, coalesce(change, 0), urgent;', st, &
            stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, int(lander, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            l%id = stmt_int(st, 0)
            l%repo = stmt_text(st, 1)
            l%branch = stmt_text(st, 2)
            l%account = stmt_text(st, 3)
            l%created = stmt_text(st, 4)
            l%change = stmt_int(st, 5)
            l%urgent = stmt_int(st, 6) /= 0
            l%state = 'running'
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_landing_next

    subroutine store_landing_take(db, lander, repo, l, found)
        !! Takes the oldest queued landing of `repo` for `lander`, which already
        !! runs a landing of that repository, so both can share a candidate. An
        !! urgent landing forms a candidate of its own and is not taken here.
        type(db_t), intent(in) :: db
        integer, intent(in) :: lander
        character(len=*), intent(in) :: repo
        type(landing_t), intent(out) :: l
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, "UPDATE landings SET state = 'running', lander = ?1, started_at = "// &
            NOW//", sealed = 0 WHERE id = (SELECT min(id) FROM landings WHERE state = 'queued'"// &
            " AND kind = 'landing' AND repo = ?2 AND urgent = 0)"// &
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

    subroutine store_landing_left(db, lander, ls, checks)
        !! What a stopped `lander` left: the landings it was running (lander 1
        !! also those of a release before there were several landers) and the
        !! other members, in any state, of the candidates whose checks they
        !! share, oldest first, which is their order in the candidate.
        !! `checks(k)` is the check that decides `ls(k)`, 0 when none.
        type(db_t), intent(in) :: db
        integer, intent(in) :: lander
        type(landing_t), allocatable, intent(out) :: ls(:)
        integer(int64), allocatable, intent(out) :: checks(:)
        type(stmt_t) :: st
        type(landing_t) :: l
        integer :: stat

        allocate (ls(0), checks(0))
        call db_prepare(db, "SELECT id, repo, branch, account, created_at, coalesce(change, 0),"// &
            ' urgent, state, coalesce(check_id, 0) FROM landings WHERE id IN (SELECT id FROM'// &
            " landings WHERE state = 'running' AND kind = 'landing' AND (lander = ?1 OR"// &
            ' (lander IS NULL AND ?1 = 1)) UNION SELECT m.id FROM landings r JOIN landings m'// &
            " ON m.check_id = r.check_id WHERE r.state = 'running' AND r.kind = 'landing' AND"// &
            " (r.lander = ?1 OR (r.lander IS NULL AND ?1 = 1))) ORDER BY id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, int(lander, int64), stat)
        do while (stmt_step(st) == SQLITE_ROW)
            l%id = stmt_int(st, 0)
            l%repo = stmt_text(st, 1)
            l%branch = stmt_text(st, 2)
            l%account = stmt_text(st, 3)
            l%created = stmt_text(st, 4)
            l%change = stmt_int(st, 5)
            l%urgent = stmt_int(st, 6) /= 0
            l%state = stmt_text(st, 7)
            ls = [ls, l]
            checks = [checks, stmt_int(st, 8)]
        end do
        call stmt_finalize(st)
    end subroutine store_landing_left

    subroutine store_landing_finish(db, id, state, commit, log, found)
        !! Ends landing `id` in `state`. A cancelled landing stays cancelled
        !! (`found` false) unless it landed: what is on main has landed.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: state, commit, log
        logical, intent(out), optional :: found
        type(stmt_t) :: st
        integer :: stat

        if (present(found)) found = .false.
        call db_prepare(db, 'UPDATE landings SET state = ?2, commit_id = ?3, log = ?4,'// &
            ' finished_at = '//NOW//", sealed = 0 WHERE id = ?1 AND (state <> 'cancelled'"// &
            " OR ?2 = 'landed') RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_text(st, 3, commit, stat)
        call stmt_bind_text(st, 4, log, stat)
        if (stmt_step(st) == SQLITE_ROW .and. present(found)) found = .true.
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

    subroutine store_landing_cancel(db, repo, id, account, state, found, cancelled, verify)
        !! Cancels landing `id` of `repo` for `account` when it is queued, or
        !! running and not sealed; its log says who cancelled it and when. The
        !! lander drops a running one at its next step. With `verify` `id` may
        !! also be a verification of main: its checks are withdrawn at once,
        !! and the verifier goes on as if it had ended. `state` is the
        !! landing's state afterwards; `found` is false when `repo` has no
        !! landing `id`, `cancelled` whether this call cancelled it.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, account
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: state
        logical, intent(out) :: found
        logical, intent(out) :: cancelled
        logical, intent(in), optional :: verify
        character(len=:), allocatable :: kinds
        character(len=24) :: num
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        state = ''
        cancelled = .false.
        kinds = "kind = 'landing'"
        if (present(verify)) then
            if (verify) kinds = "kind IN ('landing', 'verify')"
        end if
        call db_prepare(db, "UPDATE landings SET state = 'cancelled', finished_at = "//NOW// &
            ", log = coalesce(nullif(log, '') || char(10), '') || 'Cancelled by ' || ?3 ||"// &
            " ' at ' || "//NOW//" || '.' WHERE id = ?1 AND repo = ?2 AND "//kinds// &
            " AND state IN ('queued', 'running') AND sealed = 0 RETURNING state;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, repo, stat)
        call stmt_bind_text(st, 3, account, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            state = stmt_text(st, 0)
            found = .true.
        end if
        call stmt_finalize(st)
        cancelled = found
        if (found) then
            ! A verification's checks are named by its number; a landing's
            ! are withdrawn by the lander.
            write (num, '(i0)') id
            call db_prepare(db, "UPDATE checks SET state = 'cancelled', finished_at = "//NOW// &
                " WHERE repo = ?1 AND ref = ?2 AND state IN ('queued', 'running');", st, stat)
            if (stat /= SQLITE_OK) return
            call stmt_bind_text(st, 1, repo, stat)
            call stmt_bind_text(st, 2, 'refs/fortyard/verify/'//trim(num), stat)
            stat = stmt_step(st)
            call stmt_finalize(st)
            return
        end if
        call db_prepare(db, "SELECT state FROM landings WHERE id = ?1 AND repo = ?2 AND "// &
            kinds//";", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, repo, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            state = stmt_text(st, 0)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_landing_cancel

    subroutine store_landing_cancel_all(db, repo, account, ids)
        !! Cancels every queued landing of `repo` and every running one that is
        !! not sealed, as store_landing_cancel does one; `ids` are the cancelled.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, account
        integer(int64), allocatable, intent(out) :: ids(:)
        type(stmt_t) :: st
        integer :: stat

        allocate (ids(0))
        call db_prepare(db, "UPDATE landings SET state = 'cancelled', finished_at = "//NOW// &
            ", log = coalesce(nullif(log, '') || char(10), '') || 'Cancelled by ' || ?2 ||"// &
            " ' at ' || "//NOW//" || '.' WHERE repo = ?1 AND kind = 'landing'"// &
            " AND state IN ('queued', 'running') AND sealed = 0 RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, account, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            ids = [ids, stmt_int(st, 0)]
        end do
        call stmt_finalize(st)
    end subroutine store_landing_cancel_all

    function store_landing_state(db, id) result(state)
        !! The state of landing `id`, '' when there is none.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable :: state
        type(stmt_t) :: st
        integer :: stat

        state = ''
        call db_prepare(db, 'SELECT state FROM landings WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) state = stmt_text(st, 0)
        call stmt_finalize(st)
    end function store_landing_state

    subroutine store_landing_seal(db, ids, sealed)
        !! Seals the landings `ids` before main moves to a commit that holds
        !! them, in one statement: all of them, if each is still running, else
        !! none (`sealed` false), so a cancel and the move never both happen.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: ids(:)
        logical, intent(out) :: sealed
        type(stmt_t) :: st
        integer :: stat, n

        sealed = size(ids) == 0
        if (sealed) return
        call db_prepare(db, 'UPDATE landings SET sealed = 1 WHERE id IN ('//id_list(ids)// &
            ") AND NOT EXISTS (SELECT 1 FROM landings WHERE id IN ("//id_list(ids)// &
            ") AND state <> 'running') RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        n = 0
        do while (stmt_step(st) == SQLITE_ROW)
            n = n + 1
        end do
        call stmt_finalize(st)
        sealed = n == size(ids)
    end subroutine store_landing_seal

    subroutine store_landing_unseal(db, ids)
        !! The landings `ids` can be cancelled again: main did not move to them.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: ids(:)
        integer :: stat

        if (size(ids) == 0) return
        call db_exec(db, 'UPDATE landings SET sealed = 0 WHERE id IN ('//id_list(ids)//');', stat)
    end subroutine store_landing_unseal

    function id_list(ids) result(s)
        !! The numbers `ids` separated by commas, for an SQL list.
        integer(int64), intent(in) :: ids(:)
        character(len=:), allocatable :: s
        character(len=24) :: num
        integer :: k

        s = ''
        do k = 1, size(ids)
            write (num, '(i0)') ids(k)
            if (k > 1) s = s//','
            s = s//trim(num)
        end do
    end function id_list

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

    subroutine store_check_add(db, repo, ref, id, stat, kind, class)
        !! Queues a check of `ref` in `repo` for a runner: of a landing ("check",
        !! the default) or a verification of main ("verify"), on a runner of
        !! `class` (default linux).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, ref
        integer(int64), intent(out) :: id
        integer, intent(out) :: stat
        character(len=*), intent(in), optional :: kind, class
        type(stmt_t) :: st

        id = 0
        call db_prepare(db, 'INSERT INTO checks (repo, ref, created_at, kind, class) VALUES (?1,'// &
            ' ?2, '//NOW//', ?3, ?4) RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, ref, stat)
        if (present(kind)) then
            call stmt_bind_text(st, 3, kind, stat)
        else
            call stmt_bind_text(st, 3, 'check', stat)
        end if
        if (present(class)) then
            call stmt_bind_text(st, 4, class, stat)
        else
            call stmt_bind_text(st, 4, 'linux', stat)
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

    subroutine store_check_cancel(db, id)
        !! Withdraws check `id` of a candidate that lost a member: no runner
        !! takes it any more, and the runner that holds it has its result refused.
        !! Its twin, when raced, goes with it.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        type(stmt_t) :: st
        integer :: stat

        call db_prepare(db, "UPDATE checks SET state = 'cancelled', finished_at = "//NOW// &
            ' WHERE (id = ?1 OR id = (SELECT twin FROM checks WHERE id = ?1)) AND state IN'// &
            " ('queued', 'running');", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_check_cancel

    logical function store_check_raced(db, id) result(raced)
        !! Whether check `id` is the twin that raced an earlier check of the
        !! same ref for its home class (store_check_twin).
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        type(stmt_t) :: st
        integer :: stat

        raced = .false.
        call db_prepare(db, 'SELECT count(*) FROM checks WHERE id = ?1 AND twin < id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) raced = stmt_int(st, 0) > 0
        call stmt_finalize(st)
    end function store_check_raced

    subroutine store_check_claim(db, runner, id, repo, ref, found, kind, limit, class, &
            max_minutes, max_running)
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
        !! Only checks of the runner's `class` (default linux) are given, and
        !! affinity is to the runner that last served the repository in it.
        !! The claim records the class the check ran on and starts its clock.
        !! A backend class (fy_dispatch) is given one of the checks
        !! dispatch_candidates names for its time limit `max_minutes`, while
        !! fewer than `max_running` checks run on that class; 0 or absent: the
        !! defaults of the class; each is taken
        !! only while it is still queued. A raced candidate is not taken:
        !! the backend gets its twin (store_check_twin) and `id` is the
        !! twin's. Every claim records when the runner last asked (runners),
        !! for wait(C).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner
        character(len=*), intent(in), optional :: class
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: repo, ref
        logical, intent(out) :: found
        character(len=:), allocatable, intent(out), optional :: kind
        integer, intent(out), optional :: limit
        integer, intent(in), optional :: max_minutes, max_running
        character(len=:), allocatable :: cls
        integer(int64), allocatable :: ids(:)
        logical, allocatable :: race(:)
        type(stmt_t) :: st
        integer :: stat, minutes, most, k

        found = .false.
        id = 0
        repo = ''
        ref = ''
        if (present(limit)) limit = 0
        cls = 'linux'
        if (present(class)) cls = class
        minutes = dispatch_max_minutes(cls)
        if (present(max_minutes)) then
            if (max_minutes > 0) minutes = max_minutes
        end if
        most = dispatch_max_running(cls)
        if (present(max_running)) then
            if (max_running > 0) most = max_running
        end if
        call db_prepare(db, 'INSERT INTO runners (name, class, seen_at, max_minutes, max_running)'// &
            ' VALUES (?1, ?2, '//NOW//', ?3, ?4) ON CONFLICT (name) DO UPDATE SET class ='// &
            ' excluded.class, seen_at = excluded.seen_at, max_minutes = excluded.max_minutes,'// &
            ' max_running = excluded.max_running;', st, stat)
        if (stat == SQLITE_OK) then
            call stmt_bind_text(st, 1, runner, stat)
            call stmt_bind_text(st, 2, cls, stat)
            if (dispatch_backend(cls)) then
                call stmt_bind_int(st, 3, int(minutes, int64), stat)
                call stmt_bind_int(st, 4, int(most, int64), stat)
            end if
            stat = stmt_step(st)
            call stmt_finalize(st)
        end if
        if (dispatch_backend(cls)) then
            call dispatch_candidates(db, cls, minutes, ids, race)
            do k = 1, size(ids)
                if (race(k)) then
                    call store_check_twin(db, runner, cls, ids(k), most, found, id, repo, ref, &
                        kind, limit)
                else
                    call store_check_take(db, runner, cls, ids(k), most, found, id, repo, ref, &
                        kind, limit)
                end if
                if (found) return
            end do
            return
        end if
        call db_prepare(db, TAKE//"(SELECT c.id FROM checks c WHERE c.state = 'queued' AND"// &
            ' c.class = ?2 AND NOT'// &
            ' (c.created_at > strftime('//"'%Y-%m-%dT%H:%M:%SZ', 'now',"// &
            " '-"//AFFINITY_SECONDS//" seconds') AND EXISTS (SELECT 1 FROM checks p WHERE p.id ="// &
            ' (SELECT q.id FROM checks q WHERE q.repo = c.repo AND'// &
            ' coalesce(q.ran_class, q.class) = c.class AND'// &
            ' q.runner IS NOT NULL ORDER BY'// &
            ' q.id DESC LIMIT 1) AND p.runner <> ?1 AND NOT EXISTS (SELECT 1 FROM checks b WHERE'// &
            " b.state = 'running' AND b.runner = p.runner)))"// &
            ' AND '//nightly_room()// &
            " ORDER BY c.kind IN ('verify', 'nightly'), (SELECT count(*) FROM checks r WHERE r.state ="// &
            " 'running' AND r.repo = c.repo AND coalesce(r.ran_class, r.class) = c.class), c.id LIMIT 1)"// &
            " RETURNING id,"// &
            ' repo, ref, kind,'// &
            ' time_limit;', st, stat)
        if (stat /= SQLITE_OK) return
        call claim_row()

    contains

        subroutine claim_row()
            call stmt_bind_text(st, 1, runner, stat)
            call stmt_bind_text(st, 2, cls, stat)
            if (stmt_step(st) == SQLITE_ROW) then
                id = stmt_int(st, 0)
                repo = stmt_text(st, 1)
                ref = stmt_text(st, 2)
                if (present(kind)) kind = stmt_text(st, 3)
                if (present(limit)) limit = int(stmt_int(st, 4))
                found = .true.
            end if
            call stmt_finalize(st)
        end subroutine claim_row

    end subroutine store_check_claim

    subroutine store_check_take(db, runner, class, check, most, found, id, repo, ref, kind, limit)
        !! Gives `runner` of the backend class `class` the check `check` while
        !! it is still queued, fewer than `most` checks run on the class and,
        !! for a GitHub class, fewer than the largest limit a GitHub backend
        !! process seen lately sent run on all GitHub classes: GitHub limits
        !! the jobs of an organisation. One statement, so of two claims that
        !! chose the same check only one gets it.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner, class
        integer(int64), intent(in) :: check
        integer, intent(in) :: most
        logical, intent(out) :: found
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: repo, ref
        character(len=:), allocatable, intent(out), optional :: kind
        integer, intent(out), optional :: limit
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        id = 0
        repo = ''
        ref = ''
        if (present(limit)) limit = 0
        ! The twin's row exists before the check names it (store_check_twin).
        call db_prepare(db, TAKE//"?3 AND state = 'queued' AND NOT EXISTS (SELECT 1 FROM"// &
            " checks t WHERE t.twin = checks.id AND t.state IN ('queued', 'running')) AND "// &
            backend_room()//' RETURNING id, repo, ref, kind, time_limit;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        call stmt_bind_text(st, 2, class, stat)
        call stmt_bind_int(st, 3, check, stat)
        call stmt_bind_int(st, 4, int(most, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            repo = stmt_text(st, 1)
            ref = stmt_text(st, 2)
            if (present(kind)) kind = stmt_text(st, 3)
            if (present(limit)) limit = int(stmt_int(st, 4))
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_check_take

    subroutine store_check_twin(db, runner, class, check, most, found, id, repo, ref, kind, limit)
        !! Races the queued check `check` (doc/backends.md "Racing"): gives
        !! `runner` of the backend class `class` a twin of it, a second check
        !! of the same ref and home class that runs on `class`, linked to it
        !! through `twin` both ways, while `check` stays queued for its home
        !! class. Only while `check` is queued and has no twin, under the
        !! limits of store_check_take; one statement, so of two claims only
        !! one makes a twin; store_check_take does not take `check` once its
        !! twin runs, before `check` names it. `id` is the twin's.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner, class
        integer(int64), intent(in) :: check
        integer, intent(in) :: most
        logical, intent(out) :: found
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: repo, ref
        character(len=:), allocatable, intent(out), optional :: kind
        integer, intent(out), optional :: limit
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        id = 0
        repo = ''
        ref = ''
        if (present(limit)) limit = 0
        call db_prepare(db, 'INSERT INTO checks (repo, ref, created_at, kind, class, state,'// &
            ' runner, ran_class, claimed_at, started_at, time_limit, twin) SELECT c.repo, c.ref, '// &
            NOW//", c.kind, c.class, 'running', ?1, ?2, "//NOW//', '//NOW//", CASE WHEN c.kind IN"// &
            " ('verify', 'nightly') THEN 21600 ELSE coalesce((SELECT 60*policy_check_timeout FROM repos WHERE"// &
            ' name = c.repo), 3600) END, c.id FROM checks c WHERE c.id = ?3 AND c.state ='// &
            " 'queued' AND c.twin IS NULL AND NOT EXISTS (SELECT 1 FROM checks t WHERE t.twin ="// &
            ' c.id) AND '//backend_room()//' RETURNING id, repo, ref, kind, time_limit;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        call stmt_bind_text(st, 2, class, stat)
        call stmt_bind_int(st, 3, check, stat)
        call stmt_bind_int(st, 4, int(most, int64), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            repo = stmt_text(st, 1)
            ref = stmt_text(st, 2)
            if (present(kind)) kind = stmt_text(st, 3)
            if (present(limit)) limit = int(stmt_int(st, 4))
            found = .true.
        end if
        call stmt_finalize(st)
        if (.not. found) return
        call db_prepare(db, 'UPDATE checks SET twin = ?2 WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, check, stat)
        call stmt_bind_int(st, 2, id, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_check_twin

    function backend_room() result(sql)
        !! The condition under which runner ?1 of backend class ?2 may run one
        !! more check: fewer than ?4 run on the class and, for a GitHub class,
        !! fewer than the largest limit a GitHub backend process seen lately
        !! sent run on all GitHub classes.
        character(len=:), allocatable :: sql

        sql = "(SELECT count(*) FROM checks r WHERE r.state = 'running' AND r.ran_class = ?2)"// &
            " < ?4 AND (?2 NOT LIKE 'github-%' OR (SELECT count(*) FROM checks r WHERE r.state ="// &
            " 'running' AND r.ran_class LIKE 'github-%') < (SELECT max(max_running) FROM runners"// &
            " WHERE class LIKE 'github-%' AND (seen_at >= "//DISPATCH_SEEN//' OR name = ?1)))'
    end function backend_room

    function nightly_room() result(sql)
        !! The condition that check c may start: a nightly check only while
        !! fewer than FY_NIGHTLY_SLOTS (default 2) nightly checks run on its
        !! runner class, so landings at night keep runners.
        character(len=:), allocatable :: sql
        character(len=16) :: slots
        integer :: n, stat

        call get_environment_variable('FY_NIGHTLY_SLOTS', slots, n, stat)
        if (stat /= 0 .or. n == 0 .or. verify(trim(slots), '0123456789') > 0) slots = '2'
        sql = "(c.kind <> 'nightly' OR (SELECT count(*) FROM checks n WHERE n.state ="// &
            " 'running' AND n.kind = 'nightly' AND coalesce(n.ran_class, n.class) = c.class) < "// &
            trim(slots)//')'
    end function nightly_room

    subroutine store_check_release(db, runner, count)
        !! Queues the running checks of `runner` again; `count` of them. A
        !! twin is not queued but ends cancelled: the check it raced is
        !! still queued for its home class, as if queued again now.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner
        integer, intent(out) :: count
        type(stmt_t) :: st
        integer :: stat

        count = 0
        call db_prepare(db, 'UPDATE checks SET queued_at = '//NOW//" WHERE state = 'queued' AND"// &
            " id IN (SELECT twin FROM checks WHERE state = 'running' AND runner = ?1 AND"// &
            ' twin < id);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
        call db_prepare(db, "UPDATE checks SET state = 'cancelled', finished_at = "//NOW// &
            " WHERE state = 'running' AND runner = ?1 AND twin < id RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            count = count + 1
        end do
        call stmt_finalize(st)
        call db_prepare(db, "UPDATE checks SET state = 'queued', runner = NULL, claimed_at = NULL,"// &
            ' ran_class = NULL, started_at = NULL, queued_at = '//NOW//" WHERE state = 'running' AND"// &
            ' runner = ?1 RETURNING id;', st, stat)
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
        !! state "timeout". `log` goes before its log (LOG_FIRST); `state` is
        !! the new one.
        !! With `runner`, only while that runner holds the check, so a runner
        !! taken as lost cannot speak for a later claim. With `lost_grace`,
        !! only when the claim is older than twice its time limit (a failure
        !! is run again from an empty build directory) plus `lost_grace`
        !! seconds: its runner is lost.
        !! A twin (store_check_twin) is not queued again but ends cancelled,
        !! deciding nothing: the check it raced still waits for its home class,
        !! as if queued again now.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: log
        logical, intent(in) :: timeout
        character(len=:), allocatable, intent(out) :: state
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: runner
        integer, intent(in), optional :: lost_grace
        !! The attempt ends here rather than going back to the queue.
        character(len=*), parameter :: ENDS = '(twin < id OR ?2 = 1 AND timeouts >= 1)'
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
        call db_prepare(db, 'UPDATE checks SET timeouts = timeouts + ?2, log = '// &
            LOG_FIRST//", state = CASE WHEN twin < id THEN 'cancelled' WHEN ?2 = 1 AND"// &
            " timeouts >= 1 THEN 'timeout' ELSE 'queued' END, runner = CASE WHEN "//ENDS//' THEN'// &
            ' runner END, claimed_at = CASE WHEN '//ENDS//' THEN claimed_at END,'// &
            ' ran_class = CASE WHEN '//ENDS//' THEN ran_class END, started_at = CASE'// &
            ' WHEN '//ENDS//' THEN started_at END,'// &
            ' finished_at = CASE WHEN '//ENDS//' THEN '//NOW//' END, queued_at ='// &
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
        if (state /= 'cancelled') return
        ! The raced check waits for its home class as if queued again now.
        call db_prepare(db, 'UPDATE checks SET queued_at = '//NOW//" WHERE state = 'queued' AND"// &
            ' id = (SELECT twin FROM checks WHERE id = ?1 AND twin < id);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_check_error

    subroutine store_check_finish(db, id, runner, state, log, found, started_at)
        !! Records the result of a check that `runner` holds: "passed" or
        !! "failed". A runner whose claim was taken from it as lost finds
        !! nothing. `log` goes before what earlier attempts left in its log.
        !! `started_at` (ISO 8601) is when it actually started running, for a
        !! backend the external job's start; kept between the check's
        !! creation and now. Without it the claim's time stays.
        !! Of a raced pair (store_check_twin) the first verdict decides: the
        !! other check is cancelled, so its runner's next log upload and its
        !! result are refused and a backend sees it withdrawn; a result that
        !! comes after the other's verdict finds nothing.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: runner, state, log
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: started_at
        character(len=24) :: num
        integer(int64) :: twin
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        twin = 0
        call db_prepare(db, 'UPDATE checks SET state = ?2, log = '//LOG_FIRST// &
            ', finished_at = '//NOW//', started_at = coalesce(max(created_at,'// &
            " min(strftime('%Y-%m-%dT%H:%M:%SZ', ?5), "//NOW//')), started_at)'// &
            " WHERE id = ?1 AND state = 'running' AND runner = ?4 AND NOT EXISTS (SELECT 1 FROM"// &
            " checks t WHERE t.id = checks.twin AND t.state IN ('passed', 'failed'))"// &
            ' RETURNING coalesce(twin, 0);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_text(st, 3, log, stat)
        call stmt_bind_text(st, 4, runner, stat)
        if (present(started_at)) call stmt_bind_text(st, 5, started_at, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            found = .true.
            twin = stmt_int(st, 0)
        end if
        call stmt_finalize(st)
        if (twin == 0) return
        write (num, '(i0)') id
        call db_prepare(db, "UPDATE checks SET state = 'cancelled', finished_at = "//NOW// &
            ', log = '//LOG_FIRST//" WHERE id = ?1 AND state IN ('queued', 'running');", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, twin, stat)
        call stmt_bind_text(st, 3, 'Withdrawn: its twin check '//trim(num)//' ran the same'// &
            ' commit and '//state//' first, which decides.'//new_line('a'), stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
    end subroutine store_check_finish

    subroutine store_check_get(db, id, state, log, age, limit, which, ref)
        !! State and log of a check, and seconds since it was queued, queued
        !! again or claimed; `limit`, the time limit in seconds its last claim
        !! was given (0: none). A raced pair reads as one check: its twin
        !! when that has the verdict, or runs while the check itself neither
        !! runs nor has a verdict; `which` is the check read, `ref` what it checks.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: state, log
        integer, intent(out) :: age
        integer, intent(out), optional :: limit
        integer(int64), intent(out), optional :: which
        character(len=:), allocatable, intent(out), optional :: ref
        type(stmt_t) :: st
        integer :: stat

        state = ''
        log = ''
        age = 0
        if (present(limit)) limit = 0
        if (present(which)) which = id
        if (present(ref)) ref = ''
        call db_prepare(db, "SELECT state, coalesce(log, ''), CAST(strftime('%s', 'now') -"// &
            " strftime('%s', coalesce(claimed_at, queued_at, created_at)) AS INTEGER),"// &
            ' coalesce(time_limit, 3600), id, ref FROM checks WHERE id = (SELECT CASE WHEN t.state'// &
            " IN ('passed', 'failed') OR (c.state NOT IN ('passed', 'failed', 'running') AND"// &
            " t.state = 'running') THEN t.id ELSE c.id END FROM checks c LEFT JOIN checks t ON"// &
            ' t.id = c.twin WHERE c.id = ?1);', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            state = stmt_text(st, 0)
            log = stmt_text(st, 1)
            age = int(stmt_int(st, 2))
            if (present(limit)) limit = int(stmt_int(st, 3))
            if (present(which)) which = stmt_int(st, 4)
            if (present(ref)) ref = stmt_text(st, 5)
        end if
        call stmt_finalize(st)
    end subroutine store_check_get

    subroutine store_check_holder(db, id, runner, found)
        !! The runner that holds check `id` while it runs; found is false when
        !! the check does not run.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: runner
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        runner = ''
        found = .false.
        call db_prepare(db, "SELECT coalesce(runner, '') FROM checks WHERE id = ?1 AND"// &
            " state = 'running';", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            runner = stmt_text(st, 0)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_check_holder

    subroutine store_check_info(db, id, repo, ref, kind, class, state, runner, found)
        !! What check `id` is of and where it stands: its repository, ref,
        !! kind, class, state and the runner that holds or last held it
        !! (empty for none).
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: repo, ref, kind, class, state, runner
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        repo = ''
        ref = ''
        kind = ''
        class = ''
        state = ''
        runner = ''
        found = .false.
        call db_prepare(db, "SELECT repo, ref, kind, class, state, coalesce(runner, '') FROM"// &
            ' checks WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            repo = stmt_text(st, 0)
            ref = stmt_text(st, 1)
            kind = stmt_text(st, 2)
            class = stmt_text(st, 3)
            state = stmt_text(st, 4)
            runner = stmt_text(st, 5)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_check_info

    subroutine store_check_jobs(db, id, jobs)
        !! The jobs that check `id` decides: the one whose number ends its ref,
        !! and the members of a candidate that share the check.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        integer(int64), allocatable, intent(out) :: jobs(:)
        type(stmt_t) :: st
        integer :: stat

        allocate (jobs(0))
        call db_prepare(db, "SELECT CAST(substr(ref, length(rtrim(ref, '0123456789')) + 1)"// &
            ' AS INTEGER) FROM checks WHERE id = ?1 UNION SELECT id FROM landings'// &
            ' WHERE check_id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            if (stmt_int(st, 0) > 0) jobs = [jobs, stmt_int(st, 0)]
        end do
        call stmt_finalize(st)
    end subroutine store_check_jobs

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

    logical function store_time_valid(db, text)
        !! `text` is a time SQLite reads, so a stored one compares and counts.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: text
        type(stmt_t) :: st
        integer :: stat

        store_time_valid = .false.
        call db_prepare(db, "SELECT strftime('%s', ?1) IS NOT NULL;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, text, stat)
        if (stmt_step(st) == SQLITE_ROW) store_time_valid = stmt_int(st, 0) == 1
        call stmt_finalize(st)
    end function store_time_valid

end module fy_store
