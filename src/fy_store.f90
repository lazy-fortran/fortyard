module fy_store
    !! Schema, accounts and API tokens. Tokens are stored only as SHA-256
    !! hashes; the plaintext is shown once when created.
    use, intrinsic :: iso_fortran_env, only: int8, int64
    use fx_hash, only: sha256_string
    use fy_sqlite, only: db_t, stmt_t, db_open, db_exec, db_errmsg, db_prepare, &
        stmt_bind_text, stmt_bind_int, stmt_step, stmt_text, stmt_int, &
        stmt_finalize, SQLITE_OK, SQLITE_ROW, SQLITE_DONE
    implicit none
    private

    public :: store_open, store_migrate, store_schema_version, store_snapshot
    public :: store_bootstrap, store_token_lookup, store_token_create, store_account_find
    public :: store_token_list, store_token_revoke, store_token_revoke_hash
    public :: token_identity_t, token_info_t, random_token
    public :: repo_info_t, store_repo_add, store_repo_list, store_repo_fetched, store_repo_set_mode, &
        store_repo_set_number, store_landing_take
    public :: landing_t, store_landing_add, store_landing_next, store_landing_requeue
    public :: store_landing_finish, store_landing_list
    public :: release_t, store_release_add, store_release_ready, store_release_list
    public :: store_jobs, store_job, store_job_counts, store_jobs_state
    public :: store_check_add, store_check_claim, store_check_finish, store_check_get, &
        store_check_release

    integer, parameter, public :: SCHEMA_VERSION = 12

    type :: token_identity_t
        logical :: found = .false.
        integer(int64) :: account_id = 0
        integer(int64) :: token_id = 0
        character(len=:), allocatable :: account
        character(len=:), allocatable :: token_name
        character(len=:), allocatable :: scopes
    end type token_identity_t

    type :: token_info_t
        integer(int64) :: id = 0
        character(len=:), allocatable :: name, scopes, created_at, expires_at
        logical :: revoked = .false., expired = .false.
    end type token_info_t

    type :: repo_info_t
        character(len=:), allocatable :: name, upstream, fetched_at
        !! "mirror" (fetched from upstream) or "primary" (pushed to fortyard).
        character(len=:), allocatable :: mode
        !! Exit status of the last clone or fetch; -1 before the first.
        integer :: fetch_status = -1
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
    end type landing_t

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"
    !! How long a check waits for the runner that checked its repository last.
    character(len=*), parameter :: AFFINITY_SECONDS = '20'

contains

    subroutine store_open(path, db, stat)
        character(len=*), intent(in) :: path
        type(db_t), intent(out) :: db
        integer, intent(out) :: stat

        call db_open(path, db, stat)
        if (stat /= SQLITE_OK) return
        call db_exec(db, 'PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;', stat)
        if (stat /= SQLITE_OK) return
        call store_migrate(db, stat)
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

    subroutine store_migrate(db, stat)
        !! Forward-only, additive migrations, so the previous release still runs.
        type(db_t), intent(in) :: db
        integer, intent(out) :: stat
        integer :: version

        call db_exec(db, 'CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL);', &
            stat)
        if (stat /= SQLITE_OK) return
        version = store_schema_version(db)
        if (version < 1) then
            call db_exec(db, 'BEGIN;'// &
                'CREATE TABLE accounts (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL,'// &
                ' kind TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1,'// &
                ' created_at TEXT NOT NULL);'// &
                'CREATE TABLE tokens (id INTEGER PRIMARY KEY,'// &
                ' account_id INTEGER NOT NULL REFERENCES accounts(id),'// &
                ' name TEXT NOT NULL, hash TEXT UNIQUE NOT NULL, scopes TEXT NOT NULL,'// &
                ' created_at TEXT NOT NULL, expires_at TEXT,'// &
                ' revoked INTEGER NOT NULL DEFAULT 0);'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (1);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 2) then
            call db_exec(db, 'BEGIN;'// &
                'CREATE TABLE repos (id INTEGER PRIMARY KEY, name TEXT UNIQUE NOT NULL,'// &
                ' upstream TEXT NOT NULL, created_at TEXT NOT NULL, fetched_at TEXT,'// &
                ' fetch_status INTEGER);'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (2);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 3) then
            ! "mirror": fetched from upstream; "primary": pushed to fortyard.
            call db_exec(db, 'BEGIN;'// &
                "ALTER TABLE repos ADD COLUMN mode TEXT NOT NULL DEFAULT 'mirror';"// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (3);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 4) then
            call db_exec(db, 'BEGIN;'// &
                'CREATE TABLE landings (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
                ' branch TEXT NOT NULL, account TEXT NOT NULL,'// &
                " state TEXT NOT NULL DEFAULT 'queued', created_at TEXT NOT NULL,"// &
                ' finished_at TEXT, commit_id TEXT, log TEXT);'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (4);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 5) then
            call db_exec(db, 'BEGIN;'// &
                'CREATE TABLE releases (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
                ' version TEXT NOT NULL, commit_id TEXT NOT NULL, notes TEXT NOT NULL,'// &
                ' uploader TEXT NOT NULL, created_at TEXT NOT NULL,'// &
                " state TEXT NOT NULL DEFAULT 'open', sha256 TEXT, size INTEGER,"// &
                ' UNIQUE (repo, version));'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (5);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 6) then
            call db_exec(db, 'BEGIN;ALTER TABLE landings ADD COLUMN started_at TEXT;'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (6);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 7) then
            ! Checks a runner performs for a landing: queued, running, passed, failed.
            call db_exec(db, 'BEGIN;'// &
                'CREATE TABLE checks (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
                " ref TEXT NOT NULL, state TEXT NOT NULL DEFAULT 'queued', runner TEXT,"// &
                ' log TEXT, created_at TEXT NOT NULL, claimed_at TEXT, finished_at TEXT);'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (7);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 8) then
            ! The lander process (1, 2, ...) that runs a landing.
            call db_exec(db, 'BEGIN;ALTER TABLE landings ADD COLUMN lander INTEGER;'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (8);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 9) then
            ! Changes (fy_change): one per reviewed branch, with its versions and
            ! comments; a landing may land a change.
            call db_exec(db, 'BEGIN;'// &
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
                'ALTER TABLE landings ADD COLUMN change INTEGER;'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (9);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 10) then
            ! Issues (fy_issue), numbered with the changes of their repository.
            call db_exec(db, 'BEGIN;'// &
                'CREATE TABLE issues (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
                ' number INTEGER NOT NULL, title TEXT NOT NULL, body TEXT NOT NULL,'// &
                " author TEXT NOT NULL, state TEXT NOT NULL DEFAULT 'open',"// &
                ' created_at TEXT NOT NULL, updated_at TEXT NOT NULL, UNIQUE (repo, number));'// &
                'CREATE TABLE issue_comments (id INTEGER PRIMARY KEY, issue INTEGER NOT NULL,'// &
                ' account TEXT NOT NULL, body TEXT NOT NULL, created_at TEXT NOT NULL);'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (10);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 11) then
            ! Verifications of main are jobs too (kind 'verify', fy_verify), and
            ! their runner checks run .fortyard/verify; a regression found by
            ! bisection is reported once per culprit commit.
            call db_exec(db, 'BEGIN;'// &
                "ALTER TABLE landings ADD COLUMN kind TEXT NOT NULL DEFAULT 'landing';"// &
                "ALTER TABLE checks ADD COLUMN kind TEXT NOT NULL DEFAULT 'check';"// &
                'CREATE TABLE verify_reports (id INTEGER PRIMARY KEY, repo TEXT NOT NULL,'// &
                " culprit TEXT NOT NULL, issue INTEGER NOT NULL, state TEXT NOT NULL DEFAULT 'open',"// &
                ' created_at TEXT NOT NULL, UNIQUE (repo, culprit));'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (11);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
        if (store_schema_version(db) < 12) then
            ! A repository can count its changes and issues from a set number,
            ! so a fork does not continue the numbers in its upstream's history.
            call db_exec(db, 'BEGIN;ALTER TABLE repos ADD COLUMN number_next INTEGER;'// &
                'DELETE FROM schema_version; INSERT INTO schema_version VALUES (12);'// &
                'COMMIT;', stat)
            if (stat /= SQLITE_OK) then
                call db_exec(db, 'ROLLBACK;', version)
                return
            end if
        end if
    end subroutine store_migrate

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

    subroutine store_token_create(db, account_id, name, scopes, token, stat, days)
        !! A token that expires after `days`, or never when absent.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: account_id
        character(len=*), intent(in) :: name, scopes
        character(len=:), allocatable, intent(out) :: token
        integer, intent(out) :: stat
        integer, intent(in), optional :: days
        type(stmt_t) :: st
        integer(int64) :: lifetime

        lifetime = -1
        if (present(days)) lifetime = days
        token = random_token()
        call db_prepare(db, 'INSERT INTO tokens (account_id, name, hash, scopes, created_at,'// &
            ' expires_at) VALUES (?1, ?2, ?3, ?4, '//NOW//', CASE WHEN ?5 < 0 THEN NULL'// &
            " ELSE strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '+' || ?5 || ' days') END);", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, account_id, stat)
        call stmt_bind_text(st, 2, name, stat)
        call stmt_bind_text(st, 3, sha256_string(token), stat)
        call stmt_bind_text(st, 4, scopes, stat)
        call stmt_bind_int(st, 5, lifetime, stat)
        stat = SQLITE_OK
        if (stmt_step(st) /= SQLITE_DONE) stat = 1
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
        if (len(token) < 8 .or. len(token) > 128) return
        call db_prepare(db, 'SELECT a.id, a.name, t.name, t.scopes, t.id FROM tokens t'// &
            ' JOIN accounts a ON a.id = t.account_id'// &
            ' WHERE t.hash = ?1 AND t.revoked = 0 AND a.active = 1'// &
            ' AND (t.expires_at IS NULL OR t.expires_at > '//NOW//');', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, sha256_string(token), stat)
        if (stmt_step(st) == SQLITE_ROW) then
            who%found = .true.
            who%account_id = stmt_int(st, 0)
            who%account = stmt_text(st, 1)
            who%token_name = stmt_text(st, 2)
            who%scopes = stmt_text(st, 3)
            who%token_id = stmt_int(st, 4)
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
            ' revoked, coalesce(expires_at <= '//NOW//', 0) FROM tokens WHERE account_id = ?1'// &
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
            ' coalesce(fetch_status, -1), mode FROM repos ORDER BY name;', st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            r%name = stmt_text(st, 0)
            r%upstream = stmt_text(st, 1)
            r%fetched_at = stmt_text(st, 2)
            r%fetch_status = int(stmt_int(st, 3))
            r%mode = stmt_text(st, 4)
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

    subroutine store_landing_add(db, repo, branch, account, id, stat, change)
        !! Queues the landing of `branch` on `main` of `repo`, of `change` if given.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, branch, account
        integer(int64), intent(in), optional :: change
        integer(int64), intent(out) :: id
        integer, intent(out) :: stat
        type(stmt_t) :: st

        id = 0
        call db_prepare(db, 'INSERT INTO landings (repo, branch, account, created_at, change)'// &
            ' VALUES (?1, ?2, ?3, '//NOW//', nullif(?4, 0)) RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, branch, stat)
        call stmt_bind_text(st, 3, account, stat)
        if (present(change)) then
            call stmt_bind_int(st, 4, change, stat)
        else
            call stmt_bind_int(st, 4, 0_int64, stat)
        end if
        stat = 1
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            stat = SQLITE_OK
        end if
        call stmt_finalize(st)
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
    end subroutine store_landing_finish

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
            " coalesce(finished_at, ''), coalesce(commit_id, ''), coalesce(log, '')"// &
            ' FROM landings WHERE repo = ?1 ORDER BY id DESC LIMIT 50;', st, stat)
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
            ls = [ls, l]
        end do
        call stmt_finalize(st)
        stat = SQLITE_OK
    end subroutine store_landing_list

    subroutine store_jobs(db, repo, ls, stat)
        !! Running and queued landings, then the 40 newest finished ones, of
        !! `repo` or of all repositories when it is empty; without logs.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(landing_t), allocatable, intent(out) :: ls(:)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        type(landing_t) :: l
        character(len=*), parameter :: COLS = "SELECT id, repo, branch, account, state,"// &
            " created_at, coalesce(started_at, ''), coalesce(finished_at, ''),"// &
            " coalesce(commit_id, ''), kind FROM landings WHERE (?1 = '' OR repo = ?1)"
        integer :: pass

        allocate (ls(0))
        do pass = 1, 2
            if (pass == 1) then
                call db_prepare(db, COLS//" AND state IN ('running', 'queued')"// &
                    " ORDER BY state = 'queued', id;", st, stat)
            else
                call db_prepare(db, COLS//" AND state NOT IN ('running', 'queued')"// &
                    ' ORDER BY id DESC LIMIT 40;', st, stat)
            end if
            if (stat /= SQLITE_OK) return
            call stmt_bind_text(st, 1, repo, stat)
            do while (stmt_step(st) == SQLITE_ROW)
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
        call db_prepare(db, "SELECT id, repo, branch, account, state, created_at,"// &
            " coalesce(started_at, ''), coalesce(finished_at, ''), coalesce(commit_id, ''),"// &
            " coalesce(log, ''), kind FROM landings WHERE id = ?1;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            l%id = stmt_int(st, 0)
            l%repo = stmt_text(st, 1)
            l%branch = stmt_text(st, 2)
            l%account = stmt_text(st, 3)
            l%state = stmt_text(st, 4)
            l%created = stmt_text(st, 5)
            l%started = stmt_text(st, 6)
            l%finished = stmt_text(st, 7)
            l%commit = stmt_text(st, 8)
            l%log = stmt_text(st, 9)
            l%kind = stmt_text(st, 10)
            found = .true.
        end if
        call stmt_finalize(st)
    end subroutine store_job

    subroutine store_job_counts(db, names, counts, stat)
        !! Per repository: running, queued, landed and failed in the last 24 hours.
        type(db_t), intent(in) :: db
        character(len=64), allocatable, intent(out) :: names(:)
        integer, allocatable, intent(out) :: counts(:, :)
        integer, intent(out) :: stat
        type(stmt_t) :: st
        character(len=64) :: nm
        integer :: n, k

        allocate (names(0), counts(4, 0))
        call db_prepare(db, "SELECT repo, sum(state = 'running'), sum(state = 'queued'),"// &
            " sum(state = 'landed' AND finished_at >= strftime('%Y-%m-%dT%H:%M:%SZ', 'now',"// &
            " '-1 day')), sum(state = 'failed' AND finished_at >= strftime('%Y-%m-%dT%H:%M:%SZ',"// &
            " 'now', '-1 day')) FROM landings GROUP BY repo ORDER BY repo;", st, stat)
        if (stat /= SQLITE_OK) return
        do while (stmt_step(st) == SQLITE_ROW)
            n = size(names) + 1
            nm = stmt_text(st, 0)
            names = [names, nm]
            counts = reshape([counts, [(int(stmt_int(st, k)), k = 1, 4)]], [4, n])
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
        call db_prepare(db, "SELECT count(*) || '.' || coalesce(max(id), 0) || '.' ||"// &
            " coalesce(max(started_at), '') || '.' || coalesce(max(finished_at), '')"// &
            ' FROM landings;', st, stat)
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

    subroutine store_check_claim(db, runner, id, repo, ref, found, kind)
        !! Gives `runner` the oldest queued check it may take, landing checks
        !! before verifications: one of a repository it checked last, or that
        !! no runner checked yet, or one that has waited AFFINITY_SECONDS. A
        !! repository so stays on the runner whose build tree and caches are
        !! warm for it.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: runner
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: repo, ref
        logical, intent(out) :: found
        character(len=:), allocatable, intent(out), optional :: kind
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        id = 0
        repo = ''
        ref = ''
        call db_prepare(db, "UPDATE checks SET state = 'running', runner = ?1, claimed_at = "// &
            NOW//" WHERE id = (SELECT c.id FROM checks c WHERE c.state = 'queued' AND"// &
            ' (coalesce((SELECT p.runner FROM checks p WHERE p.repo = c.repo AND p.runner'// &
            ' IS NOT NULL ORDER BY p.id DESC LIMIT 1), ?1) = ?1 OR c.created_at <='// &
            " strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-"//AFFINITY_SECONDS//" seconds'))"// &
            " ORDER BY c.kind = 'verify', c.id LIMIT 1) RETURNING id, repo, ref, kind;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            id = stmt_int(st, 0)
            repo = stmt_text(st, 1)
            ref = stmt_text(st, 2)
            if (present(kind)) kind = stmt_text(st, 3)
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
        call db_prepare(db, "UPDATE checks SET state = 'queued', runner = NULL, claimed_at = NULL"// &
            " WHERE state = 'running' AND runner = ?1 RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, runner, stat)
        do while (stmt_step(st) == SQLITE_ROW)
            count = count + 1
        end do
        call stmt_finalize(st)
    end subroutine store_check_release

    subroutine store_check_finish(db, id, state, log, found)
        !! Records the result of a running check: "passed" or "failed".
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=*), intent(in) :: state, log
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'UPDATE checks SET state = ?2, log = ?3, finished_at = '//NOW// &
            " WHERE id = ?1 AND state = 'running' RETURNING id;", st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        call stmt_bind_text(st, 2, state, stat)
        call stmt_bind_text(st, 3, log, stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine store_check_finish

    subroutine store_check_get(db, id, state, log, age)
        !! State and log of a check, and seconds since it was queued or claimed.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: id
        character(len=:), allocatable, intent(out) :: state, log
        integer, intent(out) :: age
        type(stmt_t) :: st
        integer :: stat

        state = ''
        log = ''
        age = 0
        call db_prepare(db, "SELECT state, coalesce(log, ''), CAST(strftime('%s', 'now') -"// &
            " strftime('%s', coalesce(claimed_at, created_at)) AS INTEGER) FROM checks"// &
            ' WHERE id = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, id, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            state = stmt_text(st, 0)
            log = stmt_text(st, 1)
            age = int(stmt_int(st, 2))
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
