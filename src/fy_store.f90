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

    public :: store_open, store_migrate, store_schema_version
    public :: store_bootstrap, store_token_lookup, store_token_create
    public :: store_token_list, store_token_revoke, store_token_revoke_hash
    public :: token_identity_t, token_info_t, random_token
    public :: repo_info_t, store_repo_add, store_repo_list, store_repo_fetched, store_repo_set_mode

    integer, parameter, public :: SCHEMA_VERSION = 3

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

    character(len=*), parameter :: NOW = "strftime('%Y-%m-%dT%H:%M:%SZ','now')"

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
