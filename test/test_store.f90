program test_store
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_token_lookup, &
        store_token_create, store_schema_version, token_identity_t, SCHEMA_VERSION, &
        store_snapshot, store_check_add, store_check_claim, store_check_finish, &
        store_check_release
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len=*), parameter :: path = 'build/test_store.db'
    type(db_t) :: db
    type(token_identity_t) :: who
    character(len=:), allocatable :: admin, second, again
    integer :: stat, released
    integer(int64) :: id
    character(len=:), allocatable :: repo, ref
    logical :: found

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm'//' '//path//'.snap')
    call store_open(path, db, stat)
    call check(stat == 0, 'store opens and migrates')
    call check(store_schema_version(db) == SCHEMA_VERSION, 'schema at current version')

    call store_bootstrap(db, admin, stat)
    call check(stat == 0 .and. len(admin) == 67 .and. admin(1:3) == 'fy_', 'bootstrap token')
    call store_bootstrap(db, again, stat)
    call check(stat /= 0 .and. len(again) == 0, 'bootstrap succeeds only once')

    call store_token_lookup(db, admin, who)
    call check(who%found .and. who%account == 'admin' .and. who%scopes == 'admin', &
        'bootstrap token identifies the administrator')

    call store_token_create(db, who%account_id, 'laptop', who%scopes, second, stat)
    call check(stat == 0 .and. second /= admin, 'second token created and distinct')
    call store_token_lookup(db, second, who)
    call check(who%found .and. who%token_name == 'laptop', 'second token valid')

    call store_token_lookup(db, admin(1:66)//'x', who)
    call check(.not. who%found, 'altered token rejected')
    call store_token_lookup(db, '', who)
    call check(.not. who%found, 'empty token rejected')

    call db_exec(db, "UPDATE tokens SET revoked = 1 WHERE name = 'laptop';", stat)
    call store_token_lookup(db, second, who)
    call check(.not. who%found, 'revoked token rejected')

    call db_exec(db, "UPDATE tokens SET expires_at = '2000-01-01T00:00:00Z'"// &
        " WHERE name = 'bootstrap';", stat)
    call store_token_lookup(db, admin, who)
    call check(.not. who%found, 'expired token rejected')

    ! A snapshot taken while the store is open is a complete database.
    call store_snapshot(db, path//'.snap', stat)
    call check(stat == 0, 'snapshot written')
    call db_close(db)
    call store_open(path, db, stat)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION, &
        'reopening an existing store is a no-op migration')
    call db_close(db)
    call store_open(path//'.snap', db, stat)
    call store_token_lookup(db, second, who)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION .and. &
        .not. who%found, 'the snapshot holds the database as it was')
    call db_close(db)

    ! A check goes to the runner that checked its repository last; another
    ! runner takes it only after it has waited.
    call store_open(path, db, stat)
    call store_check_add(db, 'demo', 'refs/a', id, stat)
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(found .and. repo == 'demo', 'the first check of a repository goes to any runner')
    call store_check_finish(db, id, 'passed', '', found)
    call store_check_add(db, 'demo', 'refs/b', id, stat)
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(.not. found, 'another runner leaves a fresh check to the warm one')
    call store_check_claim(db, 'r1', id, repo, ref, found)
    call check(found .and. ref == 'refs/b', 'the warm runner takes it')
    call store_check_finish(db, id, 'passed', '', found)
    call store_check_add(db, 'demo', 'refs/c', id, stat)
    call db_exec(db, "UPDATE checks SET created_at = '2000-01-01T00:00:00Z' WHERE ref = 'refs/c';", &
        stat)
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(found .and. ref == 'refs/c', 'another runner takes a check that has waited')
    ! A runner that restarts hands back the check it held; it runs again.
    call store_check_release(db, 'r1', released)
    call check(released == 0, 'a runner holding nothing hands nothing back')
    call store_check_release(db, 'r2', released)
    call check(released == 1, 'a restarted runner hands back its running check')
    call store_check_claim(db, 'r2', id, repo, ref, found)
    call check(found .and. ref == 'refs/c', 'the handed back check is taken again')
    call db_close(db)

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_store
