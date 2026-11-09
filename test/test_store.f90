program test_store
    use fy_sqlite, only: db_t, db_close, db_exec
    use fy_store, only: store_open, store_bootstrap, store_token_lookup, &
        store_token_create, store_schema_version, token_identity_t, SCHEMA_VERSION
    implicit none

    character(len=*), parameter :: path = 'build/test_store.db'
    type(db_t) :: db
    type(token_identity_t) :: who
    character(len=:), allocatable :: admin, second, again
    integer :: stat

    call execute_command_line('rm -f '//path//' '//path//'-wal '//path//'-shm')
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

    call db_close(db)
    call store_open(path, db, stat)
    call check(stat == 0 .and. store_schema_version(db) == SCHEMA_VERSION, &
        'reopening an existing store is a no-op migration')
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
