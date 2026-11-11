module fy_migrations
    !! The registry of schema migrations that have a name. The store
    !! (fy_store) applies them after the numbered ones, in the order they
    !! stand here, and records each applied name with the SHA-256 of its SQL.
    !!
    !! To add a migration, put one block directly above the last line of
    !! `migrations_registry`: comment lines that say what the schema is for,
    !! the call, and an empty line.
    !!
    !!     ! Why the table or column exists.
    !!     call migration_add(list, 'changes-merge-class', &
    !!         'ALTER TABLE changes ADD COLUMN merge_class TEXT;'// &
    !!         'CREATE INDEX changes_by_merge_class ON changes (merge_class);')
    !!
    !! - The name is new, lower case with hyphens, and says what is added. Two
    !!   blocks with one name refuse to migrate.
    !! - The block touches no other line and refers to no other block. Two
    !!   branches that each add one are merged with both blocks kept whole,
    !!   the earlier landing first, so a block must not depend on what a block
    !!   that was written at the same time adds. (git's union merge is not
    !!   that: it keeps lines that both blocks end with only once.)
    !! - The SQL only adds (tables, columns with a default or NULL, indexes),
    !!   so the previous release still runs on the new schema. It has no
    !!   BEGIN, COMMIT or ROLLBACK: the store runs it in one transaction with
    !!   its record and refuses SQL that would end it.
    !! - A block that has landed is never moved or removed, and its SQL is
    !!   never edited, not even in its spacing: a database that has it
    !!   refuses SQL that differs. A later change to the schema is a new
    !!   block.
    !! - Nothing else is added to this file, and SCHEMA_VERSION stays.
    implicit none
    private

    public :: &
        migration_add, &
        migration_t, &
        migrations_registry

    type :: migration_t
        character(len=:), allocatable :: name, sql
    end type migration_t

contains

    subroutine migration_add(list, name, sql)
        type(migration_t), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: name, sql
        type(migration_t), allocatable :: grown(:)
        integer :: i

        ! Moved, not `[list, migration_t(name, sql)]`: gfortran does not free
        ! the strings of that temporary, and the store builds the registry
        ! on every open.
        allocate (grown(size(list) + 1))
        do i = 1, size(list)
            call move_alloc(list(i)%name, grown(i)%name)
            call move_alloc(list(i)%sql, grown(i)%sql)
        end do
        grown(size(list) + 1)%name = name
        grown(size(list) + 1)%sql = sql
        call move_alloc(grown, list)
    end subroutine migration_add

    subroutine migrations_registry(list)
        type(migration_t), allocatable, intent(out) :: list(:)

        allocate (list(0))

        ! Changes nothing. It makes the first release with named migrations
        ! record a name, so that release and the rollback from it show the
        ! mechanism working on the production database before a migration
        ! depends on it.
        call migration_add(list, 'named-migrations', &
            'SELECT 1;')

        ! The session, the name of the token, that pushed a version or queued a
        ! landing (doc/parallel-development.md); NULL where it was not recorded.
        call migration_add(list, 'versions-landings-session', &
            'ALTER TABLE change_versions ADD COLUMN session TEXT;'// &
            'ALTER TABLE landings ADD COLUMN session TEXT;')

        ! The token a browser session was opened with: the session carries that
        ! token's name into the lists and ends with it. NULL for every other token.
        call migration_add(list, 'tokens-parent', &
            'ALTER TABLE tokens ADD COLUMN parent INTEGER;')

        ! Runners shared fairly with a time limit per check: the minutes a
        ! repository's check may run (0: no limit), how often a check ended at
        ! that limit, when a check was queued again after a runner error, and
        ! the limit in seconds its claim was given. The indexes serve the
        ! claim, which counts each repository's running checks and finds the
        ! runner that last served it.
        call migration_add(list, 'checks-fair-share-timeout', &
            'ALTER TABLE repos ADD COLUMN policy_check_timeout INTEGER NOT NULL DEFAULT 60;'// &
            'ALTER TABLE checks ADD COLUMN timeouts INTEGER NOT NULL DEFAULT 0;'// &
            'ALTER TABLE checks ADD COLUMN queued_at TEXT;'// &
            'ALTER TABLE checks ADD COLUMN time_limit INTEGER;'// &
            'CREATE INDEX checks_by_state_repo ON checks (state, repo);'// &
            "CREATE INDEX checks_running ON checks (runner) WHERE state = 'running';"// &
            'CREATE INDEX checks_served ON checks (repo, id) WHERE runner IS NOT NULL;')

        ! The list of changes reads the entries of each of its rows; without
        ! this it walks every landing with its log for each of them.
        call migration_add(list, 'landings-by-change', &
            'CREATE INDEX landings_by_change ON landings (change, kind, id, state);')

    end subroutine migrations_registry

end module fy_migrations
