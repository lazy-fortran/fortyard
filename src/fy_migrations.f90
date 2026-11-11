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

    public :: migration_t, migration_add, migrations_registry

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

    end subroutine migrations_registry

end module fy_migrations
