module fy_policy
    !! The policy of a repository: what a landing has to pass and what follows
    !! it. Each setting has the value every repository had before there were
    !! policies, so a repository whose policy nobody set behaves as before.
    !!
    !!   tests             full | affected | none   the check before landing;
    !!                     affected is stored and behaves as full until
    !!                     results exist per test case
    !!   review            after | before           before: a change lands only
    !!                     in a version that is reviewed
    !!   bisect            on | off                 bisection of a failing
    !!                     verification of main
    !!   batch             1..BATCH_MAX             most landings in one candidate
    !!   unreviewed_limit  0 (no limit) or N        nothing is queued to land
    !!                     while N landed changes wait for a review
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
        json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, JSON_ARRAY_END, &
        JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_ERROR, JSON_END_OF_INPUT
    use fy_change, only: change_t, change_by_id, change_last_version
    use fy_store, only: store_landing_add
    use fy_sqlite, only: db_t, stmt_t, db_prepare, db_exec, stmt_bind_text, stmt_bind_int, stmt_step, &
        stmt_text, stmt_int, stmt_finalize, SQLITE_OK, SQLITE_ROW
    implicit none
    private

    public :: policy_t, policy_get, policy_put, policy_set, policy_json, policy_from_json, &
        policy_reviewed, policy_unreviewed, policy_refusal, policy_queue

    integer, parameter, public :: BATCH_MAX = 64
    character(len=*), parameter, public :: POLICY_KEYS = &
        'tests=full|affected|none review=after|before bisect=on|off batch=1..64'// &
        ' unreviewed_limit=N (0: no limit)'

    type :: policy_t
        character(len=:), allocatable :: tests, review
        logical :: bisect = .true.
        integer :: batch = 8
        integer :: unreviewed_limit = 0
    end type policy_t

    !! A landed change that still waits for a review: flagged for a person, or
    !! not reviewed by anyone. Changes landed before there were reviews
    !! ('none') do not wait.
    character(len=*), parameter :: WAITING = "(c.flagged <> 0 OR c.review_state IS NULL OR"// &
        " c.review_state NOT IN ('ai-reviewed', 'human-reviewed', 'none'))"

contains

    subroutine policy_get(db, repo, p, found)
        !! The policy of `repo`; the defaults when there is no such repository.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(policy_t), intent(out) :: p
        logical, intent(out), optional :: found
        type(stmt_t) :: st
        integer :: stat

        p%tests = 'full'
        p%review = 'after'
        if (present(found)) found = .false.
        call db_prepare(db, 'SELECT policy_tests, policy_review, policy_bisect, policy_batch,'// &
            ' policy_unreviewed_limit FROM repos WHERE name = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        if (stmt_step(st) == SQLITE_ROW) then
            p%tests = stmt_text(st, 0)
            p%review = stmt_text(st, 1)
            p%bisect = stmt_int(st, 2) /= 0
            p%batch = int(stmt_int(st, 3))
            p%unreviewed_limit = int(stmt_int(st, 4))
            if (present(found)) found = .true.
        end if
        call stmt_finalize(st)
    end subroutine policy_get

    subroutine policy_put(db, repo, p, found)
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(policy_t), intent(in) :: p
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'UPDATE repos SET policy_tests = ?2, policy_review = ?3,'// &
            ' policy_bisect = ?4, policy_batch = ?5, policy_unreviewed_limit = ?6'// &
            ' WHERE name = ?1 RETURNING id;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        call stmt_bind_text(st, 2, p%tests, stat)
        call stmt_bind_text(st, 3, p%review, stat)
        call stmt_bind_int(st, 4, int(merge(1, 0, p%bisect), int64), stat)
        call stmt_bind_int(st, 5, int(p%batch, int64), stat)
        call stmt_bind_int(st, 6, int(p%unreviewed_limit, int64), stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine policy_put

    subroutine policy_set(p, key, value, error)
        !! Sets one setting of `p`; `error` says what is wrong with it, else ''.
        type(policy_t), intent(inout) :: p
        character(len=*), intent(in) :: key, value
        character(len=:), allocatable, intent(out) :: error
        integer :: n

        error = ''
        select case (key)
        case ('tests')
            if (value == 'full' .or. value == 'affected' .or. value == 'none') then
                p%tests = value
            else
                error = 'tests is full, affected or none'
            end if
        case ('review')
            if (value == 'after' .or. value == 'before') then
                p%review = value
            else
                error = 'review is after or before'
            end if
        case ('bisect')
            if (value == 'on' .or. value == 'off') then
                p%bisect = value == 'on'
            else
                error = 'bisect is on or off'
            end if
        case ('batch')
            n = number(value)
            if (n >= 1 .and. n <= BATCH_MAX) then
                p%batch = n
            else
                error = 'batch is a number from 1 to 64'
            end if
        case ('unreviewed_limit')
            n = number(value)
            if (n >= 0) then
                p%unreviewed_limit = n
            else
                error = 'unreviewed_limit is 0 (no limit) or a positive number'
            end if
        case default
            error = 'no such setting: '//key//' ('//POLICY_KEYS//')'
        end select

    contains

        integer function number(text) result(n)
            character(len=*), intent(in) :: text
            integer :: ios

            n = -1
            if (len(text) == 0 .or. len(text) > 8 .or. verify(text, '0123456789') > 0) return
            read (text, *, iostat=ios) n
            if (ios /= 0) n = -1
        end function number
    end subroutine policy_set

    subroutine policy_from_json(p, body, error)
        !! Applies the members of the JSON object `body` that name a setting;
        !! the others stay. A value is a string, the two numbers also a JSON
        !! integer; a body that is no object or a value of another type is an
        !! error, so that no caller takes an ignored setting for an applied one.
        type(policy_t), intent(inout) :: p
        character(len=*), intent(in) :: body
        character(len=:), allocatable, intent(out) :: error
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        character(len=:), allocatable :: key
        character(len=24) :: num
        type(policy_t) :: q
        integer :: depth

        q = p
        error = 'the body must be a JSON object of settings: '//POLICY_KEYS
        key = ''
        depth = 0
        call json_parser_init_strict(parser, body)
        do
            call json_parser_next(parser, event)
            select case (event%event_type)
            case (JSON_ERROR)
                return
            case (JSON_END_OF_INPUT)
                exit
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                if (depth == 0 .and. event%event_type == JSON_ARRAY_START) return
                if (depth == 1 .and. is_setting(key)) exit
                depth = depth + 1
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
            case (JSON_KEY)
                if (depth == 1) key = event%string_val
                cycle
            case default
                if (depth == 0) return
                if (depth == 1 .and. is_setting(key)) then
                    if (event%event_type == JSON_STRING) then
                        call policy_set(q, key, event%string_val, error)
                    else if (event%event_type == JSON_INTEGER .and. event%int64_valid .and. &
                            (key == 'batch' .or. key == 'unreviewed_limit')) then
                        write (num, '(i0)') event%int64_val
                        call policy_set(q, key, trim(num), error)
                    else
                        exit
                    end if
                    if (len(error) > 0) return
                    error = 'the body must be a JSON object of settings: '//POLICY_KEYS
                end if
            end select
            if (depth == 1) key = ''
        end do
        if (event%event_type /= JSON_END_OF_INPUT) then
            ! Says what the value has to be.
            call policy_set(q, key, '', error)
            return
        end if
        error = ''
        p = q

    contains

        pure logical function is_setting(name)
            character(len=*), intent(in) :: name
            is_setting = name == 'tests' .or. name == 'review' .or. name == 'bisect' .or. &
                name == 'batch' .or. name == 'unreviewed_limit'
        end function is_setting
    end subroutine policy_from_json

    function policy_json(repo, p) result(json)
        character(len=*), intent(in) :: repo
        type(policy_t), intent(in) :: p
        character(len=:), allocatable :: json

        json = '{"name":"'//json_escape_string(repo)//'","tests":"'//p%tests//'","review":"'// &
            p%review//'","bisect":"'//trim(merge('on ', 'off', p%bisect))//'","batch":'// &
            itoa(p%batch)//',"unreviewed_limit":'//itoa(p%unreviewed_limit)//'}'
    end function policy_json

    pure logical function policy_reviewed(c)
        !! A person reviewed the change, or the review agent did and did not
        !! flag it for a person.
        type(change_t), intent(in) :: c

        policy_reviewed = c%review == 'human-reviewed' .or. &
            (c%review == 'ai-reviewed' .and. .not. c%flagged)
    end function policy_reviewed

    integer function policy_unreviewed(db, repo) result(n)
        !! Landed changes of `repo` that wait for a review, and changes on
        !! their way to main that will: those of queued and running landings.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        type(stmt_t) :: st
        integer :: stat

        n = 0
        call db_prepare(db, "SELECT (SELECT count(*) FROM changes c WHERE c.repo = ?1 AND"// &
            " c.state = 'landed' AND "//WAITING//") + (SELECT count(DISTINCT c.id) FROM"// &
            " landings l JOIN changes c ON c.id = l.change WHERE l.repo = ?1 AND"// &
            " l.kind = 'landing' AND l.state IN ('queued', 'running') AND c.state = 'open'"// &
            ' AND '//WAITING//');', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, repo, stat)
        if (stmt_step(st) == SQLITE_ROW) n = int(stmt_int(st, 0))
        call stmt_finalize(st)
    end function policy_unreviewed

    function policy_refusal(db, repo, change, queueing) result(message)
        !! Why the policy of `repo` does not let the change with the id
        !! `change` (0: a branch without a change) land now, or ''. The limit
        !! on unreviewed changes holds when a landing is queued (`queueing`),
        !! not for one that is queued already.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo
        integer(int64), intent(in) :: change
        logical, intent(in) :: queueing
        character(len=:), allocatable :: message
        type(policy_t) :: p
        character(len=:), allocatable :: head, base
        type(change_t) :: c
        logical :: found
        integer :: n

        message = ''
        call policy_get(db, repo, p)
        if (p%review == 'before') then
            found = .false.
            if (change > 0) call change_by_id(db, change, c, found)
            if (.not. found) then
                message = 'The policy of '//repo//' asks for a review before landing'// &
                    ' (review=before), and this branch has no change to review.'
                return
            end if
            call change_last_version(db, c%id, head, base)
            if (.not. policy_reviewed(c)) then
                message = 'The policy of '//repo//' asks for a review before landing'// &
                    ' (review=before), and change #'//itoa(c%number)
                if (c%flagged) then
                    message = message//' is flagged for a person: '//c%flag_reason
                else
                    message = message//' is not reviewed yet.'
                end if
                return
            else if (len(head) == 0 .or. c%reviewed_head /= head) then
                ! A review under review=after stays when a version is pushed.
                message = 'The policy of '//repo//' asks for a review before landing'// &
                    ' (review=before), and the review of change #'//itoa(c%number)// &
                    ' is not of its newest version.'
                return
            end if
        end if
        if (queueing .and. p%unreviewed_limit > 0) then
            n = policy_unreviewed(db, repo)
            if (n >= p%unreviewed_limit) message = itoa(n)//' change(s) of '//repo// &
                ' landed or are about to land without a review, and its policy lets nothing'// &
                ' more land from '//itoa(p%unreviewed_limit)//' on (unreviewed_limit='// &
                itoa(p%unreviewed_limit)//'). Review them first: fy change list --all,'// &
                ' fy change reviewed N.'
        end if
    end function policy_refusal

    subroutine policy_queue(db, repo, branch, account, change, id, refusal, stat)
        !! Queues the landing `id` of `branch`, of the change with the id
        !! `change` (0: none), when the policy of `repo` lets it; else `refusal`
        !! says why not. `stat` is not 0 when the store fails.
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: repo, branch, account
        integer(int64), intent(in) :: change
        integer(int64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: refusal
        integer, intent(out) :: stat
        integer :: ignored

        id = 0
        refusal = ''
        ! The count of unreviewed changes and the new landing are one write
        ! transaction, so simultaneous requests do not pass the limit together.
        call db_exec(db, 'BEGIN IMMEDIATE;', stat)
        if (stat /= SQLITE_OK) return
        refusal = policy_refusal(db, repo, change, .true.)
        if (len(refusal) == 0) call store_landing_add(db, repo, branch, account, id, stat, change)
        if (len(refusal) == 0 .and. stat == SQLITE_OK) then
            call db_exec(db, 'COMMIT;', stat)
        else
            call db_exec(db, 'ROLLBACK;', ignored)
        end if
    end subroutine policy_queue

    function itoa(k) result(s)
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') k
        s = trim(b)
    end function itoa

end module fy_policy
