module fy_receipt
    !! Closing receipts for GitHub pull requests (doc/github-pull-requests.md).
    !! The server holds no GitHub credentials: it lists the landings whose
    !! receipt is still due, and the GitHub backend (fy_backend_github)
    !! comments on and closes the pull requests and records the receipt here.
    !!   GET  /api/v1/receipts?owner=OWNER
    !!   POST /api/v1/receipts/N {"prs":"435,436"}
    use, intrinsic :: iso_fortran_env, only: &
        int64
    use fx_json_build, only: &
        json_escape_string
    use fy_sqlite, only: &
        SQLITE_OK, &
        SQLITE_ROW, &
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
        receipt_due, &
        receipt_owner_ok, &
        receipt_record

    !! Landings older than this get no receipt, so a repository whose pull
    !! requests the backend cannot close stops costing GitHub requests.
    character(len=*), parameter :: WINDOW = '-14 days'
    integer, parameter :: BATCH = 50

contains

    pure logical function receipt_owner_ok(owner)
        !! A GitHub account name: 1 to 39 of a-z, A-Z, 0-9 and -.
        character(len=*), intent(in) :: owner
        receipt_owner_ok = len(owner) >= 1 .and. len(owner) <= 39 .and. &
            verify(owner, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-') == 0
    end function receipt_owner_ok

    subroutine receipt_due(db, owner, base_url, answer, stat)
        !! The landings of the last two weeks into a primary repository whose
        !! upstream is a GitHub repository of `owner` that have no receipt yet,
        !! oldest first: {"receipts":[{"id","repo","branch","commit",
        !! "finished","repository","url"}]}, with "repository" OWNER/NAME and
        !! "url" the change's page ('' without a change or a public address).
        type(db_t), intent(in) :: db
        character(len=*), intent(in) :: owner, base_url
        character(len=:), allocatable, intent(out) :: answer
        integer, intent(out) :: stat
        type(stmt_t) :: st
        character(len=:), allocatable :: repo, url
        character(len=24) :: num, cnum
        integer :: number
        logical :: first

        answer = '{"receipts":[]}'
        call db_prepare(db, 'SELECT l.id, l.repo, l.branch, l.commit_id, l.finished_at,'// &
            ' r.upstream, coalesce(c.number, 0) FROM landings l JOIN repos r ON r.name = l.repo'// &
            ' LEFT JOIN changes c ON c.id = l.change'// &
            " WHERE l.state = 'landed' AND l.kind = 'landing' AND r.mode = 'primary'"// &
            " AND coalesce(l.commit_id, '') <> ''"// &
            " AND l.finished_at >= strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '"//WINDOW//"')"// &
            " AND (instr(lower(r.upstream), 'github.com:' || lower(?1) || '/') > 0"// &
            " OR instr(lower(r.upstream), 'github.com/' || lower(?1) || '/') > 0)"// &
            ' AND NOT EXISTS (SELECT 1 FROM github_receipts g WHERE g.landing = l.id)'// &
            ' ORDER BY l.id LIMIT ?2;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_text(st, 1, owner, stat)
        call stmt_bind_int(st, 2, int(BATCH, int64), stat)
        answer = '{"receipts":['
        first = .true.
        do while (stmt_step(st) == SQLITE_ROW)
            write (num, '(i0)') stmt_int(st, 0)
            repo = stmt_text(st, 1)
            number = int(stmt_int(st, 6))
            url = ''
            if (number > 0 .and. len(base_url) > 0) then
                write (cnum, '(i0)') number
                url = base_url//'/'//repo//'/-/changes/'//trim(cnum)
            end if
            if (.not. first) answer = answer//','
            first = .false.
            answer = answer//'{"id":'//trim(num)//',"repo":"'//json_escape_string(repo)// &
                '","branch":"'//json_escape_string(stmt_text(st, 2))// &
                '","commit":"'//json_escape_string(stmt_text(st, 3))// &
                '","finished":"'//json_escape_string(stmt_text(st, 4))// &
                '","repository":"'//json_escape_string(github_slug(stmt_text(st, 5)))// &
                '","url":"'//json_escape_string(url)//'"}'
        end do
        call stmt_finalize(st)
        answer = answer//']}'
        stat = SQLITE_OK
    end subroutine receipt_due

    subroutine receipt_record(db, landing, prs, found)
        !! Records that landing `landing` has its receipt: `prs` lists the pull
        !! requests that were closed for it. `found` is false for a landing
        !! that did not land; recording twice keeps the first record.
        type(db_t), intent(in) :: db
        integer(int64), intent(in) :: landing
        character(len=*), intent(in) :: prs
        logical, intent(out) :: found
        type(stmt_t) :: st
        integer :: stat

        found = .false.
        call db_prepare(db, 'INSERT INTO github_receipts (landing, prs, sent_at)'// &
            " SELECT id, ?2, strftime('%Y-%m-%dT%H:%M:%SZ','now') FROM landings"// &
            " WHERE id = ?1 AND state = 'landed' AND kind = 'landing'"// &
            ' ON CONFLICT (landing) DO NOTHING;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, landing, stat)
        call stmt_bind_text(st, 2, prs, stat)
        stat = stmt_step(st)
        call stmt_finalize(st)
        call db_prepare(db, 'SELECT 1 FROM github_receipts WHERE landing = ?1;', st, stat)
        if (stat /= SQLITE_OK) return
        call stmt_bind_int(st, 1, landing, stat)
        found = stmt_step(st) == SQLITE_ROW
        call stmt_finalize(st)
    end subroutine receipt_record

    function github_slug(url) result(slug)
        !! OWNER/NAME of git@github.com:OWNER/NAME.git, https://github.com/OWNER/NAME
        !! or ssh://git@github.com/OWNER/NAME.
        character(len=*), intent(in) :: url
        character(len=:), allocatable :: slug
        integer :: k

        k = index(url, 'github.com', back=.true.)
        slug = ''
        if (k == 0) return
        slug = trim(url(k + 11:))
        do while (len(slug) > 0)
            if (slug(len(slug):) /= '/') exit
            slug = slug(1:len(slug) - 1)
        end do
        if (len(slug) > 4) then
            if (slug(len(slug) - 3:) == '.git') slug = slug(1:len(slug) - 4)
        end if
    end function github_slug

end module fy_receipt
