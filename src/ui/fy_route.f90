module fy_route
    !! The application's addresses. A repository lives at its path,
    !! GROUP/.../REPO as in GitLab, and its pages follow a /-/ separator:
    !! /PATH/-/tree/DIR, /PATH/-/blob/FILE, /PATH/-/commits, /PATH/-/commit/ID.
    !! The application's own pages are /, /tokens and /server; the first
    !! segments in RESERVED can never name a group or repository.
    implicit none
    private

    public :: route_t, route_parse, route_is_page, route_href, url_encode, url_decode
    public :: go, go_take, route_set_base, at

    integer, parameter, public :: R_REPOS = 1, R_TOKENS = 2, R_SERVER = 3, R_TREE = 4, &
        R_BLOB = 5, R_COMMITS = 6, R_COMMIT = 7, R_MISSING = 8, R_JOBS = 9, R_CHANGES = 10, &
        R_CHANGE = 11, R_ISSUES = 12, R_ISSUE = 13

    character(len=7), parameter :: RESERVED(*) = [character(len=7) :: 'tokens', 'server', 'jobs', &
        'status', 'healthz', 'api', 'app', '-']

    type :: route_t
        integer :: kind = R_MISSING
        !! Repository path, and the directory, file or commit of its page (decoded).
        character(len=:), allocatable :: repo, path
        !! ?plain=1: a Markdown file as code instead of rendered.
        logical :: plain = .false.
        !! ?all: a list of changes or issues with the closed and landed ones.
        logical :: all = .false.
    end type route_t

    character(len=:), allocatable :: pending
    !! Where the application lives below the host, e.g. "/yard"; empty at the root.
    !! Paths inside the application never carry it; rendered links do.
    character(len=:), allocatable :: base

contains

    subroutine route_parse(location, r)
        !! The page a location (path and query, as in the address bar) shows.
        character(len=*), intent(in) :: location
        type(route_t), intent(out) :: r
        character(len=:), allocatable :: p, q, first, rest
        integer :: qpos

        r%repo = ''
        r%path = ''
        qpos = index(location, '?')
        if (qpos > 0) then
            p = location(1:qpos - 1)
            q = location(qpos + 1:)
        else
            p = location
            q = ''
        end if
        r%plain = index('&'//q//'&', '&plain=1&') > 0
        r%all = index('&'//q//'&', '&all&') > 0
        do while (len(p) > 1)
            if (p(len(p):) /= '/') exit
            p = p(1:len(p) - 1)
        end do
        if (len(p) == 0 .or. p == '/') then
            r%kind = R_REPOS
            return
        end if
        r%kind = R_MISSING
        if (p(1:1) /= '/') return
        qpos = index(p(2:), '/')
        if (qpos == 0) then
            first = p(2:)
        else
            first = p(2:qpos)
        end if
        if (first == 'tokens' .and. qpos == 0) r%kind = R_TOKENS
        if (first == 'server' .and. qpos == 0) r%kind = R_SERVER
        if (first == 'jobs' .and. qpos == 0) then
            r%kind = R_JOBS
            r%repo = url_decode(query_value(q, 'repo'))
        end if
        if (is_reserved(first)) return

        qpos = index(p, '/-/')
        if (qpos == 0) then
            r%repo = p(2:)
            rest = 'tree'
        else
            r%repo = p(2:qpos - 1)
            rest = p(qpos + 3:)
        end if
        if (len(r%repo) == 0 .or. index(r%repo, '//') > 0) return
        r%repo = url_decode(r%repo)
        if (rest == 'tree' .or. rest == 'commits') then
            r%kind = merge(R_TREE, R_COMMITS, rest == 'tree')
        else if (index(rest, 'tree/') == 1) then
            r%kind = R_TREE
            r%path = url_decode(rest(6:))
        else if (index(rest, 'blob/') == 1 .and. len(rest) > 5) then
            r%kind = R_BLOB
            r%path = url_decode(rest(6:))
        else if (index(rest, 'commit/') == 1 .and. len(rest) > 7) then
            r%kind = R_COMMIT
            r%path = url_decode(rest(8:))
        else if (rest == 'changes') then
            r%kind = R_CHANGES
        else if (index(rest, 'changes/') == 1 .and. len(rest) > 8) then
            r%kind = R_CHANGE
            r%path = rest(9:)
        else if (rest == 'issues') then
            r%kind = R_ISSUES
        else if (index(rest, 'issues/') == 1 .and. len(rest) > 7) then
            r%kind = R_ISSUE
            r%path = rest(8:)
        end if
    end subroutine route_parse

    logical function route_is_page(path)
        !! Whether the server answers `path` with the application's page: every
        !! location except those under a reserved name (a page may then say that
        !! nothing is there).
        character(len=*), intent(in) :: path
        type(route_t) :: r
        integer :: slash
        call route_parse(path, r)
        route_is_page = r%kind /= R_MISSING
        if (route_is_page .or. len(path) < 2) return
        slash = index(path(2:), '/')
        if (slash == 0) slash = len(path)
        route_is_page = .not. is_reserved(path(2:slash))
    end function route_is_page

    subroutine route_set_base(prefix)
        character(len=*), intent(in) :: prefix
        base = prefix
        if (len(base) > 0) then
            if (base(len(base):) == '/') base = base(1:len(base) - 1)
        end if
    end subroutine route_set_base

    function at(path) result(href)
        !! The rendered link to application path `path`.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: href
        if (.not. allocated(base)) base = ''
        href = base//path
    end function at

    function unbased(href) result(path)
        !! `href` without the base, as the application's own path.
        character(len=*), intent(in) :: href
        character(len=:), allocatable :: path
        if (.not. allocated(base)) base = ''
        path = href
        if (len(base) == 0 .or. len(href) < len(base)) return
        if (href(1:len(base)) /= base) return
        path = href(len(base) + 1:)
        if (len(path) == 0) path = '/'
    end function unbased

    function route_href(kind, repo, path, plain) result(href)
        !! The location of a page of repository `repo`. (rendered, with the base).
        integer, intent(in) :: kind
        character(len=*), intent(in) :: repo
        character(len=*), intent(in), optional :: path
        logical, intent(in), optional :: plain
        character(len=:), allocatable :: href, sub

        sub = ''
        if (present(path)) sub = url_encode(path)
        href = '/'//url_encode(repo)
        select case (kind)
        case (R_TREE)
            if (len(sub) > 0) href = href//'/-/tree/'//sub
        case (R_BLOB)
            href = href//'/-/blob/'//sub
            if (present(plain)) then
                if (plain) href = href//'?plain=1'
            end if
        case (R_COMMITS)
            href = href//'/-/commits'
        case (R_COMMIT)
            href = href//'/-/commit/'//sub
        case (R_CHANGES)
            href = href//'/-/changes'
        case (R_CHANGE)
            href = href//'/-/changes/'//sub
        case (R_ISSUES)
            href = href//'/-/issues'
        case (R_ISSUE)
            href = href//'/-/issues/'//sub
        end select
        href = at(href)
    end function route_href

    subroutine go(href)
        !! Asks the application to show `href` (a link was followed).
        character(len=*), intent(in) :: href
        pending = unbased(href)
    end subroutine go

    logical function go_take(href)
        !! The location a link asked for, once.
        character(len=:), allocatable, intent(out) :: href
        go_take = allocated(pending)
        if (go_take) then
            href = pending
            deallocate (pending)
        else
            href = ''
        end if
    end function go_take

    function query_value(q, name) result(v)
        !! The raw value of `name` in query string `q`, or empty.
        character(len=*), intent(in) :: q, name
        character(len=:), allocatable :: v
        integer :: k, e
        v = ''
        k = index('&'//q, '&'//name//'=')
        if (k == 0) return
        v = q(k + len(name) + 1:)
        e = index(v, '&')
        if (e > 0) v = v(1:e - 1)
    end function query_value

    logical function is_reserved(segment)
        character(len=*), intent(in) :: segment
        integer :: k
        is_reserved = .false.
        do k = 1, size(RESERVED)
            if (segment == trim(RESERVED(k))) is_reserved = .true.
        end do
    end function is_reserved

    function url_encode(s) result(e)
        !! Percent-encoding of a path or query value; '/' stays readable.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: e
        character(len=*), parameter :: hexd = '0123456789ABCDEF'
        integer :: k, c

        e = ''
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (index('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._~/-', &
                s(k:k)) > 0) then
                e = e//s(k:k)
            else
                e = e//'%'//hexd(c / 16 + 1:c / 16 + 1)//hexd(mod(c, 16) + 1:mod(c, 16) + 1)
            end if
        end do
    end function url_encode

    function url_decode(s) result(d)
        !! %XX escapes; a malformed escape stays as it is.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: d
        integer :: i, hi, lo

        d = ''
        i = 1
        do while (i <= len(s))
            if (s(i:i) == '%' .and. i + 2 <= len(s)) then
                hi = hexval(s(i + 1:i + 1))
                lo = hexval(s(i + 2:i + 2))
                if (hi >= 0 .and. lo >= 0) then
                    d = d//achar(16 * hi + lo)
                    i = i + 3
                    cycle
                end if
            end if
            d = d//s(i:i)
            i = i + 1
        end do
    end function url_decode

    pure integer function hexval(c)
        character, intent(in) :: c
        hexval = index('0123456789abcdef', c) - 1
        if (hexval < 0) hexval = index('0123456789ABCDEF', c) - 1
    end function hexval

end module fy_route
