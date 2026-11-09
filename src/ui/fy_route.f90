module fy_route
    !! The application's addresses. A repository lives at its path,
    !! GROUP/.../REPO as in GitLab, and its pages follow a /-/ separator:
    !! /PATH/-/tree/DIR, /PATH/-/blob/FILE, /PATH/-/commits, /PATH/-/commit/ID.
    !! The application's own pages are /, /tokens and /server; the first
    !! segments in RESERVED can never name a group or repository.
    implicit none
    private

    public :: route_t, route_parse, route_is_page, route_href, url_encode, url_decode
    public :: go, go_take

    integer, parameter, public :: R_REPOS = 1, R_TOKENS = 2, R_SERVER = 3, R_TREE = 4, &
        R_BLOB = 5, R_COMMITS = 6, R_COMMIT = 7, R_MISSING = 8

    character(len=7), parameter :: RESERVED(*) = [character(len=7) :: 'tokens', 'server', &
        'status', 'healthz', 'api', 'app', '-']

    type :: route_t
        integer :: kind = R_MISSING
        !! Repository path, and the directory, file or commit of its page (decoded).
        character(len=:), allocatable :: repo, path
        !! ?plain=1: a Markdown file as code instead of rendered.
        logical :: plain = .false.
    end type route_t

    character(len=:), allocatable :: pending

contains

    subroutine route_parse(location, r)
        !! The page a location (path and query, as in the address bar) shows.
        character(len=*), intent(in) :: location
        type(route_t), intent(out) :: r
        character(len=:), allocatable :: p, q, first, rest
        integer :: at

        r%repo = ''
        r%path = ''
        at = index(location, '?')
        if (at > 0) then
            p = location(1:at - 1)
            q = location(at + 1:)
        else
            p = location
            q = ''
        end if
        r%plain = index('&'//q//'&', '&plain=1&') > 0
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
        at = index(p(2:), '/')
        if (at == 0) then
            first = p(2:)
        else
            first = p(2:at)
        end if
        if (first == 'tokens' .and. at == 0) r%kind = R_TOKENS
        if (first == 'server' .and. at == 0) r%kind = R_SERVER
        if (is_reserved(first)) return

        at = index(p, '/-/')
        if (at == 0) then
            r%repo = p(2:)
            rest = 'tree'
        else
            r%repo = p(2:at - 1)
            rest = p(at + 3:)
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

    function route_href(kind, repo, path, plain) result(href)
        !! The location of a page of repository `repo`.
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
        end select
    end function route_href

    subroutine go(href)
        !! Asks the application to show `href` (a link was followed).
        character(len=*), intent(in) :: href
        pending = href
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
