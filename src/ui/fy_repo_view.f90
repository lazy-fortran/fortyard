module fy_repo_view
    !! Repository pages of the browser application: the list of mirrored
    !! repositories, a file browser, the history and one commit with its diff.
    use fortui
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string, json_parser_t, json_event_t, &
        json_parser_init, json_parser_next, JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_BOOL, &
        JSON_OBJECT_END, JSON_END_OF_INPUT, JSON_ERROR
    use fy_fetch, only: request_t, fetch_start, fetch_ready
    implicit none
    private

    public :: repo_view_open, repo_view_compose, repo_view_receive, repo_view_load, &
        repo_view_reset

    type :: line_t
        character(len=:), allocatable :: s
    end type line_t

    type :: repo_t
        character(len=:), allocatable :: name, upstream, fetched
        logical :: ready = .false.
    end type repo_t

    type :: entry_t
        character(len=:), allocatable :: name, kind
        integer :: size = 0
    end type entry_t

    type :: commit_t
        character(len=:), allocatable :: id, author, date, subject
    end type commit_t

    integer, parameter :: MAX_FILE_LINES = 3000, MAX_DIFF_LINES = 4000
    real, parameter :: BODY = 15.0, SMALL = 13.0, HEADING = 17.0, CTRL = 15.0, CODE = 14.0
    real, parameter :: GAP = 12.0, ROW_H = 30.0

    type(request_t) :: repos_req, tree_req, blob_req, log_req, commit_req, add_req
    type(repo_t), allocatable :: repos(:)
    type(entry_t), allocatable :: entries(:)
    type(commit_t), allocatable :: commits(:)
    type(line_t), allocatable :: file_lines(:), diff_lines(:)
    character(len=:), allocatable :: repo, dir, file, commit_id, commit_head, note
    character(len=:), allocatable :: new_repo, new_upstream
    logical :: file_cut = .false., diff_cut = .false., loaded = .false.
    integer :: sub_tab = 1
    character(len=16), parameter :: sub_labels(2) = [character(len=16) :: "Code", "History"]

contains

    subroutine repo_view_open()
        !! Shows the repository list, loading it again.
        call init()
        repo = ''
        call fetch_start(repos_req, 'GET', '/api/v1/repos')
    end subroutine repo_view_open

    subroutine repo_view_reset()
        !! Forgets everything: the server renders pages for different people.
        loaded = .false.
        if (allocated(repos)) deallocate (repos)
        if (allocated(entries)) deallocate (entries)
        if (allocated(commits)) deallocate (commits)
        if (allocated(file_lines)) deallocate (file_lines)
        if (allocated(diff_lines)) deallocate (diff_lines)
        sub_tab = 1
        file_cut = .false.
        diff_cut = .false.
        call init()
    end subroutine repo_view_reset

    subroutine repo_view_load(json)
        !! Shows the repository list from a response already at hand.
        character(len=*), intent(in) :: json
        call init()
        repo = ''
        call parse_repos(json)
    end subroutine repo_view_load

    subroutine init()
        if (loaded) return
        loaded = .true.
        allocate (repos(0), entries(0), commits(0), file_lines(0), diff_lines(0))
        repo = ''
        dir = ''
        file = ''
        commit_id = ''
        commit_head = ''
        note = ''
        new_repo = ''
        new_upstream = ''
    end subroutine init

    subroutine repo_view_compose(page_w, narrow, admin)
        real, intent(in) :: page_w
        logical, intent(in) :: narrow, admin

        call init()
        if (len(repo) == 0) then
            call list_view(page_w, admin)
        else
            call repo_page(page_w, narrow)
        end if
        if (len(note) > 0) call text(note, size=SMALL, style=UI_CAPTION, tone=UI_TONE_NEGATIVE)
    end subroutine repo_view_compose

    subroutine list_view(page_w, admin)
        real, intent(in) :: page_w
        logical, intent(in) :: admin
        integer :: k
        logical :: submitted

        call column_begin(6.0)
        call text("Repositories", size=HEADING, style=UI_HEADING)
        if (size(repos) == 0) call text("No repositories yet.", size=BODY, style=UI_CAPTION)
        do k = 1, size(repos)
            call row_begin(GAP, id="r"//ui_str(k), height=ROW_H)
            if (button("open", repos(k)%name, size=CTRL, enabled=repos(k)%ready, &
                width=200.0, ellipsis=.true.)) call open_repo(repos(k)%name)
            call text(state_of(repos(k)), size=SMALL, style=UI_CAPTION, width=150.0)
            call text(repos(k)%upstream, size=SMALL, style=UI_CAPTION, width=UI_FILL, &
                ellipsis=.true.)
            call row_end()
        end do
        call column_end()

        if (.not. admin) return
        call column_begin(6.0)
        call text("Mirror a repository", size=HEADING, style=UI_HEADING)
        call row_begin(GAP, height=ROW_H + 4.0)
        if (text_input("newrepo", new_repo, placeholder="name, e.g. fx", width=160.0, &
            size=CTRL)) continue
        if (text_input("newupstream", new_upstream, &
            placeholder="git@github.com:org/repo.git or https://...", submitted=submitted, &
            width=UI_FILL, size=CTRL)) continue
        if (button("add", "Add", size=CTRL) .or. submitted) then
            if (len_trim(new_repo) > 0 .and. len_trim(new_upstream) > 0) then
                call fetch_start(add_req, 'POST', '/api/v1/repos', '{"name":"'// &
                    json_escape_string(trim(new_repo))//'","upstream":"'// &
                    json_escape_string(trim(new_upstream))//'"}')
            end if
        end if
        call row_end()
        call text("The server clones it within a few seconds and fetches it every minute.", &
            size=SMALL, style=UI_CAPTION)
        call column_end()
    end subroutine list_view

    function state_of(r) result(s)
        type(repo_t), intent(in) :: r
        character(len=:), allocatable :: s
        if (r%ready) then
            s = "fetched "//r%fetched(1:min(16, len(r%fetched)))
            if (len(s) >= 19) s(19:19) = ' '
        else
            s = "cloning..."
        end if
        s = trim(s)
    end function state_of

    subroutine repo_page(page_w, narrow)
        real, intent(in) :: page_w
        logical, intent(in) :: narrow

        call column_begin(GAP)
        call row_begin(GAP, height=ROW_H)
        if (button("back", "All repositories", size=CTRL)) then
            call repo_view_open()
            call column_end_row()
            return
        end if
        call text(repo, size=HEADING, style=UI_HEADING)
        call row_end()
        if (tabs("sub", sub_labels, sub_tab, width=min(240.0, page_w), size=CTRL)) then
            file = ''
            commit_id = ''
            if (sub_tab == 2 .and. size(commits) == 0) &
                call fetch_start(log_req, 'GET', '/api/v1/repos/'//repo//'/log')
        end if
        if (sub_tab == 1) then
            call code_view(page_w, narrow)
        else if (len(commit_id) > 0) then
            call commit_view(page_w)
        else
            call history_view(page_w, narrow)
        end if
        call column_end()
    end subroutine repo_page

    subroutine column_end_row()
        call row_end()
        call column_end()
    end subroutine column_end_row

    subroutine code_view(page_w, narrow)
        real, intent(in) :: page_w
        logical, intent(in) :: narrow
        integer :: k, start
        character(len=:), allocatable :: here

        ! Path bar: the repository root, then one button per directory.
        call row_begin(4.0, height=ROW_H)
        if (button("root", repo, size=SMALL)) call open_dir('')
        here = ''
        start = 1
        do k = 1, len(dir) + 1
            if (k <= len(dir)) then
                if (dir(k:k) /= '/') cycle
            end if
            if (k > start) then
                if (len(here) > 0) here = here//'/'
                here = here//dir(start:k - 1)
                call text("/", size=SMALL, style=UI_CAPTION)
                if (button("seg", dir(start:k - 1), key=k, size=SMALL)) call open_dir(here)
            end if
            start = k + 1
        end do
        if (len(file) > 0) then
            call text("/", size=SMALL, style=UI_CAPTION)
            call text(basename(file), size=SMALL)
        end if
        call row_end()

        if (len(file) > 0) then
            call file_view(page_w)
            return
        end if
        call column_begin(2.0)
        do k = 1, size(entries)
            call row_begin(GAP, id="e"//ui_str(k), height=26.0)
            if (entries(k)%kind == 'tree') then
                if (button("open", entries(k)%name//"/", size=SMALL, width=280.0, &
                    ellipsis=.true.)) call open_dir(join(dir, entries(k)%name))
            else
                if (button("open", entries(k)%name, size=SMALL, width=280.0, &
                    ellipsis=.true.)) call open_file(join(dir, entries(k)%name))
                if (.not. narrow) call text(human_size(entries(k)%size), size=SMALL, &
                    style=UI_CAPTION, tone=UI_TONE_MUTED)
            end if
            call row_end()
        end do
        call column_end()
    end subroutine code_view

    subroutine file_view(page_w)
        real, intent(in) :: page_w
        integer :: k
        real :: gutter

        gutter = 8.0 * 0.62 * CODE
        call column_begin(0.0)
        do k = 1, size(file_lines)
            call row_begin(10.0, id="l"//ui_str(k))
            call text(ui_str(k), style=UI_CODE, size=CODE, width=gutter, tone=UI_TONE_MUTED)
            call text(file_lines(k)%s, style=UI_CODE, size=CODE, width=UI_FILL)
            call row_end()
        end do
        call column_end()
        if (file_cut) call text("Only the first "//ui_str(MAX_FILE_LINES)//" lines are shown.", &
            size=SMALL, style=UI_CAPTION)
    end subroutine file_view

    subroutine history_view(page_w, narrow)
        real, intent(in) :: page_w
        logical, intent(in) :: narrow
        integer :: k
        call column_begin(2.0)
        do k = 1, size(commits)
            call row_begin(GAP, id="c"//ui_str(k), height=ROW_H)
            if (button("id", commits(k)%id(1:min(8, len(commits(k)%id))), key=k, size=SMALL, &
                width=90.0)) then
                commit_id = commits(k)%id
                call fetch_start(commit_req, 'GET', '/api/v1/repos/'//repo//'/commits/'// &
                    commit_id)
            end if
            call text(commits(k)%subject, size=BODY, width=UI_FILL, ellipsis=.true.)
            if (.not. narrow) then
                call text(commits(k)%author, size=SMALL, style=UI_CAPTION, width=180.0, &
                    ellipsis=.true.)
                call text(commits(k)%date(1:min(10, len(commits(k)%date))), size=SMALL, &
                    style=UI_CAPTION, width=110.0)
            end if
            call row_end()
        end do
        call column_end()
    end subroutine history_view

    subroutine commit_view(page_w)
        real, intent(in) :: page_w
        integer :: k, tone

        if (button("history", "Back to history", size=SMALL)) then
            commit_id = ''
            return
        end if
        call text(commit_head, size=BODY, width=UI_FILL, ellipsis=.true.)
        call text(commit_id, style=UI_CODE, size=SMALL, tone=UI_TONE_MUTED)
        call column_begin(0.0)
        do k = 1, size(diff_lines)
            tone = UI_TONE_NORMAL
            associate (s => diff_lines(k)%s)
                if (len(s) > 0) then
                    if (index(s, 'diff --git') == 1 .or. index(s, '+++') == 1 .or. &
                        index(s, '---') == 1) then
                        tone = UI_TONE_MUTED
                    else if (s(1:1) == '+') then
                        tone = UI_TONE_POSITIVE
                    else if (s(1:1) == '-') then
                        tone = UI_TONE_NEGATIVE
                    else if (index(s, '@@') == 1) then
                        tone = UI_TONE_ACCENT
                    end if
                end if
                call text(s, style=UI_CODE, size=CODE, width=UI_FILL, tone=tone, &
                    id="d"//ui_str(k))
            end associate
        end do
        call column_end()
        if (diff_cut) call text("The diff is cut here.", size=SMALL, style=UI_CAPTION)
    end subroutine commit_view

    subroutine open_repo(name)
        character(len=*), intent(in) :: name
        repo = name
        sub_tab = 1
        deallocate (commits)
        allocate (commits(0))
        commit_id = ''
        call open_dir('')
    end subroutine open_repo

    subroutine open_dir(path)
        character(len=*), intent(in) :: path
        dir = path
        file = ''
        note = ''
        call fetch_start(tree_req, 'GET', '/api/v1/repos/'//repo//'/tree?path='//encode(path))
    end subroutine open_dir

    subroutine open_file(path)
        character(len=*), intent(in) :: path
        file = path
        note = ''
        call set_lines(file_lines, 'Loading...', 1, file_cut)
        call fetch_start(blob_req, 'GET', '/api/v1/repos/'//repo//'/blob?path='//encode(path))
    end subroutine open_file

    subroutine repo_view_receive()
        !! Applies finished requests to the repository pages.
        logical :: found
        character(len=:), allocatable :: s

        call init()
        if (fetch_ready(repos_req)) then
            if (repos_req%status == 200) call parse_repos(repos_req%body)
        end if
        if (fetch_ready(add_req)) then
            if (add_req%status == 201) then
                new_repo = ''
                new_upstream = ''
                note = ''
            else
                call json_extract_string(add_req%body, 'error', s, found)
                note = 'Not added'
                if (found) note = note//': '//s
            end if
            call fetch_start(repos_req, 'GET', '/api/v1/repos')
        end if
        if (fetch_ready(tree_req)) then
            if (tree_req%status == 200) then
                call parse_entries(tree_req%body)
            else
                note = 'That directory could not be read.'
            end if
        end if
        if (fetch_ready(blob_req)) then
            if (blob_req%status == 200) then
                call set_lines(file_lines, blob_req%body, MAX_FILE_LINES, file_cut)
            else if (blob_req%status == 413) then
                call set_lines(file_lines, 'This file is larger than 1 MiB.', 1, file_cut)
            else
                call set_lines(file_lines, 'This file could not be read.', 1, file_cut)
            end if
        end if
        if (fetch_ready(log_req)) then
            if (log_req%status == 200) call parse_commits(log_req%body)
        end if
        if (fetch_ready(commit_req)) then
            if (commit_req%status == 200) then
                call json_extract_string(commit_req%body, 'message', s, found)
                commit_head = first_line(s)
                call json_extract_string(commit_req%body, 'patch', s, found)
                call set_lines(diff_lines, s, MAX_DIFF_LINES, diff_cut)
            else
                commit_head = 'This commit could not be read.'
                call set_lines(diff_lines, '', 0, diff_cut)
            end if
        end if
    end subroutine repo_view_receive

    subroutine set_lines(lines, content, limit, cut)
        !! Lines of `content`, tabs expanded to four spaces, at most `limit`.
        type(line_t), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: content
        integer, intent(in) :: limit
        logical, intent(out) :: cut
        integer :: n, pos, nl, k

        n = 0
        do k = 1, len(content)
            if (content(k:k) == new_line('a')) n = n + 1
        end do
        if (len(content) > 0) then
            if (content(len(content):) /= new_line('a')) n = n + 1
        end if
        cut = n > limit
        n = min(n, limit)
        if (allocated(lines)) deallocate (lines)
        allocate (lines(n))
        pos = 1
        do k = 1, n
            nl = index(content(pos:), new_line('a'))
            if (nl == 0) then
                lines(k)%s = untab(content(pos:))
                pos = len(content) + 1
            else
                lines(k)%s = untab(content(pos:pos + nl - 2))
                pos = pos + nl
            end if
        end do
    end subroutine set_lines

    function untab(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: k

        t = ''
        do k = 1, len(s)
            if (s(k:k) == achar(9)) then
                t = t//repeat(' ', 4 - mod(len(t), 4))
            else if (s(k:k) /= achar(13)) then
                t = t//s(k:k)
            end if
        end do
    end function untab

    subroutine parse_repos(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        type(repo_t) :: r
        character(len=:), allocatable :: key

        deallocate (repos)
        allocate (repos(0))
        r = repo_t('', '', '', .false.)
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                if (key == 'name') r%name = ev%string_val
                if (key == 'upstream') r%upstream = ev%string_val
                if (key == 'fetched') r%fetched = ev%string_val
            case (JSON_BOOL)
                if (key == 'ready') r%ready = ev%bool_val
            case (JSON_OBJECT_END)
                if (len(r%name) > 0) repos = [repos, r]
                r = repo_t('', '', '', .false.)
            end select
        end do
    end subroutine parse_repos

    subroutine parse_entries(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        type(entry_t) :: e
        character(len=:), allocatable :: key

        deallocate (entries)
        allocate (entries(0))
        e = entry_t('', '', 0)
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                if (key == 'name') e%name = ev%string_val
                if (key == 'type') e%kind = ev%string_val
            case (JSON_INTEGER)
                if (key == 'size') e%size = ev%int_val
            case (JSON_OBJECT_END)
                if (len(e%name) > 0) entries = [entries, e]
                e = entry_t('', '', 0)
            end select
        end do
    end subroutine parse_entries

    subroutine parse_commits(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        type(commit_t) :: c
        character(len=:), allocatable :: key

        deallocate (commits)
        allocate (commits(0))
        c = commit_t('', '', '', '')
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                select case (key)
                case ('id'); c%id = ev%string_val
                case ('author'); c%author = ev%string_val
                case ('date'); c%date = ev%string_val
                case ('subject'); c%subject = ev%string_val
                end select
            case (JSON_OBJECT_END)
                if (len(c%id) > 0) commits = [commits, c]
                c = commit_t('', '', '', '')
            end select
        end do
    end subroutine parse_commits

    function encode(s) result(e)
        !! Percent-encoding for a query value; '/' stays readable.
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
    end function encode

    function join(a, b) result(s)
        character(len=*), intent(in) :: a, b
        character(len=:), allocatable :: s
        if (len(a) == 0) then
            s = b
        else
            s = a//'/'//b
        end if
    end function join

    function basename(path) result(s)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: s
        s = path(index(path, '/', back=.true.) + 1:)
    end function basename

    function first_line(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: nl
        nl = index(s, new_line('a'))
        if (nl == 0) then
            t = s
        else
            t = s(1:nl - 1)
        end if
    end function first_line

    function human_size(bytes) result(s)
        integer, intent(in) :: bytes
        character(len=:), allocatable :: s
        if (bytes < 1024) then
            s = ui_str(bytes)//" B"
        else if (bytes < 1048576) then
            s = ui_str(real(bytes) / 1024.0, 1)//" KiB"
        else
            s = ui_str(real(bytes) / 1048576.0, 1)//" MiB"
        end if
    end function human_size

end module fy_repo_view
