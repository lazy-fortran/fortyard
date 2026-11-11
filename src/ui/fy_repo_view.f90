module fy_repo_view
    !! Repository pages: the list of mirrored repositories, a repository's
    !! files with its README, a file with highlighted lines (Markdown rendered),
    !! the history, and a commit with its diff per file. Every page has its own
    !! location (fy_route); a page already loaded for the same location is shown
    !! again without asking the server.
    use fortui
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        JSON_BOOL, &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_INTEGER, &
        JSON_KEY, &
        JSON_OBJECT_END, &
        JSON_STRING, &
        json_event_t, &
        json_extract_string, &
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    use fy_code_lines, only: &
        diff_compose, &
        diff_load, &
        line_t, &
        numbered, &
        plain_lines, &
        right, &
        shown_file_t, &
        untab
    use fy_fetch, only: &
        fetch_ready, &
        fetch_start, &
        request_t
    use fy_highlight, only: &
        highlight, &
        lang_of
    use fy_markdown, only: &
        md_compose, &
        md_doc_t, &
        md_parse
    use fy_nav, only: &
        nav_copy
    use fy_pages
    use fy_route, only: &
        at, &
        go, &
        route_href, &
        route_t, &
        url_encode
    use fy_time, only: &
        ago, &
        day_of
    implicit none
    private

    public :: &
        repo_view_compose, &
        repo_view_kind, &
        repo_view_list, &
        repo_view_list_compose, &
        repo_view_missing, &
        repo_view_open_issues, &
        repo_view_receive, &
        repo_view_repo, &
        repo_view_repos, &
        repo_view_reset, &
        repo_view_show, &
        repo_view_title

    type :: repo_t
        character(len=:), allocatable :: name, upstream, fetched
        !! "primary" (takes pushes here, pushed on to upstream) or "mirror".
        character(len=:), allocatable :: mode
        logical :: ready = .false.
        integer :: open_issues = 0
    end type repo_t

    type :: entry_t
        character(len=:), allocatable :: name, kind
        integer :: size = 0
    end type entry_t

    type :: commit_t
        character(len=:), allocatable :: id, author, date, subject
    end type commit_t

    integer, parameter :: MAX_FILE_LINES = 5000
    real, parameter :: BODY = 14.0, SMALL = 12.0, HEADING = 16.0, CODE = 12.0
    real, parameter :: GAP = 8.0, ROW_H = 40.0

    type(request_t) :: repos_req, refs_req, tree_req, blob_req, readme_req, log_req, &
        commit_req, add_req
    type(repo_t), allocatable :: repos(:)
    type(entry_t), allocatable :: entries(:)
    type(commit_t), allocatable :: commits(:)
    type(line_t), allocatable :: file_lines(:)
    type(shown_file_t), allocatable :: diff_files(:)
    type(md_doc_t) :: readme, file_md
    !! The page shown: repository, its directory, file or commit.
    character(len=:), allocatable :: repo, dir, file, commit_id, branch, note
    !! What is loaded, so going back to it needs no request.
    character(len=:), allocatable :: refs_of, log_of, tree_of, blob_of, commit_of
    character(len=:), allocatable :: new_repo, new_upstream, readme_name, file_info
    character(len=:), allocatable :: c_subject, c_body, c_author, c_date
    logical :: file_cut = .false., diff_cut = .false., loaded = .false., has_readme = .false.
    logical :: as_code = .false., is_md = .false., repos_loaded = .false., missing = .false.
    logical :: tree_ready = .false., log_ready = .false., copied = .false.
    integer :: kind = R_TREE, file_bytes = 0

contains

    subroutine repo_view_reset()
        !! Forgets everything: the server renders pages for different people.
        loaded = .false.
        if (allocated(repos)) deallocate (repos)
        if (allocated(entries)) deallocate (entries)
        if (allocated(commits)) deallocate (commits)
        if (allocated(file_lines)) deallocate (file_lines)
        if (allocated(diff_files)) deallocate (diff_files)
        if (allocated(readme%blocks)) deallocate (readme%blocks)
        if (allocated(file_md%blocks)) deallocate (file_md%blocks)
        call init()
    end subroutine repo_view_reset

    subroutine init()
        if (loaded) return
        loaded = .true.
        allocate (repos(0), entries(0), commits(0), file_lines(0), diff_files(0))
        allocate (readme%blocks(0), file_md%blocks(0))
        repo = ''
        dir = ''
        file = ''
        commit_id = ''
        branch = ''
        note = ''
        refs_of = ''
        log_of = ''
        tree_of = ''
        blob_of = ''
        commit_of = ''
        new_repo = ''
        new_upstream = ''
        readme_name = ''
        file_info = ''
        c_subject = ''
        c_body = ''
        c_author = ''
        c_date = ''
        file_cut = .false.
        diff_cut = .false.
        has_readme = .false.
        as_code = .false.
        is_md = .false.
        repos_loaded = .false.
        missing = .false.
        tree_ready = .false.
        log_ready = .false.
        kind = R_TREE
    end subroutine init

    subroutine repo_view_list()
        !! The repository list, asked for again (it changes as mirrors are added).
        call init()
        repo = ''
        note = ''
        call fetch_start(repos_req, 'GET', '/api/v1/repos')
    end subroutine repo_view_list

    subroutine repo_view_repos()
        !! Asks for the repositories with their numbers, which the repository
        !! row of every page shows; what is known stays until the answer.
        call init()
        call fetch_start(repos_req, 'GET', '/api/v1/repos')
    end subroutine repo_view_repos

    integer function repo_view_open_issues(name) result(n)
        !! The open issues of repository `name`, or -1 while that is not known.
        character(len=*), intent(in) :: name
        integer :: k
        call init()
        n = -1
        do k = 1, size(repos)
            if (repos(k)%name == name) n = repos(k)%open_issues
        end do
    end function repo_view_open_issues

    subroutine repo_view_show(r)
        !! A page of a repository.
        type(route_t), intent(in) :: r
        character(len=:), allocatable :: key

        call init()
        if (r%repo /= repo) branch = ''
        repo = r%repo
        kind = r%kind
        as_code = r%plain
        note = ''
        copied = .false.
        missing = .false.
        if (refs_of /= repo) then
            refs_of = repo
            call fetch_start(refs_req, 'GET', api(repo, 'refs'))
        end if
        if (log_of /= repo) then
            log_of = repo
            log_ready = .false.
            deallocate (commits)
            allocate (commits(0))
            call fetch_start(log_req, 'GET', api(repo, 'log'))
        end if
        select case (kind)
        case (R_TREE)
            dir = r%path
            file = ''
            key = repo//':'//dir
            if (tree_of /= key) then
                tree_of = key
                tree_ready = .false.
                has_readme = .false.
                deallocate (entries)
                allocate (entries(0))
                call fetch_start(tree_req, 'GET', api(repo, 'tree')//'?path='//url_encode(dir))
            end if
        case (R_BLOB)
            file = r%path
            dir = parent_of(file)
            is_md = lang_of(file) == lang_of('x.md')
            key = repo//':'//file
            if (blob_of /= key) then
                blob_of = key
                file_info = ''
                call plain_lines(file_lines, 'Loading...')
                if (allocated(file_md%blocks)) deallocate (file_md%blocks)
                allocate (file_md%blocks(0))
                call fetch_start(blob_req, 'GET', api(repo, 'blob')//'?path='//url_encode(file))
            end if
        case (R_COMMIT)
            commit_id = r%path
            key = repo//':'//commit_id
            if (commit_of /= key) then
                commit_of = key
                c_subject = 'Loading...'
                c_body = ''
                c_author = ''
                c_date = ''
                deallocate (diff_files)
                allocate (diff_files(0))
                call fetch_start(commit_req, 'GET', api(repo, 'commits/'//url_encode(commit_id)))
            end if
        end select
    end subroutine repo_view_show

    function api(name, sub) result(path)
        !! The API location of a repository's resource; the one place that maps
        !! repository paths to the API.
        character(len=*), intent(in) :: name, sub
        character(len=:), allocatable :: path
        path = '/api/v1/repos/'//url_encode(name)//'/'//sub
    end function api

    function repo_view_repo() result(s)
        character(len=:), allocatable :: s
        call init()
        s = repo
    end function repo_view_repo

    integer function repo_view_kind()
        repo_view_kind = kind
    end function repo_view_kind

    logical function repo_view_missing()
        !! Whether the repository, path or commit shown does not exist.
        repo_view_missing = missing
    end function repo_view_missing

    function repo_view_title() result(s)
        !! The page's title, as the browser shows it in tabs and history.
        character(len=:), allocatable :: s
        call init()
        select case (kind)
        case (R_BLOB)
            s = file//' · '//repo
        case (R_COMMITS)
            s = 'History · '//repo
        case (R_COMMIT)
            s = c_subject//' · '//repo//'@'//short(commit_id)
        case default
            s = repo
            if (len(dir) > 0) s = repo//'/'//dir
        end select
    end function repo_view_title

    ! ---- repository list --------------------------------------------------------

    subroutine repo_view_list_compose(admin)
        logical, intent(in) :: admin
        integer :: k
        logical :: submitted

        call init()
        call row_begin(GAP, id="pagehead", height=ROW_H)
        call text("Repositories", size=20.0, style=UI_HEADING)
        if (size(repos) > 0) call text(ui_str(size(repos)), size=SMALL, id="count")
        call row_end()
        if (size(repos) == 0) then
            if (repos_loaded) then
                call text("No repositories are mirrored yet.", size=BODY, tone=UI_TONE_MUTED)
            else
                call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            end if
        else
            call column_begin(0.0, id="card-repos")
            do k = 1, size(repos)
                call row_begin(12.0, id="row-r"//ui_str(k), height=64.0)
                call text("", id="icon-repo")
                call column_begin(4.0)
                if (repos(k)%ready) then
                    if (link("name", repos(k)%name, route_href(R_TREE, repos(k)%name), &
                        size=16.0, style=UI_HEADING)) call go(route_href(R_TREE, repos(k)%name))
                else
                    call text(repos(k)%name, size=16.0, style=UI_HEADING, tone=UI_TONE_MUTED)
                end if
                if (len(repos(k)%upstream) > 0) call text(origin_of(repos(k)), size=SMALL, &
                    tone=UI_TONE_MUTED, width=UI_FILL)
                call text(state_of(repos(k)), size=SMALL, tone=UI_TONE_MUTED)
                call column_end()
                call row_end()
            end do
            call column_end()
        end if

        if (.not. admin) then
            if (len(note) > 0) call text(note, size=SMALL, tone=UI_TONE_NEGATIVE)
            return
        end if
        call column_begin(8.0, id="mirror")
        call text("Mirror a repository", size=HEADING, style=UI_HEADING)
        call row_begin(GAP, id="addform", height=ROW_H)
        if (text_input("newrepo", new_repo, placeholder="Name, e.g. fx", width=180.0, &
            size=BODY)) continue
        if (text_input("newupstream", new_upstream, &
            placeholder="git@github.com:org/repo.git or https://...", submitted=submitted, &
            width=UI_FILL, size=BODY)) continue
        if (button("add", "Add mirror", size=BODY) .or. submitted) then
            if (len_trim(new_repo) > 0 .and. len_trim(new_upstream) > 0) then
                call fetch_start(add_req, 'POST', '/api/v1/repos', '{"name":"'// &
                    json_escape_string(trim(new_repo))//'","upstream":"'// &
                    json_escape_string(trim(new_upstream))//'"}')
            end if
        end if
        call row_end()
        call text("The server clones it within a few seconds and then fetches it every minute.", &
            size=SMALL, tone=UI_TONE_MUTED)
        if (len(note) > 0) call text(note, size=SMALL, tone=UI_TONE_NEGATIVE)
        call column_end()
    end subroutine repo_view_list_compose

    function state_of(r) result(s)
        type(repo_t), intent(in) :: r
        character(len=:), allocatable :: s
        if (r%mode == 'primary') then
            s = "Hosted here"
        else if (r%ready .and. len(r%fetched) == 0) then
            s = "Mirrored"
        else if (r%ready) then
            s = "Updated "//ago(r%fetched)
        else
            s = "Cloning..."
        end if
    end function state_of

    function origin_of(r) result(s)
        !! Where a repository's history comes from or goes to.
        type(repo_t), intent(in) :: r
        character(len=:), allocatable :: s
        if (r%mode == 'primary') then
            s = "Pushed on to "//r%upstream
        else
            s = "Mirror of "//r%upstream
        end if
    end function origin_of

    function upstream_of(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        integer :: k
        s = ''
        do k = 1, size(repos)
            if (repos(k)%name == name .and. len(repos(k)%upstream) > 0) s = origin_of(repos(k))
        end do
    end function upstream_of

    ! ---- one repository ---------------------------------------------------------------

    subroutine repo_view_compose()
        logical :: on_code

        call init()
        on_code = kind == R_TREE .or. kind == R_BLOB
        call row_begin(0.0, id="repotabs", height=48.0)
        if (link("code", "Code", route_href(R_TREE, repo), size=BODY, &
            tone=merge(UI_TONE_STRONG, UI_TONE_NORMAL, on_code))) &
            call go(route_href(R_TREE, repo))
        if (link("history", "History", route_href(R_COMMITS, repo), size=BODY, &
            tone=merge(UI_TONE_NORMAL, UI_TONE_STRONG, on_code))) &
            call go(route_href(R_COMMITS, repo))
        call row_end()
        select case (kind)
        case (R_TREE)
            call code_view()
        case (R_BLOB)
            call code_view()
        case (R_COMMIT)
            call commit_view()
        case default
            call history_view()
        end select
        if (len(note) > 0) call text(note, size=BODY, tone=UI_TONE_NEGATIVE, id="note")
    end subroutine repo_view_compose

    subroutine code_view()
        integer :: k, start
        character(len=:), allocatable :: here, up

        ! Branch and path.
        call row_begin(GAP, id="pathbar", height=ROW_H)
        if (len(branch) > 0) call text(branch, size=SMALL, id="branch")
        call row_begin(4.0, id="crumbs")
        if (len(dir) == 0 .and. len(file) == 0) then
            call text(leaf(repo), size=HEADING, style=UI_HEADING)
        else
            if (link("root", leaf(repo), route_href(R_TREE, repo), size=HEADING)) &
                call go(route_href(R_TREE, repo))
        end if
        here = ''
        start = 1
        do k = 1, len(dir) + 1
            if (k <= len(dir)) then
                if (dir(k:k) /= '/') cycle
            end if
            if (k > start) then
                if (len(here) > 0) here = here//'/'
                here = here//dir(start:k - 1)
                call text("/", size=HEADING, tone=UI_TONE_MUTED)
                if (k > len(dir) .and. len(file) == 0) then
                    call text(dir(start:k - 1), size=HEADING, style=UI_HEADING)
                else if (link("seg", dir(start:k - 1), route_href(R_TREE, repo, here), &
                        key=k, size=HEADING)) then
                    call go(route_href(R_TREE, repo, here))
                end if
            end if
            start = k + 1
        end do
        if (len(file) > 0) then
            call text("/", size=HEADING, tone=UI_TONE_MUTED)
            call text(basename(file), size=HEADING, style=UI_HEADING)
        end if
        call row_end()
        if (len(dir) == 0 .and. len(file) == 0) then
            call text("", width=UI_FILL)
            if (copied) then
                if (button("clone", "Copied", height=36.0, size=BODY)) continue
            else if (button("clone", "Copy clone URL", height=36.0, size=BODY)) then
                ! The browser makes the address absolute; git asks for a token.
                call nav_copy('/'//url_encode(repo)//'.git')
                copied = .true.
            end if
        end if
        call row_end()

        if (len(file) > 0) then
            call file_view()
            return
        end if

        if (len(dir) == 0 .and. len(upstream_of(repo)) > 0) &
            call text(upstream_of(repo), size=SMALL, tone=UI_TONE_MUTED, &
            width=UI_FILL, id="upstream")

        ! The latest commit, as a bar above the files.
        if (size(commits) > 0) then
            call row_begin(GAP, id="card-lastcommit", height=48.0)
            call text(commits(1)%author, size=SMALL, style=UI_HEADING, id="author")
            if (link("lastsubject", commits(1)%subject, &
                route_href(R_COMMIT, repo, commits(1)%id), size=SMALL, width=UI_FILL, &
                ellipsis=.true., tone=UI_TONE_MUTED)) &
                call go(route_href(R_COMMIT, repo, commits(1)%id))
            if (link("lastid", short(commits(1)%id), route_href(R_COMMIT, repo, commits(1)%id), &
                size=SMALL, style=UI_CODE, tone=UI_TONE_MUTED)) &
                call go(route_href(R_COMMIT, repo, commits(1)%id))
            call text(ago(commits(1)%date), size=SMALL, tone=UI_TONE_MUTED, id="when")
            if (link("count", ui_str(size(commits))//merge(" commits", " commit ", &
                size(commits) /= 1), route_href(R_COMMITS, repo), size=SMALL, &
                style=UI_HEADING)) call go(route_href(R_COMMITS, repo))
            call row_end()
        end if

        call column_begin(0.0, id="card-files")
        if (len(dir) > 0) then
            up = parent_of(dir)
            call row_begin(GAP, id="row-up", height=ROW_H)
            call text("", id="icon-dir")
            if (link("open", "..", route_href(R_TREE, repo, up), size=BODY)) &
                call go(route_href(R_TREE, repo, up))
            call row_end()
        end if
        if (.not. tree_ready .and. len(note) == 0) then
            call row_begin(GAP, id="row-wait", height=ROW_H)
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            call row_end()
        end if
        do k = 1, size(entries)
            call row_begin(GAP, id="row-e"//ui_str(k), height=ROW_H)
            if (entries(k)%kind == 'tree') then
                call text("", id="icon-dir")
                if (link("open", entries(k)%name, &
                    route_href(R_TREE, repo, join(dir, entries(k)%name)), size=BODY)) &
                    call go(route_href(R_TREE, repo, join(dir, entries(k)%name)))
            else
                call text("", id="icon-file")
                if (link("open", entries(k)%name, &
                    route_href(R_BLOB, repo, join(dir, entries(k)%name)), size=BODY)) &
                    call go(route_href(R_BLOB, repo, join(dir, entries(k)%name)))
            end if
            call text("", width=UI_FILL)
            if (entries(k)%kind /= 'tree') call text(human_size(entries(k)%size), size=SMALL, &
                tone=UI_TONE_MUTED)
            call row_end()
        end do
        call column_end()

        if (has_readme) then
            call column_begin(0.0, id="card-readme")
            call row_begin(GAP, id="readmehead", height=ROW_H)
            call text("", id="icon-book")
            if (link("readme", readme_name, route_href(R_BLOB, repo, join(dir, readme_name)), &
                size=BODY, style=UI_HEADING, tone=UI_TONE_NORMAL)) &
                call go(route_href(R_BLOB, repo, join(dir, readme_name)))
            call row_end()
            call column_begin(16.0, id="markdown")
            call md_compose(readme)
            call column_end()
            call column_end()
        end if
    end subroutine code_view

    subroutine file_view()
        integer :: k
        logical :: preview

        preview = is_md .and. .not. as_code
        if (missing) then
            call text("This file does not exist.", size=BODY, tone=UI_TONE_MUTED, id="note")
            return
        end if
        call column_begin(0.0, id="card-file")
        call row_begin(GAP, id="filehead", height=48.0)
        if (is_md) then
            call row_begin(0.0, id="seg")
            if (link("preview", "Preview", route_href(R_BLOB, repo, file), size=BODY, &
                tone=merge(UI_TONE_STRONG, UI_TONE_NORMAL, preview))) &
                call go(route_href(R_BLOB, repo, file))
            if (link("code", "Code", route_href(R_BLOB, repo, file, plain=.true.), size=BODY, &
                tone=merge(UI_TONE_NORMAL, UI_TONE_STRONG, preview))) &
                call go(route_href(R_BLOB, repo, file, plain=.true.))
            call row_end()
        end if
        call text(file_info, size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL, id="info")
        if (link("raw", "Raw", at(api(repo, 'blob')//'?path='//url_encode(file)), size=SMALL)) &
            call go(api(repo, 'blob')//'?path='//url_encode(file))
        call row_end()
        if (preview .and. size(file_md%blocks) > 0) then
            call column_begin(16.0, id="markdown")
            call md_compose(file_md)
            call column_end()
        else
            call column_begin(0.0, id="code")
            do k = 1, size(file_lines)
                call text(file_lines(k)%s, style=UI_CODE, size=CODE, width=UI_FILL, &
                    ends=file_lines(k)%ends, tones=file_lines(k)%tones)
            end do
            call column_end()
        end if
        call column_end()
        if (file_cut .and. .not. preview) call text("Only the first "//ui_str(MAX_FILE_LINES)// &
            " lines are shown.", size=SMALL, tone=UI_TONE_MUTED)
    end subroutine file_view

    subroutine history_view()
        integer :: k
        character(len=:), allocatable :: href

        if (.not. log_ready) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        if (size(commits) == 0) then
            call text("No commits.", size=BODY, tone=UI_TONE_MUTED)
            return
        end if
        do k = 1, size(commits)
            ! Commits of one day under one heading, as forges list them.
            if (k == 1) then
                call column_begin(0.0, id="card-commits"//ui_str(k))
            else if (day_of(commits(k)%date) /= day_of(commits(k - 1)%date)) then
                call column_end()
                call column_begin(0.0, id="card-commits"//ui_str(k))
            end if
            if (k == 1) then
                call text("Commits on "//day_of(commits(k)%date), size=SMALL, id="day", &
                    width=UI_FILL)
            else if (day_of(commits(k)%date) /= day_of(commits(k - 1)%date)) then
                call text("Commits on "//day_of(commits(k)%date), size=SMALL, id="day", &
                    width=UI_FILL)
            end if
            href = route_href(R_COMMIT, repo, commits(k)%id)
            call row_begin(12.0, id="row-c"//ui_str(k), height=60.0)
            call column_begin(4.0)
            if (link("subject", commits(k)%subject, href, size=BODY, style=UI_HEADING, &
                tone=UI_TONE_NORMAL, width=UI_FILL)) call go(href)
            call text(commits(k)%author//" committed "//ago(commits(k)%date), size=SMALL, &
                tone=UI_TONE_MUTED, width=UI_FILL)
            call column_end()
            if (link("id", short(commits(k)%id), href, size=SMALL, style=UI_CODE, &
                tone=UI_TONE_MUTED)) call go(href)
            call row_end()
        end do
        call column_end()
    end subroutine history_view

    subroutine commit_view()
        call column_begin(8.0, id="card-commithead")
        call text(c_subject, size=20.0, style=UI_HEADING, width=UI_FILL, id="subject")
        if (len(c_body) > 0) call text(c_body, size=BODY, width=UI_FILL, tone=UI_TONE_MUTED, &
            id="commitbody")
        call row_begin(GAP, id="commitmeta")
        call text(c_author, size=SMALL, style=UI_HEADING)
        call text("committed "//ago(c_date), size=SMALL, tone=UI_TONE_MUTED)
        call text("", width=UI_FILL)
        call text("commit", size=SMALL, tone=UI_TONE_MUTED)
        call text(commit_id, style=UI_CODE, size=SMALL, id="sha")
        call row_end()
        call column_end()

        if (len(c_author) > 0) call diff_compose(diff_files, diff_cut)
    end subroutine commit_view

    ! ---- responses ----------------------------------------------------------------------

    subroutine repo_view_receive()
        !! Applies finished requests to the repository pages.
        logical :: found
        character(len=:), allocatable :: s
        integer :: k

        call init()
        if (fetch_ready(repos_req)) then
            if (repos_req%status == 200) then
                call parse_repos(repos_req%body)
                repos_loaded = .true.
            end if
        end if
        if (fetch_ready(add_req)) then
            if (add_req%status == 201) then
                new_repo = ''
                new_upstream = ''
                note = ''
            else
                call json_extract_string(add_req%body, 'error', s, found)
                note = 'The repository was not added'
                if (found) note = note//': '//s
            end if
            call fetch_start(repos_req, 'GET', '/api/v1/repos')
        end if
        if (fetch_ready(refs_req)) then
            if (refs_req%status == 200) then
                call json_extract_string(refs_req%body, 'default', branch, found)
            else
                refs_of = ''
                if (refs_req%status == 404) call lost('This repository does not exist.')
            end if
        end if
        if (fetch_ready(tree_req)) then
            tree_ready = .true.
            if (tree_req%status == 200) then
                call parse_entries(tree_req%body)
                do k = 1, size(entries)
                    if (entries(k)%kind == 'blob' .and. (lower(entries(k)%name) == 'readme.md' &
                        .or. lower(entries(k)%name) == 'readme')) then
                        readme_name = entries(k)%name
                        call fetch_start(readme_req, 'GET', api(repo, 'blob')//'?path='// &
                            url_encode(join(dir, readme_name)))
                        exit
                    end if
                end do
            else
                tree_of = ''
                call lost('This directory does not exist.')
            end if
        end if
        if (fetch_ready(readme_req)) then
            if (readme_req%status == 200) then
                call md_parse(readme_req%body, readme)
                has_readme = .true.
            end if
        end if
        if (fetch_ready(blob_req)) then
            if (blob_req%status == 200) then
                file_bytes = len(blob_req%body)
                call code_lines(blob_req%body, lang_of(file))
                if (is_md) call md_parse(blob_req%body, file_md)
                file_info = ui_str(size(file_lines))//merge(" lines", " line ", &
                    size(file_lines) /= 1)//" · "//human_size(file_bytes)
            else if (blob_req%status == 413) then
                call plain_lines(file_lines, 'This file is larger than 1 MiB.')
            else
                blob_of = ''
                call plain_lines(file_lines, 'This file does not exist.')
                missing = .true.
            end if
        end if
        if (fetch_ready(log_req)) then
            log_ready = .true.
            if (log_req%status == 200) then
                call parse_commits(log_req%body)
            else
                log_of = ''
            end if
        end if
        if (fetch_ready(commit_req)) then
            if (commit_req%status == 200) then
                call json_extract_string(commit_req%body, 'message', s, found)
                k = index(s, new_line('a'))
                if (k == 0) then
                    c_subject = s
                    c_body = ''
                else
                    c_subject = s(1:k - 1)
                    c_body = trim(adjustl(s(k + 1:)))
                    do while (len(c_body) > 0)
                        if (c_body(1:1) /= new_line('a')) exit
                        c_body = c_body(2:)
                    end do
                end if
                call json_extract_string(commit_req%body, 'author', c_author, found)
                call json_extract_string(commit_req%body, 'date', c_date, found)
                call json_extract_string(commit_req%body, 'patch', s, found)
                call diff_load(s, diff_files, diff_cut)
            else
                commit_of = ''
                c_subject = 'This commit does not exist.'
                missing = .true.
            end if
        end if
    end subroutine repo_view_receive

    subroutine lost(message)
        character(len=*), intent(in) :: message
        note = message
        missing = .true.
    end subroutine lost

    ! ---- turning content into lines ----------------------------------------------------

    subroutine code_lines(content, lang)
        !! Numbered, highlighted lines of a file; the number is a muted run.
        character(len=*), intent(in) :: content
        integer, intent(in) :: lang
        integer :: n, pos, nl, k, state, width
        character(len=:), allocatable :: line

        n = 0
        do k = 1, len(content)
            if (content(k:k) == new_line('a')) n = n + 1
        end do
        if (len(content) > 0) then
            if (content(len(content):) /= new_line('a')) n = n + 1
        end if
        file_cut = n > MAX_FILE_LINES
        n = min(n, MAX_FILE_LINES)
        width = max(3, len(ui_str(n)))
        if (allocated(file_lines)) deallocate (file_lines)
        allocate (file_lines(n))
        state = 0
        pos = 1
        do k = 1, n
            nl = index(content(pos:), new_line('a'))
            if (nl == 0) then
                line = untab(content(pos:))
                pos = len(content) + 1
            else
                line = untab(content(pos:pos + nl - 2))
                pos = pos + nl
            end if
            call numbered(file_lines(k), right(ui_str(k), width)//'  ', line, lang, state, 0)
        end do
    end subroutine code_lines

    ! ---- parsing responses --------------------------------------------------------------

    subroutine parse_repos(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        type(repo_t) :: r
        character(len=:), allocatable :: key

        deallocate (repos)
        allocate (repos(0))
        r = repo_t('', '', '', 'mirror', .false., 0)
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
                if (key == 'mode') r%mode = ev%string_val
            case (JSON_INTEGER)
                if (key == 'open_issues') r%open_issues = int(ev%int_val)
            case (JSON_BOOL)
                if (key == 'ready') r%ready = ev%bool_val
            case (JSON_OBJECT_END)
                if (len(r%name) > 0) repos = [repos, r]
                r = repo_t('', '', '', 'mirror', .false., 0)
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

    ! ---- small helpers ----------------------------------------------------------------

    function short(id) result(s)
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: s
        s = id(1:min(7, len(id)))
    end function short

    function leaf(path) result(s)
        !! The repository's own name, the last segment of its path.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: s
        s = path(index(path, '/', back=.true.) + 1:)
    end function leaf

    function join(a, b) result(s)
        character(len=*), intent(in) :: a, b
        character(len=:), allocatable :: s
        if (len(a) == 0) then
            s = b
        else
            s = a//'/'//b
        end if
    end function join

    function parent_of(path) result(s)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: s
        s = path(1:max(0, index(path, '/', back=.true.) - 1))
    end function parent_of

    function basename(path) result(s)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: s
        s = path(index(path, '/', back=.true.) + 1:)
    end function basename

    function human_size(bytes) result(s)
        integer, intent(in) :: bytes
        character(len=:), allocatable :: s
        if (bytes < 1024) then
            s = ui_str(bytes)//" B"
        else if (bytes < 1048576) then
            s = ui_str(real(bytes) / 1024.0, 1)//" KB"
        else
            s = ui_str(real(bytes) / 1048576.0, 1)//" MB"
        end if
    end function human_size

    pure function lower(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: i
        r = s
        do i = 1, len(r)
            if (r(i:i) >= 'A' .and. r(i:i) <= 'Z') r(i:i) = achar(iachar(r(i:i)) + 32)
        end do
    end function lower

end module fy_repo_view
