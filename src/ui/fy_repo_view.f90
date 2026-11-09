module fy_repo_view
    !! Repository pages: the list of mirrored repositories, a repository's
    !! files with its README, a file with highlighted lines, the history, and a
    !! commit with its diff per file.
    use fortui
    use fx_json_build, only: json_escape_string
    use fx_json_parse, only: json_extract_string, json_parser_t, json_event_t, &
        json_parser_init, json_parser_next, JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_BOOL, &
        JSON_OBJECT_END, JSON_END_OF_INPUT, JSON_ERROR
    use fy_fetch, only: request_t, fetch_start, fetch_ready
    use fy_highlight, only: highlight, lang_of
    use fy_diff, only: diff_file_t, parse_patch, DL_ADDED, DL_REMOVED, DL_HUNK, DL_NOTE
    use fy_markdown, only: md_doc_t, md_parse, md_compose
    use fy_time, only: ago
    implicit none
    private

    public :: repo_view_open, repo_view_compose, repo_view_receive, repo_view_load, &
        repo_view_reset

    type :: line_t
        character(len=:), allocatable :: s
        integer, allocatable :: ends(:), tones(:)
        integer :: tone = 0
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

    type :: shown_file_t
        !! A file of a commit's diff, ready to compose.
        character(len=:), allocatable :: path, status
        integer :: additions = 0, deletions = 0
        type(line_t), allocatable :: lines(:)
    end type shown_file_t

    integer, parameter :: MAX_FILE_LINES = 5000, MAX_DIFF_LINES = 6000
    real, parameter :: BODY = 15.0, SMALL = 13.0, HEADING = 17.0, CTRL = 15.0, CODE = 13.5
    real, parameter :: GAP = 12.0, ROW_H = 30.0

    type(request_t) :: repos_req, refs_req, tree_req, blob_req, readme_req, log_req, &
        commit_req, add_req
    type(repo_t), allocatable :: repos(:)
    type(entry_t), allocatable :: entries(:)
    type(commit_t), allocatable :: commits(:)
    type(line_t), allocatable :: file_lines(:)
    type(shown_file_t), allocatable :: diff_files(:)
    type(md_doc_t) :: readme
    character(len=:), allocatable :: repo, dir, file, commit_id, branch, note
    character(len=:), allocatable :: new_repo, new_upstream, readme_name, file_info
    character(len=:), allocatable :: c_subject, c_body, c_author, c_date
    logical :: file_cut = .false., diff_cut = .false., loaded = .false., has_readme = .false.
    integer :: sub_tab = 1, file_bytes = 0
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
        if (allocated(diff_files)) deallocate (diff_files)
        if (allocated(readme%blocks)) deallocate (readme%blocks)
        sub_tab = 1
        file_cut = .false.
        diff_cut = .false.
        has_readme = .false.
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
        allocate (repos(0), entries(0), commits(0), file_lines(0), diff_files(0))
        repo = ''
        dir = ''
        file = ''
        commit_id = ''
        branch = ''
        note = ''
        new_repo = ''
        new_upstream = ''
        readme_name = ''
        file_info = ''
        c_subject = ''
        c_body = ''
        c_author = ''
        c_date = ''
    end subroutine init

    subroutine repo_view_compose(page_w, narrow, admin)
        real, intent(in) :: page_w
        logical, intent(in) :: narrow, admin

        call init()
        if (len(repo) == 0) then
            call list_view(admin)
        else
            call repo_page(page_w, narrow)
        end if
        if (len(note) > 0) call text(note, size=SMALL, style=UI_CAPTION, tone=UI_TONE_NEGATIVE)
    end subroutine repo_view_compose

    ! ---- repository list --------------------------------------------------------

    subroutine list_view(admin)
        logical, intent(in) :: admin
        integer :: k
        logical :: submitted

        call column_begin(10.0)
        call text("Repositories", size=HEADING, style=UI_HEADING)
        if (size(repos) == 0) then
            call text("No repositories are mirrored yet.", size=BODY, tone=UI_TONE_MUTED)
        else
            call column_begin(0.0, id="card-repos")
            do k = 1, size(repos)
                call row_begin(GAP, id="row-r"//ui_str(k), height=48.0)
                call text("", id="icon-repo")
                if (button("open", repos(k)%name, size=BODY, enabled=repos(k)%ready)) &
                    call open_repo(repos(k)%name)
                call text(repos(k)%upstream, size=SMALL, tone=UI_TONE_MUTED, width=UI_FILL, &
                    ellipsis=.true.)
                call text(state_of(repos(k)), size=SMALL, tone=UI_TONE_MUTED)
                call row_end()
            end do
            call column_end()
        end if
        call column_end()

        if (.not. admin) return
        call column_begin(8.0)
        call text("Mirror a repository", size=HEADING, style=UI_HEADING)
        call row_begin(GAP, height=ROW_H + 4.0)
        if (text_input("newrepo", new_repo, placeholder="Name, e.g. fx", width=180.0, &
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
        call text("The server clones it within a few seconds and then fetches it every minute.", &
            size=SMALL, tone=UI_TONE_MUTED)
        call column_end()
    end subroutine list_view

    function state_of(r) result(s)
        type(repo_t), intent(in) :: r
        character(len=:), allocatable :: s
        if (r%ready) then
            s = "updated "//ago(r%fetched)
        else
            s = "cloning..."
        end if
    end function state_of

    ! ---- one repository ---------------------------------------------------------------

    subroutine repo_page(page_w, narrow)
        real, intent(in) :: page_w
        logical, intent(in) :: narrow
        logical :: back

        call column_begin(16.0)
        call row_begin(6.0, id="repohead", height=ROW_H)
        call text("", id="icon-repo")
        back = button("back", "Repositories", size=HEADING)
        call text("/", size=HEADING, tone=UI_TONE_MUTED)
        call text(repo, size=HEADING, style=UI_HEADING)
        call row_end()
        if (back) then
            call column_end()
            call repo_view_open()
            return
        end if
        if (tabs("sub", sub_labels, sub_tab, width=min(240.0, page_w), size=CTRL)) then
            file = ''
            commit_id = ''
            if (sub_tab == 2 .and. size(commits) <= 1) call load_log()
        end if
        if (sub_tab == 1) then
            call code_view(narrow)
        else if (len(commit_id) > 0) then
            call commit_view(narrow)
        else
            call history_view(narrow)
        end if
        call column_end()
    end subroutine repo_page

    subroutine code_view(narrow)
        logical, intent(in) :: narrow
        integer :: k, start
        character(len=:), allocatable :: here

        ! Branch and path.
        call row_begin(6.0, id="pathbar", height=ROW_H)
        if (len(branch) > 0) call text(branch, size=SMALL, id="branch")
        if (button("root", repo, size=BODY)) call open_dir('')
        here = ''
        start = 1
        do k = 1, len(dir) + 1
            if (k <= len(dir)) then
                if (dir(k:k) /= '/') cycle
            end if
            if (k > start) then
                if (len(here) > 0) here = here//'/'
                here = here//dir(start:k - 1)
                call text("/", size=BODY, tone=UI_TONE_MUTED)
                if (button("seg", dir(start:k - 1), key=k, size=BODY)) call open_dir(here)
            end if
            start = k + 1
        end do
        if (len(file) > 0) then
            call text("/", size=BODY, tone=UI_TONE_MUTED)
            call text(basename(file), size=BODY, style=UI_HEADING)
        end if
        call row_end()

        if (len(file) > 0) then
            call file_view()
            return
        end if

        ! The latest commit, as a bar above the files.
        if (size(commits) > 0) then
            call row_begin(10.0, id="card-lastcommit", height=44.0)
            call text(commits(1)%author, size=SMALL, style=UI_HEADING)
            if (button("lastsubject", commits(1)%subject, size=SMALL, width=UI_FILL, &
                ellipsis=.true.)) call open_commit(commits(1)%id)
            if (.not. narrow) call text(commits(1)%id(1:min(7, len(commits(1)%id))), &
                style=UI_CODE, size=12.0, tone=UI_TONE_MUTED)
            call text(ago(commits(1)%date), size=SMALL, tone=UI_TONE_MUTED)
            call row_end()
        end if

        call column_begin(0.0, id="card-files")
        if (len(dir) > 0) then
            call row_begin(GAP, id="row-up", height=36.0)
            call text("", id="icon-dir")
            if (button("open", "..", size=BODY)) call open_dir(parent_of(dir))
            call row_end()
        end if
        do k = 1, size(entries)
            call row_begin(GAP, id="row-e"//ui_str(k), height=36.0)
            if (entries(k)%kind == 'tree') then
                call text("", id="icon-dir")
                if (button("open", entries(k)%name, size=BODY)) &
                    call open_dir(join(dir, entries(k)%name))
            else
                call text("", id="icon-file")
                if (button("open", entries(k)%name, size=BODY)) &
                    call open_file(join(dir, entries(k)%name))
            end if
            call text("", width=UI_FILL)
            if (entries(k)%kind /= 'tree') call text(human_size(entries(k)%size), size=SMALL, &
                tone=UI_TONE_MUTED)
            call row_end()
        end do
        call column_end()

        if (has_readme) then
            call column_begin(0.0, id="card-readme")
            call row_begin(8.0, id="readmehead", height=40.0)
            call text("", id="icon-book")
            call text(readme_name, size=SMALL, style=UI_HEADING)
            call row_end()
            call column_begin(12.0, id="readme")
            call md_compose(readme)
            call column_end()
            call column_end()
        end if
    end subroutine code_view

    subroutine file_view()
        integer :: k

        call column_begin(0.0, id="card-file")
        call row_begin(GAP, id="filehead", height=40.0)
        call text(file, style=UI_CODE, size=SMALL, width=UI_FILL, ellipsis=.true.)
        call text(file_info, size=SMALL, tone=UI_TONE_MUTED)
        call row_end()
        call column_begin(0.0, id="code")
        do k = 1, size(file_lines)
            call text(file_lines(k)%s, style=UI_CODE, size=CODE, width=UI_FILL, &
                ends=file_lines(k)%ends, tones=file_lines(k)%tones)
        end do
        call column_end()
        call column_end()
        if (file_cut) call text("Only the first "//ui_str(MAX_FILE_LINES)//" lines are shown.", &
            size=SMALL, tone=UI_TONE_MUTED)
    end subroutine file_view

    subroutine history_view(narrow)
        logical, intent(in) :: narrow
        integer :: k

        call column_begin(0.0, id="card-commits")
        do k = 1, size(commits)
            call row_begin(GAP, id="row-c"//ui_str(k), height=52.0)
            if (button("subject", commits(k)%subject, size=BODY, width=UI_FILL, &
                ellipsis=.true.)) call open_commit(commits(k)%id)
            if (.not. narrow) call text(commits(k)%author, size=SMALL, tone=UI_TONE_MUTED)
            call text(ago(commits(k)%date), size=SMALL, tone=UI_TONE_MUTED)
            if (button("id", commits(k)%id(1:min(7, len(commits(k)%id))), key=k, size=12.0)) &
                call open_commit(commits(k)%id)
            call row_end()
        end do
        call column_end()
    end subroutine history_view

    subroutine commit_view(narrow)
        logical, intent(in) :: narrow
        integer :: f, k, adds, dels

        adds = 0
        dels = 0
        do f = 1, size(diff_files)
            adds = adds + diff_files(f)%additions
            dels = dels + diff_files(f)%deletions
        end do

        call column_begin(10.0, id="card-commithead")
        if (button("history", "Back to history", size=SMALL)) then
            commit_id = ''
            call column_end()
            return
        end if
        call text(c_subject, size=20.0, style=UI_HEADING, width=UI_FILL)
        if (len(c_body) > 0) call text(c_body, size=BODY, width=UI_FILL, tone=UI_TONE_MUTED, &
            id="commitbody")
        call row_begin(8.0, id="commitmeta")
        call text(c_author, size=SMALL, style=UI_HEADING)
        call text("committed "//ago(c_date), size=SMALL, tone=UI_TONE_MUTED)
        call text("", width=UI_FILL)
        if (.not. narrow) call text(commit_id, style=UI_CODE, size=12.0, tone=UI_TONE_MUTED)
        call row_end()
        call column_end()

        call row_begin(10.0, id="diffstat", height=ROW_H)
        if (size(diff_files) == 1) then
            call text("1 file changed", size=SMALL)
        else
            call text(ui_str(size(diff_files))//" files changed", size=SMALL)
        end if
        call text("+"//ui_str(adds), size=SMALL, tone=UI_TONE_POSITIVE)
        call text("−"//ui_str(dels), size=SMALL, tone=UI_TONE_NEGATIVE)
        call row_end()

        do f = 1, size(diff_files)
            call column_begin(0.0, id="card-diff"//ui_str(f))
            call row_begin(10.0, id="dhead", height=40.0)
            if (diff_files(f)%status /= 'modified') call text(diff_files(f)%status, size=12.0, &
                id="badge", tone=badge_tone(diff_files(f)%status))
            call text(diff_files(f)%path, style=UI_CODE, size=SMALL, width=UI_FILL, &
                ellipsis=.true.)
            call text("+"//ui_str(diff_files(f)%additions), size=SMALL, tone=UI_TONE_POSITIVE)
            call text("−"//ui_str(diff_files(f)%deletions), size=SMALL, tone=UI_TONE_NEGATIVE)
            call row_end()
            call column_begin(0.0, id="dcode")
            do k = 1, size(diff_files(f)%lines)
                associate (l => diff_files(f)%lines(k))
                    call text(l%s, style=UI_CODE, size=CODE, width=UI_FILL, tone=l%tone, &
                        ends=l%ends, tones=l%tones)
                end associate
            end do
            call column_end()
            call column_end()
        end do
        if (diff_cut) call text("The diff is longer; only its first "//ui_str(MAX_DIFF_LINES)// &
            " lines are shown.", size=SMALL, tone=UI_TONE_MUTED)
    end subroutine commit_view

    integer function badge_tone(status)
        character(len=*), intent(in) :: status
        select case (status)
        case ('added'); badge_tone = UI_TONE_POSITIVE
        case ('deleted'); badge_tone = UI_TONE_NEGATIVE
        case default; badge_tone = UI_TONE_MUTED
        end select
    end function badge_tone

    ! ---- navigation and requests ------------------------------------------------------

    subroutine open_repo(name)
        character(len=*), intent(in) :: name
        repo = name
        sub_tab = 1
        deallocate (commits)
        allocate (commits(0))
        commit_id = ''
        branch = ''
        call fetch_start(refs_req, 'GET', '/api/v1/repos/'//repo//'/refs')
        call load_log()
        call open_dir('')
    end subroutine open_repo

    subroutine load_log()
        call fetch_start(log_req, 'GET', '/api/v1/repos/'//repo//'/log')
    end subroutine load_log

    subroutine open_dir(path)
        character(len=*), intent(in) :: path
        dir = path
        file = ''
        note = ''
        has_readme = .false.
        call fetch_start(tree_req, 'GET', '/api/v1/repos/'//repo//'/tree?path='//encode(path))
    end subroutine open_dir

    subroutine open_file(path)
        character(len=*), intent(in) :: path
        file = path
        note = ''
        file_info = ''
        call plain_lines(file_lines, 'Loading...')
        call fetch_start(blob_req, 'GET', '/api/v1/repos/'//repo//'/blob?path='//encode(path))
    end subroutine open_file

    subroutine open_commit(id)
        character(len=*), intent(in) :: id
        sub_tab = 2
        commit_id = id
        c_subject = 'Loading...'
        c_body = ''
        c_author = ''
        c_date = ''
        deallocate (diff_files)
        allocate (diff_files(0))
        call fetch_start(commit_req, 'GET', '/api/v1/repos/'//repo//'/commits/'//id)
    end subroutine open_commit

    subroutine repo_view_receive()
        !! Applies finished requests to the repository pages.
        logical :: found
        character(len=:), allocatable :: s
        integer :: k

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
                note = 'The repository was not added'
                if (found) note = note//': '//s
            end if
            call fetch_start(repos_req, 'GET', '/api/v1/repos')
        end if
        if (fetch_ready(refs_req)) then
            if (refs_req%status == 200) call json_extract_string(refs_req%body, 'default', &
                branch, found)
        end if
        if (fetch_ready(tree_req)) then
            if (tree_req%status == 200) then
                call parse_entries(tree_req%body)
                do k = 1, size(entries)
                    if (entries(k)%kind == 'blob' .and. (lower(entries(k)%name) == 'readme.md' &
                        .or. lower(entries(k)%name) == 'readme')) then
                        readme_name = entries(k)%name
                        call fetch_start(readme_req, 'GET', '/api/v1/repos/'//repo// &
                            '/blob?path='//encode(join(dir, readme_name)))
                        exit
                    end if
                end do
            else
                note = 'That directory could not be read.'
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
                file_info = ui_str(size(file_lines))//" lines · "//human_size(file_bytes)
            else if (blob_req%status == 413) then
                call plain_lines(file_lines, 'This file is larger than 1 MiB.')
            else
                call plain_lines(file_lines, 'This file could not be read.')
            end if
        end if
        if (fetch_ready(log_req)) then
            if (log_req%status == 200) call parse_commits(log_req%body)
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
                call diff_lines(s)
            else
                c_subject = 'This commit could not be read.'
            end if
        end if
    end subroutine repo_view_receive

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

    subroutine diff_lines(patch)
        !! Each file of a commit's patch with its lines: both line numbers as a
        !! muted run, the sign and the highlighted code.
        character(len=*), intent(in) :: patch
        type(diff_file_t), allocatable :: files(:)
        integer :: f, k, state, lang, kept

        call parse_patch(patch, files, MAX_DIFF_LINES)
        deallocate (diff_files)
        allocate (diff_files(size(files)))
        kept = 0
        do f = 1, size(files)
            diff_files(f)%path = files(f)%path
            if (files(f)%status == 'renamed') diff_files(f)%path = files(f)%old_path// &
                ' → '//files(f)%path
            diff_files(f)%status = files(f)%status
            diff_files(f)%additions = files(f)%additions
            diff_files(f)%deletions = files(f)%deletions
            allocate (diff_files(f)%lines(files(f)%count))
            lang = lang_of(files(f)%path)
            state = 0
            do k = 1, files(f)%count
                associate (l => files(f)%lines(k), d => diff_files(f)%lines(k))
                    select case (l%kind)
                    case (DL_HUNK)
                        call plain(d, l%text, UI_TONE_HUNK)
                        d%tone = UI_TONE_HUNK
                    case (DL_NOTE)
                        call plain(d, l%text, UI_TONE_MUTED)
                    case (DL_ADDED)
                        call numbered(d, gutter(0, l%new_no)//'+ ', untab(l%text), lang, state, &
                            UI_TONE_ADDED)
                    case (DL_REMOVED)
                        call numbered(d, gutter(l%old_no, 0)//'- ', untab(l%text), lang, state, &
                            UI_TONE_REMOVED)
                    case default
                        call numbered(d, gutter(l%old_no, l%new_no)//'  ', untab(l%text), lang, &
                            state, 0)
                    end select
                end associate
            end do
            kept = kept + files(f)%count
        end do
        diff_cut = kept >= MAX_DIFF_LINES
    end subroutine diff_lines

    subroutine numbered(l, prefix, code, lang, state, tone)
        !! A line whose prefix (line numbers) is a muted run, then highlighted code.
        type(line_t), intent(inout) :: l
        character(len=*), intent(in) :: prefix, code
        integer, intent(in) :: lang, tone
        integer, intent(inout) :: state
        integer, allocatable :: ends(:), tones(:)

        call highlight(code, lang, state, ends, tones)
        l%s = prefix//code
        l%ends = [len(prefix), ends + len(prefix)]
        l%tones = [UI_TONE_MUTED, tones]
        l%tone = tone
    end subroutine numbered

    subroutine plain(l, s, tone)
        type(line_t), intent(inout) :: l
        character(len=*), intent(in) :: s
        integer, intent(in) :: tone
        l%s = s
        l%ends = [len(s)]
        l%tones = [tone]
    end subroutine plain

    subroutine plain_lines(lines, s)
        type(line_t), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: s
        if (allocated(lines)) deallocate (lines)
        allocate (lines(1))
        call plain(lines(1), s, UI_TONE_MUTED)
    end subroutine plain_lines

    function gutter(old_no, new_no) result(s)
        integer, intent(in) :: old_no, new_no
        character(len=:), allocatable :: s
        s = right(number(old_no), 5)//' '//right(number(new_no), 5)//'  '
    contains
        function number(k) result(t)
            integer, intent(in) :: k
            character(len=:), allocatable :: t
            t = ''
            if (k > 0) t = ui_str(k)
        end function number
    end function gutter

    function right(s, width) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: width
        character(len=:), allocatable :: r
        r = repeat(' ', max(0, width - len_trim(s)))//trim(s)
    end function right

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

    ! ---- responses ----------------------------------------------------------------------

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

    ! ---- small helpers ----------------------------------------------------------------


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
