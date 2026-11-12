module fy_backend_gitlab
    !! fy backend gitlab: a backend process that hands checks to the runners of
    !! a GitLab group we belong to. It claims a check over the runner API,
    !! pushes the check's commit with a project access token to a private
    !! project as branch check/<id>, starts a pipeline whose definition is fixed
    !! by the project (deploy/gitlab/fortyard.yml on its protected branch
    !! config), follows the one job, and reports its trace as the check's log.
    !! It runs one job at a time and gives way to other users of the runner: a
    !! job that does not start within the start wait is cancelled, the check
    !! goes back to the queue, and the backend pauses, as it does after any
    !! other trouble of GitLab or the runner. Pipeline and branch are deleted
    !! at the result; a sweep while idle removes what an earlier run left
    !! behind. fo's cache travels as an archive the backend itself uploads to
    !! the project's package registry and pins by SHA-256 for the next job,
    !! since every job's own token can write there. It talks to the server and
    !! to GitLab through fy_client, and moves files with curl.
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64, &
        output_unit
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        JSON_ARRAY_END, &
        JSON_ARRAY_START, &
        JSON_BOOL, &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_INTEGER, &
        JSON_KEY, &
        JSON_OBJECT_END, &
        JSON_OBJECT_START, &
        JSON_STRING, &
        json_event_t, &
        json_extract_int, &
        json_extract_string, &
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    use fy_build_info, only: &
        FY_COMMIT
    use fy_client, only: &
        client_request
    use fy_live, only: &
        live_gap_line
    use fy_proc, only: &
        fd_close, &
        fd_read, &
        fd_write_all, &
        proc_exit_status, &
        proc_find, &
        proc_run, &
        proc_sleep_ms, &
        proc_spawn
    implicit none
    private

    public :: &
        gitlab_backend_main, &
        gitlab_backend_t, &
        gitlab_setup

    type :: gitlab_backend_t
        !! server, token: fortyard and the runner token; gitlab: the GitLab
        !! address (https://gitlab.tugraz.at); project: GROUP/NAME;
        !! project_token: its access token; git_url: where check branches are
        !! pushed (default GITLAB/PROJECT.git); data_dir: mirrors, the git
        !! credentials file and the record of fo's cache archives.
        character(len=:), allocatable :: server, token, gitlab, project, project_token
        character(len=:), allocatable :: class, data_dir, git_url
        !! Minutes a job may run, seconds a job may wait to start, seconds of
        !! the pause after it did not, seconds between polls.
        integer :: max_minutes = 15, start_wait = 60, backoff = 600, poll = 5
    end type gitlab_backend_t

    type :: cell_t
        character(len=:), allocatable :: s
    end type cell_t

    !! The stored log keeps head and tail of a longer trace.
    integer, parameter :: LOG_KEEP = 393216
    !! The package of fo's cache archives, one version per repository.
    character(len=*), parameter :: CACHE_PACKAGE = 'fo-cache'
    !! Archives of fo's cache kept per repository.
    integer, parameter :: CACHE_KEEP = 3
    !! Seconds a running job may take beyond the check's limit (image, clone,
    !! fo's cache, artifacts) before the backend cancels it.
    integer, parameter :: JOB_OVERHEAD = 600
    !! A job in one of these states has not started.
    character(len=*), parameter :: WAITING = ' created waiting_for_resource preparing pending '// &
        'scheduled manual '

contains

    subroutine gitlab_backend_main(b)
        !! Claims and runs checks of b%class until the server runs a different
        !! release (then returns, so that the process restarts on it).
        type(gitlab_backend_t), intent(inout) :: b
        character(len=:), allocatable :: body, commit
        integer :: code, stat, s
        integer(int64) :: now, rate, last_sweep
        logical :: pause
        character(len=16) :: minutes

        if (.not. allocated(b%git_url)) b%git_url = ''
        if (len(b%git_url) == 0) b%git_url = b%gitlab//'/'//b%project//'.git'
        call prepare(b, stat)
        if (stat /= 0) return
        ! Checks it held when it stopped go back to the queue.
        call client_request(b%server, 'POST', '/api/v1/checks/release', b%token, '{}', code, &
            body, stat)
        call sweep(b)
        call system_clock(last_sweep, rate)
        write (minutes, '(i0)') b%max_minutes
        do
            call client_request(b%server, 'GET', '/api/v1/version', '', '', code, body, stat)
            if (stat == 0 .and. code == 200) then
                commit = ''
                s = index(body, '"commit":"')
                if (s > 0) then
                    commit = body(s + 10:)
                    commit = commit(1:index(commit, '"') - 1)
                end if
                if (len(commit) > 0 .and. commit /= FY_COMMIT) then
                    write (error_unit, '(a)') 'fy backend gitlab: the server runs '//commit// &
                        ', this backend '//FY_COMMIT//'; restarting'
                    return
                end if
            end if
            call client_request(b%server, 'POST', '/api/v1/checks/claim', b%token, &
                '{"class":"'//b%class//'","max_minutes":'//trim(minutes)// &
                ',"max_running":1}', code, body, stat)
            if (stat == 0 .and. code == 200) then
                call run_check(b, body, pause)
                if (pause) then
                    write (error_unit, '(a)') 'fy backend gitlab: backing off'
                    flush (error_unit)
                    call proc_sleep_ms(1000*b%backoff)
                end if
                cycle
            end if
            call system_clock(now)
            if (now - last_sweep > 600*rate) then
                call sweep(b)
                last_sweep = now
            end if
            call proc_sleep_ms(1000*b%poll)
        end do
    end subroutine gitlab_backend_main

    subroutine prepare(b, stat)
        !! The data directory with the mirrors and the record of fo's cache
        !! archives, and the credentials file.
        type(gitlab_backend_t), intent(in) :: b
        integer, intent(out) :: stat
        character(len=:), allocatable :: out

        call proc_run([character(len=1024) :: 'mkdir', '-p', b%data_dir//'/mirrors', &
            b%data_dir//'/cache'], out, stat, 4096)
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy backend gitlab: cannot create '//b%data_dir
            return
        end if
        call credentials(b, stat)
    end subroutine prepare

    subroutine credentials(b, stat)
        !! The credentials file git reads for the server and for GitLab (mode
        !! 600, so no token is on a command line). It is written before every
        !! use: git's store helper erases a line when a server rejects it once.
        type(gitlab_backend_t), intent(in) :: b
        integer, intent(out) :: stat
        character(len=:), allocatable :: out
        integer :: u

        call proc_run([character(len=1024) :: 'sh', '-c', 'umask 077 && : > "$1"', 'creds', &
            b%data_dir//'/credentials'], out, stat, 4096)
        if (stat == 0) open (newunit=u, file=b%data_dir//'/credentials', status='old', &
            action='write', iostat=stat)
        if (stat /= 0) then
            write (error_unit, '(a)') 'fy backend gitlab: cannot write '//b%data_dir//'/credentials'
            return
        end if
        write (u, '(a)') with_user(b%server, 'runner:'//b%token)
        if (index(b%git_url, '://') > 0) write (u, '(a)') with_user(b%git_url, 'oauth2:'// &
            b%project_token)
        close (u)
    end subroutine credentials

    function with_user(url, user) result(line)
        !! SCHEME://USER@HOST of url, a line of git's credential store.
        character(len=*), intent(in) :: url, user
        character(len=:), allocatable :: line, host
        integer :: s, e

        s = index(url, '://') + 3
        e = index(url(s:), '/')
        host = url(s:)
        if (e > 0) host = url(s:s + e - 2)
        line = url(1:s - 1)//user//'@'//host
    end function with_user

    subroutine run_check(b, claim, pause)
        !! One claimed check, from the push of its commit to the report; `pause`
        !! is true when GitLab or its runner was the trouble (a job that did
        !! not start in time, a failed push, a job that ended without a
        !! verdict), so that the backend does not claim again at once. A check
        !! withdrawn on the server while its job waits or runs is cancelled
        !! there and not reported.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: claim
        logical, intent(out) :: pause
        character(len=:), allocatable :: repo, ref, kind, id, out, body, pipeline, jobs, status, &
            started, reason, job_id, trace, outcome, cache_file, cache_sum, note, state
        character(len=16) :: num
        type(cell_t), allocatable :: rows(:, :)
        integer :: code, stat, timeout, limit, pid
        integer(int64) :: created, now, rate, running_since
        logical :: found

        pause = .true.
        call json_extract_string(claim, 'repo', repo, found)
        call json_extract_string(claim, 'ref', ref, found)
        call json_extract_string(claim, 'kind', kind, found)
        if (.not. found) kind = 'check'
        call json_extract_int(claim, 'id', pid, found)
        write (num, '(i0)') pid
        id = trim(num)
        call json_extract_int(claim, 'timeout', timeout, found)
        if (.not. found) timeout = 0
        limit = 60*b%max_minutes
        if (timeout > 0) limit = min(limit, timeout)
        ! Said in the log of a timeout that was the backend's bound, not the
        ! repository's own limit.
        note = ''
        if (timeout == 0 .or. timeout > limit) then
            write (num, '(i0)') b%max_minutes
            note = ' (the GitLab backend runs checks for at most '//trim(num)//' min)'
        end if
        write (error_unit, '(a)') 'fy backend gitlab: check '//id//' of '//repo//' '//ref
        flush (error_unit)

        call credentials(b, stat)
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'h="credential.helper=store --file=$(cd "$1" && pwd)/credentials"; '// &
            'm="$1/mirrors/$3.git"; '// &
            '{ if [ -d "$m" ]; then git -c credential.helper= -c "$h" -C "$m" fetch --quiet '// &
            '--prune origin; else git -c credential.helper= -c "$h" clone --quiet --mirror '// &
            '"$2/$3.git" "$m"; fi && '// &
            'c=$(git -C "$m" rev-parse --verify --quiet "$4^{commit}") && '// &
            'git -c credential.helper= -c "$h" -C "$m" push --quiet --force "$5" '// &
            '"$c:refs/heads/check/$6"; } 2>&1', 'push', b%data_dir, b%server, repo, ref, &
            b%git_url, id], out, stat, 65536)
        if (stat /= 0) then
            call report(b, id, '{"error":"runner","log":"'//json_escape_string( &
                'The GitLab backend could not push '//ref//' of '//repo//' to '//b%project// &
                ':'//new_line('a')//out)//'"}')
            call delete_branch(b, id)
            return
        end if

        write (num, '(i0)') limit
        body = '{"ref":"check/'//id//'","variables":['// &
            '{"key":"FY_REPO","value":"'//json_escape_string(repo)//'"},'// &
            '{"key":"FY_KIND","value":"'//json_escape_string(kind)//'"},'// &
            '{"key":"FY_TIMEOUT","value":"'//trim(num)//'"}'
        call cache_latest(b, repo, cache_file, cache_sum)
        if (len(cache_file) > 0) body = body//',{"key":"FY_CACHE_FILE","value":"'// &
            json_escape_string(cache_file)//'"},{"key":"FY_CACHE_SHA256","value":"'// &
            json_escape_string(cache_sum)//'"}'
        body = body//']}'
        call api(b, 'POST', '/pipeline', body, code, out, stat)
        call json_extract_int(out, 'id', pid, found)
        if (stat /= 0 .or. code /= 201 .or. .not. found) then
            call report(b, id, '{"error":"runner","log":"'//json_escape_string( &
                'GitLab did not start a pipeline for the check:'//new_line('a')//out)//'"}')
            call delete_branch(b, id)
            return
        end if
        write (num, '(i0)') pid
        pipeline = trim(num)

        call system_clock(created, rate)
        running_since = -1
        status = ''
        started = ''
        reason = ''
        job_id = ''
        do
            call proc_sleep_ms(1000*b%poll)
            call system_clock(now)
            ! A check withdrawn on the server (cancelled, or decided by its
            ! twin) is not reported: its pipeline is cancelled.
            call client_request(b%server, 'GET', '/api/v1/checks/'//id//'/publish', b%token, '', &
                code, out, stat)
            if (stat == 0) then
                call json_extract_string(out, 'state', state, found)
                if (code == 403 .or. code == 404 .or. ((code == 200 .or. code == 409) .and. &
                    found .and. state /= 'running')) then
                    call api(b, 'POST', '/pipelines/'//pipeline//'/cancel', '', code, out, stat)
                    write (error_unit, '(a)') 'fy backend gitlab: check '//id// &
                        ' was withdrawn; its pipeline is cancelled'
                    flush (error_unit)
                    call clean_up(b, id, pipeline)
                    pause = .false.
                    return
                end if
            end if
            call api(b, 'GET', '/pipelines/'//pipeline//'/jobs', '', code, jobs, stat)
            if (stat == 0 .and. code == 200) then
                rows = json_rows(jobs, [character(len=16) :: 'id', 'status', 'started_at', &
                    'failure_reason'])
                if (size(rows, 2) > 0) then
                    job_id = rows(1, 1)%s
                    status = rows(2, 1)%s
                    started = rows(3, 1)%s
                    reason = rows(4, 1)%s
                end if
            end if
            if (len(status) == 0 .or. index(WAITING, ' '//status//' ') > 0) then
                if (now - created > int(b%start_wait, int64)*rate) then
                    write (num, '(i0)') b%start_wait
                    call api(b, 'POST', '/pipelines/'//pipeline//'/cancel', '', code, out, stat)
                    call report(b, id, '{"error":"runner","log":"'//json_escape_string( &
                        'The GitLab job did not start within '//trim(num)//' s: the runner is '// &
                        'busy with other work, so the check goes back to the queue.')//'"}')
                    call clean_up(b, id, pipeline)
                    return
                end if
            else if (status == 'running') then
                if (running_since < 0) running_since = now
                ! The job ends the check at its limit itself; a job still
                ! running well after that is stuck in GitLab or the runner.
                if (now - running_since > int(limit + JOB_OVERHEAD, int64)*rate) then
                    call api(b, 'POST', '/pipelines/'//pipeline//'/cancel', '', code, out, stat)
                    write (num, '(i0)') (limit + JOB_OVERHEAD)/60
                    trace = job_trace(b, job_id)
                    call report(b, id, '{"error":"runner","log":"'//json_escape_string( &
                        trace//new_line('a')//'The GitLab job still ran after '//trim(num)// &
                        ' min, so the GitLab backend cancelled it.')//'"}')
                    call clean_up(b, id, pipeline)
                    return
                end if
            else
                exit
            end if
        end do

        trace = job_trace(b, job_id)
        ! The job writes its outcome after the check has ended, so nothing the
        ! check prints decides it.
        outcome = ''
        if (status == 'failed' .and. reason == 'script_failure') then
            call api(b, 'GET', '/jobs/'//job_id//'/artifacts/fy-out/outcome', '', code, out, stat)
            if (stat == 0 .and. code == 200) outcome = trim(adjustl(first_line(out)))
        end if
        body = ',"log":"'//json_escape_string(trace)//'"'
        if (len(started) > 0) body = body//',"started_at":"'//json_escape_string(started)//'"'
        if (status == 'success') then
            call save_cache(b, repo, job_id, pipeline)
            call report(b, id, '{"passed":true'//body//'}')
            pause = .false.
        else if (outcome == 'failed') then
            call report(b, id, '{"passed":false'//body//'}')
            pause = .false.
        else if (outcome == 'timeout') then
            call report(b, id, '{"error":"timeout","log":"'//json_escape_string(trace// &
                new_line('a')//'The check reached its time limit'//note//'.')//'"}')
            pause = .false.
        else if (outcome == 'stale') then
            ! The next run starts without the archive that may be stale.
            call cache_forget(b, repo)
            call report(b, id, '{"error":"runner","log":"'//json_escape_string(trace// &
                new_line('a')//'The check goes back to the queue and runs again without '// &
                'fo''s cache.')//'"}')
            pause = .false.
        else
            ! Trouble before the check ran, a job cancelled by someone else
            ! or one the runner lost: no verdict on the change.
            call report(b, id, '{"error":"runner","log":"'//json_escape_string(trace// &
                new_line('a')//'The GitLab job ended '//status//' ('//reason//'), which is no '// &
                'verdict on the change.')//'"}')
        end if
        call clean_up(b, id, pipeline)
    end subroutine run_check

    function first_line(text) result(line)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: line
        integer :: k
        k = index(text, new_line('a'))
        if (k == 0) then
            line = text
        else
            line = text(1:k - 1)
        end if
    end function first_line

    subroutine report(b, id, payload)
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: id, payload
        character(len=:), allocatable :: body
        integer :: code, stat

        call client_request(b%server, 'POST', '/api/v1/checks/'//id//'/result', b%token, &
            payload, code, body, stat)
        if (stat /= 0 .or. code /= 200) write (error_unit, '(a)') &
            'fy backend gitlab: the server did not take the result of check '//id//': '//body
    end subroutine report

    function job_trace(b, job) result(text)
        !! The job's log without terminal colours and section markers, its
        !! middle left out when it is long.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: job
        character(len=:), allocatable :: text, raw, head, tail
        integer :: code, stat, k

        text = ''
        if (len(job) == 0) return
        call api(b, 'GET', '/jobs/'//job//'/trace', '', code, raw, stat)
        if (stat /= 0 .or. code /= 200) return
        text = plain(raw)
        if (len(text) <= LOG_KEEP) return
        head = text(1:LOG_KEEP/2)
        k = index(head, new_line('a'), back=.true.)
        if (k > 0) head = head(1:k)
        tail = text(len(text) - LOG_KEEP/2 + 1:)
        k = index(tail, new_line('a'))
        if (k > 0 .and. k < len(tail)) tail = tail(k + 1:)
        text = head//live_gap_line(int(len(text) - len(head) - len(tail), int64))// &
            new_line('a')//tail
    end function job_trace

    function plain(raw) result(text)
        !! raw without escape sequences (ESC [ ... letter), GitLab's
        !! section markers and carriage returns.
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: text
        character(len=len(raw)) :: buf
        integer :: i, n, j

        n = 0
        i = 1
        do while (i <= len(raw))
            if (raw(i:i) == achar(27)) then
                j = i + 1
                if (j <= len(raw)) then
                    if (raw(j:j) == '[') then
                        j = j + 1
                        do while (j <= len(raw))
                            if (index('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', &
                                raw(j:j)) > 0) exit
                            j = j + 1
                        end do
                    end if
                end if
                i = j + 1
                cycle
            end if
            if (raw(i:i) == achar(13)) then
                i = i + 1
                cycle
            end if
            if (marker(raw(i:), n == 0 .or. buf(max(n, 1):max(n, 1)) == new_line('a'), j)) then
                i = i + j
                cycle
            end if
            n = n + 1
            buf(n:n) = raw(i:i)
            i = i + 1
        end do
        text = buf(1:n)
    end function plain

    logical function marker(s, at_start, length)
        !! s begins with one of GitLab's section markers
        !! (section_start:TIME:NAME or section_end:TIME:NAME, then CR), which
        !! the runner writes at the start of a line; `length` runs to the CR.
        !! Text the check prints is never one: its CR would have to come
        !! before the line ends.
        character(len=*), intent(in) :: s
        logical, intent(in) :: at_start
        integer, intent(out) :: length
        integer :: k, e, lf

        marker = .false.
        length = 0
        if (.not. at_start) return
        if (starts(s, 'section_start:')) then
            k = len('section_start:') + 1
        else if (starts(s, 'section_end:')) then
            k = len('section_end:') + 1
        else
            return
        end if
        e = verify(s(k:), '0123456789')
        if (e <= 1) return
        if (s(k + e - 1:k + e - 1) /= ':') return
        length = index(s, achar(13))
        lf = index(s, new_line('a'))
        if (length == 0 .or. (lf > 0 .and. lf < length)) return
        if (verify(s(k + e:length - 1), &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-[]=,') > 0) return
        marker = .true.
    end function marker

    logical function starts(s, prefix)
        character(len=*), intent(in) :: s, prefix
        starts = .false.
        if (len(s) >= len(prefix)) starts = s(1:len(prefix)) == prefix
    end function starts

    subroutine clean_up(b, id, pipeline)
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: id, pipeline
        character(len=:), allocatable :: body
        integer :: code, stat

        call api(b, 'DELETE', '/pipelines/'//pipeline, '', code, body, stat)
        call delete_branch(b, id)
    end subroutine clean_up

    subroutine delete_branch(b, id)
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: out
        integer :: stat

        call credentials(b, stat)
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'git -c credential.helper= -c "credential.helper=store --file=$(cd "$1" && pwd)/'// &
            'credentials" push --quiet "$2" --delete "refs/heads/check/$3" 2>&1', 'delete', &
            b%data_dir, b%git_url, id], out, stat, 65536)
    end subroutine delete_branch

    subroutine sweep(b)
        !! Removes every branch check/N and every pipeline of one: called only
        !! while the backend holds no check, so all of them are left over.
        type(gitlab_backend_t), intent(in) :: b
        character(len=:), allocatable :: body, out
        type(cell_t), allocatable :: rows(:, :)
        integer :: code, stat, k

        call api(b, 'GET', '/pipelines?per_page=100', '', code, body, stat)
        if (stat == 0 .and. code == 200) then
            rows = json_rows(body, [character(len=16) :: 'id', 'ref', 'status'])
            do k = 1, size(rows, 2)
                if (.not. starts(rows(2, k)%s, 'check/')) cycle
                if (index(WAITING//'running ', ' '//rows(3, k)%s//' ') > 0) &
                    call api(b, 'POST', '/pipelines/'//rows(1, k)%s//'/cancel', '', code, out, &
                    stat)
                call api(b, 'DELETE', '/pipelines/'//rows(1, k)%s, '', code, out, stat)
            end do
        end if
        call credentials(b, stat)
        call proc_run([character(len=1024) :: 'sh', '-c', &
            'h="credential.helper=store --file=$(cd "$1" && pwd)/credentials"; '// &
            'r=$(git -c credential.helper= -c "$h" ls-remote --heads "$2" '// &
            '"refs/heads/check/*" | cut -f2) && { [ -z "$r" ] || '// &
            'git -c credential.helper= -c "$h" push --quiet "$2" --delete $r; } 2>&1', 'sweep', &
            b%data_dir, b%git_url], out, stat, 65536)
    end subroutine sweep

    subroutine cache_latest(b, repo, file, sum)
        !! The newest archive of fo's cache the backend uploaded for repo: its
        !! file name in the package registry and its SHA-256, '' for none. The
        !! record (DATA/cache/REPO) has a line "ID NAME SHA256" per archive,
        !! oldest first.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: repo
        character(len=:), allocatable, intent(out) :: file, sum
        type(cell_t), allocatable :: ids(:), names(:), sums(:)

        file = ''
        sum = ''
        call cache_record(b, repo, ids, names, sums)
        if (size(ids) == 0) return
        file = names(size(ids))%s
        sum = sums(size(ids))%s
    end subroutine cache_latest

    subroutine cache_record(b, repo, ids, names, sums)
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: repo
        type(cell_t), allocatable, intent(out) :: ids(:), names(:), sums(:)
        character(len=512) :: line
        character(len=128) :: f1, f3
        character(len=256) :: f2
        integer :: u, ios

        allocate (ids(0), names(0), sums(0))
        open (newunit=u, file=b%data_dir//'/cache/'//repo, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            read (line, *, iostat=ios) f1, f2, f3
            if (ios /= 0) cycle
            ids = [ids, cell_t(trim(f1))]
            names = [names, cell_t(trim(f2))]
            sums = [sums, cell_t(trim(f3))]
        end do
        close (u)
    end subroutine cache_record

    subroutine cache_forget(b, repo)
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: repo
        integer :: u, ios

        open (newunit=u, file=b%data_dir//'/cache/'//repo, status='old', iostat=ios)
        if (ios == 0) close (u, status='delete')
    end subroutine cache_forget

    subroutine save_cache(b, repo, job, pipeline)
        !! After a pass: the job's archive of fo's cache (its artifact
        !! fy-out/fo-cache.tar.gz) goes to the package registry, uploaded with
        !! the project token as fo-cache/REPO/fo-cache-PIPELINE.tar.gz, and
        !! into the record with its SHA-256; then the package keeps only the
        !! newest CACHE_KEEP recorded archives, whoever added the others.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: repo, job, pipeline
        character(len=:), allocatable :: tmp, out, name, sum, fid, junk
        type(cell_t), allocatable :: ids(:), names(:), sums(:)
        integer :: code, stat, k, u, first
        logical :: found

        tmp = b%data_dir//'/cache/upload.tar.gz'
        call transfer(b, 'GET', '/jobs/'//job//'/artifacts/fy-out/fo-cache.tar.gz', tmp, code, &
            out, stat)
        if (stat /= 0 .or. code /= 200) then
            call proc_run([character(len=1024) :: 'rm', '-f', tmp], out, stat, 4096)
            return
        end if
        call proc_run([character(len=1024) :: 'sha256sum', tmp], out, stat, 4096)
        if (stat /= 0 .or. len(out) < 64) return
        sum = out(1:64)
        name = 'fo-cache-'//pipeline//'.tar.gz'
        call transfer(b, 'PUT', '/packages/generic/'//CACHE_PACKAGE//'/'//repo//'/'//name// &
            '?select=package_file', tmp, code, out, stat)
        call proc_run([character(len=1024) :: 'rm', '-f', tmp], junk, k, 4096)
        if (stat /= 0 .or. code /= 201) then
            write (error_unit, '(a)') 'fy backend gitlab: fo''s cache of '//repo// &
                ' was not uploaded: '//out
            return
        end if
        call json_extract_int(out, 'id', k, found)
        if (.not. found) return
        fid = itoa(k)
        call cache_record(b, repo, ids, names, sums)
        ids = [ids, cell_t(fid)]
        names = [names, cell_t(name)]
        sums = [sums, cell_t(sum)]
        first = max(1, size(ids) - CACHE_KEEP + 1)
        open (newunit=u, file=b%data_dir//'/cache/'//repo, status='replace', action='write', &
            iostat=stat)
        if (stat /= 0) return
        do k = first, size(ids)
            write (u, '(a)') ids(k)%s//' '//names(k)%s//' '//sums(k)%s
        end do
        close (u)
        call prune_cache(b, repo, ids(first:))
    end subroutine save_cache

    subroutine prune_cache(b, repo, keep)
        !! Deletes every file of the package fo-cache, version repo, but the
        !! archives `keep` (package file ids) the backend recorded.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: repo
        type(cell_t), intent(in) :: keep(:)
        character(len=:), allocatable :: body, package, out
        type(cell_t), allocatable :: rows(:, :)
        integer :: code, stat, k, j
        logical :: kept

        call api(b, 'GET', '/packages?package_type=generic&package_name='//CACHE_PACKAGE// &
            '&per_page=100', '', code, body, stat)
        if (stat /= 0 .or. code /= 200) return
        rows = json_rows(body, [character(len=16) :: 'id', 'version'])
        package = ''
        do k = 1, size(rows, 2)
            if (rows(2, k)%s == repo) package = rows(1, k)%s
        end do
        if (len(package) == 0) return
        call api(b, 'GET', '/packages/'//package//'/package_files?per_page=100', '', code, body, &
            stat)
        if (stat /= 0 .or. code /= 200) return
        rows = json_rows(body, [character(len=16) :: 'id'])
        do k = 1, size(rows, 2)
            kept = .false.
            do j = 1, size(keep)
                if (keep(j)%s == rows(1, k)%s) kept = .true.
            end do
            if (kept .or. len(rows(1, k)%s) == 0) cycle
            call api(b, 'DELETE', '/packages/'//package//'/package_files/'//rows(1, k)%s, '', &
                code, out, stat)
        end do
    end subroutine prune_cache

    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, '(i0)') n
        s = trim(buf)
    end function itoa

    subroutine transfer(b, method, path, file, code, body, stat)
        !! A file to (PUT) or from (GET) the project's part of GitLab's API
        !! through curl, which reads the token from its standard input, never
        !! from its command line. `body` is the answer to an upload.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: method, path, file
        integer, intent(out) :: code, stat
        character(len=:), allocatable, intent(out) :: body
        character(len=:), allocatable :: curl, conf
        character(len=65536) :: buf
        character(len=16) :: argv(3)
        character(len=40) :: env(1)
        integer :: pid, to_child, from_child, n, cut, ios

        code = 0
        body = ''
        stat = 1
        curl = proc_find('curl')
        if (len(curl) == 0) return
        conf = 'url = "'//quoted(b%gitlab//'/api/v4/projects/'//encoded(b%project)//path)//'"'// &
            new_line('a')//'request = "'//method//'"'//new_line('a')// &
            'max-time = 900'//new_line('a')//'silent'//new_line('a')// &
            'proto = "=http,https"'//new_line('a')// &
            'write-out = "\n%{http_code}"'//new_line('a')// &
            'header = "Authorization: Bearer '//quoted(b%project_token)//'"'//new_line('a')
        if (method == 'GET') then
            conf = conf//'output = "'//quoted(file)//'"'//new_line('a')
        else
            conf = conf//'upload-file = "'//quoted(file)//'"'//new_line('a')
        end if
        argv(1) = 'curl'
        argv(2) = '-K'
        argv(3) = '-'
        env(1) = 'PATH=/usr/local/bin:/usr/bin:/bin'
        call proc_spawn(curl, argv, env, pid, to_child, from_child)
        if (pid < 0) return
        call fd_write_all(to_child, conf, stat)
        call fd_close(to_child)
        do
            n = fd_read(from_child, buf)
            if (n <= 0) exit
            if (len(body) + n > 1048576) exit
            body = body//buf(1:n)
        end do
        call fd_close(from_child)
        stat = proc_exit_status(pid)
        if (stat /= 0) return
        cut = index(body, new_line('a'), back=.true.)
        read (body(cut + 1:), *, iostat=ios) code
        if (ios /= 0) then
            stat = 1
            return
        end if
        body = body(1:max(0, cut - 1))
    end subroutine transfer

    function quoted(text) result(out)
        !! text inside a double-quoted curl configuration value.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i

        out = ''
        do i = 1, len(text)
            if (text(i:i) == '\' .or. text(i:i) == '"') out = out//'\'
            out = out//text(i:i)
        end do
    end function quoted

    subroutine api(b, method, path, payload, code, body, stat)
        !! A request to the project's part of GitLab's API.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: method, path, payload
        integer, intent(out) :: code, stat
        character(len=:), allocatable, intent(out) :: body

        call client_request(b%gitlab, method, '/api/v4/projects/'//encoded(b%project)//path, &
            b%project_token, payload, code, body, stat)
    end subroutine api

    function encoded(path) result(out)
        !! GROUP/NAME as one path segment.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: out
        integer :: i

        out = ''
        do i = 1, len(path)
            if (path(i:i) == '/') then
                out = out//'%2F'
            else
                out = out//path(i:i)
            end if
        end do
    end function encoded

    function json_rows(body, fields) result(cells)
        !! The values of `fields` (as text; '' for null or missing) in each
        !! object of the top-level array `body`.
        character(len=*), intent(in) :: body
        character(len=*), intent(in) :: fields(:)
        type(cell_t), allocatable :: cells(:, :)
        type(cell_t), allocatable :: grown(:, :)
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key
        character(len=32) :: b
        integer :: depth, rows, k, j

        allocate (cells(size(fields), 0))
        rows = 0
        depth = 0
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                depth = depth + 1
                if (depth == 2 .and. ev%event_type == JSON_OBJECT_START) then
                    allocate (grown(size(fields), rows + 1))
                    grown(:, 1:rows) = cells
                    do k = 1, size(fields)
                        grown(k, rows + 1)%s = ''
                    end do
                    call move_alloc(grown, cells)
                    rows = rows + 1
                end if
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
            case (JSON_KEY)
                key = ev%string_val
            case default
                if (depth /= 2 .or. rows == 0) cycle
                k = findloc([(trim(fields(j)) == key, j=1, size(fields))], .true., 1)
                if (k == 0) cycle
                select case (ev%event_type)
                case (JSON_STRING)
                    cells(k, rows)%s = ev%string_val
                case (JSON_INTEGER)
                    write (b, '(i0)') ev%int64_val
                    cells(k, rows)%s = trim(b)
                case (JSON_BOOL)
                    cells(k, rows)%s = trim(merge('true ', 'false', ev%bool_val))
                end select
            end select
        end do
    end function json_rows

    subroutine gitlab_setup(b, config, stat)
        !! Makes the project run only the fixed pipeline: its file `config`
        !! (deploy/gitlab/fortyard.yml) as fortyard.yml on the protected
        !! branch config, which the project's CI/CD configuration path names,
        !! and pipeline variables allowed for the token's role. Reports each
        !! step on standard output; stat /= 0 when one failed.
        type(gitlab_backend_t), intent(in) :: b
        character(len=*), intent(in) :: config
        integer, intent(out) :: stat
        character(len=:), allocatable :: content, body, current, action, start
        integer :: code, u, ios
        integer(int64) :: size_
        logical :: has_branch

        inquire (file=config, size=size_)
        stat = 1
        if (size_ <= 0) then
            write (error_unit, '(a)') 'fy backend gitlab setup: cannot read '//config
            return
        end if
        allocate (character(len=size_) :: content)
        open (newunit=u, file=config, access='stream', form='unformatted', action='read', &
            iostat=ios)
        if (ios == 0) read (u, iostat=ios) content
        if (ios /= 0) then
            write (error_unit, '(a)') 'fy backend gitlab setup: cannot read '//config
            return
        end if
        close (u)

        call api(b, 'GET', '/repository/branches/config', '', code, body, stat)
        if (stat /= 0 .or. (code /= 200 .and. code /= 404)) then
            call fail('read branch config')
            return
        end if
        has_branch = code == 200
        action = 'create'
        start = ''
        if (has_branch) then
            call api(b, 'GET', '/repository/files/fortyard.yml/raw?ref=config', '', code, current, &
                stat)
            if (stat /= 0) then
                call fail('read fortyard.yml')
                return
            end if
            if (code == 200) action = 'update'
            if (code == 200 .and. current == content) action = ''
        else
            ! In an empty project the branch config becomes the default one,
            ! so that no check branch ever is.
            call api(b, 'GET', '', '', code, body, stat)
            if (stat /= 0 .or. code /= 200) then
                call fail('read the project')
                return
            end if
            call json_extract_string(body, 'default_branch', start, has_branch)
        end if
        if (len(action) > 0) then
            body = '{"branch":"config",'
            if (len(start) > 0) body = body//'"start_branch":"'//json_escape_string(start)//'",'
            body = body//'"commit_message":"fortyard''s pipeline definition",'// &
                '"actions":[{"action":"'//action//'","file_path":"fortyard.yml",'// &
                '"content":"'//json_escape_string(content)//'"}]}'
            call api(b, 'POST', '/repository/commits', body, code, current, stat)
            if (stat /= 0 .or. code /= 201) then
                call fail('commit fortyard.yml to config: '//current)
                return
            end if
            write (output_unit, '(a)') 'fortyard.yml on branch config: '//action//'d'
        else
            write (output_unit, '(a)') 'fortyard.yml on branch config: unchanged'
        end if
        call api(b, 'POST', '/protected_branches', '{"name":"config","push_access_level":40,'// &
            '"merge_access_level":40}', code, body, stat)
        ! 409: protected already (an empty project's first branch is).
        if (stat /= 0 .or. (code /= 201 .and. code /= 409)) then
            call fail('protect config: '//body)
            return
        end if
        write (output_unit, '(a)') 'branch config: protected'
        call api(b, 'PUT', '', '{"ci_config_path":"fortyard.yml@'//json_escape_string(b%project)// &
            ':config","ci_pipeline_variables_minimum_override_role":"maintainer",'// &
            '"container_expiration_policy_attributes":{"enabled":true,"cadence":"1d",'// &
            '"keep_n":1,"older_than":"7d","name_regex":".*"}}', code, body, stat)
        if (stat /= 0 .or. code /= 200) then
            call fail('set the CI/CD configuration file: '//body)
            return
        end if
        write (output_unit, '(a)') 'CI/CD configuration file: fortyard.yml@'//b%project//':config'
        stat = 0

    contains

        subroutine fail(what)
            character(len=*), intent(in) :: what
            write (error_unit, '(a)') 'fy backend gitlab setup: cannot '//what
            stat = 1
        end subroutine fail

    end subroutine gitlab_setup

end module fy_backend_gitlab
