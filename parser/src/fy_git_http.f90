module fy_git_http
    !! Git's smart HTTP protocol (clone, fetch, push) through `git http-backend`
    !! as a CGI program. Request and response bodies are passed through in
    !! blocks, so a large clone or push needs no more memory than a small one.
    use fy_http, only: http_request_t, http_status_text, http_chunk_size
    use fy_posix, only: sock_recv, sock_send_all
    use fy_proc, only: proc_spawn, proc_find, proc_exit_status, fd_read, fd_write_all, fd_close
    implicit none
    private

    public :: git_http_route, git_http_serve, git_http_copy_chunked

    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    !! Largest request body passed on (a push), in bytes.
    integer, parameter :: MAX_PUSH = 1073741824

contains

    logical function git_http_route(path, query, repo, write)
        !! True for a smart-HTTP path "/REPO[.git]/info/refs",
        !! "/REPO[.git]/git-upload-pack" or "/REPO[.git]/git-receive-pack";
        !! `repo` is the repository path without ".git", `write` whether the
        !! request is part of a push.
        character(len=*), intent(in) :: path, query
        character(len=:), allocatable, intent(out) :: repo
        logical, intent(out) :: write
        character(len=*), parameter :: SUFFIX(3) = [character(len=17) :: '/info/refs', &
            '/git-upload-pack', '/git-receive-pack']
        integer :: k, n

        git_http_route = .false.
        write = .false.
        repo = ''
        do k = 1, size(SUFFIX)
            n = len_trim(SUFFIX(k))
            if (len(path) <= n + 1) cycle
            if (path(len(path) - n + 1:) /= SUFFIX(k)(1:n)) cycle
            repo = path(2:len(path) - n)
            if (len(repo) > 4) then
                if (repo(len(repo) - 3:) == '.git') repo = repo(1:len(repo) - 4)
            end if
            select case (k)
            case (1)
                write = index(query, 'service=git-receive-pack') > 0
            case (3)
                write = .true.
            end select
            git_http_route = len(repo) > 0
            return
        end do
    end function git_http_route

    subroutine git_http_serve(fd, req, rest, root, repo, user, status)
        !! Runs `git http-backend` for `req` on repository root/REPO.git and
        !! relays its answer. `rest` holds body bytes already read with the head.
        !! `status` is 0, or an HTTP error code when nothing was sent yet.
        integer, intent(in) :: fd
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: rest, root, repo, user
        integer, intent(out) :: status
        character(len=:), allocatable :: backend, git
        ! Fixed-length locals: gfortran overflows a typed array constructor
        ! around a deferred-length string.
        character(len=1024) :: env(14), argv(1)
        character(len=16) :: num
        integer :: pid, to_child, from_child, stat, code, ne

        status = 500
        git = proc_find('git')
        if (len(git) == 0) return
        backend = git_exec_dir(git)//'/git-http-backend'
        ne = 0
        call add('GIT_PROJECT_ROOT='//root)
        call add('GIT_HTTP_EXPORT_ALL=1')
        ! The path below the repository is the last segment, or info/refs.
        if (index(req%path, '/info/refs') > 0) then
            call add('PATH_INFO=/'//repo//'.git/info/refs')
        else
            call add('PATH_INFO=/'//repo//'.git'//req%path(index(req%path, '/', back=.true.):))
        end if
        call add('REQUEST_METHOD='//req%method)
        call add('QUERY_STRING='//req%query)
        call add('CONTENT_TYPE='//req%content_type)
        call add('REMOTE_USER='//user)
        call add('REMOTE_ADDR=127.0.0.1')
        call add('PATH=/usr/local/bin:/usr/bin:/bin')
        call add('GIT_CONFIG_NOSYSTEM=1')
        call add('HOME=/nonexistent')
        if (len(req%git_protocol) > 0) call add('GIT_PROTOCOL='//req%git_protocol)
        if (len(req%content_encoding) > 0) &
            call add('HTTP_CONTENT_ENCODING='//req%content_encoding)
        ! Only a POST body is passed on; the backend waits forever for a
        ! length it was told and never gets.
        if (req%has_length .and. req%method == 'POST') then
            write (num, '(i0)') req%content_length
            call add('CONTENT_LENGTH='//trim(num))
        end if
        if (req%content_length > MAX_PUSH) then
            status = 413
            return
        end if

        argv(1) = backend
        call proc_spawn(backend, argv, env(1:ne), pid, to_child, from_child)
        if (pid < 0) return
        if (req%method == 'POST') then
            if (req%chunked) then
                call git_http_copy_chunked(fd, rest, to_child, stat)
            else
                call copy_length(fd, rest, req%content_length, to_child, stat)
            end if
        end if
        call fd_close(to_child)
        call relay(from_child, fd, stat)
        call fd_close(from_child)
        code = proc_exit_status(pid)
        status = 0

    contains

        subroutine add(entry)
            character(len=*), intent(in) :: entry
            ne = ne + 1
            env(ne) = entry
        end subroutine add

    end subroutine git_http_serve

    subroutine copy_length(fd, rest, length, out, stat)
        !! Passes `length` body bytes from the socket to `out`.
        integer, intent(in) :: fd, length, out
        character(len=*), intent(in) :: rest
        integer, intent(out) :: stat
        character(len=65536) :: buf
        integer :: left, n

        left = length
        n = min(len(rest), left)
        call fd_write_all(out, rest(1:n), stat)
        left = left - n
        do while (left > 0 .and. stat == 0)
            n = sock_recv(fd, buf)
            if (n <= 0) then
                stat = 1
                return
            end if
            n = min(n, left)
            call fd_write_all(out, buf(1:n), stat)
            left = left - n
        end do
    end subroutine copy_length

    subroutine git_http_copy_chunked(fd, rest, out, stat)
        !! Decodes a chunked body from the socket and passes its data to `out`,
        !! up to MAX_PUSH bytes.
        integer, intent(in) :: fd, out
        character(len=*), intent(in) :: rest
        integer, intent(out) :: stat
        character(len=:), allocatable :: pend
        character(len=65536) :: buf
        integer :: eol, csize, n, total, semi

        pend = rest
        total = 0
        stat = 1
        do
            ! Chunk size line.
            do
                eol = index(pend, CRLF)
                if (eol > 0) exit
                if (len(pend) > 1024) return
                if (.not. more()) return
            end do
            semi = index(pend(1:eol - 1), ';')
            if (semi == 0) semi = eol
            csize = http_chunk_size(pend(1:semi - 1))
            if (csize < 0) return
            pend = pend(eol + 2:)
            if (csize == 0) then
                stat = 0
                return
            end if
            ! Compared before adding: the sum of two sizes can pass huge(total).
            if (csize > MAX_PUSH - total) return
            total = total + csize
            ! Chunk data, then its CRLF.
            do while (csize > 0)
                if (len(pend) == 0) then
                    if (.not. more()) return
                end if
                n = min(len(pend), csize)
                call fd_write_all(out, pend(1:n), stat)
                if (stat /= 0) return
                stat = 1
                pend = pend(n + 1:)
                csize = csize - n
            end do
            do while (len(pend) < 2)
                if (.not. more()) return
            end do
            if (pend(1:2) /= CRLF) return
            pend = pend(3:)
        end do

    contains

        logical function more()
            n = sock_recv(fd, buf)
            more = n > 0
            if (more) pend = pend//buf(1:n)
        end function more

    end subroutine git_http_copy_chunked

    subroutine relay(from, fd, stat)
        !! Turns the CGI program's header block into an HTTP status line and
        !! headers, then passes its body through until it ends.
        integer, intent(in) :: from, fd
        integer, intent(out) :: stat
        character(len=65536) :: buf
        character(len=:), allocatable :: head, block, line, headers
        integer :: n, cut, sep, pos, eol, code, ios
        character(len=12) :: num

        head = ''
        cut = 0
        sep = 0
        do
            n = fd_read(from, buf)
            if (n <= 0) exit
            head = head//buf(1:n)
            cut = index(head, CRLF//CRLF)
            sep = 4
            if (cut == 0) then
                cut = index(head, new_line('a')//new_line('a'))
                sep = 2
            end if
            if (cut > 0 .or. len(head) > 65536) exit
        end do
        code = 502
        headers = ''
        if (cut > 0) then
            code = 200
            block = head(1:cut - 1)//new_line('a')
            pos = 1
            do while (pos <= len(block))
                eol = index(block(pos:), new_line('a'))
                line = block(pos:pos + eol - 2)
                pos = pos + eol
                if (len(line) > 0) then
                    if (line(len(line):) == achar(13)) line = line(1:len(line) - 1)
                end if
                if (len(line) == 0) cycle
                if (len(line) > 7) then
                    if (line(1:7) == 'Status:') then
                        read (line(8:), *, iostat=ios) code
                        if (ios /= 0) code = 502
                        cycle
                    end if
                end if
                headers = headers//line//CRLF
            end do
        end if
        write (num, '(i0)') code
        call sock_send_all(fd, 'HTTP/1.1 '//trim(num)//' '//http_status_text(code)//CRLF// &
            headers//'Connection: close'//CRLF//CRLF, stat)
        if (stat /= 0 .or. cut == 0) return
        if (cut + sep <= len(head)) call sock_send_all(fd, head(cut + sep:), stat)
        do while (stat == 0)
            n = fd_read(from, buf)
            if (n <= 0) exit
            call sock_send_all(fd, buf(1:n), stat)
        end do
    end subroutine relay

    function git_exec_dir(git) result(dir)
        !! Where git keeps its helper programs, next to git itself for the usual
        !! layouts (/usr/bin/git and /usr/lib/git-core, or libexec).
        character(len=*), intent(in) :: git
        character(len=:), allocatable :: dir
        character(len=*), parameter :: CANDIDATES(4) = [character(len=32) :: &
            '/usr/lib/git-core', '/usr/libexec/git-core', '/usr/local/libexec/git-core', &
            '/opt/homebrew/libexec/git-core']
        integer :: k
        logical :: exists

        do k = 1, size(CANDIDATES)
            inquire (file=trim(CANDIDATES(k))//'/git-http-backend', exist=exists)
            if (exists) then
                dir = trim(CANDIDATES(k))
                return
            end if
        end do
        dir = git(1:index(git, '/', back=.true.) - 1)
    end function git_exec_dir

end module fy_git_http
