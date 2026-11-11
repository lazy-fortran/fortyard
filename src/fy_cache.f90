module fy_cache
    !! The shared build cache (DESIGN.md, "Shared build cache"): build tools
    !! such as fo store regenerable build products under their action keys.
    !!   GET|HEAD /cache/v1/NAMESPACE/KEY   an object, with X-Content-SHA256
    !!   PUT      /cache/v1/NAMESPACE/KEY   with X-Content-SHA256 and
    !!                                      Content-Length (at most 64 MiB)
    !! NAMESPACE is `shared`, which runners (runner scope) and administrators
    !! write and every account reads, or `u<account id>`, which only its account
    !! reads and writes. KEY is 64 hexadecimal digits. Objects are kept on local
    !! disk under DATA/cache and removed least recently used first once they
    !! exceed FY_CACHE_MAX (bytes, K/M/G suffixes; default 10G).
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, &
        int64
    use fx_hash, only: &
        sha256_file
    use fy_http, only: &
        http_bearer_token, &
        http_request_t, &
        http_send_response
    use fy_posix, only: &
        sock_recv
    use fy_sqlite, only: &
        db_t
    use fy_store, only: &
        store_token_lookup, &
        token_identity_t
    implicit none
    private

    public :: &
        cache_route, &
        cache_serve

    integer(int64), parameter :: MAX_OBJECT = 67108864_int64
    character(len=*), parameter :: PREFIX = '/cache/v1/'
    character(len=*), parameter :: HEX_DIGITS = '0123456789abcdef'
    !! Uploads between two size checks of the whole cache.
    integer, parameter :: PRUNE_EVERY = 64
    integer, save :: uploads = 0

contains

    logical function cache_route(req, namespace, key)
        !! A cache request, with its namespace and key.
        type(http_request_t), intent(in) :: req
        character(len=:), allocatable, intent(out) :: namespace, key
        character(len=:), allocatable :: rest
        integer :: slash

        cache_route = .false.
        namespace = ''
        key = ''
        if (index(req%path, PREFIX) /= 1) return
        if (req%method /= 'GET' .and. req%method /= 'HEAD' .and. req%method /= 'PUT') return
        rest = req%path(len(PREFIX) + 1:)
        slash = index(rest, '/')
        if (slash < 2) return
        namespace = rest(1:slash - 1)
        key = rest(slash + 1:)
        cache_route = namespace_ok(namespace) .and. key_ok(key)
    end function cache_route

    subroutine cache_serve(cfd, req, rest, db, data_dir, namespace, key)
        !! Answers one cache request; `rest` holds body bytes already read.
        integer, intent(in) :: cfd
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: rest, data_dir, namespace, key
        type(db_t), intent(in) :: db
        type(token_identity_t) :: who
        character(len=:), allocatable :: path
        character(len=16) :: own

        call store_token_lookup(db, http_bearer_token(req), who)
        if (.not. who%found) then
            call reply(401, 'missing or invalid token')
            return
        end if
        write (own, '(a,i0)') 'u', who%account_id
        path = data_dir//'/cache/'//namespace//'/'//key(1:2)//'/'//key
        if (req%method == 'PUT') then
            if (namespace == 'shared') then
                if (.not. has_scope(who, 'runner') .and. .not. has_scope(who, 'admin')) then
                    call reply(403, 'the shared namespace takes uploads from runners only')
                    return
                end if
            else if (namespace /= trim(own)) then
                call reply(403, 'another account''s namespace')
                return
            end if
            call receive(path)
        else
            if (namespace /= 'shared' .and. namespace /= trim(own)) then
                call reply(403, 'another account''s namespace')
                return
            end if
            call send(path)
        end if

    contains

        subroutine receive(target)
            character(len=*), intent(in) :: target
            character(len=:), allocatable :: part
            character(len=65536) :: buf
            character(len=64) :: hex
            integer(int64) :: left
            integer :: u, n, ierr, stat

            if (req%chunked .or. .not. req%has_length) then
                call reply(411, 'send Content-Length')
                return
            end if
            if (int(req%content_length, int64) > MAX_OBJECT) then
                call reply(413, 'cache objects are limited to 64 MiB')
                return
            end if
            if (.not. allocated(req%content_sha256)) then
                call reply(400, 'send X-Content-SHA256')
                return
            end if
            if (len(req%content_sha256) /= 64) then
                call reply(400, 'send X-Content-SHA256')
                return
            end if
            call execute_command_line('mkdir -p '''//target(1:index(target, '/', back=.true.) - 1) &
                //'''', exitstat=stat)
            part = target//'.part'
            open (newunit=u, file=part, status='replace', access='stream', form='unformatted', &
                action='write', iostat=ierr)
            if (ierr /= 0) then
                call reply(500, 'object not stored')
                return
            end if
            left = int(req%content_length, int64)
            n = int(min(int(len(rest), int64), left))
            if (n > 0) write (u) rest(1:n)
            left = left - n
            do while (left > 0)
                n = sock_recv(cfd, buf)
                if (n <= 0) exit
                n = int(min(int(n, int64), left))
                write (u) buf(1:n)
                left = left - n
            end do
            close (u)
            call sha256_file(part, hex, ierr, max_bytes=int(MAX_OBJECT))
            if (left /= 0 .or. ierr /= 0 .or. hex /= req%content_sha256) then
                call execute_command_line('rm -f '''//part//'''')
                call reply(422, 'the object is incomplete or its checksum differs')
                return
            end if
            ! Rename is atomic: a reader sees the old object or the new one.
            call execute_command_line('mv -f '''//part//''' '''//target//'''', exitstat=stat)
            uploads = uploads + 1
            if (mod(uploads, PRUNE_EVERY) == 1) call prune(data_dir//'/cache')
            call http_send_response(cfd, 201, 'application/json', '{"sha256":"'//hex//'"}')
            call log_line(201)
        end subroutine receive

        subroutine send(target)
            character(len=*), intent(in) :: target
            character(len=:), allocatable :: object
            character(len=64) :: hex
            integer :: u, ierr, size, stat
            logical :: exists

            inquire (file=target, exist=exists, size=size)
            if (.not. exists .or. size < 0) then
                call reply(404, 'not cached')
                return
            end if
            call sha256_file(target, hex, ierr, max_bytes=int(MAX_OBJECT))
            if (ierr /= 0) then
                call reply(404, 'not cached')
                return
            end if
            ! The access time orders eviction; relatime mounts may not set it.
            call execute_command_line('touch -a '''//target//'''', exitstat=stat)
            if (req%method == 'HEAD') then
                call http_send_response(cfd, 200, 'application/octet-stream', '', &
                    'X-Content-SHA256: '//hex//achar(13)//achar(10))
                call log_line(200)
                return
            end if
            allocate (character(len=size) :: object)
            open (newunit=u, file=target, access='stream', form='unformatted', action='read', &
                iostat=ierr)
            if (ierr == 0 .and. size > 0) read (u, iostat=ierr) object
            if (ierr == 0) close (u)
            if (ierr /= 0) then
                call reply(404, 'not cached')
                return
            end if
            call http_send_response(cfd, 200, 'application/octet-stream', object, &
                'X-Content-SHA256: '//hex//achar(13)//achar(10), 'private, max-age=31536000')
            call log_line(200)
        end subroutine send

        subroutine reply(code, message)
            integer, intent(in) :: code
            character(len=*), intent(in) :: message

            call http_send_response(cfd, code, 'application/json', '{"error":"'//message//'"}')
            call log_line(code)
        end subroutine reply

        subroutine log_line(code)
            integer, intent(in) :: code

            write (error_unit, '(a,1x,a,1x,i0)') req%method, req%path, code
            flush (error_unit)
        end subroutine log_line

    end subroutine cache_serve

    subroutine prune(root)
        !! Removes the least recently used objects until the cache is within
        !! FY_CACHE_MAX again (to 90 % of it, so pruning does not run on every
        !! upload).
        character(len=*), intent(in) :: root
        character(len=64) :: setting
        integer :: n, stat

        call get_environment_variable('FY_CACHE_MAX', setting, n, stat)
        if (stat /= 0 .or. n == 0) setting = '10G'
        call execute_command_line('max=$(numfmt --from=iec '''//trim(setting)// &
            ''' 2>/dev/null) || exit 0; '// &
            'find '''//root//''' -type f ! -name ''*.part'' -printf ''%A@ %s %p\n'' | '// &
            'sort -n | awk -v max="$max" ''{size[NR]=$2; path[NR]=$3; total+=$2} '// &
            'END {for (i = 1; i <= NR && total > 0.9*max; i++) {print path[i]; total-=size[i]}}'' '// &
            '| xargs -r rm -f', exitstat=stat)
    end subroutine prune

    pure logical function namespace_ok(name)
        character(len=*), intent(in) :: name

        namespace_ok = name == 'shared'
        if (namespace_ok .or. len(name) < 2 .or. len(name) > 20) return
        namespace_ok = name(1:1) == 'u' .and. verify(name(2:), '0123456789') == 0
    end function namespace_ok

    pure logical function key_ok(key)
        character(len=*), intent(in) :: key

        key_ok = len(key) == 64 .and. verify(key, HEX_DIGITS) == 0
    end function key_ok

    logical function has_scope(who, scope)
        type(token_identity_t), intent(in) :: who
        character(len=*), intent(in) :: scope

        has_scope = index(' '//who%scopes//' ', ' '//scope//' ') > 0
    end function has_scope

end module fy_cache
