program test_release
    !! A release is opened, its archive uploaded with its checksum, listed and
    !! downloaded intact; a wrong checksum or a missing scope is refused.
    use, intrinsic :: iso_fortran_env, only: int64
    use fy_posix, only: tcp_listen, sock_close, sock_bound_port
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_token_create
    implicit none

    character(len=*), parameter :: data = 'build/test_release_data', &
        work = 'build/test_release_work'
    character(len=*), parameter :: commit = '0123456789abcdef0123456789abcdef01234567'
    type(db_t) :: db
    character(len=:), allocatable :: admin, builder, reader, base
    character(len=16) :: out
    integer :: lfd, port, stat, sup, i

    sup = 0
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data//' '//work// &
        ' && head -c 3000000 /dev/urandom > '//work//'/release.tar', exitstat=stat)
    call check(stat == 0, 'directories prepared')
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_token_create(db, 1_int64, 'builder', 'release', builder, stat)
    call store_token_create(db, 1_int64, 'reader', 'repo:read', reader, stat)
    call db_close(db)
    call server_set_data_dir(data)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    port = sock_bound_port(lfd)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    write (out, '(i0)') port
    base = 'http://127.0.0.1:'//trim(out)//'/api/v1/repos/fortyard/releases'
    do i = 1, 50
        call execute_command_line('curl -fs -o /dev/null '//base//'?format=text -H '// &
            '"Authorization: Bearer '//builder//'"', exitstat=stat)
        if (stat == 0) exit
        call proc_sleep_ms(100)
    end do

    call check(code('-X POST -H "Content-Type: application/json" -d ''{"version":'// &
        '"2026.11.09.1","commit":"'//commit//'","notes":"First"}'' '//base, reader) == '403', &
        'opening a release needs the release scope')
    call check(code('-X POST -H "Content-Type: application/json" -d ''{"version":'// &
        '"2026.11.09.1","commit":"'//commit//'","notes":"First"}'' '//base, builder) == '201', &
        'a builder opens a release')
    call check(code('-T '//work//'/release.tar -H "X-Content-SHA256: '//repeat('0', 64)//'" '// &
        base//'/2026.11.09.1/release.tar', builder) == '422', 'a wrong checksum is refused')
    call check(code('-T '//work//'/release.tar -H "X-Content-SHA256: $(sha256sum '//work// &
        '/release.tar | cut -c1-64)" '//base//'/2026.11.09.1/release.tar', builder) == '201', &
        'the archive is uploaded with its checksum')
    call execute_command_line('curl -fs -H "Authorization: Bearer '//reader//'" "'//base// &
        '?format=text" > '//work//'/list && grep -q "^2026.11.09.1 '//commit//' ready " '// &
        work//'/list && curl -fs -H "Authorization: Bearer '//reader//'" '//base// &
        '/2026.11.09.1/release.tar | cmp -s - '//work//'/release.tar', exitstat=stat)
    call check(stat == 0, 'the release is listed as ready and downloads intact')
    call check(code('-T '//work//'/release.tar -H "X-Content-SHA256: $(sha256sum '//work// &
        '/release.tar | cut -c1-64)" '//base//'/2026.11.09.1/release.tar', builder) == '409', &
        'a ready release cannot be overwritten')

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'releases: all checks passed'

contains

    function code(args, token) result(c)
        !! The HTTP status of a curl request with `token`.
        character(len=*), intent(in) :: args, token
        character(len=3) :: c
        integer :: u, st
        call execute_command_line('curl -s -o /dev/null -w "%{http_code}" -H "Authorization: '// &
            'Bearer '//token//'" '//args//' > '//work//'/code', exitstat=st)
        c = '000'
        open (newunit=u, file=work//'/code', action='read', iostat=st)
        if (st == 0) then
            read (u, '(a3)', iostat=st) c
            close (u)
        end if
    end function code

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_release
