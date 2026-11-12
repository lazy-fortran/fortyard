program test_egress_config
    !! deploy/egress/render.sh turns a role's list into the egress proxy's
    !! squid configuration: listed domains with their subdomains are allowed
    !! first, then TU Graz, private, loopback and link-local networks and
    !! literal IPs are denied on the resolved address, and everything else is
    !! denied (or, learning, allowed) and logged; only ports 80 and 443, and
    !! CONNECT to 443. Lists with anything but domains, unknown roles and
    !! learning for another role than the executor are refused.
    implicit none

    character(len=*), parameter :: work = 'build/test_egress_config_work'
    character(len=*), parameter :: render = 'sh deploy/egress/render.sh '
    character(len=:), allocatable :: conf
    integer :: stat

    call execute_command_line('rm -rf '//work//' && mkdir -p '//work//'/lists && '// &
        'printf "# code\nGitHub.com\napi.github.com\n.ghcr.io\n\nfiles.example.org # x\n" > '// &
        work//'/lists/executor.txt && printf "pypi.org\n" > '//work//'/lists/packages.txt && '// &
        'printf "api.github.com\n" > '//work//'/lists/server.txt && '// &
        'printf "code.tugraz.at\n" > '//work//'/lists/controller.txt', exitstat=stat)
    call check(stat == 0, 'lists written')

    conf = rendered('server', '')
    call check(has(conf, 'acl listed dstdomain -n .api.github.com'//new_line('a')), &
        'the server list is a dstdomain entry with its subdomains')
    call check(.not. has(conf, 'pypi.org'), 'package hosts are the executor''s only')
    call check(.not. has(conf, 'allow all'), 'without learning nothing unlisted passes')
    call check(has(conf, 'acl tu_graz dst 129.27.0.0/16 80.120.198.216/29 2001:628:2090::/48'), &
        'TU Graz networks of IPv4 and IPv6')
    call check(has(conf, ' 10.0.0.0/8 ') .and. has(conf, ' 172.16.0.0/12 ') .and. &
        has(conf, ' 192.168.0.0/16 ') .and. has(conf, ' 127.0.0.0/8 ') .and. &
        has(conf, ' 169.254.0.0/16 ') .and. has(conf, ' ::1 ') .and. has(conf, ' fc00::/7 ') .and. &
        has(conf, ' fe80::/10'), 'private, loopback and link-local networks')
    call check(has(conf, 'acl ports_ok port 80 443') .and. has(conf, 'acl tls_port port 443'), &
        'ports 80 and 443, CONNECT to 443')
    call check(in_order(conf, [character(len=40) :: 'http_access deny !ports_ok', &
        'http_access deny CONNECT !tls_port', 'http_access deny literal_ip', &
        'http_access allow listed', 'http_access deny tu_graz', &
        'http_access deny local_nets', 'http_access deny all']), &
        'ports and literal IPs first, then the list, then the networks, then everything else')
    call check(has(conf, 'acl literal_ip dstdom_regex -n '), 'a literal IP is judged as written')

    conf = rendered('executor', 'FY_EGRESS_LOG=/log FY_EGRESS_EXTRA=/extra/executor.txt')
    call check(has(conf, 'acl listed dstdomain -n .files.example.org .ghcr.io .github.com'// &
        new_line('a')), 'one entry per domain, lower case, sorted, subdomains of listed ones dropped')
    call check(has(conf, 'acl packages dstdomain -n .pypi.org'), 'the executor gets the package hosts')
    call check(in_order(conf, [character(len=40) :: 'http_access deny literal_ip', &
        'http_access allow listed', 'http_access allow packages !private_repo']), &
        'no list lets a literal IP pass by its reverse name')
    call check(in_order(conf, [character(len=40) :: 'http_access allow listed', &
        'http_access allow packages !private_repo', 'http_access deny tu_graz', &
        'http_access deny local_nets', 'http_access allow extra', 'http_access deny all']), &
        'a repository''s extra hosts pass only after the network rules')
    call check(has(conf, ' domain /extra/executor.txt') .and. &
        has(conf, ' private /extra/executor.txt'), 'the controller''s file is read by the helper')
    call check(has(conf, 'access_log stdio:/log/unlisted.log logformat=fy rotate=7 '// &
        '!listed !packages'), &
        'requests to unlisted hosts are logged apart')
    call check(.not. has(conf, 'allow all'), 'learning is off by default')

    conf = rendered('executor', 'FY_EGRESS_LEARN=1')
    call check(in_order(conf, [character(len=40) :: 'http_access deny local_nets', &
        'http_access allow all', 'http_access deny all']), &
        'learning lets public hosts pass after the network rules')

    conf = rendered('controller,executor', '')
    call check(has(conf, '.code.tugraz.at') .and. has(conf, '.github.com') .and. &
        has(conf, 'acl packages'), 'a container with two roles gets both lists')

    call check(fails('server', 'FY_EGRESS_LEARN=1'), 'learning is refused for the server')
    call check(fails('nobody', ''), 'an unknown role is refused')
    call execute_command_line('printf "github.com\n10.1.2.3\n" > '//work//'/lists/server.txt', &
        exitstat=stat)
    call check(fails('server', ''), 'a literal IP on a list is refused')
    call execute_command_line('printf "*.github.com\n" > '//work//'/lists/server.txt', exitstat=stat)
    call check(fails('server', ''), 'a wildcard on a list is refused')
    call execute_command_line('rm -rf '//work)
    write (*, '(a)') 'test_egress_config: ok'

contains

    function rendered(role, env) result(text)
        character(len=*), intent(in) :: role, env
        character(len=:), allocatable :: text
        character(len=4096) :: line
        integer :: u, ios, stat

        call execute_command_line('env '//env//' FY_EGRESS_HELPER=/h '//render//role//' '// &
            work//'/lists > '//work//'/squid.conf', exitstat=stat)
        call check(stat == 0, 'render.sh '//role//' succeeds')
        text = ''
        open (newunit=u, file=work//'/squid.conf', action='read', iostat=ios)
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text//trim(line)//new_line('a')
        end do
        close (u)
    end function rendered

    logical function fails(role, env)
        character(len=*), intent(in) :: role, env
        integer :: stat

        call execute_command_line('env '//env//' '//render//role//' '//work// &
            '/lists > /dev/null 2>&1', exitstat=stat)
        fails = stat /= 0
    end function fails

    logical function has(text, part)
        character(len=*), intent(in) :: text, part
        has = index(text, part) > 0
    end function has

    logical function in_order(text, lines)
        !! Each line occurs as a whole line, after the one before it.
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: lines(:)
        integer :: i, at, from

        in_order = .false.
        from = 1
        do i = 1, size(lines)
            at = index(text(from:), new_line('a')//trim(lines(i))//new_line('a'))
            if (at == 0) return
            from = from + at
        end do
        in_order = .true.
    end function in_order

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_egress_config
