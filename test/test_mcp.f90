program test_mcp
    !! fy mcp against a running server, over its standard input and output as
    !! an agent drives it: every tool is listed with an input schema, and the
    !! tools read and change repositories, changes, issues, landings and jobs
    !! like the client commands do.
    use fy_posix, only: tcp_listen, sock_bound_port, sock_close
    use fy_proc, only: proc_fork, proc_exit, proc_kill, proc_wait, proc_sleep_ms, SIGTERM
    use fy_server, only: server_supervise, server_set_data_dir
    use fy_sqlite, only: db_t, db_close
    use fy_store, only: store_open, store_bootstrap, store_repo_add
    implicit none

    character(len=*), parameter :: data = 'build/test_mcp_data', work = 'build/test_mcp_work'
    character(len=*), parameter :: git = 'env GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 '// &
        'git -c user.name=T -c user.email=t@example.org '
    character(len=*), parameter :: demo = '"repo":"demo"', one = demo//',"number":1', &
        two = demo//',"number":2'
    type(db_t) :: db
    character(len=:), allocatable :: admin, server, fy, env, out, requests, a
    character(len=4096) :: cwd
    character(len=16) :: num
    integer :: lfd, stat, sup, i
    logical :: exists

    sup = 0
    call getcwd(cwd)
    call execute_command_line('rm -rf '//data//' '//work//' && mkdir -p '//data, exitstat=stat)
    call store_open(data//'/fortyard.db', db, stat)
    call store_bootstrap(db, admin, stat)
    call store_repo_add(db, 'demo', '', stat, mode='primary')
    call db_close(db)
    call server_set_data_dir(data)
    call tcp_listen('127.0.0.1', 0, lfd, stat)
    write (num, '(i0)') sock_bound_port(lfd)
    server = 'http://127.0.0.1:'//trim(num)
    sup = proc_fork()
    if (sup == 0) then
        call server_supervise(lfd, data//'/fortyard.db', 1, 2)
        call proc_exit(0)
    end if
    call sock_close(lfd)
    do i = 1, 150
        inquire (file=data//'/repos/demo.git/HEAD', exist=exists)
        if (exists) exit
        call proc_sleep_ms(100)
    end do
    call execute_command_line('fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | '// &
        'head -1); echo $PWD/$fy > '//data//'/fy', exitstat=stat)
    fy = trim(read_line(data//'/fy'))
    ! The client's own configuration and git's stay inside the test.
    env = 'env XDG_CONFIG_HOME='//trim(cwd)//'/'//data//'/config GIT_CONFIG_GLOBAL=/dev/null '// &
        'GIT_CONFIG_NOSYSTEM=1 '

    call run('printf "%s\n" '//admin//' | '//env//fy//' login --server '//server)
    call check(stat == 0, 'fy login: '//out)
    call execute_command_line('git init -q -b main '//work//' && mkdir '//work//'/.fortyard'// &
        ' && printf "hello\n" > '//work//'/README && printf "#!/bin/sh\necho check ran\n'// &
        'echo check done\n" > '//work//'/.fortyard/check && chmod +x '//work// &
        '/.fortyard/check && '//git//'-C '//work//' add -A && '//git//'-C '//work// &
        ' commit -q -m First && '//git//'-C '//work//' remote add yard '//server// &
        '/demo.git && '//git//'-C '//work//' checkout -q -b first', exitstat=stat)
    call run('cd '//work//' && '//env//fy//' push')
    call check(stat == 0 .and. index(out, 'change #1') > 0, 'fy push opens change 1: '//out)

    requests = ''
    call request(1, 'initialize', '{"protocolVersion":"2025-03-26"}')
    call request(2, 'tools/list', '{}')
    call tool(3, 'repos', '')
    call tool(4, 'changes', demo)
    call tool(5, 'change_diff', one)
    call tool(6, 'change_comment', one//',"body":"looks \"fine\""')
    call tool(7, 'change_close', one)
    call tool(8, 'changes', demo)
    call tool(9, 'changes', demo//',"state":"all"')
    call tool(10, 'change_reopen', one)
    call tool(11, 'change', one)
    ! Changes and issues of a repository share their numbers.
    call tool(12, 'issue_new', demo//',"title":"First issue","body":"It breaks."')
    call tool(13, 'issue_comment', two//',"body":"Seen too."')
    call tool(14, 'issue_close', two)
    call tool(15, 'issues', demo)
    call tool(16, 'issue_reopen', two)
    call tool(17, 'issues', demo)
    call tool(18, 'issue', two)
    call tool(19, 'land', demo//',"branch":"first"')
    call tool(20, 'change', demo)
    call tool(21, 'changes', '"repo":"../demo"')
    call tool(22, 'nonsense', demo)
    call tool(23, 'fortyard', '"action":"whoami"')
    ! Clients differ in the order of the members; one SDK sends the id last.
    requests = requests//'{"method":"tools/call","params":{"name":"whoami","arguments":{}},'// &
        '"jsonrpc":"2.0","id":24}'//new_line('a')
    requests = requests//'{"method":"initialize","params":{"protocolVersion":"2025-06-18",'// &
        '"capabilities":{},"clientInfo":{"name":"x","version":"1"}},"jsonrpc":"2.0","id":25}'// &
        new_line('a')
    call request(26, 'initialize', '{"protocolVersion":"a\"b"}')
    call request(-3, 'ping', '{}')
    requests = requests//'{"jsonrpc":"2.0","id":"a\"b","method":"ping"}'//new_line('a')
    requests = requests//'{"jsonrpc":"2.0","id":27,"result":{}}'//new_line('a')
    requests = requests//'{"jsonrpc":"2.0","id":28,"method":'//new_line('a')
    call tool(29, 'issues', demo//',"state":"flagged"')
    call tool(30, 'changes', demo//',"state":"flagged"')
    call tool(31, 'change_diff', one//',"version":1')
    ! Longer than the 32 kB at which fx's own loop gives up.
    call tool(33, 'issue_new', demo//',"title":"Long","body":"'//repeat('long ', 8000)//'"')
    call tool(34, 'issue', demo//',"number":3')
    call tool(37, 'changes', '"state":"all"')
    call tool(38, 'changes', demo//',"state":"all","review":"flagged"')
    call tool(39, 'changes', demo//',"review":"a b"')
    call request(35, 'shutdown', '{}')
    ! Nothing after a shutdown is read.
    call request(36, 'ping', '{}')
    call mcp()

    call check(index(answer(1), '"serverInfo":{"name":"fortyard"') > 0, &
        'initialize names the server: '//answer(1))
    a = answer(2)
    call check(index(a, '{"name":"repos","description":') > 0 .and. &
        index(a, '{"name":"landings","description":') > 0 .and. index(a, '{"name":"job",') > 0, &
        'tools/list names each tool: '//a)
    call check(index(a, '"name":"land","description":"Queue the landing of a pushed branch '// &
        '(admin); answers its job","inputSchema":{"type":"object","properties":{"repo":'// &
        '{"type":"string","description":"Repository name"},"branch":{"type":"string",'// &
        '"description":"Branch on the server"}},"required":["repo","branch"]}}') > 0, &
        'a tool has an input schema with its required arguments: '//a)
    call check(index(a, '"lines":{"type":"integer"') > 0 .and. &
        index(a, '"required":["id"]') > 0, 'an optional argument is not required: '//a)
    call check(index(a, '"name":"issues","description":"Issues of a repository, the open ones '// &
        'by default","inputSchema":{"type":"object","properties":{"repo":{"type":"string",'// &
        '"description":"Repository name"},"state":{"type":"string","description":'// &
        '"open (default), closed or all"}}') > 0 .and. index(a, 'landed, flagged or all') > 0, &
        'issues and changes each name their own states: '//a)
    call check(passed(37) .and. index(answer(37), '\"repo\":\"demo\"') > 0 .and. &
        index(answer(37), '\"status\":') > 0, &
        'changes without a repository lists those of all: '//answer(37))
    call check(passed(38) .and. index(answer(38), '\"changes\":[]') > 0, &
        'changes filters by review: '//answer(38))
    call check(index(answer(39), 'changes needs a valid review') > 0, &
        'changes refuses a review that is no word: '//answer(39))
    call check(passed(3) .and. index(answer(3), '\"name\":\"demo\"') > 0, &
        'repos lists the repositories: '//answer(3))
    call check(passed(4) .and. index(answer(4), '\"branch\":\"first\"') > 0, &
        'changes lists the open changes: '//answer(4))
    call check(passed(5) .and. index(answer(5), '+hello') > 0, &
        'change_diff answers the diff: '//answer(5))
    call check(passed(6) .and. passed(7), 'change_comment and change_close: '//answer(7))
    call check(passed(8) .and. index(answer(8), '\"branch\":\"first\"') == 0, &
        'a closed change is no longer listed as open: '//answer(8))
    call check(passed(9) .and. index(answer(9), '\"state\":\"closed\"') > 0, &
        'changes with state all lists it as closed: '//answer(9))
    call check(passed(10) .and. passed(11) .and. index(answer(11), '\"state\":\"open\"') > 0 &
        .and. index(answer(11), 'looks \\\"fine\\\"') > 0, &
        'change shows the reopened change with its comment: '//answer(11))
    call check(passed(12) .and. index(answer(12), '\"issue\":2') > 0, &
        'issue_new opens an issue: '//answer(12))
    call check(passed(13) .and. passed(14) .and. passed(15) .and. &
        index(answer(15), 'First issue') == 0, 'a closed issue is not listed: '//answer(15))
    call check(index(answer(14), '\"number\":2,\"title\":\"First issue\",\"state\":\"closed\"') &
        > 0, 'issue_close answers the closed issue: '//answer(14))
    call check(index(answer(16), '\"number\":2,\"title\":\"First issue\",\"state\":\"open\"') &
        > 0, 'issue_reopen answers the open issue: '//answer(16))
    call check(passed(16) .and. passed(17) .and. index(answer(17), 'First issue') > 0, &
        'a reopened issue is listed: '//answer(17))
    call check(passed(18) .and. index(answer(18), 'It breaks.') > 0 .and. &
        index(answer(18), 'Seen too.') > 0, 'issue shows the issue with its comments: '// &
        answer(18))
    call check(passed(19) .and. index(answer(19), '\"landing\":1') > 0, &
        'land queues a pushed branch and answers its job: '//answer(19))
    call check(index(answer(20), 'change needs a valid number') > 0 .and. &
        index(answer(20), '"isError":true') > 0, 'a missing argument is an error: '//answer(20))
    call check(index(answer(21), 'changes needs a valid repo') > 0 .and. &
        index(answer(21), '"isError":true') > 0, 'a repository name cannot leave its path: '// &
        answer(21))
    call check(index(answer(22), 'unknown tool') > 0 .and. &
        index(answer(22), '"isError":true') > 0, 'an unknown tool is an error: '//answer(22))
    call check(passed(23) .and. index(answer(23), '\"account\":\"admin\"') > 0, &
        'the earlier form with an action still works: '//answer(23))
    call check(passed(24) .and. index(answer(24), '\"account\":\"admin\"') > 0, &
        'a tool call with its id after the params is answered: '//out)
    call check(index(answer(25), '"protocolVersion":"2025-06-18"') > 0, &
        'initialize with its id after the params is answered: '//out)
    call check(index(answer(26), '"protocolVersion":"a\"b"') > 0, &
        'initialize escapes the version it echoes: '//answer(26))
    call check(answer(-3) == '{"jsonrpc":"2.0","id":-3,"result":{}}', &
        'a negative id comes back as a number: '//out)
    call check(index(out, '{"jsonrpc":"2.0","id":"a\"b","result":{}}') > 0, &
        'a string id comes back as that string: '//out)
    call check(index(answer(27), '"error":{"code":-32600,') > 0, &
        'a message with an id and no method is an invalid request: '//answer(27))
    call check(index(out, '{"jsonrpc":"2.0","id":null,"error":{"code":-32700,') > 0, &
        'a broken message is a parse error: '//out)
    call check(index(answer(29), 'issues needs a valid state') > 0 .and. &
        index(answer(29), '"isError":true') > 0, 'issues knows no flagged state: '//answer(29))
    call check(passed(30) .and. index(answer(30), '\"changes\":[') > 0, &
        'changes lists the flagged ones: '//answer(30))
    call check(passed(31) .and. index(answer(31), '+hello') > 0, &
        'change_diff answers the version asked for: '//answer(31))
    call check(passed(33) .and. passed(34) .and. &
        index(answer(34), repeat('long ', 8000)) > 0, &
        'a message beyond 32 kB is read whole: '//answer(33))
    call check(answer(35) == '{"jsonrpc":"2.0","id":35,"result":null}' .and. &
        len(answer(36)) == 0, 'shutdown is answered and ends the server: '//answer(35))

    ! An editor's client frames each message with its length.
    requests = '{"jsonrpc":"2.0","id":7,"method":"ping"}'
    write (num, '(i0)') len(requests)
    requests = 'Content-Length: '//trim(num)//achar(13)//new_line('a')//achar(13)// &
        new_line('a')//requests
    call mcp()
    call check(index(out, 'Content-Length: 36'//achar(13)//new_line('a')//achar(13)// &
        new_line('a')//'{"jsonrpc":"2.0","id":7,"result":{}}') > 0, &
        'a message framed with its length is answered in that framing: '//out)

    do i = 1, 300
        requests = ''
        call tool(1, 'job', '"id":1')
        call mcp()
        if (index(answer(1), ', landed') > 0 .or. index(answer(1), ', failed') > 0) exit
        call proc_sleep_ms(200)
    end do
    call check(passed(1) .and. index(answer(1), 'job 1: landing of demo first, landed\n') > 0 &
        .and. index(answer(1), 'check ran\ncheck done') > 0, &
        'job answers the state and the log: '//answer(1))
    requests = ''
    call tool(1, 'job', '"id":1,"lines":1')
    call tool(2, 'jobs', '')
    call tool(3, 'landings', demo)
    call tool(4, 'jobs', demo)
    call tool(5, 'job', '"id":1,"lines":0')
    call tool(6, 'job', '"id":1,"lines":99999999999999')
    call mcp()
    a = answer(1)
    call check(passed(1) .and. index(a, 'first, landed\n') > 0 .and. index(a, 'check ran') == 0 &
        .and. index(a(index(a, 'landed\n') + 8:), '\n') == 0, &
        'job with lines answers that many lines from the end of the log: '//a)
    call check(passed(2) .and. index(answer(2), '\"jobs\":[{\"id\":1,') > 0 .and. &
        passed(4) .and. index(answer(4), '\"jobs\":[{\"id\":1,') > 0, &
        'jobs lists the jobs: '//answer(2))
    call check(passed(3) .and. index(answer(3), '\"landings\":[') > 0 .and. &
        index(answer(3), '\"state\":\"landed\"') > 0, &
        'landings lists the landings of a repository: '//answer(3))
    a = answer(5)
    call check(passed(5) .and. index(a, 'first, landed\n"}]') > 0, &
        'job with no lines answers the state alone: '//a)
    call check(passed(6) .and. index(answer(6), 'check ran\ncheck done') > 0, &
        'job with more lines than the log has answers all of it: '//answer(6))

    call proc_kill(sup, SIGTERM)
    call proc_wait(sup, stat)
    print '(a)', 'mcp: all checks passed'

contains

    subroutine request(id, method, params)
        !! Adds a JSON-RPC request to those of the next fy mcp run.
        integer, intent(in) :: id
        character(len=*), intent(in) :: method, params
        write (num, '(i0)') id
        requests = requests//'{"jsonrpc":"2.0","id":'//trim(num)//',"method":"'//method// &
            '","params":'//params//'}'//new_line('a')
    end subroutine request

    subroutine tool(id, name, arguments)
        integer, intent(in) :: id
        character(len=*), intent(in) :: name, arguments
        call request(id, 'tools/call', '{"name":"'//name//'","arguments":{'//arguments//'}}')
    end subroutine tool

    subroutine mcp()
        !! One fy mcp process reads the requests; `out` holds its answers.
        integer :: u
        open (newunit=u, file=data//'/requests', access='stream', form='unformatted', &
            action='write', status='replace')
        write (u) requests
        close (u)
        call run(env//fy//' mcp < '//data//'/requests')
    end subroutine mcp

    function answer(id) result(line)
        !! The answer to request `id` of the last fy mcp run, '' without one.
        integer, intent(in) :: id
        character(len=:), allocatable :: line
        integer :: at, eol
        write (num, '(i0)') id
        line = ''
        at = index(new_line('a')//out, new_line('a')//'{"jsonrpc":"2.0","id":'//trim(num)//',')
        if (at == 0) return
        eol = index(out(at:), new_line('a'))
        if (eol == 0) eol = len(out) - at + 2
        line = out(at:at + eol - 2)
    end function answer

    logical function passed(id)
        integer, intent(in) :: id
        passed = index(answer(id), '"isError":false') > 0
    end function passed

    subroutine run(command)
        !! Runs a shell command; `out` is its output, `stat` its exit status.
        character(len=*), intent(in) :: command
        call execute_command_line('('//command//') > '//data//'/out 2>&1', exitstat=stat)
        out = read_all(data//'/out')
    end subroutine run

    function read_line(path) result(line)
        character(len=*), intent(in) :: path
        character(len=4096) :: line
        integer :: u, ios
        line = ''
        open (newunit=u, file=path, action='read', status='old', iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        close (u)
    end function read_line

    function read_all(path) result(text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: text
        integer :: u, ios, n
        text = ''
        open (newunit=u, file=path, access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        deallocate (text)
        allocate (character(len=n) :: text)
        if (n > 0) read (u, iostat=ios) text
        close (u)
    end function read_all

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            if (sup > 0) call proc_kill(sup, SIGTERM)
            error stop 1
        end if
    end subroutine check

end program test_mcp
