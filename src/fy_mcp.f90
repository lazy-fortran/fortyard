module fy_mcp
    !! fy mcp: an MCP server on standard input and output for coding agents.
    !! Each tool is one request of the stored login to the server's API and
    !! answers with the API's JSON; job answers as fy job prints it, with the
    !! end of the log only.
    use fx_json_build, only: &
        json_escape_string
    use fx_json_parse, only: &
        JSON_ARRAY_END, &
        JSON_ARRAY_START, &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_INTEGER, &
        JSON_KEY, &
        JSON_MAX_DEPTH, &
        JSON_OBJECT_END, &
        JSON_OBJECT_START, &
        JSON_REAL, &
        JSON_STRING, &
        json_event_t, &
        json_parser_init_strict, &
        json_parser_next, &
        json_parser_t
    use fx_mcp, only: &
        MCP_FRAME_BARE_JSON, &
        mcp_read_message, &
        mcp_send_response
    use fy_cli, only: &
        cli_field, &
        cli_tail
    use fy_client, only: &
        client_request, &
        config_load, &
        config_t
    use fy_repo, only: &
        repo_name_ok, &
        repo_ref_ok
    implicit none
    private

    public :: &
        mcp_main

    !! An issue or a comment may be long; fx's own loop stops at 32 kB.
    integer, parameter :: MAX_MESSAGE = 1048576

    !! Name, description and arguments of each tool; ? marks an optional one.
    !! One tool per line, grouped by what it works on; tools/list answers in
    !! this order.
    character(len=64), parameter :: TOOL_LINES(*) = [character(len=64) :: &
    ! server
        'whoami', 'Account of the stored fortyard token', '', &
        'version', 'Version of the fortyard server', '', &
        'repos', 'Repositories of the server', '', &
    ! changes
        'changes', 'Changes of a repository, the open ones by default', 'repo state?', &
        'change', 'A change with its versions, comments and landings', 'repo number', &
        'change_diff', 'The diff of a change', 'repo number version? from?', &
        'change_comment', 'Comment on a change', 'repo number body', &
        'change_close', 'Close a change', 'repo number', &
        'change_reopen', 'Reopen a closed change', 'repo number', &
        'change_land', 'Queue the landing of a change (admin); answers its job', 'repo number', &
    ! issues
        'issues', 'Issues of a repository, the open ones by default', 'repo state?', &
        'issue', 'An issue with its comments', 'repo number', &
        'issue_new', 'Open an issue', 'repo title body?', &
        'issue_comment', 'Comment on an issue', 'repo number body', &
        'issue_close', 'Close an issue', 'repo number', &
        'issue_reopen', 'Reopen a closed issue', 'repo number', &
    ! landings
        'land', 'Queue the landing of a pushed branch (admin); answers its job', 'repo branch', &
        'landings', 'Recent landings of a repository', 'repo', &
    ! jobs
        'jobs', 'Running, waiting and recent jobs', 'repo?', &
        'job', 'A job: its state and the end of its log', 'id lines?', &
        'stats', 'Queue numbers of the last 1, 7 (default) or 30 days', 'repo? days?', &
        'verify', 'Queue a verification of main with the full suite (admin)', 'repo']
    integer, parameter :: N_TOOLS = size(TOOL_LINES) / 3
    character(len=*), parameter :: TOOLS(3, N_TOOLS) = reshape(TOOL_LINES, [3, N_TOOLS])

contains

    subroutine mcp_main()
        !! Answers JSON-RPC requests until standard input ends.
        character(len=:), allocatable :: line, message, method, id, version, text
        integer :: framing, status, kind
        logical :: eof, is_error

        allocate (character(len=MAX_MESSAGE) :: line)
        framing = MCP_FRAME_BARE_JSON
        do
            call mcp_read_message(line, MAX_MESSAGE, framing, eof, status)
            if (eof) exit
            message = trim(line)
            if (status == 0 .and. len(message) == 0) cycle
            call member(message, [character(len=8) :: 'method'], method, kind)
            if (status /= 0 .or. kind == JSON_ERROR) then
                call answer('null', '"error":{"code":-32700,"message":"parse error"}')
                cycle
            end if
            if (kind /= JSON_STRING) method = ''
            ! Clients differ in where they put the id, some after the params.
            call member(message, [character(len=8) :: 'id'], id, kind)
            if (kind == JSON_STRING) id = '"'//json_escape_string(id)//'"'
            if (len(method) == 0) then
                if (len(id) > 0) call answer(id, '"error":{"code":-32600,'// &
                    '"message":"invalid request"}')
                cycle
            end if
            if (method == 'shutdown') then
                if (len(id) > 0) call answer(id, '"result":null')
                exit
            end if
            ! A notification gets no answer.
            if (len(id) == 0) cycle
            select case (method)
            case ('initialize')
                call member(message, [character(len=16) :: 'params', 'protocolVersion'], &
                    version, kind)
                if (kind /= JSON_STRING) version = '2025-03-26'
                call answer(id, '"result":{"protocolVersion":"'//json_escape_string(version)// &
                    '","capabilities":{"tools":{"listChanged":false}},'// &
                    '"serverInfo":{"name":"fortyard","version":"0.1.0"}}')
            case ('ping')
                call answer(id, '"result":{}')
            case ('tools/list')
                call answer(id, '"result":{"tools":['//tools_json()//']}')
            case ('tools/call')
                call call_tool(message, text, is_error)
                call answer(id, '"result":{"content":[{"type":"text","text":"'// &
                    json_escape_string(text)//'"}],"isError":'// &
                    trim(merge('true ', 'false', is_error))//'}')
            case default
                call answer(id, '"error":{"code":-32601,"message":"method not found"}')
            end select
        end do

    contains

        subroutine answer(id, outcome)
            !! Sends the result or error of the request `id`, a JSON value.
            character(len=*), intent(in) :: id, outcome
            call mcp_send_response('{"jsonrpc":"2.0","id":'//id//','//outcome//'}', framing)
        end subroutine answer

    end subroutine mcp_main

    subroutine member(message, keys, value, kind)
        !! The first string or number at the path `keys` of a JSON object:
        !! `kind` is JSON_STRING or JSON_INTEGER, 0 without one and JSON_ERROR
        !! if the text is no valid object. A number comes as it is written.
        character(len=*), intent(in) :: message, keys(:)
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out) :: kind
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        logical :: hit(0:JSON_MAX_DEPTH), first
        integer :: depth

        value = ''
        kind = 0
        depth = 0
        hit = .false.
        first = .true.
        call json_parser_init_strict(p, message)
        do
            call json_parser_next(p, ev)
            if (first .and. ev%event_type /= JSON_OBJECT_START) ev%event_type = JSON_ERROR
            first = .false.
            select case (ev%event_type)
            case (JSON_ERROR)
                value = ''
                kind = JSON_ERROR
                return
            case (JSON_END_OF_INPUT)
                return
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                depth = depth + 1
                hit(depth) = .false.
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
                hit(depth) = .false.
            case (JSON_KEY)
                hit(depth) = .false.
                if (depth <= size(keys)) hit(depth) = ev%string_val == keys(depth)
            case default
                if (kind == 0 .and. depth == size(keys) .and. all(hit(1:depth))) then
                    select case (ev%event_type)
                    case (JSON_STRING)
                        value = ev%string_val
                        kind = JSON_STRING
                    case (JSON_INTEGER, JSON_REAL)
                        value = ev%raw_val
                        kind = JSON_INTEGER
                    end select
                end if
                hit(depth) = .false.
            end select
        end do
    end subroutine member

    function tools_json() result(json)
        !! The tools with their input schemas, as tools/list answers them.
        character(len=:), allocatable :: json, name, properties, required
        integer :: k, j
        logical :: optional

        json = ''
        do k = 1, N_TOOLS
            properties = ''
            required = ''
            j = 0
            do
                j = j + 1
                call argument_name(trim(TOOLS(3, k)), j, name, optional)
                if (len(name) == 0) exit
                if (len(properties) > 0) properties = properties//','
                properties = properties//'"'//name//'":'//argument_schema(name, trim(TOOLS(1, k)))
                if (optional) cycle
                if (len(required) > 0) required = required//','
                required = required//'"'//name//'"'
            end do
            if (k > 1) json = json//','
            json = json//'{"name":"'//trim(TOOLS(1, k))//'","description":"'// &
                trim(TOOLS(2, k))//'","inputSchema":{"type":"object","properties":{'// &
                properties//'},"required":['//required//']}}'
        end do
    end function tools_json

    subroutine argument_name(args, j, name, optional)
        !! The j-th word of an argument list, '' past its end.
        character(len=*), intent(in) :: args
        integer, intent(in) :: j
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: optional
        integer :: at, sp, k

        at = 1
        do k = 1, j - 1
            sp = index(args(at:), ' ')
            if (sp == 0) at = len(args) + 1
            if (sp == 0) exit
            at = at + sp
        end do
        name = ''
        if (at <= len(args)) name = args(at:)
        if (index(name, ' ') > 0) name = name(1:index(name, ' ') - 1)
        optional = index(name, '?') > 0
        if (optional) name = name(1:len(name) - 1)
    end subroutine argument_name

    function argument_schema(name, tool) result(json)
        character(len=*), intent(in) :: name, tool
        character(len=:), allocatable :: json

        select case (name)
        case ('repo'); json = text_arg('Repository name')
        case ('number'); json = number_arg('Number of the change or issue')
        case ('state')
            json = text_arg('open (default), closed or all')
            if (tool == 'changes') json = text_arg('open (default), closed, landed, '// &
                'flagged or all')
        case ('version'); json = number_arg('Version of the change, the newest by default')
        case ('from'); json = number_arg('Earlier version to compare with')
        case ('body'); json = text_arg('Text, Markdown')
        case ('title'); json = text_arg('Title')
        case ('branch'); json = text_arg('Branch on the server')
        case ('days'); json = number_arg('Days of the window: 1, 7 (default) or 30')
        case ('id'); json = number_arg('Job id')
        case ('lines'); json = number_arg('Lines from the end of the log, 40 by default')
        case default; json = '{"type":"string"}'
        end select

    contains

        function text_arg(about) result(s)
            character(len=*), intent(in) :: about
            character(len=:), allocatable :: s
            s = '{"type":"string","description":"'//about//'"}'
        end function text_arg

        function number_arg(about) result(s)
            character(len=*), intent(in) :: about
            character(len=:), allocatable :: s
            s = '{"type":"integer","description":"'//about//'"}'
        end function number_arg

    end function argument_schema

    subroutine call_tool(message, text, is_error)
        !! Performs the tool of a tools/call request; `text` is its answer.
        character(len=*), intent(in) :: message
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: is_error
        type(config_t) :: cfg
        character(len=:), allocatable :: tool, name, value, repo, n, path, method, payload, body
        integer :: k, j, code, stat, expect, lines, kind
        logical :: found, optional

        is_error = .true.
        call member(message, [character(len=8) :: 'params', 'name'], tool, kind)
        if (kind /= JSON_STRING) tool = ''
        if (tool == 'fortyard') tool = argument(message, 'action')
        k = findloc(TOOLS(1, :) == tool, .true., 1)
        if (k == 0) then
            text = '{"error":"unknown tool"}'
            return
        end if
        j = 0
        do
            j = j + 1
            call argument_name(trim(TOOLS(3, k)), j, name, optional)
            if (len(name) == 0) exit
            value = argument(message, name)
            found = len(value) > 0 .or. optional
            ! What becomes part of the request's path must not reshape it.
            select case (name)
            case ('repo'); if (len(value) > 0) found = repo_name_ok(value)
            case ('branch'); if (len(value) > 0) found = repo_ref_ok(value)
            case ('days', 'number', 'id', 'lines', 'version', 'from')
                found = found .and. verify(value, '0123456789') == 0
            case ('state')
                ! The server answers a state it does not know with an empty list.
                found = len(value) == 0 .or. any(value == [character(len=8) :: 'open', &
                    'closed', 'all']) .or. (tool == 'changes' .and. &
                    any(value == [character(len=8) :: 'landed', 'flagged']))
            end select
            if (.not. found) then
                text = '{"error":"'//tool//' needs a valid '//name//'"}'
                return
            end if
        end do

        repo = argument(message, 'repo')
        n = argument(message, 'number')
        method = 'GET'
        payload = ''
        expect = 200
        select case (tool)
        case ('whoami'); path = '/api/v1/whoami'
        case ('version'); path = '/api/v1/version'
        case ('repos'); path = '/api/v1/repos'
        case ('jobs')
            path = '/api/v1/jobs'
            if (len(repo) > 0) path = path//'?repo='//repo
        case ('job'); path = '/api/v1/jobs/'//argument(message, 'id')
        case ('landings'); path = '/api/v1/repos/'//repo//'/landings'
        case ('stats')
            path = '/api/v1/stats?repo='//repo
            value = argument(message, 'days')
            if (len(value) > 0) path = path//'&days='//value
        case ('changes', 'issues')
            path = '/api/v1/repos/'//repo//'/'//tool
            value = argument(message, 'state')
            if (len(value) > 0) path = path//'?state='//value
        case ('change'); path = '/api/v1/repos/'//repo//'/changes/'//n
        case ('issue'); path = '/api/v1/repos/'//repo//'/issues/'//n
        case ('change_diff')
            path = '/api/v1/repos/'//repo//'/changes/'//n//'/diff'
            value = argument(message, 'version')
            if (len(value) > 0) path = path//'?version='//value
            value = argument(message, 'from')
            if (len(value) > 0) path = path//merge('&', '?', index(path, '?') > 0)//'from='//value
        case ('change_comment', 'issue_comment')
            path = '/api/v1/repos/'//repo//'/'//tool(1:index(tool, '_') - 1)//'s/'//n// &
                '/comments'
            method = 'POST'
            expect = 201
            payload = '{"body":"'//json_escape_string(argument(message, 'body'))//'"}'
        case ('change_close', 'change_reopen', 'issue_close', 'issue_reopen')
            path = '/api/v1/repos/'//repo//'/'//tool(1:index(tool, '_') - 1)//'s/'//n
            method = 'POST'
            payload = '{"state":"closed"}'
            if (index(tool, '_reopen') > 0) payload = '{"state":"open"}'
        case ('change_land')
            path = '/api/v1/repos/'//repo//'/changes/'//n//'/land'
            method = 'POST'
            expect = 202
        case ('issue_new')
            path = '/api/v1/repos/'//repo//'/issues'
            method = 'POST'
            expect = 201
            payload = '{"title":"'//json_escape_string(argument(message, 'title'))// &
                '","body":"'//json_escape_string(argument(message, 'body'))//'"}'
        case ('land')
            path = '/api/v1/repos/'//repo//'/land'
            method = 'POST'
            expect = 202
            payload = '{"branch":"'//json_escape_string(argument(message, 'branch'))//'"}'
        case ('verify')
            path = '/api/v1/repos/'//repo//'/verify'
            method = 'POST'
            expect = 202
        end select
        call config_load(cfg)
        if (len(cfg%server) == 0) then
            text = '{"error":"not logged in; run fy login"}'
            return
        end if
        if (method == 'POST' .and. len(payload) == 0) payload = '{}'
        call client_request(cfg%server, method, path, cfg%token, payload, code, text, stat)
        if (stat /= 0) text = '{"error":"server not reachable"}'
        is_error = stat /= 0 .or. code /= expect
        if (tool /= 'job' .or. is_error) return
        ! A whole log would crowd out the rest of an agent's context.
        lines = 40
        value = argument(message, 'lines')
        ! A count beyond the integers is more than any log has.
        if (len(value) > 9) lines = huge(lines)
        if (len(value) > 0 .and. len(value) <= 9) read (value, *, iostat=stat) lines
        body = text
        text = 'job '//cli_field(body, 'id')//': '//cli_field(body, 'kind')//' of '// &
            cli_field(body, 'repo')//' '//cli_field(body, 'branch')//', '// &
            cli_field(body, 'state')//new_line('a')
        ! cli_tail takes no lines for all of them.
        if (lines > 0) text = text//cli_tail(cli_field(body, 'log'), lines)
    end subroutine call_tool

    function argument(message, name) result(value)
        !! params.arguments.NAME of a tools/call request, a string or a number.
        character(len=*), intent(in) :: message, name
        character(len=:), allocatable :: value
        integer :: kind

        call member(message, [character(len=16) :: 'params', 'arguments', name], value, kind)
    end function argument

end module fy_mcp
