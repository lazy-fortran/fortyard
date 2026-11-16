module fy_alerts_view
    !! The top of the server page (doc/inspection.md, "Server"): the alerts
    !! that fire, each with its rule, repository, since when and what it is,
    !! and what the instance serves against main of the repository it follows.
    !! Both are asked for whenever the page is shown.
    use fortui
    use fx_json_parse, only: &
        JSON_END_OF_INPUT, &
        JSON_ERROR, &
        JSON_INTEGER, &
        JSON_KEY, &
        JSON_OBJECT_END, &
        JSON_OBJECT_START, &
        JSON_STRING, &
        json_event_t, &
        json_parser_init, &
        json_parser_next, &
        json_parser_t
    use fy_fetch, only: &
        fetch_ready, &
        fetch_start, &
        request_t
    use fy_time, only: &
        when
    implicit none
    private

    public :: &
        alerts_view_compose, &
        alerts_view_delivery, &
        alerts_view_receive, &
        alerts_view_show

    type :: alert_row_t
        character(len=:), allocatable :: rule, repo, since, detail
    end type alert_row_t

    real, parameter :: BODY = 14.0, SMALL = 12.0, HEADING = 16.0, ROW_H = 40.0

    type(request_t) :: alerts_req, delivery_req
    type(alert_row_t), allocatable :: rows(:)
    logical :: loaded = .false., refused = .false.
    !! The delivery: repository followed, main's commit, the commits of main
    !! not served (-1: not known), and the newest release with its state.
    character(len=:), allocatable :: repo, main, release, release_state
    integer :: behind = -1

contains

    subroutine alerts_view_show()
        !! Asks for the alerts and the delivery afresh.
        if (.not. allocated(rows)) allocate (rows(0))
        loaded = .false.
        refused = .false.
        if (allocated(repo)) deallocate (repo)
        call fetch_start(alerts_req, 'GET', '/api/v1/alerts')
        call fetch_start(delivery_req, 'GET', '/api/v1/delivery')
    end subroutine alerts_view_show

    subroutine alerts_view_receive()
        if (fetch_ready(alerts_req)) then
            loaded = .true.
            refused = alerts_req%status /= 200
            if (.not. refused) call parse_alerts(alerts_req%body)
        end if
        if (fetch_ready(delivery_req)) then
            if (delivery_req%status == 200) call parse_delivery(delivery_req%body)
        end if
    end subroutine alerts_view_receive

    subroutine alerts_view_compose()
        !! The section Alerts: one row per alert, the rule in the negative tone.
        integer :: k

        call text("Alerts", size=HEADING, style=UI_HEADING, id="heading-alerts")
        call column_begin(0.0, id="card-alerts")
        if (.not. loaded) then
            call text("Loading...", size=BODY, tone=UI_TONE_MUTED, id="alerts-loading")
        else if (refused) then
            call text("The alerts could not be read.", size=BODY, tone=UI_TONE_NEGATIVE, &
                id="alerts-refused")
        else if (size(rows) == 0) then
            call row_begin(12.0, id="row-noalerts", height=ROW_H)
            call text("No alerts.", size=BODY, tone=UI_TONE_MUTED, id="noalerts")
            call row_end()
        end if
        if (loaded .and. .not. refused) then
            do k = 1, size(rows)
                call row_begin(12.0, id="row-a"//ui_str(k), height=ROW_H)
                call text(rows(k)%rule, size=BODY, style=UI_HEADING, tone=UI_TONE_NEGATIVE, &
                    width=130.0)
                if (len(rows(k)%repo) > 0) call text(rows(k)%repo, size=BODY)
                if (len(rows(k)%since) > 0) call text("since "//when(rows(k)%since)//" UTC", &
                    size=SMALL, tone=UI_TONE_MUTED)
                call text(rows(k)%detail, size=SMALL, width=UI_FILL, ellipsis=.true.)
                call row_end()
            end do
        end if
        call column_end()
    end subroutine alerts_view_compose

    function alerts_view_delivery() result(s)
        !! "fortyard main 27a37ab · 2 commits not served · release
        !! 2026.11.11.7 ready", or what is known of it.
        character(len=:), allocatable :: s

        if (.not. allocated(repo)) then
            s = "unknown"
            return
        end if
        if (len(repo) == 0) then
            s = "follows no repository of this instance"
            return
        end if
        if (len(main) < 7) then
            s = repo//" has no main"
            return
        end if
        s = repo//" main "//main(1:7)
        if (behind == 0) then
            s = s//" · served"
        else if (behind == 1) then
            s = s//" · 1 commit not served"
        else if (behind > 1) then
            s = s//" · "//ui_str(behind)//" commits not served"
        else
            s = s//" · this build does not know its commit"
        end if
        if (len(release) > 0) s = s//" · release "//release//" "//release_state
    end function alerts_view_delivery

    subroutine parse_alerts(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        type(alert_row_t) :: row
        character(len=:), allocatable :: key

        if (allocated(rows)) deallocate (rows)
        allocate (rows(0))
        call clear()
        key = ''
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START)
                call clear()
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_STRING)
                select case (key)
                case ('rule'); row%rule = ev%string_val
                case ('repo'); row%repo = ev%string_val
                case ('since'); row%since = ev%string_val
                case ('detail'); row%detail = ev%string_val
                end select
            case (JSON_OBJECT_END)
                if (len(row%rule) > 0) rows = [rows, row]
                call clear()
            end select
        end do

    contains

        subroutine clear()
            row%rule = ''
            row%repo = ''
            row%since = ''
            row%detail = ''
        end subroutine clear

    end subroutine parse_alerts

    subroutine parse_delivery(body)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: key
        integer :: depth

        repo = ''
        main = ''
        release = ''
        release_state = ''
        behind = -1
        key = ''
        depth = 0
        call json_parser_init(p, body)
        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                exit
            case (JSON_OBJECT_START)
                depth = depth + 1
            case (JSON_OBJECT_END)
                depth = depth - 1
            case (JSON_KEY)
                key = ev%string_val
            case (JSON_INTEGER)
                if (depth == 1 .and. key == 'behind') behind = int(ev%int_val)
            case (JSON_STRING)
                if (depth == 1) then
                    select case (key)
                    case ('repo'); repo = ev%string_val
                    case ('main'); main = ev%string_val
                    end select
                else if (depth == 2) then
                    if (key == 'version') release = ev%string_val
                    if (key == 'state') release_state = ev%string_val
                end if
            end select
        end do
    end subroutine parse_delivery

end module fy_alerts_view
