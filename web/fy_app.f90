program fy_app
    !! The fortyard browser application: starts from the state the server put
    !! into the page and hands its frames to the browser.
    use fortui, only: ui_init, ui_run
    use fortui_web, only: web_backend_t
    use fy_ui_app, only: app_start, app_compose
    use fy_fetch, only: page_data
    implicit none
    integer :: stat

    call ui_init(web_backend_t(root_id="app"), 1000, 800, "fortyard", stat=stat)
    if (stat /= 0) error stop "fortyard needs a browser page"
    call app_start(page_data('fy-version'), page_data('fy-whoami'), page_data('fy-repos'))
    call ui_run(app_compose)
end program fy_app
