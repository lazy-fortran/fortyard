program fy_app
    !! The fortyard browser application: starts at the page's location, from
    !! the responses the server sent with the page, and hands its frames to the
    !! browser.
    use fortui, only: ui_init, ui_run
    use fortui_web, only: web_backend_t
    use fy_ui_app, only: app_start, app_compose
    use fy_nav, only: nav_location, nav_pref
    implicit none
    integer :: stat

    call ui_init(web_backend_t(root_id="app"), 1000, 800, "fortyard", stat=stat)
    if (stat /= 0) error stop "fortyard needs a browser page"
    call app_start(nav_location(), nav_pref('fy_sidebar'), nav_pref('fy_theme'))
    call ui_run(app_compose)
end program fy_app
