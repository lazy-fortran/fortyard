module fy_build_info
    !! Overwritten by scripts/release.sh in release builds.
    implicit none
    character(len=*), parameter :: FY_VERSION = '0.0.0-dev'
    character(len=*), parameter :: FY_COMMIT = 'unknown'
    character(len=*), parameter :: FY_BUILT = ''
end module fy_build_info
