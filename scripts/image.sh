#!/bin/sh
# Build the container image of a commit: image.sh REPO COMMIT VERSION TAG
# The browser assets are built here (they need LFortran and emscripten), the
# server inside Debian, the system of the production host.
set -eu
repo=$1 commit=$2 version=$3 tag=$4
: "${FO_REV:?FO_REV must name the fo commit to build}"
fpm_version=0.13.0
fpm_sha=$(curl -fsSL "https://github.com/fortran-lang/fpm/releases/download/v$fpm_version/fpm-$fpm_version-linux-x86_64-gcc-12.sha256" | cut -d' ' -f1)
ctx=$(mktemp -d "${FY_BUILD_ROOT:-/var/tmp}/fy-image.XXXXXX")
trap 'git -C "$repo" worktree remove --force "$ctx/src" >/dev/null 2>&1 || true; rm -rf "$ctx"' EXIT
git -C "$repo" worktree add --detach "$ctx/src" "$commit" >/dev/null
short=$(git -C "$ctx/src" rev-parse --short=12 HEAD)
cat > "$ctx/src/src/fy_build_info.f90" <<INFO
module fy_build_info
    implicit none
    character(len=*), parameter :: FY_VERSION = '$version'
    character(len=*), parameter :: FY_COMMIT = '$short'
    character(len=*), parameter :: FY_BUILT = '$(date -u +%Y-%m-%dT%H:%M:%SZ)'
end module fy_build_info
INFO
(cd "$ctx/src" && fo build >/dev/null 2>&1)  # fetches fortui and fx for the browser build
"$ctx/src/scripts/build-web.sh" "$ctx/src" "$ctx/web" >"$ctx/web.log" 2>&1 || {
    tail -40 "$ctx/web.log" >&2
    exit 1
}
# Keep the fetched dependency sources (fortui is private), drop the host build.
mv "$ctx/src/build/dependencies" "$ctx/deps"
rm -rf "$ctx/src/build"
mkdir -p "$ctx/src/build"
mv "$ctx/deps" "$ctx/src/build/dependencies"
find "$ctx/src/build/dependencies" -name build -type d -prune -exec rm -rf {} +
docker build --quiet --build-arg FPM_SHA256="$fpm_sha" --build-arg FO_REV="$FO_REV" \
    -f "$ctx/src/deploy/container/Dockerfile" -t "$tag" "$ctx" >/dev/null
echo "$tag"
