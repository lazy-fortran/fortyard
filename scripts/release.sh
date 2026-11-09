#!/bin/sh
# Build one release of a commit: release.sh REPO COMMIT OUTDIR
# The artifact is built once; deploy.sh promotes exactly this directory.
set -eu
repo=$1 commit=$2 out=$3
work=$(mktemp -d "${FY_BUILD_ROOT:-/tmp}/fy-build.XXXXXX")
trap 'git -C "$repo" worktree remove --force "$work/src" >/dev/null 2>&1 || true; rm -rf "$work"' EXIT
git -C "$repo" worktree add --detach "$work/src" "$commit" >/dev/null
full=$(git -C "$work/src" rev-parse HEAD)
short=$(printf %.12s "$full")
day=$(date -u +%Y.%m.%d)
# A counter per day: old releases are deleted, their numbers must not return.
n=0
if [ -n "${FY_RELEASES:-}" ]; then
    mkdir -p "$FY_RELEASES"
    counter="$FY_RELEASES/.count-$day"
    [ -f "$counter" ] && n=$(cat "$counter")
    while [ -e "$FY_RELEASES/$day.$n" ]; do n=$((n + 1)); done
    echo $((n + 1)) > "$counter"
fi
version="$day.$n"
built=$(date -u +%Y-%m-%dT%H:%M:%SZ)
cat > "$work/src/src/fy_build_info.f90" <<INFO
module fy_build_info
    implicit none
    character(len=*), parameter :: FY_VERSION = '$version'
    character(len=*), parameter :: FY_COMMIT = '$short'
    character(len=*), parameter :: FY_BUILT = '$built'
end module fy_build_info
INFO
(cd "$work/src" && fo install --prefix "$work/prefix" >"$work/build.log" 2>&1) || {
    tail -40 "$work/build.log" >&2
    exit 1
}
mkdir -p "$out"
cp "$work/prefix/bin/fy" "$out/fy"
(cd "$out" && sha256sum fy > fy.sha256)
if [ -x "$work/src/scripts/build-web.sh" ]; then
    "$work/src/scripts/build-web.sh" "$work/src" "$out/web" >"$work/web.log" 2>&1 || {
        tail -40 "$work/web.log" >&2
        exit 1
    }
    (cd "$out" && sha256sum web/* > web.sha256)
fi
mkdir -p "$out/scripts"
cp "$work/src/scripts/release.sh" "$work/src/scripts/deploy.sh" \
    "$work/src/scripts/edge-update.sh" "$out/scripts/"
printf 'version=%s\ncommit=%s\nbuilt=%s\n' "$version" "$full" "$built" > "$out/RELEASE"
echo "$version"
