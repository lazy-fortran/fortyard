#!/bin/sh
# A runner on macOS (arm64): there is no macOS release of fy, so it builds fy
# of exactly the commit the server runs from source, and runs it as
# `fy runner --class macos-arm64` in remote mode. Each check runs in the
# sandbox of sandbox-runtime (srt, settings in FY_RUNNER/srt.json) and under
# the memory limit FY_CHECK_MEMORY (GiB) that fy's check watcher enforces.
#   FY_TARGET  the instance, e.g. https://code.tugraz.at/yard
#   FY_RUNNER  runner directory with `token` (runner scope)
#   FY_SHARED  directory the runners of this host share: the source and the
#              builds of fy and the compiler caches
#   FY_CLASS   runner class (default macos-arm64)
#   FY_CHECK_MEMORY  GiB a check may use (default 7)
# The runners of a host build fy one at a time (lockf on FY_SHARED/build.lock).
set -eu
: "${FY_TARGET:?}" "${FY_RUNNER:?}" "${FY_SHARED:?}"
home=$FY_RUNNER
shared=$FY_SHARED
class=${FY_CLASS:-macos-arm64}
tok=$(cat "$home/token")
auth="Authorization: Bearer $tok"
# git reads the token from a file, so it is never on a command line.
umask 077
printf '%s\n' "$(printf %s "$FY_TARGET" | sed "s|://|://runner:$tok@|")" > "$home/git-credentials"
umask 022
helper="credential.helper=store --file=$home/git-credentials"
mkdir -p "$shared/releases" "$shared/cache" "$home/data" "$home/tmp" "$home/home"

if [ "${1:-}" = --build ]; then
    # Called under the lock: builds fy of commit $3 as release $2.
    version=$2 full=$3
    out=$shared/releases/$version
    [ -x "$out/fy" ] && exit 0
    src=$shared/src
    [ -d "$src/.git" ] || git -c "$helper" clone --quiet "$FY_TARGET/fortyard.git" "$src"
    git -C "$src" -c "$helper" fetch --quiet origin
    git -C "$src" checkout --quiet --force "$full"
    cat > "$src/src/fy_build_info.f90" <<INFO
module fy_build_info
    implicit none
    character(len=*), parameter :: FY_VERSION = '$version'
    character(len=*), parameter :: FY_COMMIT = '$(printf %.12s "$full")'
    character(len=*), parameter :: FY_BUILT = '$(date -u +%Y-%m-%dT%H:%M:%SZ)'
end module fy_build_info
INFO
    rm -rf "$out.new"
    if (cd "$src" && FO_CACHE_DIR=$shared/cache/fo fo install --prefix "$out.new") \
        > "$shared/releases/$version.log" 2>&1; then
        git -C "$src" checkout --quiet -- src/fy_build_info.f90
        mkdir -p "$out.new/scripts"
        cp "$src/scripts/mac-runner.sh" "$out.new/scripts/mac-runner.sh"
        mv "$out.new/bin/fy" "$out.new/fy"
        mv "$out.new" "$out"
        # Only the newest two builds are kept.
        ls -dt "$shared/releases"/*/ | tail -n +3 | while read -r old; do rm -rf "$old"; done
    else
        git -C "$src" checkout --quiet -- src/fy_build_info.f90
        tail -30 "$shared/releases/$version.log" >&2
        rm -rf "$out.new"
        exit 1
    fi
    exit 0
fi

while :; do
    commit=$(curl -fsS -m 10 "$FY_TARGET/api/v1/version" |
        sed -n 's/.*"commit":"\([0-9a-f]*\)".*/\1/p' || true)
    line=$(curl -fsS -m 10 -H "$auth" "$FY_TARGET/api/v1/repos/fortyard/releases?format=text" |
        awk -v c="$commit" 'c != "" && index($2, c) == 1 && $3 == "ready" { print; exit }' || true)
    if [ -n "$line" ]; then
        set -- $line
        version=$1 full=$2
        dir=$shared/releases/$version
        [ -x "$dir/fy" ] || lockf -k "$shared/build.lock" "$0" --build "$version" "$full" || true
        if [ -x "$dir/fy" ]; then
            # The runner updates itself: a release with a different start
            # script replaces this one, which restarts as it.
            if ! cmp -s "$dir/scripts/mac-runner.sh" "$0"; then
                cp "$dir/scripts/mac-runner.sh" "$0.new" && chmod 755 "$0.new" &&
                    mv -f "$0.new" "$0" && exec "$0"
            fi
            HOME=$home/home TMPDIR=$home/tmp FO_CACHE_DIR=$shared/cache/fo \
                CCACHE_DIR=$shared/cache/ccache FY_CHECK_MEMORY=${FY_CHECK_MEMORY:-7} \
                FY_CHECK_WRAP="srt --settings $home/srt.json" \
                "$dir/fy" runner --server "$FY_TARGET" --token-file "$home/token" \
                --data "$home/data" --class "$class" || true
        fi
    fi
    sleep 10
done
