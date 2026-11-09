#!/bin/sh
# Follow main: fetch, and if main moved, build and deploy it (edge channel).
set -eu
: "${FY_HOME:?}" "${FY_URL:?}" "${FY_UPSTREAM:?}"
exec 9>"$FY_HOME/edge.lock"
flock -n 9 || exit 0
mirror="$FY_HOME/src.git"
[ -d "$mirror" ] || git clone --quiet --bare "$FY_UPSTREAM" "$mirror"
git -C "$mirror" fetch --quiet --prune origin '+refs/heads/main:refs/heads/main'
head=$(git -C "$mirror" rev-parse main)
deployed=$(sed -n 's/^commit=//p' "$FY_HOME/current/RELEASE" 2>/dev/null || true)
[ "$head" = "$deployed" ] && exit 0
failed="$FY_HOME/failed-$head"
[ -e "$failed" ] && exit 0
out="$FY_HOME/staging/$head"
rm -rf "$out"
export FY_RELEASES="$FY_HOME/releases" FY_BUILD_ROOT="$FY_HOME/tmp"
mkdir -p "$FY_BUILD_ROOT"
if ! "$(dirname "$0")/release.sh" "$mirror" "$head" "$out"; then
    : > "$failed"
    echo "build of $head failed" >&2
    exit 1
fi
if ! "$(dirname "$0")/deploy.sh" "$out"; then
    : > "$failed"
    exit 1
fi
rm -rf "$out"
