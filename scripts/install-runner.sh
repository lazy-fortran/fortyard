#!/bin/sh
# Install a landing-check runner as a container on a build host:
#   install-runner.sh DIR TARGET IMAGE CPUS MEMORY [NAME [CACHE]]
# DIR (local disk) must hold `token` (runner scope); IMAGE is the toolchain
# image the checks run in (scripts/toolchain-image.sh). NAME (default
# fortyard-runner) names the container, so a host can run several runners,
# each with its own DIR; they share the compiler caches in CACHE (default
# DIR/cache) when given the same one. Rerunning it updates the start script
# and restarts the container.
set -eu
dir=$1 target=$2 image=$3 cpus=$4 memory=$5 name=${6:-fortyard-runner}
cache=${7:-$dir/cache}
here=$(cd "$(dirname "$0")/.." && pwd)
[ -s "$dir/token" ] || { echo "install-runner: $dir/token missing" >&2; exit 1; }
mkdir -p "$dir/bin" "$dir/tmp" "$cache"
cp "$here/scripts/remote-runner.sh" "$dir/bin/remote-runner.sh"
chmod 755 "$dir/bin/remote-runner.sh"
sed -e "s|@UID@|$(id -u)|g" -e "s|@GID@|$(id -g)|g" -e "s|@TARGET@|$target|g" \
    -e "s|@HOME@|$dir|g" -e "s|@IMAGE@|$image|g" -e "s|@CPUS@|$cpus|g" \
    -e "s|@MEMORY@|$memory|g" -e "s|@NAME@|$name|g" -e "s|@CACHE@|$cache|g" \
    "$here/deploy/compose/runner.yaml" \
    > "$dir/compose.yaml"
docker compose -f "$dir/compose.yaml" -p "$name" up -d --force-recreate
