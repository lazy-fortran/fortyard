#!/bin/sh
# Keep fo at main for the fortyard containers of a host:
#   install-tools-container.sh [DIR [IMAGE]]
# DIR (default /srv/computor/fortyard-tools, local disk) receives the
# binary in DIR/bin, which install-compose.sh and install-runner.sh mount
# into the server and the runner executors; IMAGE (default
# fortyard-toolchain:5) is the image they run in. Rerunning it restarts the
# container.
set -eu
dir=${1:-/srv/computor/fortyard-tools} image=${2:-fortyard-toolchain:5}
mkdir -p "$dir/bin" "$dir/src"
here=$(cd "$(dirname "$0")/.." && pwd)
cp "$here/scripts/follow-tools.sh" "$dir/follow-tools.sh"
sed -e "s|@UID@|$(id -u)|g" -e "s|@GID@|$(id -g)|g" -e "s|@HOME@|$dir|g" \
    -e "s|@IMAGE@|$image|g" "$here/deploy/compose/tools.yaml" > "$dir/compose.yaml"
docker compose -f "$dir/compose.yaml" -p fortyard-tools up -d --force-recreate
