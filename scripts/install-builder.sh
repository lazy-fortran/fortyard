#!/bin/sh
# Install the release builder as a container on a build host:
#   install-builder.sh DIR TARGET TOOLCHAIN WEB_IMAGE "LIMITS"
# DIR (local disk, not a network file system) must hold `token` (release
# scope). The images TOOLCHAIN (scripts/toolchain-image.sh) and WEB_IMAGE
# (deploy/container/Dockerfile.web) must exist on the host. Rerunning it
# updates the builder script and restarts the container.
set -eu
dir=$1 target=$2 toolchain=$3 web=$4 limits=$5
here=$(cd "$(dirname "$0")/.." && pwd)
[ -s "$dir/token" ] || { echo "install-builder: $dir/token missing" >&2; exit 1; }
mkdir -p "$dir/bin"
cp "$here/scripts/release-builder.sh" "$dir/bin/release-builder.sh"
chmod 755 "$dir/bin/release-builder.sh"
builder=$("$here/scripts/buildx-builder.sh")
docker buildx build --builder "$builder" --load --quiet -t fortyard-builder:1 \
    -f "$here/deploy/container/Dockerfile.builder" "$here/deploy/container" >/dev/null
dgid=$(stat -c %g /var/run/docker.sock)
sed -e "s|@UID@|$(id -u)|g" -e "s|@GID@|$(id -g)|g" -e "s|@DOCKER_GID@|$dgid|g" \
    -e "s|@TARGET@|$target|g" -e "s|@HOME@|$dir|g" -e "s|@TOOLCHAIN@|$toolchain|g" \
    -e "s|@WEB_IMAGE@|$web|g" -e "s|@LIMITS@|$limits|g" \
    "$here/deploy/compose/builder.yaml" > "$dir/compose.yaml"
docker compose -f "$dir/compose.yaml" -p fortyard-builder up -d --force-recreate
