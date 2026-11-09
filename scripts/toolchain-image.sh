#!/bin/sh
# Build the toolchain image: toolchain-image.sh TAG (FO_REV names the fo commit).
set -eu
tag=$1
: "${FO_REV:?FO_REV must name the fo commit to build}"
v=0.13.0
sha=$(curl -fsSL "https://github.com/fortran-lang/fpm/releases/download/v$v/fpm-$v-linux-x86_64-gcc-12.sha256" | cut -d' ' -f1)
ctx=$(mktemp -d "${TMPDIR:-/var/tmp}/fy-toolchain.XXXXXX")
trap 'rm -rf "$ctx"' EXIT
docker build --quiet --build-arg FPM_SHA256="$sha" --build-arg FO_REV="$FO_REV" \
    -f "$(dirname "$0")/../deploy/container/Dockerfile" -t "$tag" "$ctx" >/dev/null
echo "$tag"
