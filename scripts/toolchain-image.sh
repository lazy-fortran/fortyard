#!/bin/sh
# Build the toolchain image: toolchain-image.sh TAG (FO_REV names the fo commit).
# The system layers come from Docker's cache unless the Dockerfile changed;
# fo is built in them from a kept checkout and build tree under
# FY_TOOLCHAIN_HOME (default ~/.cache/fortyard-toolchain), so a new fo
# revision recompiles only what changed, and the image only copies fo in.
set -eu
tag=$1
: "${FO_REV:?FO_REV must name the fo commit to build}"
home=${FY_TOOLCHAIN_HOME:-$HOME/.cache/fortyard-toolchain}
dockerfile=$(cd "$(dirname "$0")/../deploy/container" && pwd)/Dockerfile
v=0.13.0
sha=$(curl -fsSL "https://github.com/fortran-lang/fpm/releases/download/v$v/fpm-$v-linux-x86_64-gcc-12.sha256" | cut -d' ' -f1)
ctx=$(mktemp -d "${TMPDIR:-/var/tmp}/fy-toolchain.XXXXXX")
trap 'rm -rf "$ctx"' EXIT
docker build --quiet --target base --build-arg FPM_SHA256="$sha" -f "$dockerfile" \
    -t fortyard-toolchain-base "$ctx" >/dev/null
mkdir -p "$home"
if [ -d "$home/fo/.git" ]; then
    git -C "$home/fo" fetch --quiet origin
else
    git clone --quiet https://github.com/lazy-fortran/fo.git "$home/fo"
fi
git -C "$home/fo" checkout --quiet --force "$FO_REV"
docker run --rm --init --user "$(id -u):$(id -g)" -e HOME=/tmp -v "$home/fo:/work" -w /work \
    fortyard-toolchain-base fpm install --profile release --prefix /work/prefix >/dev/null
cp "$home/fo/prefix/bin/fo" "$ctx/fo"
docker build --quiet --build-arg FPM_SHA256="$sha" -f "$dockerfile" -t "$tag" "$ctx" >/dev/null
echo "$tag"
