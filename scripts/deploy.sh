#!/bin/sh
# Install a release built by release.sh and switch to it atomically:
# deploy.sh RELEASE_DIR. On a failed health check the previous release returns.
set -eu
src=$1
: "${FY_HOME:?FY_HOME must point to the instance directory}"
: "${FY_URL:?FY_URL must be the instance base URL}"
unit=${FY_UNIT:-fortyard.service}
. "$src/RELEASE"
(cd "$src" && sha256sum -c fy.sha256 >/dev/null)
dest="$FY_HOME/releases/$version"
mkdir -p "$FY_HOME/releases"
[ -e "$dest" ] || cp -r "$src" "$dest"
previous=$(readlink "$FY_HOME/current" 2>/dev/null || true)

switch_to() {
    ln -sfn "$1" "$FY_HOME/current.new"
    mv -T "$FY_HOME/current.new" "$FY_HOME/current"
    systemctl --user restart "$unit"
}

healthy() {
    want=$(printf %.12s "$commit")
    i=0
    while [ $i -lt 50 ]; do
        if curl -fsS -m 2 "$FY_URL/healthz" 2>/dev/null | grep -q " $want"; then
            return 0
        fi
        i=$((i + 1))
        sleep 0.2
    done
    return 1
}

switch_to "$dest"
if healthy; then
    echo "deployed $version ($commit)"
    ls -1dt "$FY_HOME"/releases/* | tail -n +11 | while read -r old; do
        [ "$old" = "$previous" ] || rm -rf "$old"
    done
    exit 0
fi
echo "health check failed for $version" >&2
if [ -n "$previous" ]; then
    switch_to "$previous"
    echo "rolled back to $(basename "$previous")" >&2
fi
exit 1
