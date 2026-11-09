#!/bin/sh
# Install a long-running instance as systemd user units that follow main:
#   install-instance.sh FY_HOME LISTEN_ADDR:PORT UPSTREAM_GIT_URL
# The socket stays open across restarts, so deploys do not drop requests.
set -eu
home=$1 listen=$2 upstream=$3
root=$(cd "$(dirname "$0")/.." && pwd)
units=${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user
mkdir -p "$home/bin" "$units"
cp "$root"/scripts/release.sh "$root"/scripts/deploy.sh "$root"/scripts/edge-update.sh "$home/bin/"
cat > "$home/instance.env" <<ENV
FY_HOME=$home
FY_URL=http://$listen
FY_UPSTREAM=$upstream
PATH=$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin
EMSDK_PATH=${EMSDK_PATH:-}
FY_CACHE=$home/cache
ENV
sed -e "s|@LISTEN@|$listen|g" "$root/deploy/systemd/fortyard.socket" > "$units/fortyard.socket"
for u in fortyard.service fortyard-edge.service fortyard-edge.timer; do
    sed -e "s|@HOME@|$home|g" "$root/deploy/systemd/$u" > "$units/$u"
done
systemctl --user daemon-reload
systemctl --user enable --now fortyard.socket
if [ ! -e "$home/data/fortyard.db" ]; then
    mirror="$home/src.git"
    [ -d "$mirror" ] || git clone --quiet --bare "$upstream" "$mirror"
    mkdir -p "$home/tmp"
    FY_BUILD_ROOT="$home/tmp" "$home/bin/release.sh" "$mirror" HEAD "$home/tmp/init" >/dev/null
    "$home/tmp/init/fy" serve --init --data "$home/data" | tail -1 > "$home/bootstrap-token"
    chmod 600 "$home/bootstrap-token"
    rm -rf "$home/tmp/init"
fi
set -a; . "$home/instance.env"; set +a
"$home/bin/edge-update.sh"
systemctl --user enable --now fortyard-edge.timer
echo "instance $FY_URL, bootstrap token in $home/bootstrap-token"
