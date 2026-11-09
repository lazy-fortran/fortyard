#!/bin/sh
# Install an instance that updates itself from the releases it stores, as
# systemd user units (production, where nothing is compiled):
#   install-follower.sh FY_HOME LISTEN_ADDR:PORT BASE FIRST_RELEASE.tar
# BASE is the path prefix the proxy serves it under (e.g. /yard, or "").
# FIRST_RELEASE.tar is a release archive from the builder (release-builder.sh);
# later releases arrive through the instance itself.
set -eu
home=$1 listen=$2 base=$3 first=$4
root=$(cd "$(dirname "$0")/.." && pwd)
units=${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user
mkdir -p "$home/bin" "$units"
cp "$root/scripts/deploy.sh" "$root/scripts/self-update.sh" "$home/bin/"
cat > "$home/instance.env" <<ENV
FY_HOME=$home
FY_URL=http://$listen
FY_BASE=$base
PATH=/usr/local/bin:/usr/bin:/bin
ENV
sed -e "s|@LISTEN@|$listen|g" "$root/deploy/systemd/fortyard.socket" > "$units/fortyard.socket"
for u in fortyard.service fortyard-update.service; do
    sed -e "s|@HOME@|$home|g" "$root/deploy/systemd/$u" > "$units/$u"
done
systemctl --user daemon-reload
if [ ! -e "$home/current" ]; then
    stage=$home/staging/first
    rm -rf "$stage"
    mkdir -p "$stage" "$home/releases"
    tar -xf "$first" -C "$stage"
    version=$(sed -n 's/^version=//p' "$stage/RELEASE")
    (cd "$stage" && sha256sum -c --quiet fy.sha256)
    mv "$stage" "$home/releases/$version"
    ln -sfn "$home/releases/$version" "$home/current"
fi
if [ ! -e "$home/data/fortyard.db" ]; then
    "$home/current/fy" serve --init --data "$home/data" | tail -1 > "$home/bootstrap-token"
    chmod 600 "$home/bootstrap-token"
fi
systemctl --user enable --now fortyard.socket
if [ ! -s "$home/update-token" ]; then
    i=0
    until curl -fsS -m 2 "http://$listen/healthz" >/dev/null 2>&1 || [ $i -ge 50 ]; do
        i=$((i + 1)); sleep 0.2
    done
    curl -fsS -H "Authorization: Bearer $(cat "$home/bootstrap-token")" \
        -H 'Content-Type: application/json' -d '{"name":"self-update"}' \
        "http://$listen/api/v1/tokens" | sed -n 's/.*"token":"\([^"]*\)".*/\1/p' > "$home/update-token"
    chmod 600 "$home/update-token"
fi
systemctl --user enable --now fortyard-update.service
echo "instance at $listen below '$base'; bootstrap token in $home/bootstrap-token"
