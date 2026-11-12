#!/bin/sh
# Install fortyard as a Docker Compose project that updates itself from the
# releases it stores (production, where nothing is compiled):
#   install-compose.sh FY_HOME FIRST_RELEASE.tar BASE IMAGE RELAY [PUBLIC_URL]
# FY_HOME   project directory on the host (database, repositories, releases)
# BASE      path prefix of the reverse proxy, e.g. /yard
# IMAGE     the toolchain image (git, gfortran and fo for landing checks)
# RELAY     host address and port where the relay publishes the server for
#           the reverse proxy, e.g. 172.18.0.1:8740 (the gateway of the reverse
#           proxy's Docker network); its route for fortyard points there
# PUBLIC_URL address people reach the instance at, e.g. https://example.org/yard;
#           links in commits and issues use it
# FY_EGRESS_LEARN=0 ends the learning week of the runner's egress proxy
#           (default 1: unlisted public hosts pass and are logged)
# FY_SUBNET, e.g. 198.18.2 (a /24 of its own per installation), gives the
# networks fixed /28 subnets in it, where Docker's own address pools are used
# up; without it Docker picks them.
set -eu
if [ -n "${FY_SUBNET:-}" ]; then net="s|@NET@|$FY_SUBNET|g"; else net='/@NET@/d'; fi
home=$1 first=$2 base=$3 image=$4 relay=$5 public=${6:-}
learn=${FY_EGRESS_LEARN:-1}
case $relay in
    *:[0-9]*) ;;
    *) echo "install-compose: RELAY is ADDRESS:PORT, not a network" >&2; exit 2 ;;
esac
root=$(cd "$(dirname "$0")/.." && pwd)
mkdir -p "$home/bin" "$home/releases" "$home/staging" "$home/egress/server" \
    "$home/egress/runner" "$home/egress/extra"
cp "$root/scripts/deploy.sh" "$root/scripts/self-update.sh" "$root/scripts/start.sh" "$home/bin/"
# -L: the link may point to the path the container sees, not the host.
if [ ! -L "$home/current" ]; then
    stage=$home/staging/first
    rm -rf "$stage"
    mkdir -p "$stage"
    tar -xf "$first" -C "$stage"
    (cd "$stage" && sha256sum -c --quiet fy.sha256)
    version=$(sed -n 's/^version=//p' "$stage/RELEASE")
    mv "$stage" "$home/releases/$version"
    # Relative, as the container sees the project directory elsewhere.
    ln -sfn "releases/$version" "$home/current"
fi
egress=fortyard-egress:1
docker build --quiet -t "$egress" "$root/deploy/egress" >/dev/null
# ssh needs an account for the user it runs as; GitHub over port 443.
printf 'root:x:0:0::/root:/bin/sh\nfortyard:x:%s:%s::/tmp:/bin/sh\n' "$(id -u)" "$(id -g)" > "$home/bin/passwd"
cat > "$home/bin/ssh_config" <<'SSH'
Host github.com
    HostName ssh.github.com
    Port 443
    HostKeyAlias github.com
    ProxyCommand nc -X connect -x fortyard-egress-server:3128 %h %p
SSH
sed -e "s|@IMAGE@|$image|" -e "s|@BASE@|$base|g" -e "s|@UID@|$(id -u):$(id -g)|g" \
    -e "s|@PUBLIC_URL@|$public|" -e "s|@RELAY@|$relay|g" -e "s|@EGRESS_IMAGE@|$egress|" \
    -e "s|@RELAY_IMAGE@|alpine/socat:1.8.0.3|" -e "s|@LEARN@|$learn|" \
    -e "$net" \
    "$root/deploy/compose/compose.yaml" > "$home/compose.yaml"
run() { docker run --rm --network none --user "$(id -u):$(id -g)" -v "$home:/fy" "$image" "$@"; }
if [ ! -e "$home/data/fortyard.db" ]; then
    run /fy/current/fy serve --init --data /fy/data | tail -1 > "$home/bootstrap-token"
    chmod 600 "$home/bootstrap-token"
fi
docker compose -f "$home/compose.yaml" up -d
# A token named after its use, the time and this run, as a name is taken for
# good, also after the token in a lost file is revoked; written only when made.
mint() {
    reply=$(docker exec fortyard curl -fsS -H "Authorization: Bearer $(cat "$home/bootstrap-token")" \
        -H 'Content-Type: application/json' -d "{\"name\":\"$1 $(date -u +%Y-%m-%dT%H:%M:%SZ) $$\"}" \
        http://127.0.0.1:8740/api/v1/tokens)
    token=$(printf '%s\n' "$reply" | sed -n 's/.*"token":"\([^"]*\)".*/\1/p')
    [ -n "$token" ] || { echo "install-compose: no $1 token: $reply" >&2; exit 1; }
    (umask 077 && printf '%s' "$token" > "$2")
    chmod 600 "$2"
}
if [ ! -s "$home/update-token" ]; then
    i=0
    until docker exec fortyard curl -fsS -m 2 http://127.0.0.1:8740/healthz >/dev/null 2>&1; do
        i=$((i + 1)); [ $i -lt 50 ] || exit 1; sleep 0.2
    done
    mint self-update "$home/update-token"
fi
# The runner's token; the runner (re)starts once it exists.
if [ ! -s "$home/runner-token" ]; then
    mint runner "$home/runner-token"
    docker restart fortyard-runner >/dev/null
fi
echo "fortyard runs below $base; bootstrap token in $home/bootstrap-token"
