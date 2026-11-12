#!/bin/sh
# Install a check backend process (doc/backends.md) as a container on a host:
#   install-backend.sh DIR TARGET IMAGE NAME ROLE BACKEND...
# DIR (local disk) must hold `token` (runner scope) and the backend's own
# credential (GitHub: a fine-grained token file; GitLab: the project token
# file), named in BACKEND by their paths inside DIR. ROLE is the egress list
# of deploy/egress (github-backend or gitlab-backend). BACKEND is what follows
# `fy backend`, e.g.
#   github --owner lazy-fortran --github-token-file DIR/github.token
# Rerunning it updates the start script and restarts the container.
# FY_SUBNET, e.g. 198.18.4 (a /24 of its own per installation), gives the
# networks fixed /28 subnets in it, where Docker's own address pools are used
# up; without it Docker picks them.
set -eu
if [ -n "${FY_SUBNET:-}" ]; then net="s|@NET@|$FY_SUBNET|g"; else net='/@NET@/d'; fi
dir=$1 target=$2 image=$3 name=$4 role=$5
shift 5
backend=$*
here=$(cd "$(dirname "$0")/.." && pwd)
[ -s "$dir/token" ] || { echo "install-backend: $dir/token missing" >&2; exit 1; }
[ -f "$here/deploy/egress/$role.txt" ] || { echo "install-backend: no egress list $role" >&2; exit 2; }
case $backend in
    *'|'* | *'"'*) echo "install-backend: BACKEND may not hold | or \"" >&2; exit 2 ;;
esac
egress_dir=${dir%/}-egress
mkdir -p "$dir/bin" "$dir/tmp" "$egress_dir"
cp "$here/scripts/remote-runner.sh" "$dir/bin/remote-runner.sh"
chmod 755 "$dir/bin/remote-runner.sh"
egress=fortyard-egress:1
docker build --quiet -t "$egress" "$here/deploy/egress" >/dev/null
sed -e "s|@UID@|$(id -u)|g" -e "s|@GID@|$(id -g)|g" -e "s|@TARGET@|$target|g" \
    -e "s|@HOME@|$dir|g" -e "s|@IMAGE@|$image|g" -e "s|@NAME@|$name|g" \
    -e "s|@ROLE@|$role|g" -e "s|@BACKEND@|$backend|g" -e "s|@EGRESS_IMAGE@|$egress|g" \
    -e "s|@EGRESS@|$egress_dir|g" -e "$net" \
    "$here/deploy/compose/backend.yaml" > "$dir/compose.yaml"
docker compose -f "$dir/compose.yaml" -p "$name" up -d --force-recreate
