#!/bin/sh
# Install a landing-check runner as a container on a build host:
#   install-runner.sh DIR TARGET IMAGE CPUS MEMORY [NAME [CACHE [CACHE_SIZE [SHARES]]]]
# DIR (local disk) must hold `token` (runner scope); IMAGE is the toolchain
# image the checks run in (scripts/toolchain-image.sh). NAME (default
# fortyard-runner) names the container, so a host can run several runners,
# each with its own DIR; they share the compiler caches in CACHE (default
# DIR/cache) when given the same one. Rerunning it updates the start script
# and restarts the container.
# CACHE_SIZE bounds each of the compiler caches (default 20G). SHARES is the CPU
# weight against other containers (default 1024, Docker's default): a runner
# on a host that also serves users gets a lower one, so it only takes idle CPU.
# The runner reaches the network only through its egress proxies
# (deploy/egress), built here: the executor's for the checks and the
# controller's for fy itself. Their logs and the extra hosts of the running
# check lie in FY_EGRESS_DIR (default DIR-egress), which the executor does not
# mount; the controller writes the extra hosts to FY_EGRESS_DIR/extra.
# FY_EGRESS_LEARN=0 ends the executor's learning week (default 1:
# unlisted public hosts pass and are logged in FY_EGRESS_DIR/executor).
# FY_SUBNET, e.g. 198.18.2 (a /24 of its own per installation), gives the
# networks fixed /28 subnets in it, where Docker's own address pools are used
# up; without it Docker picks them.
set -eu
if [ -n "${FY_SUBNET:-}" ]; then net="s|@NET@|$FY_SUBNET|g"; else net='/@NET@/d'; fi
dir=$1 target=$2 image=$3 cpus=$4 memory=$5 name=${6:-fortyard-runner}
cache=${7:-$dir/cache} cache_size=${8:-20G} shares=${9:-1024}
here=$(cd "$(dirname "$0")/.." && pwd)
[ -s "$dir/token" ] || { echo "install-runner: $dir/token missing" >&2; exit 1; }
egress_dir=${FY_EGRESS_DIR:-${dir%/}-egress}
case $egress_dir/ in
    "${dir%/}"/* | "${cache%/}"/*)
        echo "install-runner: FY_EGRESS_DIR must lie outside DIR and CACHE" >&2; exit 2 ;;
esac
# fo and fy at main for the checks (install-tools-follower.sh with
# FY_TOOLS_IMAGE fills it); shared by the runners of this host.
tools=${FY_TOOLS_BIN_DIR:-$(dirname "${dir%/}")/fortyard-tools/bin}
mkdir -p "$dir/bin" "$dir/tmp" "$egress_dir/executor" "$egress_dir/controller" \
    "$egress_dir/extra" "$cache" "$tools"
cp "$here/scripts/remote-runner.sh" "$dir/bin/remote-runner.sh"
chmod 755 "$dir/bin/remote-runner.sh"
egress=fortyard-egress:1
docker build --quiet -t "$egress" "$here/deploy/egress" >/dev/null
sed -e "s|@UID@|$(id -u)|g" -e "s|@GID@|$(id -g)|g" -e "s|@TARGET@|$target|g" \
    -e "s|@HOME@|$dir|g" -e "s|@IMAGE@|$image|g" -e "s|@CPUS@|$cpus|g" \
    -e "s|@MEMORY@|$memory|g" -e "s|@NAME@|$name|g" -e "s|@CACHE@|$cache|g" -e "s|@CACHE_SIZE@|$cache_size|g" -e "s|@SHARES@|$shares|g" \
    -e "s|@EGRESS_IMAGE@|$egress|g" -e "s|@LEARN@|${FY_EGRESS_LEARN:-1}|g" -e "s|@EGRESS@|$egress_dir|g" \
    -e "s|@TOOLS@|$tools|g" \
    -e "$net" \
    "$here/deploy/compose/runner.yaml" \
    > "$dir/compose.yaml"
docker compose -f "$dir/compose.yaml" -p "$name" up -d --force-recreate
