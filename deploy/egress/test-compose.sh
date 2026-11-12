#!/bin/sh
# Renders deploy/compose/compose.yaml and runner.yaml with placeholder values,
# validates them with docker compose config and checks where the extra hosts
# of a check's repository lie: the runner controller mounts their directory
# read-write and names the file in FY_EGRESS_EXTRA_FILE, the executor's proxy
# mounts it read-only and reads the same file (FY_EGRESS_EXTRA), and the
# executor mounts neither it nor a directory above it. Needs docker compose
# and jq; starts nothing.
#   test-compose.sh
set -eu
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d "${TMPDIR:-/var/tmp}/fy-compose-test.XXXXXX")
trap 'rm -rf "$work"' EXIT INT TERM
fails=0
ok() { echo "ok   $1"; }
bad() { echo "FAIL $1"; fails=$((fails + 1)); }

# render TEMPLATE OUT UID: the install scripts' placeholders, without FY_SUBNET.
render() {
    sed -e '/@NET@/d' -e "s|@UID@|$3|g" -e 's|@GID@|1000|g' -e 's|@HOME@|/srv/runner|g' \
        -e 's|@EGRESS@|/srv/runner-egress|g' -e 's|@CACHE@|/srv/cache|g' \
        -e 's|@CPUS@|2|g' -e 's|@MEMORY@|2g|g' -e 's|@SHARES@|512|g' \
        -e 's|@RELAY@|127.0.0.1:8740|g' -e 's|@NAME@|fortyard-runner|g' \
        -e 's|@[A-Z_]*IMAGE@|example/image:1|g' -e 's|@[A-Z_]*@|x|g' "$1" > "$2"
}

# check FILE CONTROLLER PROXY EXECUTOR EXTRA_DIR
check() {
    name=$(basename "$1")
    if ! docker compose -f "$1" config --format json > "$work/$name.json" 2> "$work/err"; then
        bad "$name: docker compose config: $(cat "$work/err")"
        return
    fi
    ok "$name is a valid compose file"
    vol() { # service source target read_only
        jq -e --arg s "$1" --arg src "$2" --arg t "$3" --argjson ro "$4" \
            '.services[$s].volumes // [] | any(.source == $src and .target == $t and
                ((.read_only // false) == $ro))' "$work/$name.json" > /dev/null
    }
    env() { # service variable
        jq -r --arg s "$1" --arg v "$2" '.services[$s].environment[$v] // ""' "$work/$name.json"
    }
    if vol "$2" "$5" /egress-extra false; then ok "$name: the controller mounts extra read-write"
    else bad "$name: the controller mounts $5 read-write at /egress-extra"; fi
    if [ "$(env "$2" FY_EGRESS_EXTRA_FILE)" = /egress-extra/executor.txt ]; then
        ok "$name: the controller names the file"
    else bad "$name: FY_EGRESS_EXTRA_FILE of the controller"; fi
    if vol "$3" "$5" /extra true && [ "$(env "$3" FY_EGRESS_EXTRA)" = /extra/executor.txt ]; then
        ok "$name: the executor's proxy reads the same file read-only"
    else bad "$name: the executor's proxy mounts $5 read-only and reads /extra/executor.txt"; fi
    mounted=$(jq --arg s "$4" --arg d "$5" '.services[$s].volumes // [] | any(.source as $src |
        .type == "bind" and ($src == $d or ($d | startswith($src + "/")) or
        ($src | startswith($d + "/"))))' "$work/$name.json") || mounted=error
    if [ "$mounted" = false ]; then ok "$name: the executor does not mount the extra hosts"
    else bad "$name: the executor mounts the extra hosts ($mounted)"; fi
}

mkdir -p "$work/instance"
render "$here/../compose/compose.yaml" "$work/instance/compose.yaml" 1000:1000
check "$work/instance/compose.yaml" fortyard-runner fortyard-egress-runner fortyard-executor \
    "$work/instance/egress/extra"
render "$here/../compose/runner.yaml" "$work/runner.yaml" 1000
check "$work/runner.yaml" fortyard-runner fortyard-egress fortyard-executor /srv/runner-egress/extra

[ "$fails" = 0 ] || { echo "$fails failed"; exit 1; }
echo "compose files: all checks passed"
