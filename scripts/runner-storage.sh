#!/bin/sh
# An executor owns its workspace until the check has finished.
set -eu

mode=$1
shift
work=$(cd "${FY_RUNNER_WORK:?}" && pwd -P)
trees=$work/fy-work
scratch=$work/tmp
retained=${FY_RUNNER_RETAIN_MIB:-16384}
active=${FY_RUNNER_ACTIVE_MIB:-24576}
reserve=${FY_RUNNER_RESERVE_MIB:-20480}
for value in "$retained" "$active" "$reserve"; do
    case $value in ''|*[!0-9]*) echo 'runner storage limits must be in MiB' >&2; exit 125;; esac
done
[ "$retained" -lt "$active" ] || { echo 'runner storage: active limit must exceed retention' >&2; exit 125; }
retained=$((retained * 1024))
active=$((active * 1024))
reserve=$((reserve * 1024))

used_kib() {
    usage=$(du -skx "$trees" "$scratch") || return 1
    printf '%s\n' "$usage" | awk '{ total += $1 } END { print total }'
}

free_kib() {
    disk=$(df -Pk "$work") || return 1
    printf '%s\n' "$disk" | awk 'NR == 2 && $4 ~ /^[0-9]+$/ { print $4; found = 1 }
        END { if (!found) exit 1 }'
}

permit_removal() {
    # Fo seals its cached build generations with read-only directories.
    find "$1" -xdev -type d -uid "$(id -u)" ! -perm -0700 -exec chmod u+rwx -- {} \;
}

collect() {
    keep=$1
    [ ! -L "$trees" ] && [ ! -L "$scratch" ] || return 1
    mkdir -p "$trees" "$scratch" || return 1
    permit_removal "$scratch" || return 1
    find "$scratch" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + || return 1
    used=$(used_kib) || return 1
    free=$(free_kib) || return 1
    while [ "$used" -gt "$retained" ] || [ "$free" -lt "$reserve" ]; do
        oldest=
        for tree in "$trees"/* "$trees"/.[!.]* "$trees"/..?*; do
            [ -d "$tree" ] && [ ! -L "$tree" ] && [ "$tree" != "$keep" ] || continue
            if [ -z "$oldest" ] || [ "$oldest" -nt "$tree" ]; then oldest=$tree; fi
        done
        [ -n "$oldest" ] || break
        printf 'runner storage: evicting workspace %s\n' "${oldest##*/}"
        permit_removal "$oldest" || return 1
        rm -rf -- "$oldest" || return 1
        used=$(used_kib) || return 1
        free=$(free_kib) || return 1
    done
}

case $mode in
    collect) collect ''; exit;;
    run) ;;
    *) exit 125;;
esac

here=$(pwd -P)
[ "${here%/*}" = "$trees" ] || exit 125
collect "$here" || { echo 'runner storage: workspace cleanup failed' >&2; exit 125; }
if [ "$free" -lt "$reserve" ] || [ "$used" -ge "$active" ]; then
    echo 'runner storage: insufficient disk space to start this check' >&2
    exit 125
fi

# land_check runs the wrapper in the dedicated process group of GNU timeout.
process=$(cat "/proc/$$/stat")
group=$(printf '%s\n' "${process##*) }" | awk '{ print $3 }')
if [ "$group" != "$PPID" ] || [ "$(cat "/proc/$PPID/comm")" != timeout ]; then
    echo 'runner storage: check has no dedicated process group' >&2
    exit 125
fi

watch_space() {
    ticks=0
    while :; do
        sleep 2
        free=$(free_kib) || continue
        if [ "$free" -lt "$reserve" ]; then reason='free space fell below the reserve'; break; fi
        ticks=$((ticks + 1))
        if [ "$ticks" -lt 8 ]; then continue; fi
        ticks=0
        used=$(used_kib 2>/dev/null) || continue
        if [ "$used" -gt "$active" ]; then reason='the workspace exceeded its active limit'; break; fi
    done
    printf 'runner storage: %s. Stopping the check.\n' "$reason" >&2
    # A check can ignore TERM while it continues to allocate disk space.
    kill -KILL "-$group" 2>/dev/null || true
}

watch_space &
watcher=$!
trap 'kill -KILL "$watcher" 2>/dev/null || true' EXIT
status=0
# The outer guard also covers runners started by a check.
unset FY_CHECK_WRAP
"$@" || status=$?
kill -KILL "$watcher" 2>/dev/null || true
wait "$watcher" 2>/dev/null || true
trap - EXIT
cd "$work"
if [ "$status" -eq 0 ]; then here=; fi
collect "$here" || echo 'runner storage: workspace cleanup failed' >&2
exit "$status"
