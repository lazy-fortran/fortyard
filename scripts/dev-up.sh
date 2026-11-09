#!/bin/sh
# Throwaway local instance from the working tree: dev-up.sh [PORT]
# Prints the URL and token; stop it with dev-down.sh.
set -eu
port=${1:-8741}
root=$(cd "$(dirname "$0")/.." && pwd)
dev=${FY_DEV_DIR:-$root/build/dev}
(cd "$root" && fo build >/dev/null)
fy="$root/build/fo/bin/fy"
if [ ! -e "$dev/data/fortyard.db" ]; then
    mkdir -p "$dev"
    "$fy" serve --init --data "$dev/data" | tail -1 > "$dev/token"
    chmod 600 "$dev/token"
fi
"$fy" serve --data "$dev/data" --listen "127.0.0.1:$port" 2>>"$dev/server.log" &
echo $! > "$dev/pid"
echo "url   http://127.0.0.1:$port"
echo "token $(cat "$dev/token")"
