#!/bin/sh
# Stop the instance started by dev-up.sh; its data stays until build/dev is removed.
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
dev=${FY_DEV_DIR:-$root/build/dev}
[ -f "$dev/pid" ] && kill "$(cat "$dev/pid")" 2>/dev/null || true
rm -f "$dev/pid"
