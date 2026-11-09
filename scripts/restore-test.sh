#!/bin/sh
# Restore test: start fortyard on a copy of a file backup and check that it
# answers with its repositories and landings.
#   restore-test.sh BACKUP_DIR IMAGE
# BACKUP_DIR is the backed-up project directory (with data/fortyard-snapshot.db
# and data/repos); IMAGE the toolchain image the server runs in.
set -eu
backup=$1 image=$2
scratch=$(mktemp -d "${TMPDIR:-/var/tmp}/fy-restore.XXXXXX")
trap 'docker rm -f fy-restore >/dev/null 2>&1 || true; rm -rf "$scratch"' EXIT
mkdir -p "$scratch/data"
cp "$backup/data/fortyard-snapshot.db" "$scratch/data/fortyard.db"
cp -r "$backup/data/repos" "$scratch/data/repos"
cp -r "$backup/$(readlink "$backup/current")" "$scratch/fy-release"
docker run -d --name fy-restore --user "$(id -u):$(id -g)" -v "$scratch:/fy" "$image" \
    /fy/fy-release/fy serve --data /fy/data --listen 127.0.0.1:8740 --workers 1 >/dev/null
tok=$(cat "$backup/update-token")
i=0
until docker exec fy-restore curl -fsS -m 2 http://127.0.0.1:8740/healthz >/dev/null 2>&1; do
    i=$((i + 1)); [ $i -lt 50 ] || { echo "restore test: the server did not start" >&2; exit 1; }
    sleep 0.2
done
repos=$(docker exec fy-restore curl -fsS -H "Authorization: Bearer $tok" http://127.0.0.1:8740/api/v1/repos)
jobs=$(docker exec fy-restore curl -fsS -H "Authorization: Bearer $tok" http://127.0.0.1:8740/api/v1/jobs)
for name in fortyard fortui; do
    echo "$repos" | grep -q "\"name\":\"$name\"" || { echo "restore test: $name missing" >&2; exit 1; }
done
echo "$jobs" | grep -q '"kind":"landing"' || { echo "restore test: no landings" >&2; exit 1; }
echo "restore test passed: $(echo "$jobs" | grep -o '"kind":"landing"' | wc -l) landings, repositories present"
