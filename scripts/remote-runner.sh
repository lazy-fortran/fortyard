#!/bin/sh
# A runner on another host than the server: runs the fy of exactly the
# release the server runs, as `fy runner` in remote mode, and fetches the
# next one when the server moves on (fy runner then returns).
#   FY_TARGET  the instance, e.g. https://code.tugraz.at/yard
#   FY_RUNNER  runner directory with `token` (runner scope)
set -eu
: "${FY_TARGET:?}" "${FY_RUNNER:?}"
home=$FY_RUNNER
tok=$(cat "$home/token")
mkdir -p "$home/releases" "$home/data"
while :; do
    commit=$(curl -fsS -m 10 "$FY_TARGET/api/v1/version" |
        sed -n 's/.*"commit":"\([0-9a-f]*\)".*/\1/p' || true)
    line=$(curl -fsS -m 10 -H "Authorization: Bearer $tok" \
        "$FY_TARGET/api/v1/repos/fortyard/releases?format=text" |
        awk -v c="$commit" 'c != "" && index($2, c) == 1 && $3 == "ready" { print; exit }' || true)
    if [ -n "$line" ]; then
        set -- $line
        version=$1 sha=$4
        dir=$home/releases/$version
        if [ ! -x "$dir/fy" ]; then
            rm -rf "$dir" "$dir.tar"
            mkdir -p "$dir"
            if curl -fsS -m 300 -H "Authorization: Bearer $tok" -o "$dir.tar" \
                "$FY_TARGET/api/v1/repos/fortyard/releases/$version/release.tar" &&
                echo "$sha  $dir.tar" | sha256sum -c --quiet - &&
                tar -xf "$dir.tar" -C "$dir"; then
                rm -f "$dir.tar"
                # The runner updates itself: a release with a different
                # start script replaces this one, which restarts as it.
                if [ -f "$dir/scripts/remote-runner.sh" ] &&
                    ! cmp -s "$dir/scripts/remote-runner.sh" "$0"; then
                    cp "$dir/scripts/remote-runner.sh" "$0.new" && chmod 755 "$0.new" &&
                        mv -f "$0.new" "$0" && exec "$0"
                fi
                # Older releases are not needed any more.
                for old in "$home/releases"/*; do
                    [ "$old" = "$dir" ] || rm -rf "$old"
                done
            else
                rm -rf "$dir" "$dir.tar"
            fi
        fi
        if [ -x "$dir/fy" ]; then
            "$dir/fy" runner --server "$FY_TARGET" --token-file "$home/token" \
                --data "$home/data" || true
        fi
    fi
    sleep 10
done
