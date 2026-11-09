#!/bin/sh
# Install each new release the instance stores for itself (uploaded by
# release-builder.sh): download, verify, switch atomically with deploy.sh.
# A release that fails its health check is rolled back and never retried.
# Creating $FY_HOME/hold pauses updates.
set -eu
: "${FY_HOME:?}" "${FY_URL:?}"
repo=${FY_SELF_REPO:-fortyard}
tok=$(cat "$FY_HOME/update-token")
mkdir -p "$FY_HOME/staging"
while :; do
    if [ ! -e "$FY_HOME/hold" ] && line=$(curl -fsS -m 5 -H "Authorization: Bearer $tok" \
        "$FY_URL/api/v1/repos/$repo/releases?format=text" | awk '$3 == "ready" { print; exit }') &&
        [ -n "$line" ]; then
        set -- $line
        version=$1 sha=$4
        current=$(sed -n 's/^version=//p' "$FY_HOME/current/RELEASE" 2>/dev/null || true)
        if [ "$version" != "$current" ] && [ ! -e "$FY_HOME/failed-$version" ]; then
            stage=$FY_HOME/staging/$version
            rm -rf "$stage" "$stage.tar"
            mkdir -p "$stage"
            if curl -fsS -m 120 -H "Authorization: Bearer $tok" -o "$stage.tar" \
                "$FY_URL/api/v1/repos/$repo/releases/$version/release.tar" &&
                echo "$sha  $stage.tar" | sha256sum -c --quiet - &&
                tar -xf "$stage.tar" -C "$stage" &&
                "$FY_HOME/bin/deploy.sh" "$stage"; then
                # Take the scripts of a healthy release; mv, as this one is running.
                for f in "$FY_HOME/current/scripts/"*.sh; do
                    cp "$f" "$FY_HOME/bin/.new" && mv -f "$FY_HOME/bin/.new" "$FY_HOME/bin/$(basename "$f")"
                done
            else
                : > "$FY_HOME/failed-$version"
            fi
            rm -rf "$stage" "$stage.tar"
        fi
    fi
    sleep 3
done
