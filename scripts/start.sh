#!/bin/sh
# Entry point of the fortyard containers. Without argument: installs new
# releases in the background and runs the server, which follows the `current`
# link. With "runner": performs landing checks; it exits when a new release is
# installed, and Docker restarts it on that release.
set -eu
: "${FY_HOME:?}"
if [ "${1:-}" = runner ]; then
    exec "$FY_HOME/current/fy" runner --server http://127.0.0.1:8740 \
        --token-file "$FY_HOME/runner-token" --data "$FY_HOME/data" --follow "$FY_HOME/current" \
        ${FY_EXECUTOR:+--executor "$FY_EXECUTOR"}
fi
"$FY_HOME/bin/self-update.sh" &
exec "$FY_HOME/current/fy" serve --data "$FY_HOME/data" --listen 0.0.0.0:8740 \
    --follow "$FY_HOME/current"
