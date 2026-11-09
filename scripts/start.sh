#!/bin/sh
# Entry point of the fortyard container: installs new releases in the
# background and runs the server, which follows the `current` link.
set -eu
: "${FY_HOME:?}"
"$FY_HOME/bin/self-update.sh" &
exec "$FY_HOME/current/fy" serve --data "$FY_HOME/data" --listen 0.0.0.0:8740 \
    --follow "$FY_HOME/current"
