#!/bin/sh
# Entry point of the egress proxy container (environment as in render.sh;
# FY_EGRESS_LISTS replaces the lists built into the image). unlisted.log is
# rotated daily and kept for a week; standard output is bounded by Docker.
set -eu
: "${FY_EGRESS_ROLE:?FY_EGRESS_ROLE names the role}"
lib=/usr/local/lib/fy-egress
"$lib/render.sh" "$FY_EGRESS_ROLE" "${FY_EGRESS_LISTS:-$lib}" > /tmp/squid.conf
(while sleep "${FY_EGRESS_ROTATE:-86400}"; do squid -k rotate -f /tmp/squid.conf || :; done) &
exec squid -N -f /tmp/squid.conf
