#!/bin/sh
# Squid external ACL helper for the hosts a repository's policy adds to its
# executor's list (egress=):
#   extra.sh domain FILE   OK for a host that is a listed domain or below one
#   extra.sh private FILE  OK when FILE has the line "private"
# FILE is rewritten by the runner controller for each check and read on every
# request; a missing file adds nothing and means a public repository.
mode=$1 file=$2
while read -r host _; do
    answer=$(awk -v h="$host" -v m="$mode" '
        BEGIN { h = tolower(h); sub(/:[0-9]+$/, "", h) }
        { sub(/#.*/, ""); gsub(/[ \t\r]/, ""); $0 = tolower($0); sub(/^\./, "") }
        m == "private" && $0 == "private" { ok = 1; exit }
        m == "domain" && /^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$/ &&
            !/^[0-9.]+$/ && (h == $0 || substr(h, length(h) - length($0)) == "." $0) { ok = 1; exit }
        END { print ok ? "OK" : "ERR" }' "$file" 2>/dev/null) || answer=ERR
    [ -n "$answer" ] || answer=ERR
    echo "$answer"
done
