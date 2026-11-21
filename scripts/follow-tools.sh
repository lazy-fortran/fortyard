#!/bin/sh
# Keep fo and fy at main: when main of lazy-fortran/fo or lazy-fortran/fortyard
# moved, build it here, check that the binary runs and replace the installed
# one atomically. A commit that fails is not tried again; the installed binary
# stays. Run by a timer and, in the background, by fo and fy themselves.
#
# FY_TOOLS_HOME  state and source clones (default ~/.cache/fy-tools)
# FY_TOOLS_BIN   install directory (default: where fo is now, else ~/.local/bin)
# FY_TOOLS       the tools to follow (default "fo fy"); fortyard is private on
#                GitHub, so hosts without access follow fo only
set -eu
home=${FY_TOOLS_HOME:-$HOME/.cache/fy-tools}
mkdir -p "$home"
# flock frees the lock with its holder, however that ends. Without it
# (macOS) mkdir is the atomic step, and a lock older than an hour is left by
# a killed run.
if command -v flock >/dev/null 2>&1; then
    exec 9>"$home/lock.flock"
    flock -n 9 || exit 0
else
    lock=$home/lock
    if ! mkdir "$lock" 2>/dev/null; then
        [ -n "$(find "$lock" -maxdepth 0 -mmin +60 2>/dev/null)" ] || exit 0
        rm -rf "$lock"
        mkdir "$lock" 2>/dev/null || exit 0
    fi
    trap 'rm -rf "$lock"' EXIT INT TERM
fi
: > "$home/checked"

# Clones and build trees stay on local disk: on a network home (AFS, NFS)
# src links to a directory in /var/tmp, recreated when that was cleared.
if [ "$(uname -s)" = Linux ]; then
    case $(stat -f -c %T "$home" 2>/dev/null) in
    afs | nfs* | cifs | smb* | fuse*)
        local_src=/var/tmp/$(id -un)-fy-tools-src
        mkdir -p "$local_src"
        if [ "$(readlink "$home/src" 2>/dev/null)" != "$local_src" ]; then
            rm -rf "$home/src"
            ln -s "$local_src" "$home/src"
        fi
        ;;
    esac
fi

bin=${FY_TOOLS_BIN:-}
if [ -z "$bin" ]; then
    current=$(command -v fo 2>/dev/null || true)
    if [ -n "$current" ] && [ -w "$(dirname "$current")" ]; then
        bin=$(dirname "$current")
    else
        bin=$HOME/.local/bin
    fi
fi
mkdir -p "$bin"

log=$home/follow.log
follow() { # repository, tool
    repo=$1 tool=$2 src=$home/src/$1
    if [ ! -d "$src/.git" ]; then
        rm -rf "$src"
        git clone -q "https://github.com/lazy-fortran/$repo.git" "$src" || return 1
    fi
    git -C "$src" fetch -q origin main || return 1
    head=$(git -C "$src" rev-parse FETCH_HEAD)
    [ "$head" = "$(cat "$home/$tool.commit" 2>/dev/null)" ] && [ -x "$bin/$tool" ] && return 0
    [ -e "$home/failed-$tool-$head" ] && return 0
    git -C "$src" checkout -q --force --detach "$head"
    git -C "$src" clean -q -fdx -e build
    echo "$(date) $tool $head: building" >> "$log"
    if command -v fo >/dev/null 2>&1; then
        (cd "$src" && fo build) >> "$log" 2>&1 || { : > "$home/failed-$tool-$head"; return 1; }
        built=$src/build/fo/app/$tool
    else
        (cd "$src" && fpm build) >> "$log" 2>&1 || { : > "$home/failed-$tool-$head"; return 1; }
        built=$(ls -t "$src"/build/*/app/"$tool" 2>/dev/null | head -1)
    fi
    if [ ! -x "$built" ] || ! "$built" --version >> "$log" 2>&1; then
        : > "$home/failed-$tool-$head"
        echo "$(date) $tool $head: the built binary does not run" >> "$log"
        return 1
    fi
    cp "$built" "$bin/.$tool.new.$$" && mv -f "$bin/.$tool.new.$$" "$bin/$tool"
    echo "$head" > "$home/$tool.commit"
    echo "$(date) $tool $head: installed in $bin" >> "$log"
}
status=0
for tool in ${FY_TOOLS:-fo fy}; do
    case $tool in
    fo) follow fo fo || status=1 ;;
    fy) follow fortyard fy || status=1 ;;
    esac
done
exit $status
