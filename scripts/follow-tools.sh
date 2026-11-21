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
# fo before lazy-fortran/fo#436 writes its 10 s progress line into the output it
# parses (the archive member list), which fails a build on a slow host.
export FO_HEARTBEAT_SECONDS=0
home=${FY_TOOLS_HOME:-$HOME/.cache/fy-tools}
mkdir -p "$home"
# A network home (AFS, NFS) takes no flock and builds slowly: the lock and
# the clones and build trees then live on local disk.
local_dir=
if [ "$(uname -s)" = Linux ]; then
    case $(stat -f -c %T "$home" 2>/dev/null) in
    afs | nfs* | cifs | smb* | fuse*) local_dir=/var/tmp/$(id -un)-fy-tools ;;
    esac
fi
lock_dir=${local_dir:-$home}
mkdir -p "$lock_dir"
# flock frees the lock with its holder, however that ends. Without it
# (macOS) mkdir is the atomic step, and a lock older than an hour is left by
# a killed run.
if command -v flock >/dev/null 2>&1; then
    exec 9>"$lock_dir/lock.flock"
    flock -n 9 || exit 0
else
    lock=$lock_dir/lock
    if ! mkdir "$lock" 2>/dev/null; then
        [ -n "$(find "$lock" -maxdepth 0 -mmin +60 2>/dev/null)" ] || exit 0
        rm -rf "$lock"
        mkdir "$lock" 2>/dev/null || exit 0
    fi
    trap 'rm -rf "$lock"' EXIT INT TERM
fi
: > "$home/checked"

# On a network home src links to local disk (recreated when /var/tmp was
# cleared); the paths fo and fy look at stay the same.
if [ -n "$local_dir" ]; then
    local_src=$local_dir/src
    mkdir -p "$local_src"
    if [ "$(readlink "$home/src" 2>/dev/null)" != "$local_src" ]; then
        # AFS keeps files in use; move the old tree aside and remove what it
        # lets go.
        if [ -e "$home/src" ] || [ -L "$home/src" ]; then
            mv "$home/src" "$home/src.old.$$"
            rm -rf "$home/src.old.$$" 2>/dev/null || true
        fi
        ln -s "$local_src" "$home/src"
    fi
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
    # A commit that failed to build is tried again after a day, in case the
    # host was at fault.
    [ -n "$(find "$home/failed-$tool-$head" -mmin -1440 2>/dev/null)" ] && return 0
    git -C "$src" checkout -q --force --detach "$head"
    git -C "$src" clean -q -fdx -e build
    echo "$(date) $tool $head: building" >> "$log"
    builder="fo build"
    command -v fo >/dev/null 2>&1 || builder="fpm build"
    status=0
    (cd "$src" && $builder) >> "$log" 2>&1 || status=$?
    # A library installed without its development link (libX.so.N but no
    # libX.so, as without root on faepop35): link it privately, build again.
    if [ "$status" -ne 0 ] && [ "$status" -le 128 ]; then
        missing=$(tail -n 50 "$log" | sed -n 's/.*cannot find -l\([A-Za-z0-9_+.-]*\).*/\1/p' | sort -u)
        linked=
        for lib in $missing; do
            for so in /usr/lib/x86_64-linux-gnu/lib$lib.so.* /usr/lib64/lib$lib.so.* /usr/lib/lib$lib.so.*; do
                [ -e "$so" ] || continue
                mkdir -p "$home/lib"
                ln -sf "$so" "$home/lib/lib$lib.so"
                linked=yes
                break
            done
        done
        if [ -n "$linked" ]; then
            status=0
            (cd "$src" && LIBRARY_PATH="$home/lib${LIBRARY_PATH:+:$LIBRARY_PATH}" $builder) \
                >> "$log" 2>&1 || status=$?
        fi
    fi
    # A build stopped by a signal is not a failure of the commit.
    [ "$status" -gt 128 ] && return 1
    if [ "$status" -ne 0 ]; then
        : > "$home/failed-$tool-$head"
        return 1
    fi
    if [ "$builder" = "fo build" ]; then
        built=$src/build/fo/app/$tool
    else
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
result=0
for tool in ${FY_TOOLS:-fo fy}; do
    case $tool in
    fo) follow fo fo || result=1 ;;
    fy) follow fortyard fy || result=1 ;;
    esac
done
# Timers and fo/fy run this stable copy; refresh it from the clone.
if [ -f "$home/src/fortyard/scripts/follow-tools.sh" ] &&
    ! cmp -s "$home/src/fortyard/scripts/follow-tools.sh" "$home/follow-tools.sh"; then
    cp "$home/src/fortyard/scripts/follow-tools.sh" "$home/follow-tools.new.$$" &&
        mv -f "$home/follow-tools.new.$$" "$home/follow-tools.sh"
fi
exit $result
