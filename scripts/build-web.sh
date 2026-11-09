#!/bin/sh
# Builds the browser application into OUTDIR (index.html, fy_app.js,
# fy_app.wasm): build-web.sh SRCDIR OUTDIR
# Needs LFortran and an emscripten SDK in EMSDK_PATH, and fx under
# SRCDIR/build/dependencies (fo build fetches it). fortui comes from FORTUI_DIR,
# or is checked out at the commit in web/fortui.rev.
set -eu
src=$(cd "$1" && pwd)
out=$2
fx=$src/build/dependencies/fx/src
if [ -z "${FORTUI_DIR:-}" ]; then
    rev=$(cat "$src/web/fortui.rev")
    FORTUI_DIR=${FY_CACHE:-$HOME/.cache/fortyard}/fortui
    [ -d "$FORTUI_DIR/.git" ] || git clone -q git@github.com:lazy-fortran/fortui.git "$FORTUI_DIR"
    git -C "$FORTUI_DIR" cat-file -e "$rev^{commit}" 2>/dev/null || git -C "$FORTUI_DIR" fetch -q origin
    git -C "$FORTUI_DIR" checkout -q --detach "$rev"
fi
mkdir -p "$out"
FORTUI_WEB_SOURCES="$fx/fx_string.f90 $fx/fx_json_build.f90 $fx/fx_json_parse.f90 $src/web/fy_fetch.f90 $src/web/fy_repo_view.f90" \
FORTUI_WEB_JS="$src/web/fy_web.js" \
    "$FORTUI_DIR/web/build.sh" "$src/web/fy_app.f90" "$out" >/dev/null
rm -rf "$out/obj"
# Served to browsers that accept gzip; about a third of the size.
gzip -9 -k -f "$out/fy_app.wasm" "$out/fy_app.js"
ls "$out"
