#!/bin/sh
# Builds the browser application into OUTDIR: build-web.sh SRCDIR OUTDIR
# The WebAssembly and its loader get content-hashed names (cached for good by
# browsers), with gzip copies; index.html is the page template with those
# names and fortui's base styles filled in. The server puts the first frame
# and its data into that template for each page.
# Needs LFortran and an emscripten SDK in EMSDK_PATH, and fx and fortui under
# SRCDIR/build/dependencies (fo build fetches them at the versions fpm.toml
# pins); FORTUI_DIR overrides the fortui checkout.
set -eu
src=$(cd "$1" && pwd)
out=$2
deps=$src/build/dependencies
fx=$deps/fx/src
fortui=${FORTUI_DIR:-$deps/fortui}
mkdir -p "$out"
rm -f "$out"/fy_app.*
FORTUI_WEB_SOURCES="$fx/fx_string.f90 $fx/fx_json_build.f90 $fx/fx_json_parse.f90 \
$src/web/fy_fetch.f90 $src/src/ui/fy_highlight.f90 $src/src/ui/fy_diff.f90 \
$src/src/ui/fy_markdown.f90 $src/src/ui/fy_time.f90 $src/src/ui/fy_repo_view.f90 $src/src/ui/fy_ui_app.f90" \
FORTUI_WEB_JS="$src/web/fy_web.js" \
    "$fortui/web/build.sh" "$src/web/fy_app.f90" "$out" >/dev/null
rm -rf "$out/obj" "$out/fortui.css"
hash=$(cat "$out/fy_app.wasm" "$out/fy_app.js" | sha256sum | cut -c1-12)
mv "$out/fy_app.wasm" "$out/fy_app.$hash.wasm"
mv "$out/fy_app.js" "$out/fy_app.$hash.js"
gzip -9 -k -f "$out/fy_app.$hash.wasm" "$out/fy_app.$hash.js"
# The template with the asset names and fortui's styles (one line, so sed can
# take it); the server fills in the first frame and its data per page.
css=$(tr '\n' ' ' < "$fortui/web/fortui.css" | sed 's/[&|]/\\&/g')
sed -e "s|%WASM%|fy_app.$hash.wasm|g" -e "s|%JS%|fy_app.$hash.js|g" \
    -e "s|%FORTUI_CSS%|$css|" "$src/web/index.html" > "$out/index.html"
ls "$out"
