#!/bin/sh
# Build every new commit of an instance's own repository and upload it there
# as a release, which the instance then installs itself (self-update.sh).
#   FY_TARGET  the instance, e.g. https://code.tugraz.at/yard
#   FY_REPO    the repository to follow (default fortyard)
#   FY_BUILDER builder directory with `token` (scopes release and read access)
#   FY_TOOLCHAIN the toolchain image of the instance's system
#   FY_PROXY   optional SOCKS proxy (socks5h://HOST:PORT) to reach FY_TARGET
#   FY_WEB_IMAGE optional image with LFortran and emscripten
#              (deploy/container/Dockerfile.web); without it the browser files
#              are built with the host's tools. When the commit pins LFortran
#              in web/lfortran.rev, the image of that LFortran is used instead
#              and built here when missing (fortyard-web:lf-<12 hex digits>)
#   FY_BUILD_LIMITS optional docker run limits for the build containers,
#              e.g. "--cpus 8 --memory 12g"
# fy is compiled in the toolchain image, incrementally (its build tree and fo's
# cache persist); the browser files are built here and reused while the code
# they come from is unchanged.
set -eu
: "${FY_TARGET:?}" "${FY_BUILDER:?}" "${FY_TOOLCHAIN:?}"
repo=${FY_REPO:-fortyard}
if [ -n "${FY_PROXY:-}" ]; then
    export ALL_PROXY="$FY_PROXY" https_proxy="$FY_PROXY" http_proxy="$FY_PROXY"
fi
home=$FY_BUILDER
limits=${FY_BUILD_LIMITS:-}
user="$(id -u):$(id -g)"
tok=$(cat "$home/token")
auth="Authorization: Bearer $tok"
api="$FY_TARGET/api/v1/repos/$repo/releases"
mirror=$home/src.git
mkdir -p "$home/cache/home" "$home/out"
[ -d "$mirror" ] || git -c http.extraHeader="$auth" clone --quiet --bare \
    "$FY_TARGET/$repo.git" "$mirror"
for tree in fy web; do
    [ -d "$home/$tree" ] || git clone --quiet --shared "$mirror" "$home/$tree"
done

build() {
    head=$1 version=$2
    out=$home/out/$version
    rm -rf "$out"
    mkdir -p "$out/scripts"
    short=$(printf %.12s "$head")
    built=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    git -C "$home/fy" fetch --quiet origin main
    git -C "$home/fy" checkout --quiet --force "$head"
    cat > "$home/fy/src/fy_build_info.f90" <<INFO
module fy_build_info
    implicit none
    character(len=*), parameter :: FY_VERSION = '$version'
    character(len=*), parameter :: FY_COMMIT = '$short'
    character(len=*), parameter :: FY_BUILT = '$built'
end module fy_build_info
INFO
    # The tree borrows objects from the mirror, so the mirror is mounted at its
    # path. --init: fo cannot start compilers as process 1 of a container.
    # shellcheck disable=SC2086 # limits are separate words
    docker run --rm --init $limits --user "$user" -v "$home/fy:/work" -v "$home/cache:/cache" \
        -v "$mirror:$mirror:ro" \
        -w /work "$FY_TOOLCHAIN" fo install --prefix /work/build/prefix >"$out.log" 2>&1 || {
        tail -30 "$out.log" >&2
        return 1
    }
    git -C "$home/fy" checkout --quiet -- src/fy_build_info.f90
    cp "$home/fy/build/prefix/bin/fy" "$out/fy"
    # The browser files change only with the code they are built from.
    last=$(cat "$home/web.commit" 2>/dev/null || true)
    if [ -z "$last" ] || [ ! -d "$home/web.out" ] || ! git -C "$mirror" diff --quiet "$last" "$head" \
        -- src/ui web fpm.toml scripts/build-web.sh; then
        git -C "$home/web" fetch --quiet origin main
        git -C "$home/web" checkout --quiet --force "$head"
        rm -rf "$home/web.new"
        web_image=${FY_WEB_IMAGE:-}
        rev=$(git -C "$mirror" show "$head:web/lfortran.rev" 2>/dev/null | head -c 40 || true)
        if [ -n "$web_image" ] && [ ${#rev} -eq 40 ]; then
            web_image=fortyard-web:lf-$(printf %.12s "$rev")
            if ! docker image inspect "$web_image" >/dev/null 2>&1; then
                ctx=$home/image-web
                rm -rf "$ctx" && mkdir -p "$ctx"
                git -C "$mirror" show "$head:deploy/container/Dockerfile.web" > "$ctx/Dockerfile"
                docker build --quiet -t "$web_image" --build-arg LFORTRAN_REV="$rev" \
                    --build-arg JOBS=8 "$ctx" >"$out.image.log" 2>&1 || {
                    tail -30 "$out.image.log" >&2
                    return 1
                }
            fi
        fi
        if [ -n "$web_image" ]; then
            # fo build fetches fx and fortui at the pinned versions; the
            # builder directory has the same path inside the containers.
            # shellcheck disable=SC2086
            docker run --rm --init $limits --user "$user" -v "$home:$home" -v "$home/cache:/cache" \
                -w "$home/web" "$FY_TOOLCHAIN" fo build >/dev/null 2>&1 || true
            # shellcheck disable=SC2086
            docker run --rm --init $limits --user "$user" -v "$home:$home" -e HOME="$home/cache/home" \
                -w "$home/web" "$web_image" scripts/build-web.sh "$home/web" "$home/web.new" \
                >"$out.web.log" 2>&1
        else
            (cd "$home/web" && fo build >/dev/null 2>&1) || true
            "$home/web/scripts/build-web.sh" "$home/web" "$home/web.new" >"$out.web.log" 2>&1
        fi || {
            tail -30 "$out.web.log" >&2
            return 1
        }
        rm -rf "$home/web.out"
        mv "$home/web.new" "$home/web.out"
        echo "$head" > "$home/web.commit"
    fi
    cp -r "$home/web.out" "$out/web"
    for f in release.sh deploy.sh edge-update.sh self-update.sh start.sh \
        release-builder.sh remote-runner.sh; do
        git -C "$mirror" show "$head:scripts/$f" > "$out/scripts/$f" 2>/dev/null && chmod 755 "$out/scripts/$f"
    done
    (cd "$out" && sha256sum fy > fy.sha256 && sha256sum web/* > web.sha256)
    printf 'version=%s\ncommit=%s\nbuilt=%s\n' "$version" "$head" "$built" > "$out/RELEASE"
    tar -cf "$out.tar" -C "$out" .
    sha=$(sha256sum "$out.tar" | cut -c1-64)
    notes=$(git -C "$mirror" log -1 --format=%s "$head" | sed 's/[\\"]/\\&/g')
    curl -fsS -m 30 -H "$auth" -H 'Content-Type: application/json' -d \
        "{\"version\":\"$version\",\"commit\":\"$head\",\"notes\":\"$notes\"}" "$api" >/dev/null
    curl -fsS -m 300 -H "$auth" -H "X-Content-SHA256: $sha" -T "$out.tar" \
        "$api/$version/release.tar" >/dev/null
    # The builder updates itself: a release with a different builder script
    # replaces this one, which then restarts as the new version.
    if [ -f "$out/scripts/release-builder.sh" ] &&
        ! cmp -s "$out/scripts/release-builder.sh" "$0"; then
        cp "$out/scripts/release-builder.sh" "$0.new" && chmod 755 "$0.new" &&
            mv -f "$0.new" "$0" && restart=1
    fi
    rm -rf "$out" "$out.tar" "$out.log" "$out.web.log" "$out.image.log"
    echo "released $version ($short)"
}

restart=0

while :; do
    if git -C "$mirror" -c http.extraHeader="$auth" fetch --quiet origin \
        '+refs/heads/main:refs/heads/main' 2>/dev/null; then
        head=$(git -C "$mirror" rev-parse main)
        list=$(curl -fsS -m 10 -H "$auth" "$api?format=text" || true)
        if [ -n "$head" ] && ! printf '%s\n' "$list" | grep -q " $head ready " &&
            [ ! -e "$home/failed-$head" ]; then
            day=$(date -u +%Y.%m.%d)
            n=$(printf '%s\n' "$list" | grep -c "^$day\." || true)
            build "$head" "$day.$n" || : > "$home/failed-$head"
            [ "$restart" = 1 ] && exec "$0"
        fi
    fi
    sleep 3
done
