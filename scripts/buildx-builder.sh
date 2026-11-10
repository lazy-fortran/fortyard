#!/bin/sh
# Make sure fortyard's own BuildKit instance exists and print its name. All
# fortyard image builds use it (docker buildx build --builder NAME --load), so
# they never touch, prune or fill the cache of the host's default builder.
set -eu
name=fortyard
config=$(cd "$(dirname "$0")/../deploy/container" && pwd)/buildkitd.toml
if ! docker buildx inspect "$name" >/dev/null 2>&1; then
    docker buildx create --name "$name" --driver docker-container \
        --buildkitd-config "$config" --bootstrap >/dev/null
fi
echo "$name"
