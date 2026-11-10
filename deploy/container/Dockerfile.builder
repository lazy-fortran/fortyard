# The release builder's own process (scripts/release-builder.sh): it follows
# the instance's repository and starts the build containers through the
# host's Docker socket, so a build host needs nothing but Docker.
FROM debian:trixie-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl git docker-cli \
    && rm -rf /var/lib/apt/lists/*
