# The Fortran toolchain of the production host's system (Debian 13): gfortran,
# fpm and fo, plus the tools landing checks of C/C++ and CMake projects use
# (bash, time, bzip2, xz, ccache, cmake, ninja, python3). The release builder compiles fy in it, and landing checks run in
# it with limits, discarded after each run. The server itself runs natively.
FROM debian:trixie
ARG FPM_VERSION=0.13.0
ARG FPM_SHA256
ARG FO_REV
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl git gfortran gcc g++ make libsqlite3-dev \
        bash time bzip2 xz-utils ccache cmake ninja-build python3 \
    && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL -o /usr/local/bin/fpm \
        "https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64-gcc-12" \
    && echo "${FPM_SHA256}  /usr/local/bin/fpm" | sha256sum -c - \
    && chmod 755 /usr/local/bin/fpm
RUN git clone --quiet https://github.com/lazy-fortran/fo.git /opt/fo-src \
    && git -C /opt/fo-src checkout --quiet "${FO_REV}" \
    && cd /opt/fo-src && fpm install --profile release --prefix /usr/local >/dev/null \
    && rm -rf /opt/fo-src
ENV HOME=/tmp FO_CACHE_DIR=/cache/fo
