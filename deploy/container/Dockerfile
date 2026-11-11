# The Fortran toolchain of the production host's system (Debian 13): gfortran,
# fpm and fo, plus the tools landing checks of C/C++ and CMake projects use
# (bash, time, bzip2, xz, ccache, cmake, ninja, python3), and the numerical
# libraries the hosted scientific codes link (OpenBLAS/LAPACK, netCDF,
# SuiteSparse, found through pkg-config). The release builder compiles fy in it, and landing checks run in
# it with limits, discarded after each run. The server itself runs natively.
FROM debian:trixie AS base
ARG FPM_VERSION=0.13.0
ARG FPM_SHA256
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl git gfortran gcc g++ make libsqlite3-dev \
        bash time bzip2 xz-utils ccache cmake ninja-build python3 \
        pkg-config libopenblas-dev liblapack-dev libnetcdf-dev libsuitesparse-dev \
    && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL -o /usr/local/bin/fpm \
        "https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64-gcc-12" \
    && echo "${FPM_SHA256}  /usr/local/bin/fpm" | sha256sum -c - \
    && chmod 755 /usr/local/bin/fpm
# fo is built outside the image, incrementally (scripts/toolchain-image.sh),
# and copied in as the last layer.
ENV HOME=/tmp FO_CACHE_DIR=/cache/fo

FROM base
COPY fo /usr/local/bin/fo
