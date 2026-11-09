# fortyard for hosts without a Fortran toolchain (production on faepkub3).
# Built by scripts/image.sh, which puts the source tree and the browser
# assets (built on the build host) into the build context.
FROM debian:trixie AS build
ARG FPM_VERSION=0.13.0
ARG FPM_SHA256
ARG FO_REV
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl git gfortran gcc g++ make libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL -o /usr/local/bin/fpm \
        "https://github.com/fortran-lang/fpm/releases/download/v${FPM_VERSION}/fpm-${FPM_VERSION}-linux-x86_64-gcc-12" \
    && echo "${FPM_SHA256}  /usr/local/bin/fpm" | sha256sum -c - \
    && chmod 755 /usr/local/bin/fpm
RUN git clone --quiet https://github.com/lazy-fortran/fo.git /build/fo \
    && git -C /build/fo checkout --quiet "${FO_REV}" \
    && cd /build/fo && fpm install --profile release --prefix /opt/fo >/dev/null
ENV PATH=/opt/fo/bin:$PATH
COPY src /build/fortyard
RUN cd /build/fortyard && fo install --prefix /opt/fortyard > /build/fy.log 2>&1 \
    || (tail -40 /build/fy.log; exit 1)

FROM debian:trixie-slim
# git serves and lands repositories; gfortran and fo run landing checks.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates git openssh-client libsqlite3-0 libgfortran5 libgomp1 \
        gfortran gcc libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /opt/fo/bin/ /usr/local/bin/
COPY --from=build /opt/fortyard/bin/fy /usr/local/bin/fy
COPY web /opt/fortyard/web
# Home and fo's build cache stay out of the data directory (which is backed
# up): the cache is a regenerable volume, run directories live in /tmp.
ENV HOME=/tmp/home FO_CACHE_DIR=/cache/fo TMPDIR=/tmp
EXPOSE 8740
ENTRYPOINT ["fy", "serve", "--data", "/data/db", "--web", "/opt/fortyard/web", \
    "--listen", "0.0.0.0:8740"]
