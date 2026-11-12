# fortyard's egress proxy: squid with the configuration render.sh writes for
# the role in FY_EGRESS_ROLE at each start (doc/backends.md, "Network
# allowlist"). It runs unprivileged on a read-only root with /tmp writable.
FROM alpine:3.22
RUN apk add --no-cache squid
COPY *.txt /usr/local/lib/fy-egress/
COPY render.sh extra.sh start.sh /usr/local/lib/fy-egress/
USER squid
EXPOSE 3128
ENTRYPOINT ["/usr/local/lib/fy-egress/start.sh"]
