# Operating the egress proxy and the relay

How the network allowlist and the isolation of [Check backends](backends.md)
are installed and changed. Everything here is fortyard's own: its containers,
its Docker networks and its route file; nothing of Computor changes.

## What runs

`scripts/install-compose.sh` starts, besides server and local runner:

- `fortyard-egress-server` and `fortyard-egress-runner`: squid
  (`deploy/egress`), one per role, each on the internal network of its role
  and on the normal network `egress`. A container reaches only the proxy of
  its own role.
- `fortyard-relay`: socat, on the internal network `yard` and the normal
  network `edge`; it publishes the server's port 8740 on the host at the
  address given as `RELAY`.

Server and runner have no route out (`internal` networks) and no DNS server
(`dns: 127.0.0.1`): container names resolve, other names do not. Their
`http_proxy`, `https_proxy` and `FY_PROXY` name their role's proxy; git and
curl use them; the server's ssh to GitHub goes to `ssh.github.com:443` through
the proxy (`bin/ssh_config`). `scripts/install-runner.sh` gives a runner on
another host the same: an internal network and two proxies, the executor's
for the checks (`http_proxy`) and the controller's for `fy runner` and its
updates (`FY_PROXY`, which reaches `code.tugraz.at`). Their logs and the extra
file lie in `FY_EGRESS_DIR` (default `DIR-egress`, beside the runner
directory), which the runner container does not mount, so code under check
cannot change its own allowlist. Until the runner is split, controller and
code under check share one container, so a check can still name the
controller's proxy itself; the split removes that.

## Switching over an installed instance

1. Choose `RELAY`: an address of the host that Traefik's container reaches,
   normally the gateway of Traefik's Docker network
   (`docker network inspect -f '{{(index .IPAM.Config 0).Gateway}}' NETWORK`),
   and a free port, for example `172.18.0.1:8740`.
2. Run `install-compose.sh FY_HOME FIRST_RELEASE.tar BASE IMAGE RELAY
   PUBLIC_URL` with the toolchain image rebuilt from
   `deploy/container/Dockerfile` (it now holds ssh and nc).
3. Point fortyard's route file to `http://RELAY` instead of
   `http://fortyard:8740`; Traefik reloads it. The server is no longer on
   `computor-network`.

Between steps 2 and 3 the route does not reach the server, so both are done
together.

## Lists

`deploy/egress/<role>.txt`, one domain per line, a domain including its
subdomains; `packages.txt` are the executor's package hosts. A list changes
by a reviewed change and a rerun of the install script, which rebuilds the
proxy image. The order of the rules and the TU Graz networks are in
`deploy/egress/render.sh`.

A repository's own hosts (`egress=` in its policy) reach the executor's proxy
through the file `egress/extra/executor.txt` of the instance
(`FY_EGRESS_DIR/extra/executor.txt` on other hosts), which the runner controller writes for the check it
runs: one domain per line, and the line `private` for a private repository,
which then reaches only the package hosts it names. The proxy reads it on
every request; without the file a check gets the executor list with all
package hosts. Extra hosts never pass into TU Graz, private or loopback
networks. One file serves every check behind one proxy: while checks of
several repositories run at once behind it, the file can hold only one
repository's hosts, so the runner gives each check that needs extra hosts
its own executor proxy (or runs such checks one at a time).

## Learning

The executor's proxy starts in learning mode: unlisted public hosts pass.
Every request to a host on none of the role's lists, passed or refused, is
logged in `egress/runner/unlisted.log` for the local runner and
`egress/server/unlisted.log` for the server (`FY_EGRESS_DIR/executor/unlisted.log`
on a runner host). The file is rotated daily and kept for seven days
(`unlisted.log.0` is yesterday's); every request also goes to the proxy's
standard output, of which Docker keeps 30 MB. After a week, add the needed hosts to the list and rerun the install
script with `FY_EGRESS_LEARN=0`.

## Test

`deploy/egress/test.sh` (needs docker) starts the proxy on a test network
without a way out and checks its decisions.
