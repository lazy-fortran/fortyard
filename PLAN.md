# fortyard plan

Delivery order for [DESIGN.md](DESIGN.md) and
[Testing at scale](doc/testing-at-scale.md). A milestone is done when its
behaviour is covered by tests that run with `fo test` and it runs on the
development instance.

## Development instance

One long-running instance inside the WireGuard network follows `main`: a
systemd timer fetches `main` every minute, builds a release of every new
commit once, switches to it atomically and rolls back if the health check
fails (`scripts/edge-update.sh`, `scripts/deploy.sh`). Its page shows the
running version and commit. It is installed with

```sh
scripts/install-instance.sh FY_HOME LISTEN_ADDR:PORT UPSTREAM_GIT_URL
```

For work on a branch, `scripts/dev-up.sh` starts a throwaway instance from the
working tree on the loopback address and `scripts/dev-down.sh` stops it.

## Stage A: MVP inside the WireGuard network

### M1 Skeleton (in progress)

- [x] HTTP/1.1 server in Fortran over POSIX sockets, one request per connection,
      bounded header and body sizes, socket timeouts.
- [x] systemd socket activation, so restarts do not drop requests.
- [x] SQLite store with forward-only migrations; accounts and scoped tokens
      stored as SHA-256 hashes; expiry and revocation.
- [x] `fy serve --init` prints the bootstrap administrator token once.
- [x] API: `/healthz`, `/api/v1/version`, `/api/v1/whoami`,
      `POST /api/v1/tokens`; status page at `/`.
- [x] `fy login`, `fy whoami`, `fy token create`, `fy version`; credentials
      stored with mode 600.
- [x] `fy mcp` with `whoami` and `version`.
- [x] Release pipeline: one build per commit with version `YYYY.MM.DD.N` and
      checksum, atomic switch, health check, automatic rollback, last ten
      releases kept, edge timer following `main`. Rollback verified with a
      release whose server exits at once.
- [x] Tests: request parsing, token store, socket round trip through the server.
- [x] Concurrency: a supervisor forks worker processes that share the
      listening socket; inside each, OpenMP threads run their own accept loops
      with their own database connections (`--workers P --threads T`).
- [ ] OIDC login against Keycloak (device flow for `fy login`), first against a
      local Keycloak container; registering the real client waits for approval.
- [ ] Account deactivation and token revocation when Keycloak disables a user.
- [ ] restic backup of the database and repositories, and the scheduled restore
      test; backup target to be decided.
- [ ] Runtime bounds checks kept on in release builds of the request parser.
- [x] A worker that dies, for example on a Fortran runtime error, loses only
      its own requests; the supervisor replaces it.

### M2 Web application, first screens

The browser application is Fortran compiled to WebAssembly with LFortran and
drawn with [fortui](https://github.com/lazy-fortran/fortui). fortyard is the
first consumer of fortui's browser backend, which is built in fortui itself.

- [x] fortui `web` backend: ordinary controls as DOM elements, a frame loop
      that yields to the browser, a small JavaScript host and the emscripten
      loader as the only JavaScript.
- [x] fx JSON modules compiled with gfortran (server) and LFortran (browser).
- [x] `fy serve` serves the application and its WebAssembly module at `/`;
      the server page moves to `/status`.
- [x] Browser sign-in: a token is exchanged for an HttpOnly, SameSite=Strict
      session cookie; requests that change state with it must be JSON.
- [x] Screens: server status, account, tokens (list, create, revoke), checked
      end to end in headless Chromium (`test/web_app_test.mjs`).
- [x] Pages show before the WebAssembly loads: the server composes the first
      frame with the application's own Fortran code (fortui headless and
      `fortui_html`) and embeds the data it used; the browser adopts that page.
      Assets have content-hashed names and are cached for good; the browser
      lays out the page natively (fortui DOM layout, no Asyncify).
- [ ] Visual design of the application.
- [ ] Virtualized views of long files and diffs.
- [ ] Canvas surface in fortui for diffs and plots (needed from M5 on).

### M3 CI overlay

- [x] Mirror repositories from GitHub: a supervised mirror process clones new
      ones and fetches every minute (`fy repo add NAME URL`, admin only).
- [ ] `fy runner` with leases, heartbeats, log streaming and per-case cgroups.
- [ ] Runs through fo on new heads; results per case with input hash, CPU time
      and peak memory; commit statuses posted to GitHub.
- [ ] GitHub stub for tests; GitHub App or fine-grained token decided.

### M4 fortyard primary for itself

- [ ] Git smart HTTP and SSH through `git http-backend` and `git-shell`.
- [ ] Size limit and repository checks on receive; same checks in `fy push`.
- [ ] Push-mirror to GitHub; the edge channel then builds from fortyard itself.

### M5 Web views of repositories

- [x] Repository list, file browser, history and commits with coloured diffs
      in the application, from a read-only API over the mirrors.
- [ ] CI logs, test history; browser URLs and back button for these pages;
      server-rendered HTML as permalinks and a fallback without JavaScript.

**Inspection gate:** the instance is reviewed before anything is hosted
elsewhere.

## Stage B: hosted instance

- M6 Shared build cache and the fo client.
- M7 Coverage per commit and on changed lines.
- M8 Native pull requests, stacks, reviews, merge queue, failover; fx and fo
  become primary.
- M9 Agent harness: review, then implement, address, watch, fix-regression.
- M10 Issues with one-way import from GitHub.
- M11 MILCA and further code repositories move to fortyard.

## Work in fo

Tracked in fo; needed from M3 on (see
[Testing at scale](doc/testing-at-scale.md#what-fo-needs)):

1. `fo test --plan --json` without building.
2. Running a given set of cases from a plan, results with digests.
3. Per-case cgroups, peak memory, bytes written, memory admission.
4. `fo-metric` lines in reports.
5. Remote lookup of trusted passes by key.

## LFortran and WebAssembly

The web application needs LFortran's WebAssembly target (Flang cannot target
WebAssembly). fortui's sources already compile with it and its headless tests
pass in Node and Chromium. Defects that block fortyard or fortui are fixed in
the lazy-fortran fork of LFortran. OpenMP stays in the server; modules shared
with the browser do not use it.
