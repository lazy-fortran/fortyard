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
      releases kept, edge timer following `main`.
- [x] Tests: request parsing, token store, socket round trip through the server.
- [ ] Concurrency: the accept loop serves one connection at a time; long
      requests need a worker pool before CI log streaming (M2).
- [ ] OIDC login against Keycloak (device flow for `fy login`), first against a
      local Keycloak container; registering the real client waits for approval.
- [ ] Account deactivation and token revocation when Keycloak disables a user.
- [ ] restic backup of the database and repositories, and the scheduled restore
      test; backup target to be decided.
- [ ] Runtime bounds checks kept on in release builds of the request parser.
- [ ] Server stays up after a Fortran runtime error in one request (today
      systemd restarts it within a second).

### M2 CI overlay

- [ ] Mirror fx, fo and fortyard from GitHub by polling.
- [ ] `fy runner` with leases, heartbeats, log streaming and per-case cgroups.
- [ ] Runs through fo on new heads; results per case with input hash, CPU time
      and peak memory; commit statuses posted to GitHub.
- [ ] GitHub stub for tests; GitHub App or fine-grained token decided.

### M3 fortyard primary for itself

- [ ] Git smart HTTP and SSH through `git http-backend` and `git-shell`.
- [ ] Size limit and repository checks on receive; same checks in `fy push`.
- [ ] Push-mirror to GitHub; the edge channel then builds from fortyard itself.

### M4 HTML views

- [ ] Repositories, commits, diffs, CI logs, test history.

**Inspection gate:** the instance is reviewed before anything is hosted
elsewhere.

## Stage B: hosted instance

- M5 Shared build cache and the fo client.
- M6 Coverage per commit and on changed lines.
- M7 Native pull requests, stacks, reviews, merge queue, failover; fx and fo
  become primary.
- M8 Agent harness: review, then implement, address, watch, fix-regression.
- M9 Issues with one-way import from GitHub.
- M10 WebAssembly application.
- M11 MILCA and further code repositories move to fortyard.

## Work in fo

Tracked in fo; needed from M2 on (see
[Testing at scale](doc/testing-at-scale.md#what-fo-needs)):

1. `fo test --plan --json` without building.
2. Running a given set of cases from a plan, results with digests.
3. Per-case cgroups, peak memory, bytes written, memory admission.
4. `fo-metric` lines in reports.
5. Remote lookup of trusted passes by key.

## LFortran and WebAssembly

The web application (M10) needs LFortran's WebAssembly target. Nothing before
M10 depends on it. Shared modules are checked with LFortran when M4 starts, and
blocking LFortran defects are fixed in the lazy-fortran fork of LFortran.
