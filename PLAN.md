# fortyard plan

Delivery order for [DESIGN.md](DESIGN.md),
[Testing at scale](doc/testing-at-scale.md) and [Workflow](doc/workflow.md). A milestone is done when its
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

### M4 fortyard primary for itself (next)

The instance develops itself: pushes go to fortyard, changes land through its
own queue, the edge timer deploys its own `main`, and GitHub becomes a mirror.
In order:

- [x] Git smart HTTP through `git http-backend` (clone, fetch, push) with token
      authentication, streamed in both directions; repositories are mirrors or
      primary (`fy repo create`, `fy repo mode`); pushes need the admin scope
      until roles exist. SSH later.
- [ ] Size limit and repository checks on receive; same checks in `fy push`.
- [ ] Changes, minimal: a push to a non-protected branch opens or updates a
      change (title, description, versions by change ID); `main` refuses
      direct pushes.
- [x] Landing, minimal: `main` takes no pushes; `fy land REPO BRANCH` queues a
      pushed branch, which lands one at a time as one squashed commit with
      `(#N)` and a `Change:` trailer after the repository's check (`fo test`, or
      `.fortyard/check`) passed on that commit under a memory cap.
- [ ] Batched merge queue, `Change-Id` trailers, landing from a change page.
- [x] Push of `main` to the repository's upstream after each landing.
- [x] The edge timer follows the instance's own `main`; fortyard is primary on
      its development instance and GitHub is its mirror.
- [ ] Tags to the upstream.
- [ ] `fy` and `fy mcp` grow with each step (push, change list and view, land,
      queue status).
- [ ] Audit log (append-only, hash-chained) from the first landing on.

### M4a Namespaces

- [ ] Groups with subgroups, personal namespaces, roles with downward
      inheritance, redirects after moves; existing mirrors move into a group.

### M4c Releases and production

- [x] Releases stored by the instance itself: upload with checksum, list,
      download; `release-builder.sh` builds fy incrementally in a Debian toolchain
      image and the browser files only when their sources changed;
      `self-update.sh` installs each new release with the atomic switch.
- [ ] Production at code.tugraz.at/yard (faepkub3, user service, Traefik route
      file); fortyard and fortui primary there; the development instance
      becomes staging.
- [ ] Per-repository deploy keys for pushing landings to GitHub.
- [ ] Release builder independent of any developer machine: a container on a
      build host inside the instance's network (`scripts/install-builder.sh`),
      the browser files built in an image with LFortran and emscripten
      (`deploy/container/Dockerfile.web`), every build container limited
      (`FY_BUILD_LIMITS`). Deploys then need neither a workstation nor SSH.
- [ ] Landing checks in the toolchain image with CPU and memory limits.
- [ ] Release notes drafted by the agent; health watch after a switch with
      automatic rollback.

### M4d Safety of the running instance

- [ ] Nightly consistent database snapshot before the institute's backup, and a
      restore test from that backup.
- [ ] Format and repository-hygiene checks in every landing.
- [ ] Landing checks in a runner container of their own, with their own limits
      (the sandbox agents use later); `fy test BRANCH` on the same runner.
- [ ] fo published as a Debian release in the instance's store, so the toolchain
      image builds in seconds.

### M4b First background agent

- [ ] One agent on the local model gateway, in a per-task sandbox (fresh
      container, no credentials, network only to the gateway and the API),
      with its own bot account: triage of new issues and changes, and review of
      landed changes with flags for human review. Kill switch, rate limits,
      transcripts linked to outcomes. See [Workflow](doc/workflow.md).

### M5 Web views of repositories

- [x] Repository list, file browser, history and commits with coloured diffs
      in the application, from a read-only API over the mirrors.
- [x] Every page has its own address (repositories at /GROUP/.../REPO, their
      pages after /-/ as in GitLab), Back and Forward, and server-rendered
      HTML for every address, so links can be bookmarked and opened in new tabs.
- [x] Jobs page (`/jobs`, `/jobs?repo=NAME`): running and waiting jobs, totals and
      counts per repository, recent results with their logs; live through a
      server-sent event stream (inotify and poll(2) on the server, EventSource in
      the browser) and fortui timers, without polling.
- [ ] CI logs and test history, with their own addresses.

**Inspection gate:** the instance is reviewed before anything is hosted
elsewhere.

## Stage B: hosted instance

- M6 Shared build cache and the fo client.
- M6a Downstream consumers: dependency graph across repositories, consumer
  cases in the pool keyed by dependency snapshot, classified outcomes
  (regression only against a passing baseline), libneo's registry imported
  (see [Testing at scale](doc/testing-at-scale.md#downstream-consumers)).
- M6b Declared policy for workloads fo cannot infer (runner class, private
  data, release gates); NEO-2's golden comparisons as CTest tests with policy
  entries and fully keyed results.
- M7 Coverage per commit and on changed lines.
- M8 Full changes: stacks, versions with interdiff, attachments, review
  ledger, batched merge queue, failover; fx and fo become primary.
- M9 Agents: issue gardening and exploration; implementation once a second
  model is available; instructions and settings per group and repository;
  benchmark and outcome evaluation.
- M10 Issues with two-way GitHub synchronisation; manual ingest of GitHub
  pull requests; staging and promotion for live services.
- M11 MILCA and further code repositories move to fortyard.

## Study readiness

A controlled study of integration policies with many agents
(`plasma/proj/agent-productivity` on the TU Graz GitLab) runs on fortyard.
Every feature it needs is finished before its first run. The study counts
steps, tool calls, tokens and metered compute and never relies on measured
model speed. Items that also appear in milestones above are pulled forward;
the others are new.

- [ ] S1 Policy settings per repository: tests before landing (full suite,
      affected cases within a budget, none), review before or after landing,
      bisection and repair on or off, batch size, limit on unreviewed
      changes. Defaults keep today's behaviour.
- [ ] S2 `fy runner` with leases, heartbeats and per-case limits (M3). Done:
      remote runners on build hosts (HTTPS through curl, a mirror and a kept
      work tree with its build directory per repository, restart on a new
      server release; `scripts/install-runner.sh`). Production's landing
      checks run on faepkub4 since 2026-11-10, not on the server's host.
- [ ] S3 Batched merge queue that blames a change only when the failure
      reproduces with that change alone (M8).
- [ ] S4 Continuous pool: candidates by key change, staleness and audit
      draw, ordered by staleness, failure likelihood and cost
      ([Testing at scale](doc/testing-at-scale.md#the-pool-and-its-scheduler)).
- [ ] S5 Bisection of changed outcomes and a repair task for the author of
      the culprit commit.
- [ ] S6 Review and triage agent (M4b) with flags for a person and random
      spot checks of unflagged changes; budgets in tokens and steps, never in
      seconds, and no durations in anything the model sees; transcripts
      linked to outcomes.
- [ ] S7 Outside implementers: the API leases implement and repair tasks to a
      client that runs its own harness and submits the result as a change,
      with model identity, steps and tokens recorded.
- [ ] S8 Campaign mode: an instance started from a campaign file runs on a
      virtual clock (nominal durations from token counts and metered CPU
      time), runs at most one agent session at a time, draws all random
      choices from a seed and writes a complete campaign record.

**Done when** a dress rehearsal passes on a local instance: one small
campaign under each of the study's three policies on a small repository,
with seeded defects, ground truth and complete records, without anyone
stepping in.

**Safety.** The production instance shares faepkub3 with code.tugraz.at.

- Every study feature is off by default; production enables none of them
  until Chris decides.
- The container limits in `deploy/compose/compose.yaml` are not raised.
- Campaigns, pool load and agent runs for the study use local instances
  (`scripts/dev-up.sh`), never production; full test suites of large codes
  run on the cluster.

## Work in fo

Tracked in fo; needed from M3 on (see
[Testing at scale](doc/testing-at-scale.md#what-fo-needs)):

1. `fo test --plan --json` without building.
2. Running a given set of cases from a plan, results with digests.
3. Per-case cgroups, peak memory, bytes written, memory admission.
4. `fo-metric` lines in reports.
5. Remote lookup of trusted passes by key.
6. Dependency overrides: one dependency at a given revision, in the key.
7. Changed public module interfaces of a change.
8. Starting compilers when fo is process 1 of a container (fails today).
9. CTest properties and fortyard policy entries reported per case in the plan.

## LFortran and WebAssembly

The web application needs LFortran's WebAssembly target (Flang cannot target
WebAssembly). fortui's sources already compile with it and its headless tests
pass in Node and Chromium. Defects that block fortyard or fortui are fixed in
the lazy-fortran fork of LFortran. OpenMP stays in the server; modules shared
with the browser do not use it.
