# fortyard plan

Delivery order for [DESIGN.md](DESIGN.md),
[Testing at scale](doc/testing-at-scale.md), [Workflow](doc/workflow.md),
[Parallel development](doc/parallel-development.md) and
[Inspection, review and monitoring](doc/inspection.md) and
[Check backends](doc/backends.md). A milestone is done when its
behaviour is covered by tests that run with `fo test` and it runs on the
development instance.

## Open work

What is started or decided and not finished, each with its issue:

- #60 Enable the review agent in production (server configuration); needed
  before the reviewer workers of MP8 are of use there.
- #61 Shared build cache: finish and land fo's client.
- #62 Eventual correctness per test case.
- The LFortran fork's upstream pull requests: issue #27 of the repository
  `lfortran` on this instance.

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
- [x] Runtime bounds checks kept on in release builds of the request parser:
      the parsing modules are a package of their own (`parser/`) whose flags
      keep `-fcheck=bounds` in every profile; malformed and truncated requests
      are tested.
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
- [ ] Visual design of the application (MI22).
- [ ] Long files and diffs stay fast (MI24: folded files, lists loaded by key).
- [ ] Plots in fortui as a plot node drawn as inline SVG (MI25); diffs stay text.

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
- [x] Size limit on receive: a push that brings a file over 1 MiB (or over the
      bytes in `.fortyard/size-limit`) outside `.fortyard/allow-large` is
      refused and names the file; `fy push` and `fy land` check before sending.
      Format and the same size limit are checked again at landing.
- [x] Changes, minimal: a push to a branch other than `main` and `wip/...`
      opens or updates a change (title and description from its commits,
      versions matched by `Change-Id` or branch, comments, interdiff); landing
      it gives the commit the change's number, description and trailers;
      `fy push`, `fy change list|view|diff|comment|edit|land|close|reopen`.
- [x] Issues, numbered with the changes: `fy issue list|view|new|comment|edit|
      close|reopen` and the API.
- [x] Change and issue pages in the web application: lists, a change with its
      conversation, landings, versions and diff or interdiff, comments, land
      and close; an issue with comments; a new issue.
- [x] Landing, minimal: `main` takes no pushes; `fy land REPO BRANCH` queues a
      pushed branch, which lands one at a time as one squashed commit with
      `(#N)` and a `Change:` trailer after the repository's check (`fo test`, or
      `.fortyard/check`) passed on that commit under a memory cap.
- [x] Eventual correctness, repository level: after main moves, a runner
      verifies its newest commit with the full suite (`.fortyard/verify`, else
      every fpm test run again), after landing checks; a failure after a pass
      is bisected over main's commits, reported as an issue with the log and
      on the culprit's change, and closed when main passes again; `fy verify`.
      Case level (keys from fo's plan, staleness, flaky detection) follows
      (#62).
- [x] Batched merge queue (landings queued behind a running one share a
      candidate and its check; a failing candidate is split), `Change-Id`
      trailers, landing from a change page.
- [x] Push of `main` to the repository's upstream after each landing.
- [x] The edge timer follows the instance's own `main`; fortyard is primary on
      its development instance and GitHub is its mirror.
- [x] Tags to the upstream: the mirror process pushes them within a mirror
      interval; a moved tag is refused, never forced.
- [x] `fy` covers the daily work: `fy land` pushes, lands and follows a
      branch; `fy jobs`, `fy job`, `fy landings` print tables and logs;
      `fy login` stores a proxy; `fy git-credential` serves plain git.
- [ ] `fy mcp` grows with each step. Done: one tool per operation with an input
      schema for repositories, changes, issues, landing a branch, jobs and
      landings. Open: the tools of the MP steps, until MP19.
- [x] Audit log (append-only, hash-chained): landings, tokens, repositories,
      releases and sign-ins are recorded; `fy audit list`, `fy audit verify`
      and `/api/v1/audit` (admin). Open: publishing the newest hash daily
      outside the instance ([Workflow](doc/workflow.md#audit)).

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
      (the sandbox agents use later). `fy test BRANCH` is MP12.
- [ ] fo published as a Debian release in the instance's store, so the toolchain
      image builds in seconds.

### MP Parallel development

Five to twenty agents and people work on one repository at the same time and
land without anyone merging or testing again by hand
([Parallel development](doc/parallel-development.md)). Items are in delivery
order; items with the same letter can be built at the same time, and a letter
starts when the letters before it have landed.

- [x] MP1 (A) Named migrations: a registry of migrations by name
      (`src/fy_migrations.f90`) and the set of applied names with their
      content hashes in the database. Versions 1 to 15 stay, with the version
      row frozen at 15, so the previous release still starts. A duplicate name
      or a changed body of an applied migration refuses to migrate.
- [x] MP2 (B) One name per line, sorted, in the lists every feature extends:
      the `public` and `use ..., only:` lists of the store, change and server
      modules, the usage text and the MCP actions. Lands with nothing else in
      flight; two branches that each add a store procedure then merge in these
      lists without a conflict.
- [ ] MP3 (C) Scopes `write` and `land`: a token with `write` pushes, comments
      and claims, one with `land` queues landings, neither needs `admin`
      (`fy token create NAME --scopes ...`). Versions and landings record the
      token's name, which lists show as the session.
- [ ] MP4 (C) Checks name a commit: a check carries commit, tree and a nonce
      for its claim; the runner checks out that commit, reports the tree and
      repeats its report until the server has it; a report with another nonce
      or tree is refused. A lost runner, a failed fetch or a missing runner is
      an error that queues the check again and never fails a landing.
- [ ] MP5 (C) Squash in the bare repository with `git merge-tree` and
      `git commit-tree`, without a clone: same commits as before, a conflict
      lists its paths, an empty squash is refused, size and format are checked
      on the member's own files, a merge of `main` into a branch lands
      squashed with the others. `fy serve` refuses to start with a git older
      than 2.40.
- [ ] MP6 (C) Queue numbers (built as MI9): `fy board --stats` and `GET .../board?stats=1`
      give time from `fy land` to `main`, landings per hour, failures by
      reason and checks per landing from the rows that exist, as the baseline
      for everything below.
- [ ] MP7 (C) Claims: `fy issue claim N` and `fy issue release N`; one holder,
      the second claim is refused with the holder's name; a claim expires 24
      hours after the holder's last push that names the issue and ends when
      such a change lands; issue lists show the holder.
- [ ] MP8 (C) Reviewer workers: `FY_REVIEW_WORKERS` processes take landed
      changes, each change exactly once; six changes landed together are
      reviewed by two workers without a double review.
- [ ] MP9 (D) Landings pin a version: `fy land` opens the change if there is
      none, pins the pushed commit and returns the existing landing when the
      change already has a live one. A push to a queued change cancels its
      landing and says so, as does `fy land --cancel N`. `main` moves in one
      `git update-ref` transaction
      that names its old value; a branch is deleted only if it still points
      to the pinned commit, and commits pushed later open a follow-up change.
- [ ] MP10 (D) `keep-both`: for paths listed in `.fortyard/merge` on `main`,
      a conflict in which both sides only inserted lines at the same place is
      merged with both insertions, the earlier landing's first; anything else
      stays a conflict. Each insertion is kept whole, as git shows it in
      `diff3` style: git's union merge keeps lines that both insertions start
      or end with only once, which cuts a registry entry. The landing and
      the change show what was merged.
      fortyard lists `PLAN.md` and `src/fy_migrations.f90`.
- [ ] MP11 (D) Overlap on demand: `fy push`, `fy change view` and
      `fy overlap [N]` say whether the change merges with `main` and name the
      open changes it conflicts with (number, session, state, paths);
      `fy overlap --paths P...` lists the open changes that touch given
      paths.
- [ ] MP12 (D) `fy test [BRANCH|N]`: a runner checks the version squashed
      onto the current `main` without landing it; runners take landing checks
      first, then tests, then verifications; the result is listed with the
      change.
- [ ] MP13 (E) Durable candidates: a candidate is a row with its members, an
      immutable ref, its tree and its check, and the one lander works from
      these rows without sleeping inside a check. A server restart during a
      check, as every deploy causes, goes on with the same check; a restart
      between the move of `main` and its record leaves the landings landed. A
      candidate whose tree already has a passed check (after `fy test` with
      `main` unmoved) lands without a second run. Up to 16 landings share a
      candidate.
- [ ] MP14 (F) Conflicts as outcomes: a landing that conflicts only with one
      ahead of it waits for the next candidate and lands if that one is
      rejected; one that conflicts with `main` fails with the reason
      `conflict` and a rebase task (commit to merge with, paths, the other
      changes). `fy land`, `fy test` and `fy job --wait` end with status 0, 1,
      3 or 4 (still pending after `--timeout`), `--json` prints the task, and
      the MCP action `wait` does the same. After three failed landings of one
      change the next needs the admin scope. A landing queued with `--urgent`
      (admin) goes before those not yet in a candidate.
- [ ] MP14a (G) Merge by the model (`merge.model` in the repository policy:
      `off`, `flag`, `on`; off by default): a conflict that `keep-both` does
      not cover goes to the local model, which returns the text of the
      conflict regions only; the server puts them into git's merge, the
      candidate's check decides, and a failure leaves the rebase task as
      before. The landing records model and merged regions, the change shows
      them, and with `flag` the change is flagged for a person. Paths reviewed
      before merge are never merged by the model. Tested with the stub
      gateway; needs the gateway of #60 in production.
- [ ] MP15 (G) `fy sync [--onto M] [--continue] [--land]`: merges `main` (or
      the newest version of change M) into the branch with rerere on, stops
      at conflicts with status 3 and the paths, and after `--continue` pushes
      and, with `--land`, queues again.
- [ ] MP16 (G) Halves in parallel: the halves of a failed candidate are
      checked at the same time when two runners are free; a half that passes
      on an unmoved `main` lands at once, the other is built again on it.
- [ ] MP17 (G) Queue settings in the repository policy of S1:
      `queue.batch`, `land.tier` (`affected`, `full`, `none`), `claim.hours`.
      Defaults keep the behaviour.
- [ ] MP18 (G) Stacks: a version records the version of another change it is
      built on; diff, review and squash use it as base; a child lands after
      its parent as its own commit, also when the parent was amended or has
      landed squashed; `fy land --stack` queues the session's own unlanded
      ancestors; a child of a rejected, cancelled or closed parent waits.
- [ ] MP19 (G) The board (built as MI5, MI27 and MI28): `fy board`, `GET .../board` and the MCP action
      `board` list working sessions (draft changes with claimed issue, last
      push and predicted conflicts), the queue (landings with state and
      reason, candidates with their check), landings that need their author,
      and recent landings with the first release that contains them.
- [ ] MP20 (H) The board as a page of the web application (built as MI5 and MI27), live through the
      event stream.
- [ ] MP21 (H) Speculation, off by default (`queue.speculate`): further
      candidates are built on the tip of the one being checked and checked on
      other runners; one lands only after those before it, and a failure
      supersedes those behind it. Built when MP6 shows a wait for a runner
      above the check time with two runners in place.
- [ ] MP22 (I) The rehearsal below as a test (`test/test_swarm.f90`).

**Done when** a rehearsal passes on a local instance (`scripts/dev-up.sh`
with two runners and a check of fixed duration): sixteen scripted sessions,
each with its own token and worktree of one small repository, start together
and each lands one change with `fy` alone. Four changes edit different hunks
of one file, four append an entry to a registry of named migrations that is
declared `keep-both`, two edit the same lines, two form a stack, one fails
the check by itself, two pass alone and fail together, and one touches
nothing the others touch; the server is restarted once during a check. The
rehearsal passes when

- every change is on `main` as one commit with its `(#N)`, the stack in
  order, except the change that fails by itself and the later one of the
  failing pair, which end with status 1 and their logs;
- the later one of the two changes on the same lines got status 3 once and
  landed after `fy sync` by its own session;
- no check ran twice because of the restart, and no landing is left queued,
  waiting or running;
- no command used the admin scope after the start, and nothing was done
  outside the sessions' own commands;
- the verification of the final `main` passes;
- the last landing is on `main` within ten check times of the start (a
  bound to confirm from the first run).

The same rehearsal then runs once with coding agents: seven agents each
implement one prepared issue on a scratch copy of fortyard, all of them in
`src/fy_server.f90`, `src/fy_store.f90` and `app/fy.f90`, and every change is
landed or shown on the board as waiting for the person, without the person
merging, rebasing or testing anything.

### MB Check backends

Checks run wherever the result comes soonest: own runners, GitHub's hosted
runners for public repositories, GitLab runners of our groups, behind a
network allowlist, with every credential in the smallest scope and no change
to the Computor services on the same host ([Check backends](doc/backends.md)).
Items with the same letter can be built at the same time against the
interfaces the document fixes (data model, routes, result body); a letter
starts when the letters before it have landed.

- [ ] MB1 (A) Allowlist and isolation: the egress proxy (`deploy/egress/`,
      squid, lists per role, TU Graz and private networks denied on the
      resolved address, no DNS in the containers, learning mode for public
      hosts of the executor); internal networks for server, runners and
      backend processes; the relay container publishing the server's port for
      Traefik; the local runner off the server's network namespace; compose
      files and install scripts updated. A test brings up the proxy with a
      list and checks that an allowed host is reached and a TU Graz address, a
      private address, a literal IP and an unlisted host are refused.
- [ ] MB2 (A) Runner split: `fy runner` as controller and executor in two
      containers sharing a work directory; the controller hands a check over
      as files and reads its output and exit status the same way, and stops
      it on cancellation. A test runs a check that looks for the runner token
      in its environment and the files it can read, and fails if it finds it.
- [ ] MB3 (A) Dispatch: migration with `checks.ran_class`, `started_at`,
      `twin` and `repos.public`; `fy repo public`; the policy keys `backends=`
      and `egress=` with their validation; the result body's optional
      `started_at`; standing-in classes and the estimate of the document;
      claim rule and concurrency limits; `GET /api/v1/backends` and
      `fy backends`. Tests with recorded check rows.
- [ ] MB4 (A) GitHub backend: `POST/DELETE /api/v1/checks/N/publish` (server
      pushes and deletes `fortyard/check/<id>` with the deploy key);
      `fy backend github` (App JWT with `openssl`, per-repository installation
      tokens, run found by branch and workflow path, ETag polling, job log,
      result with `started_at`, run deleted, sweep, refusal of a changed
      workflow file) and `fy backend github setup REPO` with its checks; the
      shipped workflow `deploy/github/fortyard.yml`. Tests against a local git
      remote and a fake API.
- [ ] MB5 (A) GitLab backend: `fy backend gitlab` (project token, push of
      `check/<id>`, pipeline started through the API with the fixed
      configuration file, job found and polled, log, result with `started_at`,
      pipeline and branch deleted, sweep, start-wait back-off, job timeout),
      toolchain images, the job cache and fo's cache in the package registry.
      Tests against a local git remote and a fake API.
- [ ] MB6 (B) Racing: twin checks when estimates differ by less than 25 %, the
      first verdict counts, the other run cancelled (backend cancels its run,
      own runner stops at its next log upload). Builds on MB3 and MB2.
- [ ] MB7 (A) Nightly classes: `@nightly` in `verify_on`, verification jobs
      per nightly class once a night when `main` changed, bisection at night,
      own issues.

### MI Inspection and monitoring

A person who steers many agents can inspect every change, review what is
flagged and watch the instance from outside
([Inspection, review and monitoring](doc/inspection.md)). Every item delivers
its data through the API, `fy` and `fy mcp` as well as on its page. Items are
in delivery order within their group; items with the same letter can be built
at the same time, and a letter starts when the letters before it have landed.
An item marked `any` depends on no other item. `R` is a repository's path.

MI absorbs these items, which are ticked by the MI item named:

- M5 "CI logs and test history, with their own addresses": logs are MI2;
  history per test case stays with #62.
- MP6 "Queue numbers" is MI9, which gives the time from `fy land` to
  `main`, landings per hour, the failed share and checks per landing; its
  failures by reason follow in MI10, which records the reason. MP19 "The
  board" is MI5, MI27 and MI28; MP20 "The board as a page" is MI5 and MI27.
- M2 "Visual design of the application" is MI22. M2 "Virtualized views of
  long files and diffs" is MI24: folded files, lists loaded by key and
  `content-visibility`, without a virtual list. M2 "Canvas surface in fortui
  for diffs and plots" is replaced by MIF6 and MI25: a plot node drawn as
  inline SVG, while diffs stay text.

Interleaving with MP:

- MI1 to MI26 work on the rows that exist and do not wait for any MP item.
  MI27 comes after MP9, MP12, MP13 and MP14, MI28 after MP3, MP7 and MP11,
  MI29 after S6.
- MP13 changes what a queue row is. MI2, MI4 and MI5 fix the form the pages
  read before that: a job carries `checks` as a list and `candidate` as
  null, a change carries `entries`. MP13 changes the store queries that
  fill these and keeps the form; the tests of MI2, MI4 and MI5 hold it to
  that.
- MI10 adds `landings.reason`, `version` and `head`. MP9 (pinned version)
  and MP14 (reasons) write these columns and add none of their own;
  whichever lands first adds them.
- MI10 to MI14 each add a migration. After MP1 (named migrations) they are
  parallel; before it they land one after another in this order.
- Each new resource is a module of its own with one dispatch line in
  `src/fy_server.f90`, so sessions meet only in that line until MP2 has
  landed.

Additions to fortui, landed in the fortui repository
([What fortui gains](doc/inspection.md#what-fortui-gains)). They depend on
nothing here; an MI item that names one needs it landed and the fortui
version in `fpm.toml` moved to it:

- [ ] MIF1 (any) Keys: key presses outside text fields reach the
      application, and `ui_shortcut(chord)` registers a chord.
- [ ] MIF2 (any) Focus: `ui_focus(id)`, `ui_reveal(id)`, and arrow keys and
      Enter delivered while a text field has the focus.
- [ ] MIF3 (any) `look=` on text, link, button, row and column, and the
      tone `UI_TONE_ATTENTION`.
- [ ] MIF4 (any) `anchor=`, emitted as the HTML `id`.
- [ ] MIF5 (any) Only changed nodes are updated, a row or column with an id
      keeps its element when rows are inserted before it, and `FUI.stats`
      reports frame time, bytes and nodes.
- [ ] MIF6 (any) A plot node drawn as inline SVG, with block characters in
      the terminal.

On the existing rows, without a schema change:

- [ ] MI1 (A) Changes of all states: the list shows open, landed and closed
      changes, newest activity first, each with its state, its review state
      and flag reason, commit and age. Filters are links in the address
      (`?state=all|open|landed|closed`, `?review=flagged|unreviewed`,
      `?attention=needs-author`), also in
      `fy change list --state S --review S --attention S` and the MCP tool
      `changes`. A repository whose changes all landed no longer shows an
      empty page.
- [ ] MI2 (A) Jobs name their change: a job in the API, `fy jobs` and the
      jobs page carries the number and title of its change, its kind and its
      check (state, runner, wait and run time), found through the check's
      ref. `/jobs/ID` is a page with the whole stored log (a runner stores
      the first 64 KiB of a check's output); its lines have addresses
      (`#L812`) once MIF4 has landed.
- [ ] MI3 (A) Repository row: every page of a repository, the code page
      included, links to Code, Changes, Issues and Board, with the numbers
      of open changes and issues. Issues are one click from the code. The
      link Analytics is added by MI18.
- [ ] MI4 (B) The change page shows what happened: one page with the status
      line, the decision line for a flagged change (approve as
      `fy change reviewed N` does), and the sections Checks (entries with
      their times and check, the end of a failing log, earlier entries
      folded, the verification of `main`), Reviews (review state, flag
      reason, the review's text), Files, Discussion and Timeline, each with
      an anchor (MIF4). There are no tabs. `fy change view N` prints the
      same. Later items add what they record: MI10 the reason of a failure
      and what ran, MI11 the version a review read and the decisions as
      rows, MI13 the release, MI15 the revert.
- [ ] MI5 (B) The board from the rows that exist: `GET /repos/R/board`,
      `fy board`, the MCP tool `board` and the page `/R/-/board` show the
      queue, the changes that need their author, the open changes and the
      recent landings with their review states, one row per change, and the
      verification of `main`.
- [ ] MI6 (B) Overview at `/`: per repository the state of `main`, the
      queue, and the numbers of changes that need their author, are flagged
      or are unreviewed, each a link to its filtered list; below it what
      runs now. `GET /overview`, `fy overview`, the MCP tool `overview`.
      The delivery line is added by MI13, the rows that need the person by
      MI16 and MI20.
- [ ] MI7 (any) Every page is live: a change made with `fy` shows on an
      open page within a second without a reload. The event stream takes a
      list of topics (`jobs`, `changes`, `reviews`, `issues`; MI12 adds
      `runners`, MI17 `load`), a tab holds one stream, and a page asks for
      its resources again on an event. A page says `live`, and `offline
      since TIME` while the server is stopped, and is current again after
      one request when it is back. With 28 open streams a request is still
      answered: a worker keeps four request threads free of streams, and
      the default is 32 threads.
- [ ] MI8 (any) Audit page `/audit` over the existing log, filtered by event
      and subject (`GET /audit?event=&subject=`, `fy audit list --subject`),
      with the state of the chain: the server checks the chain every 5
      minutes from the last checked entry, the page shows up to which entry
      it is intact and when that was checked, and `Verify now` checks at
      once. The page is linked from the Server page once MI17 has landed.
- [ ] MI9 (any) Queue numbers (MP6): `GET /stats`, `fy stats`,
      `fy board --stats` and the MCP tool `stats` give, for 1, 7 or 30
      days, the time from `fy land` to `main` (median and 95th percentile),
      landings per hour, the failed share, the share landed with the first
      entry, checks per landed change, wait and run time of checks, failed
      verifications per 100 landings and the flagged changes not decided.

Links between the rows:

- [ ] MI10 (C) Entries and checks carry their links: an entry records the
      version and commit it lands, its check and, when it fails, the reason
      (`conflict`, `check`, `format`, `size`, `empty`); a check records its
      job and what ran (command, image, fo version, exit status), reported
      by the runner. Existing checks get their job from the ref name. Jobs,
      board and change show `failed: REASON · entry 1 of 2` and what ran,
      and `fy stats` gives the failures by reason.
- [ ] MI11 (C) Reviews are rows: the reviewer records the version it read,
      model, thinking setting, instruction version, tokens, duration and its
      findings with file and line; a person records `approve` or `issue`
      (`GET` and `POST .../changes/N/reviews`, `fy change reviews N`,
      `fy change approve|issue N`, the MCP tools `change_reviews` and
      `review_add`). The change page shows the review as a block with the
      version it read against the newest, earlier reviews folded as
      superseded, and each decision as `approved by NAME` or `issue #N
      opened` with its time; a second decision adds a row and changes none.
      Reviews from before this item show `not recorded`.
- [ ] MI12 (C) Runners are rows: name, host, version, last call and current
      check, written by the calls a runner makes; `GET /runners`,
      `fy runners`, the MCP tool `runners`, and the event topic `runners`.
      An idle local runner asks at least once a minute.
- [ ] MI13 (C) Builds and deploys are recorded: the release builder opens a
      release as `building` and ends it `ready` or `failed` with the end of
      its log; `fy serve` records a deploy when it starts as a new version,
      and `scripts/self-update.sh` reports a release it refused. A change
      names the first release that contains it. The overview shows the
      delivery line. `GET /deploys`, `fy deploys`.
- [ ] MI14 (C) Issues and changes are linked: `#N` and `Fixes #N` in a
      description link the change to the issue; the issue list (filters in
      the address, `GET /issues` across repositories) names the linked
      changes with their states, and a change names its issues.

Review, numbers, load, metrics and alerts:

- [ ] MI15 (D) Revert as a decision: `verdict=revert` with a reason opens
      the revert of a landed change as a change, queues it (before the
      others once MP14 has landed) and records `reverted by` and `reverts`
      on both; `fy change revert N -m TEXT`, and the filter
      `?attention=reverted`.
- [ ] MI16 (D) Inbox: `GET /inbox`, `fy inbox`, the MCP tool `inbox` and
      the page `/inbox` list, across repositories, flagged changes, changes
      without a finished review with the reason, and changes stuck with
      their author, each as a card with intent, reason, evidence and
      decision. The top bar and the tab title carry the count; the sentence
      of an empty inbox is computed; the overview shows the flagged cards.
      The section Before landing comes with MI27.
- [ ] MI17 (D) Live load: `GET /load` and `fy load` give host load, memory,
      disk, database size, queue depth and the supervised processes, and
      the page `/server` shows them with the runners of MI12 and the
      deploys of MI13. One sample a minute is always taken and fills a ring
      of two hours for the sparklines; while a page listens, values arrive
      every 10 seconds over topic `load`.
- [ ] MI18 (D) Analytics pages `/analytics` and `/R/-/analytics`, linked
      from the Server page and the repository row: the numbers of MI9 with
      bars and sparklines, and the measures whose columns exist by then
      (failures by reason, push to `main`, `main` to release, landing to AI
      review, tokens per review). A click on a number opens the list of the
      rows it counts.
- [ ] MI19 (D) Metrics and readiness: `GET /metrics` in the Prometheus text
      format returns the metrics listed in the design, with a token of the
      scope `metrics` or, with `FY_METRICS_OPEN=1`, without one; the host
      metrics come with MI17 and `fy_alert` with MI20. `/readyz` answers
      503 with the reason while the database, the data directory or the
      lander fails. The log has one JSON line per request with its
      duration.
- [ ] MI20 (E) Alerts: the conditions of
      [Alerts](doc/inspection.md#alerts) are computed from the rows;
      `review-backlog` reads the limit of the repository policy (S1).
      `GET /alerts`, `fy alerts`, the MCP tool `alerts`, the gauge
      `fy_alert`, the first section of the inbox and of the overview, and
      the mark in the top bar show the same list.
- [ ] MI21 (F) Alert webhook: with `FY_ALERT_WEBHOOK` the instance posts one
      message when a rule starts to fire and one when it ends.

Frame, keys and budgets:

- [ ] MI22 (C) Frame and look (MIF3): top bar with the mark for `main` and
      the links Jobs, Inbox and Server, each shown once its page exists
      (MI16, MI17), the repository row, the account page `/account`
      (tokens, theme, sign out), no drawer, a bottom bar on phones; states
      as words in five tones, the type scale and row heights of the design.
      The HTML the server sends is the same at every width.
- [ ] MI23 (D) Commands, keys and palette (MIF1, MIF2): one command table
      feeds buttons, keys and the palette; the go keys, list keys and
      decision keys of the design work and stay inert in text fields;
      `GET /find`, `fy find`. Two commands with one key on a page fail a
      test.
- [ ] MI24 (E) Budgets as tests (MIF5): the browser tests are one file per
      page (`test/web_PAGE_test.mjs`) on a shared harness that asserts the
      budgets of the design on every page; server tests with 10 000 seeded
      landings assert the times of `GET /overview`, `/board` and `/inbox`.
      A file with more than 400 changed lines is folded, lists load older
      rows by key, code rows use `content-visibility`.
- [ ] MI25 (F) Time series (MIF6): the Analytics page shows the time to
      `main` per day and the Server page the load over the ring's two hours
      as plots, present in the HTML the server sends.
- [ ] MI26 (G) The rehearsal below as a browser test,
      `test/web_inspection_test.mjs`, next to `test/web_app_test.mjs` and
      run like it, since `fo test` runs no browser tests:
      `node test/web_inspection_test.mjs build/fo/bin/fy build/web`.

After other milestones have changed the rows:

- [ ] MI27 (H) Candidates and supersession: the board shows candidates with
      members, shared check, halves, waiting reasons and candidates
      superseded in the last hour; change and job pages show the pinned
      version, cancelled entries with the next entry, superseded and reused
      checks, tests and rebase tasks; a candidate has the page
      `/R/-/candidates/ID`, `GET .../candidates/ID` and `fy candidate ID`.
      The inbox gains the section Before landing for entries that wait for
      an approval: `verdict=approve` on the waiting version releases the
      entry, and sending back posts a comment and cancels it. Merges by the
      server are shown once MP10 has landed, speculative candidates once
      MP21 has. Its own check extends the rehearsal by two questions, each
      answered in 2 actions: why does each queued entry wait, and which
      changes shared a candidate, which check decided it and what was
      superseded?
- [ ] MI28 (H) Sessions, claims and predicted conflicts on the board, the
      issue list and the change page.
- [ ] MI29 (H) Spot checks: the inbox has the section Spot check with the
      changes the agent of S6 drew, and a decision on one is recorded like
      any other.

Not in MI: the routing of cards by an owners file and the time budget of
[Workflow](doc/workflow.md#the-human-review-queue); they need measured
reading times.

**Done when** an inspection rehearsal passes on a local instance
(`scripts/dev-up.sh` with one runner, the stub model gateway and two
repositories; the second holds one flagged change, so that overview and
inbox span repositories). One action is a key sequence, a click or a tap; a
reading step unfolds a section of the page reached. A script plays one day
in four phases, and at each phase a headless browser, starting from another
page, answers the questions within the stated number of actions, while the
`fy` command and the MCP tool of the same row of the design's API table
return the same data in one call.

While a check runs and a second entry is queued behind it:

1. What runs now, on which runner, for which change? 1 action.
2. What is queued in the repository? 2 actions.

After a change failed its first entry on the check and landed with its
second:

3. What happened to it: versions, entries, checks, review, release?
   2 actions.
4. What ran in its failed first entry (command, runner, exit status, the
   failing lines), and which entry followed? 2 actions and 1 reading step.

With the runner stopped for two minutes while a check is queued:

5. Which runner is lost, and since when? 1 action; `runner-lost` is the
   first card of the inbox, and `/metrics` reports it. The runner is then
   started again.

At the end, when twelve changes have landed, the reviewer has flagged two
of them with a finding, one open change has failed three entries, the
verification of the last `main` has failed and opened an issue, and the
release builder has reported a failed build, so that the instance serves an
older release:

6. Which changes need a decision, and why? 1 action; 0 on the overview. The
   alerts `main-broken` and `deploy-behind` stand first, in this order.
7. Is `main` verified, built and serving, and if not, why? 0 actions for
   the mark, 1 for the reason.
8. Which changes landed today, and which are flagged or unreviewed?
   2 actions.
9. What did the AI review find, with which model and how many tokens, and
   which version did it read? 2 actions.
10. Approve one flagged change and open an issue for the other with keys
    alone: on the inbox at most 2 keys each, and both cards leave.
11. Who approved the flagged change, and when? 2 actions.
12. Which issues are open, and which change addresses the issue of the
    failed verification? 2 actions.
13. What was the median time from `fy land` to `main`, and which entries
    are behind the failed count? 2 actions.

The rehearsal also passes only if every page shows a change made through
`fy` within a second without a reload, no page scrolls sideways at 390 px,
light and dark both render, the browser budgets of the design hold, and
`/metrics` reports the failed build, the failed verification and the
flagged changes.

### M4b First background agent

- [ ] One agent on the local model gateway, in a per-task sandbox (fresh
      container, no credentials, network only to the gateway and the API),
      with its own bot account: triage of new issues and changes, and review of
      landed changes with flags for human review. Kill switch, rate limits,
      transcripts linked to outcomes. See [Workflow](doc/workflow.md).
- [x] First depth of review, without a sandbox: a model behind an
      OpenAI-compatible API (`FY_REVIEW_URL`, `FY_REVIEW_MODEL`,
      `FY_REVIEW_KEY`; none: no reviews) reads the diff of each landed change
      (tried by hand with Qwen3.8-Flash-Next on two changes of this repository:
      it needs `FY_REVIEW_EXTRA='"enable_thinking":false'`, since with thinking
      on it spends the whole output budget reasoning and returns no review);
      its review is a comment of the account `review-agent`, and a change it
      flags is listed for a person (`fy change list --flagged`) until one
      marks it reviewed (`fy change reviewed N`). `fy change review N` asks
      for a review of any change. Off in production until the server is
      configured (#60).

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
- [ ] CI logs (MI2) and test history per case (#62), with their own addresses.

**Inspection gate:** the instance is reviewed before anything is hosted
elsewhere.

## Stage B: hosted instance

- M6 Shared build cache and the fo client (the server side is live; fo's
  client is an unfinished draft, #61).
- M6a Downstream consumers: dependency graph across repositories, consumer
  cases in the pool keyed by dependency snapshot, classified outcomes
  (regression only against a passing baseline), libneo's registry imported
  (see [Testing at scale](doc/testing-at-scale.md#downstream-consumers)).
- M6b Declared policy for workloads fo cannot infer (runner class, private
  data, release gates); NEO-2's golden comparisons as CTest tests with policy
  entries and fully keyed results.
- M7 Coverage per commit and on changed lines.
- M8 Full changes: versions with interdiff, attachments, review
  ledger, failover; fx and fo become primary (stacks and the queue are MP).
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

- [x] S1 Policy settings per repository (`fy repo policy NAME [KEY=VALUE ...]`):
      tests before landing (`full`, `none`; `affected` is stored and behaves as
      `full` until results per case exist, #62), review before or after
      landing, bisection on or off, batch size, limit on unreviewed changes.
      Defaults keep the behaviour. Open: reverts and fixes of flagged changes
      are not yet exempt from the limit.
- [ ] S2 `fy runner` with heartbeats and per-case limits (M3); the lease of a
      check is MP4. Done:
      remote runners on build hosts (HTTPS through curl, a mirror and a kept
      work tree with its build directory per repository, restart on a new
      server release; `scripts/install-runner.sh`). Production's landing
      checks run on faepkub4 since 2026-11-10, not on the server's host.
      Runner classes: a runner takes only checks of its class (`fy runner
      --class`, default linux); landing checks run on linux, and the
      verification of main runs once on each class of the policy `verify_on`.
      macOS arm64 runners (`scripts/install-mac-runners.sh`) build fy from
      source at the server's commit, run as launchd daemons, and run each
      check in the sandbox of sandbox-runtime (srt) under a memory limit their
      check watcher enforces.
- [x] S3 Batched merge queue that blames a change only when the failure
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
