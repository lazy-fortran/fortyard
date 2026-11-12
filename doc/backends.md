# Check backends: own runners, GitHub and GitLab

fortyard runs a check wherever its result comes soonest: on fortyard's own
runners, on GitHub's hosted runners for public repositories, or on GitLab
runners of groups we belong to. Each place is a **backend**. The server
decides per check; a backend never decides what lands. This document
describes the backends, how the server chooses between them, the network
allowlist every automatic process runs behind, and the isolation that keeps a
compromised fortyard, runner or backend away from everything else.

## Goals

- **Fastest result.** A check goes where waiting plus running is expected to
  be shortest; when two estimates are close, it runs on both.
- **No secret leaves fortyard's side.** GitHub and GitLab hold no credential
  that reaches fortyard or any TU Graz service. fortyard pushes work to them
  and reads results back.
- **Smallest scope per credential.** One deploy key per GitHub repository
  (held by the server), one short-lived GitHub App token per repository and
  request, one GitLab project token per GitLab project, one runner token per
  runner process.
- **Nothing left behind.** Branches, runs, logs and caches on other systems are
  deleted by fortyard or bounded by the system itself.
- **Other users first** on shared GitLab runners, as far as fortyard can tell
  from outside (see below).
- **No work for others and no change to other services.** Everything here
  uses what fortyard's operators already control: fortyard's own containers
  and networks on the build hosts, our GitLab groups, our GitHub organisation.
  No host configuration changes, no requests to the institute's or the
  university's administrators. The Computor services on the same host
  (Computor is the teaching platform whose Traefik reverse proxy also serves
  fortyard) are not touched; what they should change is filed as a Computor
  issue.

## Terms

- A **runner class** names a kind of machine: `linux`, `macos-arm64`, and the
  backend classes `github-linux`, `github-macos-arm64`, `github-windows` and
  `gitlab-<name>`.
- A check's **home class** is the class it is created for: `linux` for landing
  checks, each `verify_on` class for verifications (the `class` column of
  `checks`, as today). Its **ran class** is the class of the runner or backend
  that took it.
- A **backend process** is a runner of a backend class. It claims checks over
  the runner API like any runner, but instead of running a check itself it
  hands it to the external system, follows it, and reports the result.
- A **role** is what a container does (server, runner controller, executor,
  GitHub backend, GitLab backend); each role has its own network allowlist.

## Architecture

```text
                       fortyard server
           (deploy keys; pushes main and check branches)
                 │ runner API (claim, log, result)
   ┌─────────────┼─────────────────┬───────────────────┐
own runners   GitHub backend     GitLab backend      Mac runners
controller +  (App key only:     (project tokens)    (macos-arm64,
executor       read and delete                        own sandbox)
               runs)
                 │                     │
     reads runs of the        pushes check/<id>, starts the
     branch the server        pipeline, reads its job
     pushed                          │
           GitHub Actions       GitLab group runner
```

Backend processes run in containers of their own, not in the server. The
server holds no GitHub App key and no GitLab token. A backend process holds
its runner token and its own external credential, nothing else.

## Data model

Additive columns of `checks` (one named migration):

- `ran_class`: the class of the runner or backend that took the check;
- `started_at`: when the check actually started running (own runner: the
  claim; backend: the external job's start, reported with the result);
- `twin`: for a raced check, the id of the other check of the pair.

A check's duration is `finished_at - started_at`, its wait
`started_at - created_at`. Estimates are computed from these rows; nothing
else is stored.

Repositories get `public` (0 or 1), set by `fy repo public NAME on|off` and
checked against GitHub by `fy backend github setup`. The policy key
`backends=` (default empty) lists the external backends a repository may use;
`github` requires `public`, and a private repository may name neither
`github` nor `gitlab`, because both put its code on another system.

```text
fy repo policy fx backends=github,gitlab     # fx: a public repository
```

## Dispatch

A backend class may take a check whose home class it can stand in for
(`github-linux` and `gitlab-*` for `linux`, `github-macos-arm64` for
`macos-arm64`) when the repository's `backends` allow it. `github-windows`
only takes checks whose home class is `github-windows` (see Nightly classes).

**Estimate** for repository R on class C:

    eta(R, C) = wait(C) + run(R, C)

- `run(R, C)`: median duration of the last 10 checks of R on C that ended with
  a verdict. With fewer than 3 such checks: `run(R, linux) * speed(C) * 1.5`,
  where `speed(C)` is the median, over repositories with at least 3 checks on
  both, of `run(R, C) / run(R, linux)` (1.5 when no repository has those).
  With no history at all on `linux` either: the repository's check time limit.
- `wait(C)` for own runners: the expected run times of the queued checks of C
  that the claim order would give out first (landings before verifications,
  then fewer running checks of the repository, then age), divided by the
  number of runners of C seen in the last 10 minutes; zero when one of them
  is idle.
- `wait(C)` for a backend: the median wait of its checks in the last hour; 60 s
  for GitHub and 120 s for GitLab with no such check; infinite while the
  backend is at its concurrency limit or backing off (GitLab).

**Decision at claim.** An own runner gets the next check of its class as
today. A backend process is given a queued check it may take when
`eta(R, backend) <= eta(R, home)` and `1.5 * run(R, backend)` fits the
backend's time limit (`--max-minutes` of the backend process: 360 for GitHub,
15 for GitLab by default; verifications included). A check whose home class is
the backend's own class (named in `verify_on`) was asked for by the policy: the
backend takes it without an estimate, and the job's time limit bounds it. Concurrency limits are
enforced at claim: GitHub's Free plan allows 20 jobs at once per organisation,
5 of them on macOS, configured on the GitHub backend process; a GitLab backend
runs one job.

**Racing.** When the two estimates differ by less than 25 % of the larger and
both classes have capacity (the backend is below its concurrency limit; an own
runner of the home class is idle, or its check has run for three quarters of
its `run(R, home)`), the server gives the check to both: the backend's claim
creates a second check row, the twin, with the same ref, linked through `twin`
both ways, and the check stays queued for its home class. The first
**verdict** (pass or fail) of the pair counts and the other is cancelled: a
backend process cancels its external run when its poll of
`GET /api/v1/checks/N/publish` shows the check no longer running, an own
runner split into controller and executor stops the check when its next log
upload is refused (409), and one without executor stops sending its log and
runs the check to its end, whose result is refused. The lander and the
verifier read the pair as one check. A run that ends without a
verdict (the external job did not start, the runner was lost, GitHub cancelled
it) is not a verdict: it is reported as `{"error":"runner"}` and the check is
queued again, so infrastructure trouble of a backend never fails a landing;
a twin is not queued again but ends cancelled, and its check waits for the
home class as if queued again then. Racing never uses a GitLab backend.

**Visible.** `GET /api/v1/backends` and `fy backends` list each class with its
runners or backend processes, running and queued checks, concurrency limit,
median wait, and per repository `run(R, C)` with the number of checks it rests
on.

## Nightly classes

`verify_on` entries may carry a schedule, for example (fo is a repository):

```text
fy repo policy fo verify_on=linux,macos-arm64,github-windows@nightly
```

A class without schedule is verified as today: one verification job per new
`main`, with one check per class. Each `@nightly` class gets verification jobs
of its own: once a night (the first verifier pass after 01:00 server time) it
verifies the newest `main` when that changed since its last nightly
verification, bisects at night only, and opens and closes its own issues.
Policy validation accepts `@nightly` after a class name. Windows is not a
first-class target, so no repository runs it unless its policy names it;
macOS on Intel is not supported.

## The GitHub backend

**The server pushes, the backend only reads.** For a check given to a GitHub
class, the backend process asks the server to publish it
(`POST /api/v1/checks/N/publish`): the server pushes the check's commit with
the repository's deploy key as branch `fortyard/check/<id>` to the repository's
GitHub mirror, as it already pushes `main`. GitHub's rule on the mirror
protects only the default branch, so `fortyard/check/**` branches are allowed.
The backend process never holds a key that can push.

**The workflow.** Every repository using the backend has the same file
`.github/workflows/fortyard.yml` (shipped with fortyard as
`deploy/github/fortyard.yml`), which runs on pushes to `fortyard/check/**`:

- `permissions: {}`; the actions `actions/checkout` and `actions/cache`,
  pinned by commit;
- shallow checkout of the one commit;
- `actions/cache` keyed by repository, class and toolchain version for fo's
  cache, the compiler cache and fetched dependencies (GitHub bounds it: 10 GB
  per repository, entries unused for 7 days deleted);
- `fo` built from its `main`; then `.fortyard/check` (or `.fortyard/verify` for a
  verification) where the repository has one, else `fo build && fo test`.

**What a change may run on GitHub.** GitHub runs every workflow file of the
pushed commit whose trigger matches, not only this one. The workflows of
`main` are the repository's own and stay as they are, releases included. A
check whose commit adds or changes any other file of `.github/workflows`
(compared with `main` on GitHub), or whose copy of `fortyard.yml` differs
from the shipped one, is given back to its home class (`{"error":"runner"}`)
instead of being run there. The change's code therefore runs on GitHub only
in `fortyard.yml`, with no secrets and no token beyond reading the repository;
the result is read only from that workflow's run. GitHub's own permissions are
not narrowed further: what this design protects is the TU Graz side, which
GitHub cannot reach.

**Reading back.** A GitHub App `fortyard-checks`, created by an owner of the
GitHub organisation and installed for the public repositories that use the
backend, has the permissions Actions (read and write) and Metadata (read),
nothing else. The backend process holds its private key, signs the App's JWT
with `openssl`, and requests an installation token restricted to the one
repository for each request. It finds the run by branch and workflow path,
polls it every few seconds with conditional requests (ETag), copies the job
log into the check, reports the result with `started_at`, deletes the run,
and asks the server to delete the branch (`DELETE` of the same publish route).

**Nothing left behind.** A sweep in the backend process deletes runs of
`fortyard/check/*` branches older than an hour, and the server deletes such
branches it pushed for checks that have ended. The workflow uploads no
artifacts, and GitHub discards its machines after each job.

**Setup** (`fy backend github setup REPO`, run with an owner's token): checks
that the repository is public and that `fortyard.yml` on `main` is the shipped
file, enables Actions, and reports secrets available to the repository's own
workflows. Its workflows, allowed actions and permissions stay as they are.

GitHub's terms allow hosted runners only for "the software project associated
with the repository where GitHub Actions are used"; a check of repository R
therefore runs only in R's own GitHub repository.

## The GitLab backend

**Push, not pull.** The backend process holds a project access token (scope
`api`, that project only, owned by a person as the university's GitLab
requires) of a private project in a group whose runner we may use, for
example `fortyard-ci` in our institute's `itpcp-lecture-notes` group. For a
check it fetches the commit from the server and pushes it with that token as
branch `check/<id>`, then starts a pipeline through the API with the variables
`FY_REPO`, `FY_KIND` and the job timeout.

**The pipeline definition is fixed.** The project's setting "CI/CD
configuration file" points to `fortyard.yml@<group>/fortyard-ci:config`, a
file on a protected branch `config` of the same project, so pushed commits
never bring their own pipeline definition. The job runs the repository's
check as on GitHub, with no variables beyond the three above, and reaches the
internet only through the runner's own network (never fortyard).

**Other users first.** A project token cannot see jobs the group runner runs
for other projects, so the backend judges from its own jobs: it runs one job
at a time, only checks whose `1.5 * run(R, gitlab)` fits the job timeout
(default 15 minutes), and when its job waits more than 60 s before starting it
cancels it, reports `{"error":"runner"}` and backs off for 10 minutes. A job
of another user that arrives while ours runs waits at most for our time limit.

**Warm jobs.** Per repository a toolchain image in the project's container
registry, built by the backend when the toolchain changes; the GitLab job
cache keyed by repository on the runner host; fo's compiled objects as
versioned archives in the project's generic package registry, pruned to the
newest three per repository.

**Nothing left behind.** Branch and pipeline are deleted at the result; a
sweep removes older ones; the registry keeps the newest image per repository.

## Network allowlist

Everything fortyard runs automatically reaches the network only through an
allowlist. The goals: no unexpected site is reached; the usual sources of code
and packages work; access to the TU Graz network is the minimum each role
needs.

**Enforcement.** The server, runners and backend processes are on Docker
networks marked `internal`, which have no route out. fortyard's egress proxy
(squid, a container of fortyard's own, `deploy/egress/`) is on that network and
on a normal one, and is the only way out. Containers get no DNS server: names
are resolved by the proxy, which also keeps internal TU names unresolvable
from inside. The proxy, in this order:

1. allows the domains on the role's list (a domain includes its subdomains);
2. denies the TU Graz networks (129.27.0.0/16 and its IPv6 prefix), private,
   loopback and link-local networks, and requests to literal IP addresses,
   judged on the resolved address, so a public name that points into the TU
   network is denied too;
3. denies everything else and logs it.

Only ports 443 and 80 are allowed; the server's pushes to GitHub use
`ssh.github.com` on port 443.

**Lists per role** (`deploy/egress/<role>.txt`, one domain per line):

| Role | Allowed | TU Graz |
|---|---|---|
| executor | github.com, its download and content hosts, ghcr.io, pypi.org, files.pythonhosted.org, conda-forge and prefix.dev, the release hosts of fo and the toolchain | none |
| runner controller | the fortyard server | `code.tugraz.at` only, for runners on other hosts |
| GitHub backend | api.github.com | none |
| GitLab backend | gitlab.tugraz.at and its container registry | GitLab only |
| server | github.com, api.github.com, ssh.github.com | none |

A repository adds hosts its checks need (for example a compiler's download
host) through its policy (`egress=` in the repository policy), for its
executor only. Private repositories use the executor list without the
package hosts they do not name.

**Learning.** For one week after the proxy is introduced, the executor's proxy
allows public hosts that are not on its list and logs them; the server page
lists them, and a reviewed change adds the needed ones. After that week they
are denied. The TU Graz rule is strict from the start.

**What remains open.** Allowed sites that accept uploads (GitHub, PyPI) are
still ways to send data out. The executor holds no secret, and the code of a
public repository is public anyway; that is why private repositories get the
narrower list.

## Isolation

- **Own networks, Computor untouched.** The server, runners and backend
  processes leave `computor-network` (Computor's Docker network, which also
  holds Computor's database and identity provider). Traefik keeps reaching the
  server through fortyard's route file, now pointing to a port that a small
  relay container of fortyard publishes on the host; the relay is the only
  container on both a normal network and the server's internal network, and
  forwards that one port. The local runner no longer shares the server's
  network namespace (`network_mode: "service:fortyard"` today); it reaches the
  server over the internal network. No Computor container, network or
  configuration changes; Computor's own network separation is
  computor-org/issues#615.
- **Runner split.** A runner is two containers sharing a work directory: the
  **controller** holds the runner token, claims, fetches, uploads the live log
  and reports; the **executor** runs the check with no token, read-only root,
  no capabilities, limits on CPU, memory and processes, and the executor
  allowlist. Code under check has no way to the runner token.
- **Credentials by role.** Server: deploy keys (one repository each) and the
  client registration of its TU Graz login. GitHub backend: the App key.
  GitLab backend: its project tokens. No personal token, Kerberos ticket or
  AFS token in any container.
- **Every container** keeps a read-only root, no capabilities and no new
  privileges, as today.
- **Mac runners** are not containers; they keep their own sandbox
  (`FY_CHECK_WRAP`) and are not covered here.

Docker access on the build hosts is itself root-equivalent; it is the access
fortyard's operators already have and use, not something this design adds.

## Delivery

The items are MB1 to MB7 in [PLAN.md](../PLAN.md).
