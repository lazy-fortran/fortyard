# Testing at scale

This document describes how fortyard tests large scientific codes with
thousands of tests, many of them expensive, on limited shared hardware. The
target is **eventual consistency for regressions**: every regression is found,
attributed to the change that caused it and reported to its author, human or
agent, within days, without any machine running out of memory and without
testing everything on every change.

## Terms

- **fo** is the lazy-fortran build and test driver for fpm and CMake projects
  ([fo](https://github.com/lazy-fortran/fo)). It builds, lists, runs and
  measures tests; fortyard plans and schedules them.
- A **test case** is the unit fo runs and reports. An executable that follows
  fo's case listing protocol lists its cases and may narrow each case's inputs
  to the project files it actually uses; other executables are one case.
- A **runner** is an `fy runner` process on a build host that executes cases
  for the server. A **runner class** is a set of runners with the same
  capabilities, for example ordinary, large memory, quiet (for benchmarks) or,
  later, cluster.
- A **trusted result** is one produced by a runner for a commit pushed by a user
  with write access or by fortyard itself (which pushes agents' commits), on any
  branch. Results from contributors without write access and from developer
  machines are **untrusted**: they are shown, but never reused or counted as
  verification.

## The problem

A scientific code that is developed fast and with agents accumulates tests
quickly: unit tests, convergence studies against analytical limits, benchmarks
against other codes, golden records (stored reference outputs of deterministic
runs, compared on every rerun) and performance measurements. Once the whole
suite takes longer than a night, three things break:

- running everything on every change is impossible;
- a nightly run of everything is impossible too, and splitting it by hand over
  several nights leaves gaps nobody tracks;
- a test that is run rarely drifts: when it finally fails, nobody knows which of
  hundreds of changes broke it.

Memory, not CPU, is usually the scarce resource. A few large tests started in
parallel can exhaust a machine's memory, make it swap, and take down the
desktop or the host of the forge.

## How large projects handle it

| Project | Approach |
|---|---|
| GCC | The DejaGnu suite (the gfortran directory alone holds about 9,000 files in a current checkout) produces a list of pass and fail results per run; regressions are found by comparing that list with a baseline run. Many independent testers run after commits and report to the mailing lists |
| LLVM | Fast `lit` tests before merge; after merge, build bots email the authors of the commits since the last green run; LNT records performance time series |
| LFortran | Thousands of stored reference outputs (the golden-record pattern) and thousands of integration tests over several backends; a quick CI tier on every pull request, an exhaustive tier on `main` or when a pull request carries a label |
| Google (TAP) | Tests affected by a change are selected through the build dependency graph. After submission, tests run in batches on the newest commit; when a batch fails, the culprit is found by bisecting the commits in the batch |
| Mozilla | Test selection from historical failure data; skipped jobs are backfilled when something fails, to find the culprit; Perfherder detects performance changes and raises alerts |
| Linux kernel (0-day) | Tests patches posted to the mailing lists and branches, bisects many regressions including performance ones, and emails the author of the culprit commit |
| Rust | A merge queue tests each pull request (small ones are batched into rollups); compiler performance is tracked with instruction counts as the primary metric, because they vary far less than time |
| Rust (crater) | A compiler change is tested against the packages that use it: each package is built and tested with the baseline and the candidate compiler, and only a difference between the two counts as a regression |
| Debian, Ubuntu | A package moves into the testing distribution only after the test suites of the packages that depend on it pass with the new version |

The common pattern: select what a change can affect, test the rest later in
batches, and when something fails, find the culprit automatically and tell its
author. fortyard builds that pattern around Fortran and fo.

## Principles

1. **A test result is keyed by its inputs.** A case is rerun only if something
   it depends on changed. A trusted pass is valid for every commit with the same
   inputs.
2. **Each test has a known state at a known commit.** fortyard knows, for every
   case, the newest commit at which it was run on `main` and with what result.
   Expensive cases advance less often, but they always advance.
3. **Failures are bisected, then reported.** fortyard finds the first commit
   that changed a case's outcome and notifies its author. For agents, a
   regression-fix task is opened automatically.
4. **Resources are admitted, not hoped for.** A case or build starts only when
   the memory and CPU it is known to need are free, and it runs under hard
   limits, so an overrun stops that case and nothing else.
5. **Everything is measured on every run.** CPU time, wall time, peak memory and
   bytes written are recorded for every case run, which gives cost estimates for
   scheduling and time series for performance drift at no extra cost.

## Result keys and affected tests

fo already keys passing results by the **input closure digest** of each case: a
hash over the project files the case depends on, bound to the toolchain, the
driver and every input outside the project. A case whose digest matches its
recorded pass is reused and not run. If the closure is incomplete or unknown,
the digest is empty and the case always runs.

fortyard adds the **runner environment** to the key: the runner image version
and the instruction-set class of the hardware, because both can change results
without any commit. It then extends reuse from one machine to all machines:

- Runners report each result with its key. A trusted pass is valid for any
  commit or branch with the same key, so a case whose inputs did not change over
  fifty commits runs once, not fifty times.
- Before running a case locally, fo asks the server whether a trusted pass for
  that key exists and skips the case if so.
- Data files used by tests, such as golden records, are part of the closure by
  their content hash, whether they live in the repository or in a data
  repository.

The digest makes "test only what is affected" exact rather than heuristic,
provided the closure is right. Closures can be wrong, for example when a test
reads a file it does not declare. Two mechanisms catch this:

- **Audit runs.** A small share of capacity reruns randomly chosen cases whose
  key has not changed, at the newest commit. A failure there means the closure
  is missing an input or the case is not deterministic; it is reported as such,
  and the case is bisected over all commits since its last run instead of over
  closure-changing commits only.
- **Maximum staleness.** Every case is rerun after a maximum time even if its
  key did not change, for example seven days for golden records.

## Downstream consumers

A library change can leave every test of the library green and still break a
code that uses it. libneo shows the problem and an existing answer: its nightly
run dispatches the workflows of six consumers (SIMPLE, NEO-2, MEPHIT, KAMEL,
rabe, NEO-RT) against the candidate libneo commit, listed in its
`ci/downstreams` registry. fortyard makes this part of the pool, so that
consumers are tested continuously, within the same capacity limits, and
without repeating work.

**The unit of verification is a test case against a dependency snapshot.** A
case's key covers the consumer's own inputs and the resolved revision of every
dependency it is built against, with toolchain and runner environment as
before. The repositories on an instance form a versioned dependency graph,
read from `fpm.toml`, CMake `FetchContent` declarations and, where neither
exists, a registry like libneo's.

**Consumer cases join the pool.** When `main` of a library moves, every
consumer case whose key changes with the new library revision becomes a
candidate, tested at the consumer's current `main` with only that dependency
moved to the library's new `main`. The scheduler treats these cases like the
library's own: same priorities, staleness limits, fair share and memory
admission. The library's project is charged for them, so a widely used library
cannot crowd out the consumers' own testing. A capacity limit still holds: what
does not fit waits and grows stale, which the instance shows, instead of
running anyway.

**No duplicate work.** The key makes repetitions identical, so they run once:

- A consumer case that depends only on library modules the change did not
  touch keeps its key and is not rerun. fo's closure covers the files a case
  uses, not the whole library.
- A run with the library at revision R and the consumer's own run once it pins
  R have the same key; whichever comes first serves both.
- Consumers are tested against the newest library `main` only, which covers all
  library commits since their last run. Bisection over library commits happens
  only when an outcome changed.
- External downloads (source archives, data files) are cached by content on
  the instance, so a test does not depend on a remote server being up, and
  fetching it is no part of the measured work.

**Outcomes are classified before anyone is blamed.**

| Outcome | Meaning |
|---|---|
| PASS | the case passed |
| REGRESSION | fails with the new library revision, passes with the previous one, all else fixed, reproducibly |
| BASELINE_BROKEN | fails with the previous library revision too: the consumer or its other dependencies are at fault, not the library |
| INFRA_ERROR | the run failed for reasons outside the code: a download error, a runner lost or out of memory, a timeout of the platform |
| FLAKY | passes and fails at the same key |
| NOT_TESTED | no result at this key yet |

Before a consumer failure is reported against a library, the case reruns with
the previous library revision in the same environment. Only a failure that
appears with the new revision alone is a regression of the library; it is then
bisected over library commits while the consumer and all other dependencies
stay fixed, and reported to the library change's author. A broken baseline goes
to the consumer instead; an infrastructure error is retried and reported to the
instance's operators. For numerical codes the comparison includes the measured
metrics and golden records, not only pass or fail. A download failure such as
a server answering 504 is an INFRA_ERROR, never a red library.

**Where consumer tests run in the workflow.**

| Stage | Consumer cases |
|---|---|
| Before landing | none by default; for a change to a public module interface (fo sees changed `.mod` files), a few cheap smoke cases of the most affected consumers |
| After landing | every affected consumer case, by priority |
| Continuously | stale and flaky cases, audit runs |
| Stable release | designated consumers pass at the release's snapshot |
| Reference release (for publications) | a reproducible snapshot with the numerical benchmarks of the designated consumers |

**Consumers outside the instance.** A consumer that stays on GitHub can still be
tested: the instance mirrors it and runs its cases like those of its own
repositories. libneo's registry is the first one imported.

## Discovered tests and declared policy

**Design rule:** everything fo can discover is discovered, everything that can
be measured is learned, and only what cannot be inferred reliably is
configured. An ordinary project needs no CI file.

Two things are kept apart:

- **Test definition** (what a test is, how it is built, what counts as a pass)
  belongs to the project: `fpm.toml` and the test tree, or CMake and CTest
  (`add_test`, its labels and properties). fo discovers it (`fo test --list
  --json`, later the plan of [What fo needs](#what-fo-needs)).
- **Execution policy** (when a case runs, on which runner class, with which
  data, and whether it gates a landing or a release) belongs to fortyard, with
  defaults: affected cheap cases before landing, everything else in the pool,
  resources measured from earlier runs.

CTest properties are used as hints where they exist (`LABELS`, `TIMEOUT`,
`PROCESSORS`), but they cannot provision a cluster allocation or fetch
authenticated data. Such cases get a short policy entry, in `[extra.fortyard]`
of `fpm.toml` or in `fortyard.toml` for CMake projects; for example (format to
be fixed when it is built):

```toml
[tests.neo2_golden_ql]
runner = "slurm-reference"     # a runner class of the instance
cpus = 8
data = "plasma-data/golden"    # a data repository with a pinned revision
schedule = "pool"              # never before landing
required_for = "stable"        # gates stable and reference releases
```

The numerical comparison, tolerances and fixtures stay in the scientific code;
fortyard knows nothing about HDF5 files or physics.

**NEO-2 as the hard case.** NEO-2's unit tests are CTest tests that fo runs
today. Its golden-record comparisons are not: `ci/golden.sh` and its workflows
fetch private reference data (with Git LFS) from a data repository, build the
candidate, the base and the stable release, run on a reference CPU allocation
of a cluster, compare against stored records with separate tolerances, and
record timings and memory. To join the pool, each comparison is registered as
a CTest test that calls the existing scripts, and gets a policy entry like the
one above. Its result key then covers the NEO-2 commit, every dependency
revision (libneo among them), the revision and content hashes of the golden
data, the toolchain and the reference environment, so a changed outcome is
attributed to the right one of them.

| Change | Before landing | After landing |
|---|---|---|
| Ordinary numerical change | affected unit tests | golden comparisons in the pool |
| Change to a core solver | unit tests and fast physics checks | golden comparisons at high priority |
| Intentional change of golden records | human review of the changed records and policy | full golden verification, provenance archived |
| Stable or reference release | — | promotion waits for the designated golden suite |

**Deployment stays explicit.** Where a program is installed, with which
credentials, and when it is promoted cannot be inferred from a manifest; that
is configured per service (see [Workflow](workflow.md#live-services)).

## Tiers

Cases are not assigned to tiers by hand. Each case has a measured cost and
optionally a maximum staleness, and the scheduler decides where and when it
runs.

| Tier | Where | When | What |
|---|---|---|---|
| Edit loop | Developer machine, fo's continuous watcher | On every save | Affected fast cases within the local resource budget; results untrusted |
| Pre-merge | Runners | On every push to a pull request and in the merge queue | Affected cases up to a time budget per change, cheapest and most failure-prone first; the rest goes to the pool |
| Pool | Runners, idle capacity | Continuously, more at night | Every case whose state on `main` is behind, by priority |
| Heavy | Large-memory runners; later a cluster | When capacity allows | Expensive studies and benchmarks, same queue, own capacity |

There is no separate nightly suite. The pool runs all day on whatever capacity
is free and gets more capacity at night. A suite that needs five nights is a
pool whose cases advance over five nights, and fortyard shows which cases are
behind and by how much.

## The pool and its scheduler

A case is a candidate when its key at the newest commit of `main` differs from
the key of its last trusted run, when its maximum staleness is reached, or when
it is drawn for an audit run. The pool runs candidates at the newest commit of
`main`, which covers all commits since the last run in one run.

Priority grows with:

- **staleness** relative to the case's maximum staleness;
- **failure likelihood**, estimated from the case's history and how recently
  its inputs changed;
- **inverse cost**, so many cheap cases are not stuck behind one expensive one.

**Fair share.** Usage per user and per project is accounted with exponential
decay, as Slurm's fair-share scheduler does. A project or user that used much
capacity recently gets lower priority until the decay catches up, so no single
suite or agent swarm takes everything.

**Fully verified commits.** The newest commit at which every case of a
repository has passed is its fully verified commit. Releases of scientific
codes, for example versions used in publications, can be required to be fully
verified.

## Culprit finding and notification

A case's outcome is its verdict plus, for failures, a failure signature (the
failing check and message, or the metric that left its tolerance). When the
outcome at the newest commit `B` differs from the outcome at the case's previous
run at commit `A`:

1. List the commits between `A` and `B` that changed the case's key, computing
   the key at each commit from fo's test plan for that commit (a source scan, no
   build). Commits on `main` all passed the merge queue, so each one builds;
   a commit that does not build anyway is skipped, as `git bisect skip` does.
   If the key did not change at all (an audit or staleness run), list all
   commits, and if the runner environment changed, test the old commit on the
   new environment first.
2. Bisect over that list, rerunning the case at most about `log2(n)` times.
3. Report the first commit with the new outcome: a regression issue in
   fortyard with the failure, the bisection evidence and the commit's author,
   assigned to them. If the author is an agent, fortyard opens a regression-fix
   task for it.
4. Keep running the case on `main` while it fails. A changed failure signature
   is a new outcome and is bisected and reported separately, so a second
   regression in an already failing case is not hidden. The issue is closed
   automatically when the case passes again.

**Intermittent failures.** fortyard estimates each case's pass rate from
repeated runs. A case that passes on a rerun is marked flaky, but a significant
drop of its pass rate between two commits is itself an outcome change: it is
bisected with several runs per step, enough to tell the two pass rates apart.

## Memory and CPU

- **Measured cost.** Every case and every build step runs in its own cgroup, and
  its peak memory is read from the cgroup's peak counter (`memory.peak`), which
  includes child processes, MPI ranks and memory-backed scratch files. CPU time,
  wall time and bytes written are recorded too. The scheduler uses a high
  percentile of recent runs as the expected cost.
- **First runs.** A case without history runs alone on a large-memory runner the
  first time. On a development instance, which has no large-memory runner, it
  runs alone under the local budget and is skipped with a notice if it exceeds
  it.
- **Admission control.** A runner has a memory budget and a CPU budget below the
  machine's totals. It starts a case or build only if the expected memory fits
  in what is left of the budget; CPU is packed the same way. fo applies the same
  admission locally, so a developer machine is never overcommitted by builds or
  tests.
- **No starvation.** When a large case has waited longest, the runner reserves
  memory for it and starts only those smaller cases expected to finish before the
  reservation is due, as Slurm's backfill scheduling does.
- **Hard limits.** Each cgroup gets a memory limit of the expected peak plus a
  margin, with swap disabled. A case stopped at its limit is retried once
  alone on a large-memory runner with a higher limit before any verdict; only a
  case that also fails there is reported as out of memory, with its measured
  peak.
- **Threads.** Cases using OpenMP declare or are measured for their thread
  count; the runner sets `OMP_NUM_THREADS` and accounts for the threads in the
  CPU budget.
- **Requirements.** Per-case cgroups inside runner containers need cgroup v2
  delegation to the container. Instruction counts need permission to use
  hardware performance counters (`CAP_PERFMON` or a suitable
  `perf_event_paranoid` setting); without it, only times are recorded.
- **The forge host never runs builds or tests.**

## Disks

Repeated builds and test outputs wear solid-state disks. fortyard keeps writes
low by design:

- **No fresh clones.** Each runner keeps one bare mirror per repository and
  creates worktrees from it. Builds reuse fo's content-addressed cache, so an
  unchanged object file is never compiled or written again.
- **Persistent build trees for trusted jobs.** Jobs for users with write access
  reuse an incremental build tree. Only untrusted jobs get an ephemeral
  container with a fresh tree.
- **Outputs are discarded unless needed.** Scratch directories are deleted after
  a pass; failures keep their outputs for a limited time. Small scratch
  directories go to memory-backed storage when they fit in the case's memory
  limit, where they are charged.
- **Logs are short.** A passing case keeps a short summary; a failing case keeps
  its full log, compressed.
- **Writes are measured.** Bytes written per case are recorded, and cases that
  write much more than they need are reported to their owners.
- **The database writes in batches.** Results are committed in batches, not one
  transaction per case.

## Performance regressions

Two mechanisms, from cheap and noisy to expensive and precise:

1. **Drift from ordinary runs.** CPU time and peak memory of every case run form
   a time series per case. A sustained shift, not a single outlier, raises a
   performance alert.
2. **Benchmarks on quiet runners.** Cases marked as benchmarks run on the quiet
   runner class with pinned cores and nothing else running, repeated several
   times. Where permitted, instruction counts are recorded besides time.

A performance change beyond the measured noise is bisected like a failure, with
several runs per step.

## Accuracy and golden records

Scientific tests report numbers, not just pass or fail. A test can print metric
lines of the form

```text
fo-metric NAME VALUE UNIT
```

for example the error against an analytical limit, a difference to another
code's benchmark result, or time to reach a given accuracy. fortyard stores
these as time series per case. Besides the test's own pass threshold, a trend
alert fires when a metric drifts steadily while still within tolerance, which is
the drift that rarely run comparisons used to miss.

Golden records are compared by a Fortran tool with exact or tolerance-based
comparison per quantity. A deliberate change of records goes through review like
any other change, so the history shows when and why reference outputs changed.

## What fo needs

fo already has the input closure digests, the pass cache, test cases with input
narrowing, CPU-time budgets, retries with a FLAKY verdict, partitions, JUnit
output and JSON reports. It needs:

1. **A test plan.** `fo test --plan --json` lists every case with its digest,
   labels and measured cost, from a source scan without building or running,
   for fpm and CMake projects alike.
2. **Running a given set of cases** from such a plan, reporting results with
   their digests.
3. **Resource metering and limits** per case and per build step: cgroups, peak
   memory, bytes written, optional instruction counts, and memory admission.
4. **The `fo-metric` protocol** collected into its reports.
5. **A remote result lookup** for trusted passes by key, next to the remote
   build cache.

6. **Dependency overrides:** building and testing a project with one dependency
   moved to a given revision, all others fixed, with that revision in the key.
7. **Interface changes:** which public module interfaces a change alters, to
   choose consumer smoke cases.
8. **Test properties in the plan:** CTest labels, timeouts and processor counts,
   and the policy entries of `[extra.fortyard]` or `fortyard.toml`, reported
   with each case.
9. **Running as process 1 of a container.** fo fails to start compilers when it
   is the first process of a container (its process-group setup ends in
   status 126); until fixed, containers start fo under `--init`.

## Large CMake codes

A large code with hundreds of CMake test targets, where one target often runs
many cases, benefits most from finer cases. With fo's case listing protocol a
target lists its cases and narrows each case's inputs, so a change to one module
reruns the cases that use it rather than whole targets.

## Clusters

A cluster is another runner class: a runner that submits a batch of cases as
one job through the cluster's scheduler with a memory limit and reports the
results like any other runner. It is disabled by default and has its own small
fair-share quota.
