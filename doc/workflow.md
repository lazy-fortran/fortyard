# Workflow: changes, review, agents and GitHub

How work moves through fortyard: how changes land, how they are reviewed, what
the background agents do, how everything stays auditable, and how fortyard
works together with GitHub. [DESIGN.md](../DESIGN.md) gives the architecture,
[Testing at scale](testing-at-scale.md) the test pool this document relies on.

## Principles

fortyard follows established practice, adapted to a setting where agents write
code faster than people can read it:

- **Trunk-based development and continuous integration.** Everyone integrates
  into `main` often; there are no long-lived branches.
- **The "not rocket science" rule.** `main` only ever moves to a state that
  passed its gates. The merge queue enforces this, as bors, Zuul and GitHub's
  merge queue do.
- **Merge first, review later** (Apache calls it commit-then-review, ZeroMQ's
  contribution contract optimistic merging). A change lands once its automatic
  gates pass; review follows and is tracked until done. This is a hard rule of
  fortyard, with one exception: changes with effects a revert cannot undo wait
  for a human (see [Review before merge](#review-before-merge)). Projects that
  want review before every merge stay on GitHub.
- **Revert first, fix forward later.** A landed change that breaks something is
  reverted at once, which is one step because every change is one commit.
- **Test what is affected now, everything eventually.** Pre-merge tests are the
  affected cases within a budget; the pool runs the rest continuously and
  traces every changed outcome to its commit.
- **Separation of duties.** The author of a code change never judges it. For
  agents this means that the model which implements a change is not the model
  which reviews or tests it. The rule concerns code changes; triage labels and
  issue gardening are not code and may be done by the reviewing model.
- **Attention is the scarce resource.** Writing code is cheap; human review is
  spent where risk and uncertainty are highest, and everything else is
  reviewed by agents.

## Changes

A change is fortyard's pull request: one commit's worth of work under review,
with a stable identity that it keeps when it is amended or rebased. A **stack** is a sequence of changes where each
builds on the previous one; each change of a stack has its own number, page
and review. Every push of a change creates a new **version** of it.

- **Opening.** Pushing a branch other than a protected one
  opens or updates a change as a draft; branches under `wip/` never open one.
  Marking a change ready (`fy change ready`, or the button) puts it into the
  merge queue.
- **Versions.** Every push creates a new version of the same change, matched by
  its `Change-Id` trailer, which `fy` adds through a commit hook. A commit without
  one is matched by its branch name. The page shows the difference between any two versions, and line
  comments carry over where the line still exists.
- **Page.** As on GitHub and GitLab: title and a required Markdown description
  (drafted by the review agent from the issue and the diff, edited by the
  author); tabs *Conversation*, *Files changed*, *Versions* and *Checks*;
  for a stack, the whole stack around the change.
- **Attachments.** Images, PDFs and data files can be added to the description
  and to comments, from the browser, with `fy change attach FILE` or through
  the MCP server. They are stored by fortyard, not in git, within a size limit.
  Test cases can attach plots automatically, for example a before/after plot
  for every change that touches them.
- **Numbering.** Issues and changes share one sequence per repository (`#123`).
- **Landing.** A change lands as one squashed commit; a stack lands as one
  commit per change. Each commit's subject ends in its own `(#123)`; the message
  carries its description and the trailers `Change: <URL>` and
  `Change-Id: <ID>` of that change. A branch that is `main` plus one merge
  commit lands as that merge, so a merged history such as an upstream's stays
  an ancestor of `main`; a merge onto an older `main` is refused and is redone
  on the current one.
- **Style.** Changed Fortran files must pass `fo fmt --check`, except those
  matched by a shell pattern in `.fortyard/format-exclude` (one a line), for
  code kept in another project's style.
- **After landing.** The change page stays live. Its review state is also
  written to a git note (`refs/notes/review`), so `git log --notes=review`
  shows it without the browser.

## The merge queue

The queue exists in fortyard-primary repositories. Ready changes line up and
are tested on top of the current `main`, several at a time: each is squashed onto
the candidate, and `main` is fast-forwarded to it. A failing batch is split, and a
failure is blamed on a change only when it reproduces with that change alone;
the others land in order. The gates are:

- build and the affected tests within a time budget;
- hard checks: no secrets, size limits on receive, no deleted or loosened tests
  or tolerances without a flag;
- for the areas listed under [Review before merge](#review-before-merge), an
  approval.

A change that fixes an urgent issue or unblocks others moves up the queue.
Urgency changes the order and, for a hotfix, the test depth, never another gate:
a person with the maintain role may declare a hotfix to a live service, which
then lands with the affected tests only; the full tests follow in the pool.

## Review

### States

A landed change is *unreviewed*, *AI-reviewed* or *human-reviewed*; separately,
it may be *flagged* for a human, and it may have been *reverted*. A change is
**significant** when it is flagged or when its repository's settings require
human review for its paths; others count as reviewed once AI-reviewed. The
**fully reviewed commit** of a repository is the newest commit up to which every
significant change is human-reviewed. With the **fully verified commit** (the
newest commit at which every test case passed, see
[Testing at scale](testing-at-scale.md#the-pool-and-its-scheduler)) it marks what
production runs and publications use. Both only move forward; a revert is a new
commit and is reviewed like any other.

### Review before merge

Some effects cannot be undone by a revert. Changes touching them wait for the
approval of a person with the maintain role before landing. Each repository
lists the paths concerned as patterns in its settings, with defaults for:

- secrets and credentials;
- CI, deploy and runner configuration, and anything that runs with credentials;
- file formats and database migrations that write persistent data;
- releases and tags;
- permissions and authentication;
- the agents' own instructions, settings and sandbox.

Agent-authored changes to these areas always wait for a human, whatever the
settings say.

### Guardrails for merge first

1. **Silently wrong results.** Changes to numerics or physics (solvers,
   discretisations, source terms, results moving beyond their tolerance) are
   always flagged for a human, and golden records and conservation checks run
   in the pool.
2. **Irreversible effects** are reviewed before merge, as listed above.
3. **What runs in production or goes into a paper** comes from the fully
   verified and fully reviewed commit; outputs always record their commit.
4. **Backpressure.** When the unreviewed significant changes of a repository
   exceed a limit (a repository setting, by default 20 changes), new changes
   there no longer land automatically until review catches up. Reverts and
   fixes of flagged changes are exempt.

### Review is focused testing

A review reads the code *and* uses the software: it checks that the issue is
solved, runs the changed path and looks around it. Exploration (below) is the
unfocused counterpart.

### When an agent flags a change for a human

- numerics or physics changed;
- public interfaces or file formats changed;
- tests deleted or loosened;
- large deletions;
- security-relevant code;
- reviewers disagree, or the reviewer's confidence is low.

People spot-check a random sample of changes the agent did not flag, which
measures whether the flagging can be trusted.

### The human review queue

- **Blocking cards** (review before merge) come first; below them the
  post-merge queue, ordered by review need.
- **One card per change:** what was intended, why it was flagged, the hunks
  that matter (mechanical hunks folded and labelled), the evidence (tests,
  metrics and plots before and after) and the decision asked: *approve*,
  *revert* or *open an issue*.
- **A time budget instead of a list.** The reviewer states the available time;
  each card carries an estimated reading time, and the queue picks the most
  valuable cards that fit, grouped by topic.
- **Nothing starves.** Priority grows with waiting time; a high-risk change
  unreviewed beyond a limit sends a reminder.
- **Routing by area.** Cards go to the people who maintain the area, from an
  owners file in the repository (as CODEOWNERS on GitHub and GitLab).

## Triage: three separate priorities

Every issue and change is triaged by an agent when it is filed or pushed. Each
label carries a one-line reason, and people can override it.

1. **Fix urgency** of an issue. P0: wrong results, data loss, a security hole,
   a live service down. P1: a broken workflow without workaround. P2: a
   nuisance with a workaround. P3: nice to have.
2. **Merge order** of a change: first come, first served, except that changes
   fixing P0 or P1 issues or unblocking others move up.
3. **Human-review need** of a change: consequence (results, data, security,
   people's work), uncertainty (reviewer confidence, test coverage of the
   change) and reach.

The three are independent: an urgent fix of an error message needs no human,
a quiet refactor of a solver does. Every override is recorded with the
original call; the share of overridden calls measures the triage.

## Issue gardening

Agents keep the issue list current. Because a wrongly closed issue is lost
silently, they close only with evidence, every close is reversible, and the
confidence decides whether they act or only propose.

- **Closed directly, with evidence:** fixed (a landed change references it and
  its reproduction now passes); duplicate (linked, comments moved); obsolete
  (the code it concerns was removed, with the commit); no longer reproducible
  (two independent runs on current `main`).
- **Proposed only:** everything less certain gets the label *close candidate*
  with its reason; author and watchers are notified; it closes after a waiting
  period unless someone comments.
- **Never closed by an agent:** issues with recent human discussion, assigned
  to a person, labelled P0, security issues, issues from outside contributors.
- **Recovery:** closing never deletes; a repeated report reopens the old issue;
  a weekly digest lists all agent closes by reason. If reopens for a reason
  exceed a threshold, that reason falls back to proposing only.

Gardening also relabels, re-prioritises, merges scattered reports and links
issues to the changes that touch them.

## Exploration

Agents use the software as users would and file what they find. Each run
takes one angle with a short checklist, and the scheduler rotates the angles:
build and operation, code audit, user and newcomer, documentation,
architecture.

- **Web services:** browser sessions on a staging or development instance,
  never production, in
  different roles (student, tutor, administrator, phone user).
- **Tools and libraries:** follow README and tutorials, run the examples, try
  odd inputs and misuse, judge the error messages.
- **Scientific codes:** small local cases only, never cluster jobs: input
  variations, analytic limits, convergence under refinement, presets that
  should agree, odd but valid parameters, steered through parameter space
  quasi-randomly.
- **Documentation is tested as code:** every example and command in READMEs
  and tutorials runs as a test case; a newcomer run follows the README from
  scratch in a clean environment; a change to a public interface without a
  matching documentation update is flagged in review.

Rules:

- **No report without reproduction.** A bug report needs a script or input
  that shows the bug; fortyard runs it on current `main` before opening the
  issue. What does not reproduce goes to a low-priority list of suspicions.
- **No duplicates:** a repeat becomes a comment on the existing issue.
- **Every fixed bug leaves a test:** the reproduction becomes a regression
  test when the fix lands.
- **No changes during exploration;** even a typo is filed, so author and judge
  stay different models. Review runs are separate from exploration runs: a
  review uses the software only to judge one change.
- **Coverage is recorded** (pages, commands, parameter ranges and when they
  were explored), and exploration steers towards gaps.
- **Budget:** a fixed share of idle capacity, small memory caps, no cluster;
  exploration yields to tests and reviews.

## Agents

### One queue

All agent work goes through one queue with leases, time budgets, priorities and
fair share from the same capacity as the test pool. Its kinds are *triage*,
*review*, *garden*, *explore* and, later, *implement*. The instance starts with
a single background agent on a local model that takes all kinds except
implement; implementation stays in interactive sessions until a second model
is available, so that implementer and reviewer differ.

Implementer, reviewer and tester are separate roles. One model may review and
test; it may not judge changes it wrote. Every action records model and
instruction version.

### Models

- Local models through the instance's OpenAI-compatible gateway are the
  default.
- An external model is allowed per repository only where the repository's
  settings permit it; the setting defaults to local models only. Repositories
  with non-public data or collaborators' unpublished code keep that default.
- Unattended use needs credentials that permit it: an API key, or an
  enterprise access token where the provider offers one. Personal
  subscriptions of individual users are not used by the queue.

### Sandbox

Agents run code they did not write. Every task runs in a fresh container that
is discarded afterwards:

- hard memory and CPU limits;
- no credentials inside except the agent's scoped bot token; the agent acts
  only through fortyard's API, which performs every push and merge on its
  behalf;
- network only to the model gateway and the fortyard API;
- the repository read-only except the task's working copy.

The sandbox gets its own document (`doc/agent-sandbox.md`), written for an
institutional security approval of AI agents.

### Safety

- **Untrusted input.** Issue texts, comments and outside contributions are data,
  never instructions. Agents propose actions; fortyard checks the evidence
  itself before executing them.
- **Identity.** Each agent has its own visible bot account with minimal rights.
- **Kill switch and rate limits:** one switch pauses all agents; daily caps per
  agent for closing, commenting and opening issues.
- **Notification budget:** agent actions go into digests; only blocking items
  and P0 notify at once.
- **Loop limit:** after three rounds of review and fix on one change (a round
  is one review followed by one new version) it goes to a human and waits.
- **No outward actions** (upstream issues or pull requests, mail, posts on
  other forges) without a person's approval. The mechanical GitHub
  synchronisation described below is fortyard's own, not an agent's. Each repository's contribution
  conventions apply to agents.
- **Asking instead of guessing:** missing information sets an issue to *needs
  info* with a question to its author.
- **Shared model capacity:** interactive use of the local models comes first;
  the queue takes what is left.

### Instructions and settings

Instruction files (as `AGENTS.md`) and settings exist per instance, group,
subgroup and repository (groups as in
[DESIGN.md](../DESIGN.md#identity-and-authorization)). Instructions combine from the top down, the most
specific last. Settings choose which agent kinds run, which models may see the
code, budgets, whether agents may close issues, notification limits and review
thresholds. Safety-relevant settings can only be tightened further down, never
loosened: fewer agent kinds, fewer models, smaller budgets and limits, more
human review.

### Quality and learning

- **Benchmark.** A fixed set of past issues, reviews and bugs with known
  answers. A model or instruction change is measured on it before use.
- **Outcomes.** Every agent run keeps its transcript, compressed and within a
  retention period, linked to its eventual outcome: was the reviewed change
  reverted, the closed issue reopened, the flagged change important, the
  priority overridden?
- **Self-improvement with a brake.** An evaluation agent mines the outcomes
  and proposes changes to instructions and thresholds. A proposal is measured
  on the benchmark, lands as a change to the instruction files and always
  needs human review before merge. The system never changes its own
  permissions, sandbox or budgets.

## Audit

Every action of people, agents and fortyard itself (merges, closes, labels,
priorities, review verdicts, promotions, ingests, setting changes) goes into an
append-only log with actor, model and instruction version where applicable,
input and evidence. Each entry carries the hash of the previous one, so later
tampering is detectable; the newest hash is published daily outside the
instance (in a signed tag pushed to the GitHub mirror), so even someone with
database access cannot rewrite the past unnoticed. An auditor can export the
log for a repository or a period.

## Live services

Merging and deploying are separate steps.

1. Every merge deploys automatically to a **staging** instance with test data,
   where testers work.
2. **Production is promoted:** a person chooses a commit that is fully
   verified, whose significant changes are reviewed, and that testers signed
   off on staging, recorded as an approval on that commit (continuous
   delivery, not continuous deployment). A failed health check on staging
   rolls staging back, as in production.
3. **Rollback** is atomic to the previous immutable release, automatically on a
   failed health check. Database migrations are split into expand and contract
   steps, preceded by a backup, and reviewed before merge.
4. **Feature flags** keep unfinished features on `main` but off in production.
5. **Deploy freezes** from the service's calendar (exams, deadlines) block
   promotion; a security
   fix may pass with explicit confirmation.

## GitHub

Repositories that do not move to fortyard stay entirely on GitHub with
GitHub-hosted runners. Self-hosted GitHub runners are retired once fortyard
runs the CI of all repositories that move to it (milestone M8 in
[PLAN.md](../PLAN.md)). For repositories with a GitHub mirror:

- **Code: fortyard to GitHub, one way.** fortyard pushes `main`, tags and
  releases; branch protection on GitHub refuses other pushes.
- **Pull requests on GitHub are never imported and merged automatically.**
  fortyard lists them read-only as external proposals. A person *ingests* one
  after checking it: the ingest is pinned to the commit that person checked and
  refuses if the pull request's head changed since, including by a rebase; fortyard creates a change with the
  external author as commit author, runs its tests in the sandbox without
  credentials, and the person's check counts as review before merge. After
  landing, a bot comments with the commit and closes the pull request.
- **Issues are synchronised both ways.** Each GitHub issue is linked to one
  fortyard issue, which holds the full record. Comments, state and labels are
  mirrored with their original authors; when title or text changed on both
  sides between two syncs, the newer edit wins and the other stays in the
  history; conflicts of state and labels favour fortyard. Security reports go
  through GitHub's private vulnerability reporting
  and become private fortyard issues.
- **Tags and releases: fortyard to GitHub only.**
- **Polling until reachable.** While fortyard is reachable only inside the
  WireGuard network, it polls GitHub about once a minute with conditional
  requests; once it is reachable from outside, GitHub webhooks replace polling.
