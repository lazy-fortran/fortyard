# Parallel development

How one repository takes the work of many agents and people at the same time
and still lands every finished change within minutes. [Workflow](workflow.md) gives the
rules for changes, review and agents, [Testing at scale](testing-at-scale.md)
the tests, [DESIGN.md](../DESIGN.md) the architecture.

## The problem

One person steers 5 to 20 coding agents. Each implements one feature in its own
worktree, in the same repository and often in the same files, and each finished
change should be on `main` and deployed within minutes. Three things make them wait for one another. A branch that no longer merges with `main` is
sent back to its author, who merges and tests again by hand, so N changes to
one file need N rounds. Two branches both take "the next" of something that is
ordered by number, such as a schema version. And nobody sees what the others
are changing until a landing fails. The rate of landing is then set by
hand-overs to authors, not by the runners.

## Principles

1. **The server merges, the gate decides.** Every landing is merged onto the
   newest state by the server, and only a runner's result on exactly the tree
   that becomes `main` counts. Authors do not merge and test again by hand.
2. **A conflict is data for its author.** It names the paths and the other
   change and arrives in the answer to a call the session makes anyway (`fy push`,
   `fy land`).
3. **A change answers only for itself.** It is rejected when it fails on a
   base that passed without it, never because of a neighbour in the queue.
4. **Nothing that orders `main` is chosen on a branch:** no number, no
   position in a list.
5. **Coordination is advisory and derived from pushed work.** Nothing a
   stopped session holds can block another.
6. **State is rows and refs.** A deploy restarts the server after every
   landing, so every step of the queue can be repeated after a restart.
7. **One operation, three surfaces:** API, `fy` and MCP, with the same names.

## The life of a change

A **session** is one agent or person working in one worktree. Each session has
its own token (`fy token create agent-3 --scopes write,land`, given to the
agent as `FY_TOKEN`), and the token's name identifies the session in every
list. The scope `write` allows pushing, testing, commenting and claiming,
`land` allows queueing a landing, and `admin` stays with the person. Roles per
group and repository ([DESIGN.md](../DESIGN.md#identity-and-authorization))
later decide where a session may use them.

1. **Claim.** `fy issue claim N` marks the issue as taken by the session.
2. **Push early.** The first commit is pushed (`fy push`), which opens a draft
   change with its number; every further commit is pushed too. Each push
   answers with the change's mergeability and the open changes it conflicts
   with (see [Coordination](#coordination)).
3. **Test, when wanted.** `fy test` checks the pushed version on a runner
   without landing it.
4. **Land.** `fy land` pushes, pins the pushed commit as the version to land,
   queues an **entry** for it and follows it. A change has at most one live
   entry.
5. **Queue.** The lander, the server process that lands entries, squashes the
   entry onto a candidate, a runner checks
   the candidate, and `main` moves to it (see [The queue](#the-queue)).
6. **Deploy.** `main` is pushed to the repository's mirror on another forge.
   Where an instance follows its own `main`, as fortyard's does, the release
   builder (the process that builds a release from `main`) builds the newest
   `main` only, so the changes of one candidate share one build, and the
   instance switches to it; promotion to a production service follows
   [Workflow](workflow.md#live-services). The change page names the first release that
   contains its commit.
7. **Review** follows landing ([Workflow](workflow.md#review)). Several reviewer workers (processes of the review agent) take landed
   changes, each change exactly once, so the changes of a full candidate are
   reviewed in parallel.

`fy land` and `fy test` end with a status that tells the session what to do:

| Status | Meaning | Next step |
|---|---|---|
| 0 | landed, or the test passed | done |
| 1 | rejected: the check, the format or the size limit failed for this change alone, or it adds nothing; the log follows | fix, `fy land` |
| 3 | conflict; the rebase task (what to merge with, see [Conflicts and rebase tasks](#conflicts-and-rebase-tasks)) follows as JSON | `fy sync`, resolve, `fy land` |
| 4 | still queued or running after `--timeout` (8 minutes by default, below the limit agent harnesses put on one command) | call again; it resumes following |

Status 2 is a usage error. A session's work on a change is finished when the
change is landed, not when `fy land` was called. `ID` below is the job number
that `fy land` and `fy test` print.

| Operation | `fy` | API under `/api/v1/repos/R` | MCP action |
|---|---|---|---|
| Queue a landing | `fy land [BRANCH] [--stack] [--urgent]` | `POST /changes/N/land` | `change_land` |
| Withdraw it | `fy land --cancel N` | `DELETE /changes/N/land` | `change_unland` |
| Follow an entry or test | `fy job ID --wait` | `GET /api/v1/jobs/ID` | `wait` |
| Test a version | `fy test [BRANCH\|N]` | `POST /changes/N/test` | `change_test` |
| Conflicts with others | `fy overlap [N] [--paths P...]` | `GET /changes/N/overlap`, `GET /overlap?paths=` | `overlap` |
| Claim an issue | `fy issue claim N`, `fy issue release N` | `POST`, `DELETE /issues/N/claim` | `issue_claim`, `issue_release` |
| Merge `main` or a parent into the branch | `fy sync [--onto M] [--continue] [--land]` | the commit to merge with, from `GET /changes/N` | none (local git) |
| Board | `fy board [--stats]` | `GET /board` | `board` |
| Settings | `fy repo set R KEY VALUE` | `PUT /settings/KEY` | none (admin) |

## The queue

### Entries

An entry is the request to land one version of one change. It is `queued`,
`waiting` (with the reason: an entry ahead that it conflicts with, or a parent
that has not landed), `running` (member of a candidate), and ends as `landed`,
`failed` (with the reason `conflict`, `check`, `format`, `size` or `empty`) or
`cancelled`. An entry that failed for any reason but `conflict` is called
**rejected**. Holds from [Workflow](workflow.md#review-before-merge) (a missing
approval, the limit on unreviewed significant changes) are waiting reasons of
the same kind.

The entry lands the pinned commit and nothing newer. A new version of a change
with a live entry cancels that entry and says so; a candidate that held it is
built again without it. `fy land` queues the new version, a plain push does
not, so unfinished commits never land because an earlier version was queued.

Entries are taken oldest first. An entry queued with `--urgent` (a revert, a fix of a failing `main`; admin scope, the maintain role once roles exist) goes before all entries that are not yet
in a candidate.

### Candidates

A **candidate** is `main` plus an ordered set of entries, one squashed commit
each, stored under the immutable ref `refs/fortyard/candidates/ID` with its
base commit, tip commit and tree. When no candidate of the repository is being
checked, the lander builds one from the queued entries and those waiting for an entry
ahead, up to
`queue.batch` (16). For each entry in order:

- It is squashed onto the candidate's tip in the bare repository, with
  [`git merge-tree --write-tree --merge-base`](https://git-scm.com/docs/git-merge-tree)
  and `git commit-tree`, without a work tree. This is a three-way merge of the
  version's own difference onto the newest state: the server rebases every
  landing.
- Size and format are checked on the member's own files. A member that fails
  them, or adds nothing to its base, fails alone before any runner time is
  spent.
- A branch whose tip is a merge of another history into the current `main`
  (an upstream merge) is not squashed. It forms a candidate of its own and
  lands as that merge. Any other merge commit on a branch, such as a merge of
  `main` made by `fy sync`, is squashed like ordinary commits.

One runner check of the tip decides the candidate: build, and the tests that
the repository's setting `land.tier` selects (see
[Testing before landing](#testing-before-landing)). Checks have a kind:
`landing`, `test` or `verify`. A check names the commit and tree and gives the
runner that takes it a random number (nonce); the runner reports the tree it
checked with that nonce, and `main` moves only if both agree. The commits of a
candidate carry the trailer `Landing:` with the entry and its pinned commit,
next to `Change:` and `Change-Id:`. The move is one
[`git update-ref`](https://git-scm.com/docs/git-update-ref) transaction that
names the old value of `main`; a member's branch is deleted only if it still
points to the pinned commit. Commits pushed to the branch after `main` had already moved stay on the
branch and open a follow-up change.

### Server merge and resolution classes

A squash has one of four outcomes:

| Class | When | Result |
|---|---|---|
| `merge` | git's three-way merge is clean: the two sides changed different hunks | lands without author action |
| `keep-both` | the file matches a pattern in `.fortyard/merge`, and in every conflicting hunk both sides only inserted lines at the same place (the base side of the hunk is empty) | both insertions are kept, the earlier entry's first; recorded on the entry and shown on the change and its review |
| `model` | the repository allows it and the conflict is in text the model may merge (see [Merge by the model](#merge-by-the-model)) | the local model merges the conflict regions; recorded, shown on the change and, by the repository's choice, flagged for a person |
| conflict | anything else | goes to the author as a rebase task |

`keep-both` is for registries and documents whose entries stand alone: a list
of migrations, a changelog, plan items. A registry may be a source file when
each entry is a block of its own. `.fortyard/merge` lists one shell
pattern a line, is read from `main`, never from the branch, and belongs to the
paths reviewed before merge. The class is all or nothing for a member: one
hunk that modifies or deletes base lines leaves the whole squash a conflict.
It is not meant for program logic, where two insertions at one place can
depend on their order or be the same fix twice and still pass the gate.

### Merge by the model

A conflict that `keep-both` does not cover can be merged by the instance's
local model before it goes back to the author. The repository's policy sets
`merge.model` to `off` (default), `flag` or `on`.

- **What the model gets.** For each conflicting file: the base, both sides and
  git's merge of everything that did not conflict, with the conflict regions
  marked; the title and description of both changes. Nothing else, and no
  tools: the model merges text, it does not run code.
- **What it returns.** The text of each conflict region, nothing more. The
  server puts the regions into git's merge itself, so the model cannot change
  a line outside a conflict. An answer with conflict markers, with a region
  missing, or over the budget in tokens is no answer.
- **The gate decides.** The merged member joins the candidate like any other
  and lands only if the check passes. If it fails as the only member of a
  candidate, the entry ends with the reason `conflict` and the rebase task, as
  if the model had not been asked.
- **Record and review.** The entry records model, instruction version and the
  merged regions; the change page and the review show them next to both
  sides. With `flag` the change is flagged for a person
  ([Workflow](workflow.md#when-an-agent-flags-a-change-for-a-human)); with
  `on` it is reviewed like any other landed change. The author's session
  learns of the merge in the answer of `fy land`.
- **Never merged by the model:** paths reviewed before merge
  ([Workflow](workflow.md#review-before-merge)), paths declared `keep-both`,
  binary files, and a file one side deleted.

The model merges what a person would merge in a minute: two procedures added
at the same place, two entries in the same list, an argument added on one
side and a call moved on the other. Whether that trust holds is measured: the
share of model merges that fail the gate, and the share whose change is later
reverted or corrected in review.

### Conflicts and rebase tasks

A member that does not squash onto the candidate is squashed onto `main`
alone:

- **Clean on `main`:** it conflicts only with members ahead of it. It leaves
  the candidate and waits, naming the members that touch the conflicting
  paths, and is tried again in the next candidate. If those members are
  rejected, it lands unchanged.
- **Conflict with `main`:** the entry fails with the reason `conflict` and a
  **rebase task**: the commit to merge with, the conflicting paths, and
  number, title and session of the changes that last touched them.

The task belongs to the session that pushed the change and is not handed to
another agent, since two sessions resolving one change overwrite each other.
`fy sync` fetches `main` and merges it into the branch, with
[rerere](https://git-scm.com/docs/git-rerere) on. A merge resolves each
conflict once, where a rebase replays every commit, and the landing squashes
either way. `fy sync` stops at the conflicts with status 3; `fy sync --continue`
commits and pushes, `--land` queues again. The author does not retest: the
queue checks what lands. A waiting session can resolve at once with
`fy sync --onto M`, which merges the newest version of the change ahead and
makes this change a child of it (see [Stacks](#stacks)).

A member that was merged with `keep-both` and then fails as the only member of a candidate fails with
the reason `conflict`, not `check`: the branch may be right and the merge
wrong. As with the [loop limit](workflow.md#safety) for reviews, three entries
of one change that ended in a conflict or a failed check are the limit: the
next one needs the admin scope, that is, the person.

### Failing candidates

A failed candidate with one member rejects that member: it failed on a base
that passed, and the runner has already repeated the failure from an empty
build directory. A failed candidate with several members is split into halves.
Each half is built on the failed candidate's base and checked, both at once
when two runners are free. A half that passes while `main` is still at its
base lands at once; a half whose base has moved is built again on the new
`main` and checked again. Passes are remembered by tree, failures never, so a
tree that failed once is run again before anyone is blamed, and of two
changes that pass alone and fail together the earlier lands and the later is
rejected. A runner that is lost, runs out of memory or cannot fetch reports
an error, which queues the check again and is never a verdict.

### Stacks

A change may build on another change that has not landed; the stack lands as
one commit per change, each with its own `(#N)`. Nothing is declared: the
**parent head** of a version is the nearest ancestor of its tip that is a
stored version of another change, unless the tip already contains that
change's landing commit. It is recorded with the version, also when the
parent has landed or was closed since. Without a parent head the base is the
merge base with `main`.

- The version's diff, review and squash are taken against its parent head.
  Squashing with the parent head as merge base, onto a candidate that holds
  the parent's commit, applies the child's own difference only; this is also
  what restacks a child when its parent was amended or landed squashed.
- A child lands after its parent: in a later candidate, or in the same one
  directly after it. The lander checks that the parent's commit is an
  ancestor of the candidate tip before it applies the child.
- A parent is queued only by its own author. `fy land --stack` queues the
  change and those of its unlanded ancestors that the same session pushed;
  otherwise the child waits for its parent.
- A child whose parent was rejected, cancelled or closed waits. It is never
  moved onto `main` automatically, since the result would be the child
  without code it was written against.

### Speculation

By default one candidate, or the two halves of a failed one, is checked at a time, and entries that arrive during
a check form the next candidate. Without failures a change then reaches `main`
after at most two check times, however many sessions are landing. With
`queue.speculate = n`, up to n further candidates are built behind the one
being checked, each on the tip of the one before it, and checked at the same
time, as in
[Zuul's dependent pipeline](https://zuul-ci.org/docs/zuul/latest/gating.html).
A candidate lands only after all candidates before it. When one fails, those
behind it are superseded and their members return to the queue; a later pass
never clears an earlier failure. A runner process checks one tree at a time,
so the setting is at most the number of runner processes minus one. It is
switched on when the measured wait for a runner exceeds the check time.

### Restart

One lander serves all repositories and never sleeps inside a check: it reads
entries, candidates and checks, performs the next step and waits for an event.
After a restart it finds the same candidate with the same check and goes on,
so no check is repeated because a release was switched. A move of `main` made
before the restart is recognised by the `Landing:` trailer, which names the
entry and the pinned commit.

## Testing before landing

- **The landing check** is the repository's check on the candidate's tip:
  `fo build && fo test` in the runner's kept work tree, or the repository's own
  script `.fortyard/check` where it has one.
  fo skips every case whose input digest already has a recorded pass, so only
  the cases the candidate affects run. A failure is confirmed from an empty
  build directory before it is reported. `land.tier` is `affected` (default),
  `full` (every case again) or `none`.
- **`fy test [BRANCH|N]`** queues a check of kind `test` on the version
  squashed onto the current `main`, which is the commit that would land, not
  the branch as pushed. Runners take landing checks first, then tests, then verifications of `main`
  (every case run again after `main` moved, see
  [Testing at scale](testing-at-scale.md#culprit-finding-and-notification)).
  Tests run on request, not on every push as the pre-merge tier of Testing at
  scale foresees: with few runners they would delay landings.
- **Reuse.** A passed check is stored with its tree and tier. A candidate
  whose tip tree has such a pass lands without a run: a change tested with
  `fy test` and landed alone before `main` moved, or a candidate built again
  with the same content. Tests also warm the work tree and the caches of the
  landing check. The runner environment joins the key once an instance has
  several runner classes.
- **A time budget and passes shared across runners** come with results per
  case ([Testing at scale](testing-at-scale.md#result-keys-and-affected-tests))
  and change the check, not the queue.

| Event | What runs |
|---|---|
| Push | nothing; mergeability and overlap come from git alone |
| `fy test` | build and affected cases of the version squashed onto `main` |
| Candidate, or half of a failed one | build and affected cases of the tip, or nothing when the tree has a pass |
| `main` moved | verification with every case run again, bisection of a failure |
| After `fy sync` or a server merge | nothing by hand |

## Coordination

- **Claims.** `fy issue claim N` is one atomic update: it succeeds when the
  issue is unclaimed or its claim has expired, and otherwise names the holder.
  A claim expires `claim.hours` (24) after it was made or after the holder's
  last push to a change that names the issue, whichever is later, and ends when such a change lands or with
  `fy issue release N`. It refuses a second claim, never a push.
- **Intents.** What a session intends is its claimed issue and its draft
  change. fortyard stores no declared list of paths: the paths a session
  touches are read from the versions it pushed, which is why sessions push
  after every commit.
- **Overlap warnings.** For a push, a landing or on request, the server squashes a change onto `main`
  and then after each other open change in turn. The answer lists only real
  conflicts: the other change's number, title, session and state, and the
  conflicting paths. It is part of what `fy push`, `fy land` and
  `fy change view` return, so a conflict is known when the second of two
  versions is pushed, not at landing. Files that are shared without a
  conflict are not reported; with twenty sessions in a few large files that
  is the normal case. `fy overlap --paths P...` lists the open changes that
  touch given paths, before work starts.
- **Sync.** `fy sync` keeps a branch close to `main` at any time. For a
  predicted conflict a session syncs once the other change has landed, or
  builds on it with `--onto` when its work depends on it.
- **The board** (`fy board`, `GET /board`, a page of the web application)
  shows a repository's parallel work from the same rows: *working* (draft
  changes with session, claimed issue, last push and predicted conflicts),
  *queue* (entries with state and reason, candidates with members and check),
  *needs author* (failed entries with their rebase task or log and the number of failed entries so far), *landed* (recent landings with their release).

## Conventions that prevent conflicts

- **Numbers come from the server.** A change number is allocated when the change is first pushed and an issue
  number when the issue is opened, each in one write transaction; landing a branch
  without a change opens the change first. Text cites a number only after it
  exists.
- **Ordered resources are named, not numbered.** A resource whose order
  matters (schema migrations) has a name per entry and a recorded set of
  applied names, as in
  [Rails migrations](https://guides.rubyonrails.org/active_record_migrations.html).
  Each branch adds its entry to a registry file that is declared `keep-both`,
  so source order is landing order and a fresh database applies migrations in
  the order production did. An applied entry is immutable: its content hash
  is recorded, and a changed body under an applied name refuses to migrate.
  In fortyard itself the migrations up to version 13 stay as they are, with
  the version row frozen, so an older release still starts.
- **Registries are one entry per line.** Lists that every feature extends
  (exported names, imported names, command tables, MCP actions) hold one name
  per line in sorted order, so git merges most insertions by itself.
- **Positions are numbers too.** Code that depends on a position in another
  line (a column index into a column list, a numbered bind parameter) is
  never in a file declared `keep-both`.
- **Plan items.** `PLAN.md` stays a checkbox list and is declared
  `keep-both`. An item is ticked by the change that completes it, an item
  that sessions may pick up has an issue to claim, and an item that is cited
  carries a name, never a position.
- **Hot files are split.** A file that stays among the paths with the most
  conflicts per week is divided by topic, which removes them at the source.

## Invariants

1. `main` moves only forward, only by the lander, and only to a commit whose
   tree a runner passed at the repository's tier (with `land.tier = none` the
   repository has chosen to land without tests).
2. Every commit a landing adds to the first-parent history of `main` is one
   change with `(#N)` and the trailers `Change:`, `Change-Id:` and `Landing:`;
   a stack is one commit per change, parents first; an upstream merge brings
   its history as second parent. Only a
   candidate's tip is checked.
3. An entry lands exactly the commit it pinned; a change has one live entry.
4. A change is rejected only when it fails on a base that passed without it.
   A runner error, a conflict with a neighbour or a neighbour's failure never
   rejects it.
5. The server resolves only declared `keep-both` hunks, by rules read from
   `main`, and conflict regions the model merged where the repository allows
   it; it records each resolution, and never changes a line outside a
   conflict.
6. The server never rewrites a user's branch; its own commits live under
   `refs/fortyard/`.
7. Every entry is queued, waiting with a reason, running, or ended with a
   reason. None ends silently.
8. Every step of the lander gives the same result when repeated after a
   restart.
9. Numbers and claims have one holder, taken in one write transaction. Claims
   and overlap warnings never refuse a push or a landing.

## Failure cases

| Case | Behaviour |
|---|---|
| Two changes edit the same lines | The earlier lands. The later waits while the earlier is in the candidate, then gets a rebase task; its session syncs and lands again |
| Two changes pass alone and fail together | The candidate fails and is split; the earlier lands, the later is rejected with the log |
| Intermittent failure | The failed tree is always run again; a change is rejected only as a single member on a passed base. A rejected version that later lands unchanged is counted as a false rejection |
| A `keep-both` or model merge is wrong | The gate fails; the later change ends with `conflict` and is merged by its author. A wrong merge that passes the gate is found in review or by verification and reverted like any change |
| Author pushes while queued or in a candidate | The entry is cancelled, the candidate built again without it; `fy land` queues the new version |
| Parent of a stack rejected, cancelled or closed | Children wait; the author merges the new parent version (`fy sync --onto`) or rebuilds the branch on `main` without the parent |
| Server restart or release switch | The lander goes on from rows and refs; the check keeps running |
| Runner dies, its result is lost, or no runner is there | The runner repeats its report until it is acknowledged; a restarted runner releases its check, which is queued again; entries wait and nothing fails for lack of a runner |
| A result arrives for another commit | Refused: the nonce given to the runner and the reported tree must match |
| Session stops | Its claim expires, its draft change stays on the board with its last push, its failed entry stays under *needs author* |
| Agent cannot resolve | After three failed entries the change waits for the person |
| Verification of `main` fails after landing | Bisected and reported as an issue; the revert is an ordinary change queued with `--urgent` |
| Health check fails after a deploy | The instance rolls back to the previous release; `main` and the queue are not touched |

## What is measured

From the timestamps of versions, entries, candidates, checks and releases, at
`GET /board?stats=1` and on the board:

- time from `fy land` to `main` and from push to `main` (median and 95th
  percentile), time from `main` to a ready release, landed changes per hour;
- share of changes that landed with their first entry, and of the others the
  reason (conflict, check, format);
- conflicts per path and week (the hot files), conflicts that `keep-both`
  and the model resolved, and how many of those failed the gate or were
  reverted afterwards;
- candidate size and failure rate, checks per landed change, checks saved by
  reuse, wait of a check for a runner, false rejections;
- failed verifications of `main` per 100 landings, which is what the
  `affected` tier and server merges cost in correctness;
- unreviewed significant changes and time from landing to AI review.

The wait for a runner decides about speculation, the candidate failure rate
about the batch size, the conflicts per path about splitting files, and the
failed verifications about the tier.

## Prior art

- Batch, check once, split on failure:
  [bors](https://bors.tech/documentation/getting-started/), GitHub's merge
  queue.
- Speculative chains that restart only what is behind a failure:
  [Zuul project gating](https://zuul-ci.org/docs/zuul/latest/gating.html).
- A change identity that survives amend and rebase, and dependent changes:
  [Gerrit's Change-Id](https://gerrit-review.googlesource.com/Documentation/user-changeid.html).
- Merging without a work tree, with a list of conflicts:
  [`git merge-tree`](https://git-scm.com/docs/git-merge-tree).
- Affected tests before submit, everything after, culprits by splitting:
  [Software Engineering at Google, chapter 23](https://abseil.io/resources/swe-book/html/ch23.html).
- A set of applied migrations instead of one counter:
  [Rails migrations](https://guides.rubyonrails.org/active_record_migrations.html).
- Claims on tasks and conflicts resolved by the agent that hit them
  ([Anthropic, a C compiler built by parallel agents](https://www.anthropic.com/engineering/building-c-compiler));
  no file locks and no central integrator, which both cost throughput
  ([Cursor, long-running agents](https://cursor.com/blog/scaling-agents)).
