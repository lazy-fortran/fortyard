# Inspection, review and monitoring

How a person who steers many agents sees what happened to every change,
decides what needs a decision, and how the instance is watched from outside.
[Workflow](workflow.md) gives the review rules,
[Parallel development](parallel-development.md) the queue and
[DESIGN.md](../DESIGN.md) the architecture. This document fixes the pages of
the web application, the API routes, `fy` commands and MCP tools that return
the same data, the metrics and the alerts. Names such as MP13, MI10 or S6
are items of [PLAN.md](../PLAN.md); "before MP13" and "from MP13 on" say
what a page shows without and with that item. `R` stands for a repository's
path, `GROUP/REPO`. The wireframes show one instance at 10:34 on one day,
with all items in place.

## Principles

1. **The unit is the change.** Versions, entries, candidates, checks,
   verifications, reviews and releases are stages of one change. Nothing
   that ran is listed without the change it served.
2. **The answer comes first.** A page answers one question; its first lines
   hold the state, the reason and the decision asked, the diff comes last.
3. **Only recorded facts.** A page shows what a row or a ref records and
   derives nothing from log text. What was never recorded reads `not
   recorded`.
4. **Words, and colour for exceptions.** A state is a word with its reason.
   Settled states are plain; a colour means that someone has to act or
   something is broken.
5. **Nothing replaced disappears.** What was superseded stays, muted and
   folded, and names what replaced it.
6. **Every page is also an API resource.** A page shows named API
   resources, each of which `fy` prints and `fy mcp` returns; each row of
   the [table](#api-fy-and-mcp) names the three.
7. **Instant and still.** The server sends every page as finished HTML (its
   first frame), the event stream keeps it current, nothing polls. No
   spinner, toast or animation; a row changes in place, and a stream that
   is down says so.
8. **Two actions to anything.** An action is a key sequence, a click or a
   tap. At most two lead from anywhere to any page or object: a key
   sequence reaches a page, the command palette with a number an object.
   Every action has a key and a visible control, and a phone shows the
   same information and actions.

## What is taken from earlier forges

Each pattern of GitHub, GitLab, Gerrit, Phabricator and Zuul is judged by
how fast a person steering many agents reaches a decision with it.

| Pattern | Here | Why |
|---|---|---|
| Addresses `/GROUP/REPO/-/changes/N`, `#N`, the words change, issue, checks | taken | they cost nothing, and links in commit messages keep working |
| Gerrit patch sets with a diff between any two | taken | they are the versions; every entry, check and review names its version, which makes "superseded" exact |
| Gerrit attention set (whose turn it is) | extended | the inbox is one person's attention set across all repositories, with the reason; an item leaves when its cause does |
| Phabricator audit after publication | taken | it is the review model of [Workflow](workflow.md#review) |
| Merge train and Zuul status page | extended | the board shows candidates with members, the shared check and the reason of every wait; each member links to its change |
| Tabs Conversation, Checks, Files changed, Versions | changed | a tab hides three quarters of the answer behind a click; the change page has sections in a fixed order, each with an anchor |
| Flat list of CI runs as the entry point | left out | a run means nothing without its change; the list of jobs is a filterable log (`/jobs`) |
| Pipeline graph, stages, step-folded logs | left out | one check decides a candidate; a status line says what a graph would, and a log is one flat text with line anchors |
| A re-run overwrites the result; "cancelled" for replaced runs | left out | each attempt is a row; a replaced run says `superseded by` with a link, and `cancelled` is what a session did |
| Approve, request changes | changed | a landed change is approved, reverted or gets an issue; approval before landing exists only for the paths reviewed before merge |
| Line comments as a conversation | left out after landing | the author's session is gone; a remark becomes a finding that opens an issue or a revert |
| Notification feed, mail per event, toasts | left out | the inbox is computed from rows; its count sits in the top bar and the tab title |
| "Open" as the default list filter | left out | under merge first almost nothing stays open; lists show all states, newest activity first |
| Dropdown filters, pagination, "load more" | left out | filters are links in the address; lists load older rows by key (`?before=ID`) |
| Avatars, reactions, stars, label pills, green on every passed row | left out | they carry no decision, and 60 green rows hide 10 red ones |
| Health endpoints and the Prometheus text format | taken | standard; an outside monitor is needed in any case |

## Objects and how they link

| Object | What it is | Stored as |
|---|---|---|
| Change | one commit's worth of work under review, `#N` | `changes` |
| Version | one push of a change: `v1`, `v2`, ... | `change_versions` |
| Job | what a session follows by number: an entry, a test or a verification | `landings`, with `kind` `landing`, `test` or `verify` |
| Entry | the job of kind `landing`: the request to land one version | `landings` |
| Verification | the job of kind `verify`: a check of a commit of `main` after it moved | `landings` |
| Candidate | `main` plus an ordered set of entries, checked once | rows from MP13 on; before MP13 an entry is shown alone |
| Check | one run of one tree on a runner, of the kind of its job; a job has one check per attempt | `checks` |
| Review | one reading of a version, or of the landed commit, by the agent or a person | `reviews` |
| Release, deploy | a release is a build of a commit of `main`; a deploy is an instance serving a release | `releases`, `deploys` |

The links are columns, so no page joins by text:

- `landings`: `change`, `version`, `head` (the pinned commit), `reason`,
  `check_id`; `session` from MP3; from MP9 to MP14 `waiting_for`,
  `candidate` and `superseded_by`.
- `checks`: `landing` (the job it decides; from MP13 on the `candidate`
  when several entries share the check), `attempt`, `tree`, and what ran:
  `command`, `image`, `fo_version`, `exit_status`. A check stored before
  these columns gets its job from its ref, `refs/fortyard/checks/ID`,
  whose `ID` is the job's number.
- `reviews`: `change`, `version`, `head`, `answers` (the review a person's
  decision answers), `issue`, `revert_change`.
- `changes`: `first_release` (set by one `git rev-list` when a release
  becomes ready), `reverted_by`, `reverts`; `change_links(change, issue,
  kind)` from `#N` and `Fixes #N` in descriptions.
- `runners(name, host, version, last_seen, check_id)`, updated by the calls
  a runner makes anyway. `releases.state` is `building`, `ready` or
  `failed`. `deploys(instance, version, commit_id, state, at, detail)` is
  written by `fy serve` when it starts as a new version and by
  `scripts/self-update.sh` when it refuses a release.

Rows older than a column show `not recorded`; nothing is reconstructed from
old comments or logs.

**Superseded** means that an object was replaced before it reached a
verdict, or that a newer object of its kind took its place; the table gives
the words each object shows. A verdict is never superseded: a failed entry
followed by a second one reads `failed: check · entry 1 of 2`. A check
whose runner was lost reads `error, queued again` and is neither failed nor
superseded; its job gets a further attempt.

| Object | Superseded when | Shown as |
|---|---|---|
| Version | a newer version is pushed | `v2 superseded by v3`, with the interdiff |
| Entry | a new version is pushed while it is live, or it is withdrawn | `cancelled: v3 pushed` or `cancelled by SESSION`, linked to the next entry (MP9) |
| Candidate | rebuilt without a member, its base moved, split after a failure, or a candidate before it failed | `candidate 43 superseded by 44: #84 pushed v3`; stays on the board for an hour and on each member's page (MP13, MP21) |
| Check | its candidate was superseded, or a newer `main` overtook a verification | `check 410 superseded by check 412`; a pass taken from an equal tree reads `reused from check 409` |
| Review | a new version is pushed, or a new review is requested; a flag is never superseded, a person decides it | `review of v2 superseded by review of v3`, folded; the decision names the review it answers |
| Verification | a newer one finished | one line per repository: `main verified up to COMMIT` |
| Release, deploy | a newer release is ready; the next deploy serves | `replaced` |
| Change | a revert landed | `reverted by #81`; the revert reads `reverts #72` |

## States and words

Five tones carry meaning. **Plain**: settled, or open work nobody waits
for. **Accent**: in motion. **Attention**, amber: waits for someone, a
person or the author's session. **Negative**, red: broken. **Muted**:
history. A state the page does not know is shown muted, as the server sent
it, and `not recorded` is muted everywhere.

| Object | Plain | Accent | Attention | Negative | Muted |
|---|---|---|---|---|---|
| Change | `draft`, `landed` | | `needs author: REASON`, `waits for the person` | | `closed`, `reverted by #N` |
| Entry | `landed` | `queued`, `running`, `waiting: behind #71 (PATH)`, `waiting: parent #70` | `waiting: approval` | `failed: REASON` while it is the newest entry of its change | `cancelled`, earlier failed entries |
| Candidate | `landed` | `building`, `checking` | | | `failed, split`, `superseded` |
| Check | `passed` | `queued`, `running`, `error, queued again` | | `failed` | `superseded`, `reused` |
| `main` | `verified`; `fetched` for a mirror | | | `main broken` | |
| Review of a change | `AI-reviewed`, `approved by NAME`, `issue #N opened` | `review requested`, `reviewing`, `revert requested` | `unreviewed`, `flagged: REASON`, a stale review | `review failed` | `superseded` |
| Release | `ready` | `building` | | `failed` | `replaced` |
| Deploy | `serving` | | `behind` | `refused` (the health check failed) | `replaced` |
| Runner | `idle` | its check | | `lost` | |
| Process | `up` | | | `down` | `not configured` |
| Issue | `open`, `claimed by NAME`, `unclaimed` | | | | `closed` |
| Page | `live` | | | `offline since TIME` | |

- A change shows `draft` while it is open and not queued, the state of its
  entry while it has a live one, and `needs author` when its newest entry
  failed. `REASON` of an entry is `conflict`, `check`, `format`, `size` or
  `empty`. The filter word for every change that has not landed or closed
  is `open`.
- The stored review states are those of
  [Workflow](workflow.md#states): unreviewed, AI-reviewed, human-reviewed
  and, separately, flagged. `approved by NAME` is how a human-reviewed
  change is shown. An unreviewed change shows where its review stands
  (`review requested`, `reviewing`, `review failed`) or plain `unreviewed`
  when none was asked for.
- Before MP4 to MP14 there are no candidates, waiting reasons, cancelled
  entries, repeated or reused checks and tests, and `needs author` has no
  rebase task; `waits for the person` follows three failed entries (MP14).

## The frame of every page

```
fortyard   Inbox 7   Jobs   Server          serving 10 min behind   Ctrl-K   admin
fortyard/fortyard   Code  Changes  Issues 14  Board  Analytics
```

- **Top bar.** The name leads to the overview. `Inbox` carries the number of
  cards (items that ask for a decision), also shown in the tab title. The
  mark on the right reads `main verified`, or `main broken` in red or
  `serving 10 min behind` in amber for the worst repository, and links to
  the overview. The account name leads to the account page, `/account`
  (tokens, theme, sign out). There is no drawer.
- **Repository row** on every page of a repository; through it changes and
  issues are reachable from the code.
- **Keys.** `g o` overview, `g r` inbox, `g j` jobs, `g s` server, `g a`
  analytics (of the repository when in one), `g u` audit, `g m` account; in
  a repository `g b` board, `g c` changes, `g i` issues, `g f` code. `j`
  and `k` move in a list, `Enter` opens, `/` searches within the page (the
  filter field of a list, the log of a job), `c` comments, `?` lists the
  page's keys. All commands are one table (name, key, page, action) read
  by the palette, the key handler and the buttons; a test refuses two
  commands with one key on a page.
- **Palette** (`Ctrl-K`). A number goes to that change or issue, a commit
  prefix to the change that landed it, a repository name to its board;
  other text filters the commands.
- **Live.** One event stream per tab (`GET /api/v1/events?topics=...`). An
  event names a topic (`jobs`, `changes`, `reviews`, `issues`, `runners`,
  `load`); the page then requests its resources again, one request at a
  time. The page head says `live`, or `offline since 10:42` while the
  stream is down; one request follows the reconnect.
- **Addresses.** Filters, the version pair of a diff and the selected line
  are in the address. Back and Forward restore the scroll position.
- **Phone.** One column with the same content: destinations and palette in
  a bottom bar, rows of two lines with 44 px targets, the decisions of a
  card in a fixed bar, and the top bar reduced to the inbox count and the
  mark. The HTML is the same at every width; the differences are CSS.
- **Look.** One system sans-serif face, one monospace face for hashes and
  logs; 13 px rows of 28 px height, 15 px reading text, one 20 px title,
  tabular figures. Rules separate sections; rows have no frames, only
  overlays have a shadow, a state is a word, not a pill. Light and dark
  follow the system unless the person chose one.

## Pages

Each page is headed by the question it answers.

### Overview, `/`

What is going on everywhere, and what needs me.

```
fortyard  main 27a37ab verified · newest release 2026.11.11.6 of 3a96647 · build of 27a37ab failed 10:31  log
          serving 2026.11.11.6, 2 landings and 10 min behind
NEEDS YOU (7)                                                       3 more in the inbox
 broken    fortyard      serving is 2 landings behind main: the build of 27a37ab failed
 approve   fortyard #81  Tokens can be limited to one repository    permissions
 flagged   fortyard #75  Request parsing guards its own bounds      security-relevant   19 min
 flagged   lfortran #31  Fork rules                                 tests loosened         3 h
REPOSITORIES        main              queue                 needs author  flagged  unreviewed  24 h
 fortyard  primary  verified 27a37ab  2 running, 2 waiting             2        1           2  48 landed, 7 failed
 lfortran  primary  verified 8d423b9  idle                             0        1           0  10 landed, 1 failed
 fx        mirror   fetched 1 min ago
RUNNING NOW
 fortyard  candidate 44: #84, #85   check 412 (landing) on faepkub4   0:31
```

The first lines are the **delivery line** of each repository the instance
follows: verified commit, newest release, serving release, and the reason
when serving is behind. NEEDS YOU lists the alerts, the approvals and the
flagged changes; the rest of the inbox is one link away. Every number links
to the rows it counts. Phone: three lines per repository. Empty: `Nothing
is queued or running. Last landing 5 minutes ago: fortyard #79.` Live: all
of it.

### Board, `/R/-/board`

What is going on in this repository: the board of
[Parallel development](parallel-development.md#coordination). One row per
change, never per job.

```
Board · fortyard         main 27a37ab verified by job 127 (46 s)                    live
QUEUE
 candidate 44   checking 0:31, the last check took 0:45   check 412 on faepkub4
   #84 v3 repo-roles    agent-2
   #85 v1 fix-log-tail  agent-5    merged: keep-both PLAN.md
 #86 v3 claims          agent-1    waiting: behind #84 (src/fy_store.f90)
 #81 v1 token-repos     agent-7    waiting: approval (permissions)
 candidate 43   superseded by 44: #84 pushed v3
NEEDS AUTHOR
 #82 web-image-lld      agent-4    failed: check · entry 2 of 2 · 38 min ago       log
 #80 parser-fuzz        agent-6    failed: conflict with #79 in src/fy_server.f90 · 5 min ago
WORKING
 #87 inbox-api          agent-3    issue #66 · pushed 2 min ago · conflicts with #86
LANDED                                                       all · flagged · unreviewed
 #79 PLAN: receive checks ...  27a37ab  12 s                  review failed               no release yet
 #78 model merge design        27614c2  16 s                  review failed               no release yet
 #75 parser-bounds             3a96647  58 s · entry 2 of 2   flagged: security-relevant  release ...6
 #74 receive-checks            74bf821  18 s · entry 2 of 2   AI-reviewed                 release ...6
```

QUEUE holds the live entries, NEEDS AUTHOR the open changes whose newest
entry failed, WORKING the open changes without a live entry, LANDED the
recent landings. Before MP3, MP7 and MP9 to MP14 an entry stands alone in
the queue; candidates, waiting reasons, server merges, rebase tasks,
sessions, claims and predicted conflicts appear when these items write
them. A section without rows is absent. Empty: `No open work. Last landing
5 minutes ago: #79.` Live: all of it.

### Jobs, `/jobs` and `/jobs/ID`

What has run. The list is the log of all jobs, filtered in the address
(`?repo=&kind=landing|test|verify&state=`); each row has its change, kind
(a job of kind `landing` is called an entry), state, duration, runner and
commit. Empty: `No jobs match.`

```
job 113 · entry of fortyard #74 receive-checks v1 · failed: check · entry 1 of 2 · after 1 min 22 s
next entry of this change: job 114, landed
queued 09:41:12 by agent-3 · started 09:41:14 · finished 09:42:36
CHECKS
 check 398  attempt 1  failed  1 min 20 s after 2 s wait  runner faepkub4  tree 51ab...
   ran .fortyard/check · image fy-toolchain@sha256:41ab... · fo 0.9.2 · exit status 1
LOG   1 204 lines · first line with Error or FAIL: 812 · [/] search · follows the end
  812  At line 96 of file test/test_receive.f90
  813  Fortran runtime error: Index '9' of dimension 1 of array 'names' above upper bound of 8
```

The page shows the whole stored log; a runner stores the first 64 KiB of a
check's output. A line has the address `/jobs/113#L812`. While the job
runs, new lines are appended only if the reader is at the end. A
verification names the commits of `main` it covered. Phone: log lines wrap.
Live: state, checks and log.

From MP13 on a candidate has its own address, `/R/-/candidates/ID`, to
which every mention of it links: base, tip and tree, the members in order
with the outcome of each, the checks, and for a failed or superseded
candidate its halves or its successor with the reason.

### Changes, `/R/-/changes`

Find a change. All states, newest activity first. Filters are links in
three groups, `all · open · landed · closed`, `flagged · unreviewed` and
`needs author · reverted`, plus a text field for title, branch or session
(`?state=&review=&attention=&q=`).

```
#75  Request parsing guards its own bounds   landed 3a96647                       flagged: security-relevant  agent-9  19 min
#82  web-image-lld                           needs author: check · entry 2 of 2                               agent-4  41 min
```

Empty: `No flagged changes in fortyard.`, with the filter named. Live.

### Change, `/R/-/changes/N`

What happened to this change, and what is asked of me. One page with the
sections `#checks`, `#reviews`, `#files`, `#discussion` and `#timeline`.
`/R/-/changes/N/files?v=3&from=2` shows a whole diff or an interdiff.

```
fortyard #75  Request parsing guards its own bounds, so release builds are protected
parser-bounds · agent-9 · opened 10:02 · fixes #70
v3 pushed 10:14 · entry 122 landed after 58 s · main 3a96647 · verified · release 2026.11.11.6
· serving · AI: flagged · you: no review
DECISION  flagged: security-relevant code       [a] approve  [r] revert  [i] open issue
CHECKS
 entry 122 · v3 a1b2c3d · landed 10:15 as 3a96647                                  job
   check 405  landing  passed  58 s after 2 s wait  runner faepkub4  tree 9f1c...
     ran .fortyard/check · image fy-toolchain@sha256:41ab... · exit status 0       log
 > 1 earlier entry: entry 118 · v2 · failed: check · entry 1 of 2 · after 1 min 22 s
 main verified up to 3a96647 by job 123 (46 s)
REVIEWS
 review-agent · read v3 a1b2c3d · 10:15 · 41 s · flagged: security-relevant code
 model qwen3.8-flash-next · thinking off · instructions 4f2a91c · 18.2k tokens in, 1.1k out
 Findings (1)
  1  parser/src/fy_http.f90:212   length compared after the copy                 hunk
 Full text >      [request new review]      > 1 earlier review, of v2, superseded
 you: no review yet
FILES  5 files, +88 -21 · v3 against main · [v] versions  [e] open all
 > parser/src/fy_http.f90 +41 -12   > parser/src/fy_git_http.f90 +19 -6   > 3 more
DISCUSSION  2 comments >      [c] comment
TIMELINE  12 events >
```

- **Status line.** One stage per word, with its duration when done and its
  reason when not. `release` is the first release that contains the commit;
  `serving` is added where the instance follows the repository and serves
  it. An open change reads `v2 · draft`, with `[l] land`.
- **Decision line.** Present while something is asked: a flag, an approval
  before landing, or a rebase task with its paths and `fy sync`.
- **Checks.** Entries newest first, each with its checks: kind, state, run
  and wait time, runner, tree and what ran. A failed check shows its last 20
  log lines. Earlier and cancelled entries and superseded checks fold into
  one line each.
- **Reviews.** The AI review is a block, not a comment: the version it read,
  flag and reason, model, thinking setting, instruction version, tokens,
  duration, and findings that link into the diff; without a finding in the
  form asked for, its text as written. A stale review reads `read v2, the
  newest version is v3` in amber, a failed one `the review gave no usable
  answer`, a missing reviewer `No review: the review agent is not
  configured on this server.` A person's decision reads `approved by NAME`
  with its time.
- **Files.** In the order of the diff, tests and documentation last; a file
  with more than 400 changed lines is folded. Until triage labels
  mechanical hunks ([Workflow](workflow.md#the-human-review-queue)), no
  hunk is hidden by a guess about what matters. `v` chooses versions, with
  "since my review".
- **Timeline.** Every recorded event in order: versions, entries,
  candidates, checks, reviews, decisions, comments, release, and the
  change's audit entries. It is folded unless the change needed more than
  one entry or is flagged. A link hides superseded steps.

Live: status line, checks and reviews. Empty states are sentences: `Not
queued. fy land queues this version.` `No release contains this commit yet.`

### Inbox, `/inbox`

What needs me, across repositories.

```
Inbox   7 cards                                                                  live
BROKEN (1)
 fortyard  serving 2026.11.11.6 is 2 landings behind main since 10:24 · build of 27a37ab failed 10:31   log
BEFORE LANDING (1)
>fortyard #81  Tokens can be limited to one repository   waiting: approval (permissions)  +120 -14, 4 files
FLAGGED (2)
 fortyard #75  Request parsing guards its own bounds     security-relevant                +88 -21, 5 files   19 min
 lfortran #31  Fork rules                                tests loosened                   +12 -40, 2 files      3 h
UNREVIEWED (2)   the review gave no usable answer                        [request reviews again]
 fortyard #79  PLAN: receive checks ...                  review failed                    +31 -12, 1 file     5 min
 fortyard #78  model merge design                        review failed                    +64 -9, 2 files    10 min
STUCK (1)
 fortyard #82  web-image-lld   failed: check · entry 2 of 2 · no push for 41 min
--------------------------------------------------------------------------------------
#81  intent: the first lines of the description
why here: touches src/fy_store.f90 (permissions), reviewed before merge
evidence: test job 131 passed on tree 77c0... · AI: no findings · 4 files, largest first
 [a] approve and land   [b] send back with a comment   [s] skip   [Enter] open
```

- **Sections, in this order.** Broken: the firing [alerts](#alerts), in
  the order of their table. Before landing: entries that wait for an
  approval, from MP9 and MP14 on, when a missing approval is a waiting
  reason. Flagged: landed changes flagged and not decided, oldest first
  until triage gives the review need by which
  [Workflow](workflow.md#the-human-review-queue) orders them. Unreviewed:
  landed changes without a finished review, with the reason. Stuck: open
  changes whose newest entry failed and that had no push for 30 minutes,
  or that reached three failed entries. Spot check: a random sample of
  unflagged changes, once the agent of S6 draws it.
- **A person reviews** with one key, on the selected card or on the change
  page. `a` approves: a human review of the landed commit, which clears the
  flag. `i` opens an issue with the finding filled in, links it and clears
  the flag. `r` asks for one line of reason, opens the revert as a change,
  queues its entry before the others and shows `revert requested -> #91`;
  a revert is withdrawn by cancelling its entry. `s` skips. Before
  landing, `a` approves the waiting version, which releases its entry, and
  `b` posts a comment and cancels the entry. Each decision is a new
  `reviews` row, a line in the timeline and an audit entry and is never
  changed; a wrong approval is corrected by a later decision, an issue or
  a revert. The card leaves when the server has answered, and the next one
  is selected.
- **Size, not minutes.** A card shows changed lines, files and waiting time.
  Reading times, the time budget and the routing by an owners file of
  [Workflow](workflow.md#the-human-review-queue) are not part of this
  design; they need measured reading times first.
- **Empty.** A computed sentence: `Nothing needs you. 13 changes landed in
  the last 24 hours, 13 reviewed by the agent, none flagged.`
- **Phone.** The list; a tap opens the card full screen with the decision
  bar. Live: cards appear and leave.

### Issues, `/R/-/issues` and `/R/-/issues/N`

Which issues are open, who holds them, and which change addresses each.

```
Issues · fortyard    open 14 · claimed 5 · unclaimed 9 · closed            [n] new issue
 #66  Review inbox API         P1   claimed by agent-3, 3 h left   #87 draft     2 comments   1 h
 #62  Correctness per case     P2   unclaimed                                    0 comments   2 d
 #60  Enable the review agent  P1   claimed by admin               #71 landed    4 comments   3 d
```

A row names the holder of the claim and the changes that mention or fix the
issue, with their states. Priorities and claims appear with triage and MP7;
a column without data is absent. The issue page shows the text, the linked
changes, the claim, the comments and `Close` with a reason; an issue opened
by a verification names the culprit commit and its change. Empty: `No open
issues. Press n or run fy issue new.` Live.

### Analytics, `/analytics` and `/R/-/analytics`

Is the flow healthy. Numbers for 24 hours, 7 days or 30 days (`?days=`),
each a link to the rows behind it. The repository row links to the
repository's page, the Server page to the one across repositories.

```
Analytics · fortyard · 7 days
 landed            212   per day  ▂▃▅▇▆▅▇   lowest 9, highest 48
 land to main      p50 16 s   p95 1 min 3 s          push to main  p50 4 min
 first entry       83 % landed with their first entry
 failed entries    31   check 22 ██████████  conflict 6 ███  format 3 █
 checks            wait p95 3 s   run p50 38 s       1.2 checks per landed change
 main              2 failed verifications per 100 landings · red for 14 min
 review            1 flagged undecided, for 19 min · land to AI review p50 2 min · 17k tokens per review
```

Computed on request; a percentile is read from the sorted durations of the
window. The time from `fy land` to `main`, landings per hour, the failed
share, the share landed with the first entry, checks per landed change,
wait and run time of checks, failed verifications and undecided flags need
no column beyond the timestamps. Each further measure of
[What is measured](parallel-development.md#what-is-measured) appears when
its column exists, failures by reason with MI10; a measure without rows is
absent, not zero. Empty: `No landings in this window.`

### Server, `/server`

Is the machine healthy.

```
Server  2026.11.11.6 · 3a96647 · up 14 min                 Analytics · Audit      live
ALERTS    deploy-behind  fortyard  since 10:24 · build of 27a37ab failed 10:31
HOST      load 1.8 of 8 CPUs ▂▃▅▇▅▃▂▂   memory 5.1 of 8 GiB ████████░░░░   disk 38 % used
STORE     database 41 MiB, WAL 2 MiB · audit chain intact up to entry 2318, checked 10:30
RUNNERS   faepkub4  check 412 (landing) of fortyard candidate 44, 0:31   seen 2 s ago
          local     idle                                                 seen 40 s ago
QUEUE     1 check running, none queued · 2 entries running, 2 waiting · 0 reviews waiting
PROCESSES 2 workers up · lander up · verifier up · 2 reviewers up · mirror up
DELIVERY  fortyard main 27a37ab · build failed 10:31 · serving 2026.11.11.6 of 3a96647
```

One sample a minute is always taken; it fills the ring of two hours behind
the sparklines and feeds the `disk-low` alert and the host metrics. While a
page listens, the thread that holds its stream also samples every 10
seconds and sends the values over topic `load`. Request rate and latency
are in the request log, not on the page.

### Audit, `/audit`

Who did what. The hash-chained log of [Workflow](workflow.md#audit), newest
first, filtered by event and subject in the address, with the state of the
chain at its head (`intact up to entry 2318, checked 10:30`, `Verify now`).
The chain is checked every 5 minutes. A subject links to its change or
repository. The page is linked from the Server page and needs the admin
scope. Empty: `No entries for fortyard#72.`

## API, `fy` and MCP

Routes under `/api/v1`. Each row names the routes of a page with the `fy`
command and the MCP tool that return the same data.

| Page | Route | `fy` | MCP tool |
|---|---|---|---|
| Overview | `GET /overview` | `fy overview` | `overview` |
| Board | `GET /repos/R/board` | `fy board [R]` | `board` |
| Jobs | `GET /jobs?repo=&kind=&state=&before=`, `GET /jobs/ID`; from MP13 `GET /repos/R/candidates/ID` | `fy jobs`, `fy job ID [--wait]`, `fy candidate ID` | `jobs`, `job`, `wait`, `candidate` |
| Changes | `GET /repos/R/changes?state=&review=&attention=&q=&before=`, `GET /changes` | `fy change list [--state S] [--review S] [--attention S] [--all-repos]` | `changes` |
| Change | `GET /repos/R/changes/N` with entries, checks, reviews, links, release and timeline; `GET .../diff` | `fy change view N [--timeline]`, `fy change diff N` | `change`, `change_diff` |
| Reviews | `GET /repos/R/changes/N/reviews`; `POST` `{"verdict":"approve"\|"issue"\|"revert","body","answers"}`; `POST .../review` asks the agent again | `fy change reviews N`, `fy change approve\|issue\|revert N [-m TEXT]`, `fy change review N` | `change_reviews`, `review_add`, `change_review` |
| Inbox | `GET /inbox[?repo=]` | `fy inbox` | `inbox` |
| Issues | `GET /repos/R/issues?state=&claim=&q=`, `GET /issues`, `GET /repos/R/issues/N` | `fy issue list`, `fy issue view N` | `issues`, `issue` |
| Analytics | `GET /stats?repo=&days=`, also `GET /repos/R/board?stats=1` | `fy stats [R] [--days N]`, `fy board --stats` | `stats` |
| Server | `GET /load[?hours=]` (host, store, queue, processes), `GET /runners`, `GET /deploys` | `fy load`, `fy runners`, `fy deploys` | `load`, `runners`, `deploys` |
| Alerts | `GET /alerts` | `fy alerts` | `alerts` |
| Audit | `GET /audit?before=&limit=&event=&subject=` | `fy audit list` | `audit` |
| Palette | `GET /find?q=` | `fy find TEXT` | `find` |
| Live | `GET /events?topics=` | none | none |

A job object carries `change`, `version`, `kind`, `reason`, `session`,
`candidate` (null before MP13) and `checks` as a list, so candidates change
what fills the list and not its form. Members that are not recorded are
`null`. `fy job ID --wait` and the MCP tool `wait` follow `GET /jobs/ID`,
as in [Parallel development](parallel-development.md#the-life-of-a-change);
the event stream serves the pages. Without `state` a list route returns
the open rows; the pages ask for `state=all`. `review` is `flagged` or
`unreviewed`, `attention` is `needs-author` or `reverted`.

`verdict=approve` on a landed change is the human review; on a version
whose entry waits for an approval it releases the entry. `POST
.../reviewed` and `fy change reviewed N` are another spelling of it. A
decision needs the admin scope until roles exist, then the maintain role.

## Monitoring

- **Health.** `/healthz` answers with version and commit. `/readyz` answers
  200 only when the database opens with the schema this release expects,
  the data directory is writable and the lander runs, otherwise 503 with
  the failing condition.
- **Request log.** One JSON line per request with method, route pattern,
  status, milliseconds, account and request id; event streams log at close.
- **Metrics.** `GET /metrics` in the Prometheus text format, with a token of
  scope `metrics`, or open inside the WireGuard network
  (`FY_METRICS_OPEN=1`). Labels are `repo`, `kind`, `state`, `runner`,
  `rule`, `process`, `instance` and `file`, with `version` and `commit` on
  `fy_build_info` only; never a change number or a path. A scrape costs
  about twenty indexed counts.

```
fy_build_info{version,commit}          fy_main_verified{repo}                fy_runner_last_seen_seconds{runner}
fy_entries{repo,state}                 fy_main_verified_age_seconds{repo}    fy_runner_busy{runner}
fy_entry_oldest_seconds{repo}          fy_release_failed{repo}               fy_process_up{process}
fy_entries_finished_total{repo,state}  fy_deploy_lag_seconds{instance}       fy_process_restarts_total{process}
fy_checks{kind,state}                  fy_changes_flagged{repo}              fy_host_load1
fy_check_oldest_wait_seconds           fy_changes_unreviewed{repo}           fy_host_memory_available_bytes
fy_mirror_fetch_ok{repo}               fy_review_oldest_flag_seconds{repo}   fy_data_disk_available_bytes
fy_audit_rows                          fy_reviewer_up                        fy_db_bytes{file}
fy_audit_chain_intact                  fy_alert{rule,repo}
```

Totals are counts over rows that are never deleted. Durations are not
exported as histograms; they are in `GET /stats`.

## Alerts

An alert is a condition with a beginning and an end, computed from rows. It
is not an event and does not repeat per occurrence.

| Rule | Fires when | Since |
|---|---|---|
| `main-broken` | the newest finished verification of a repository failed | that verification |
| `queue-stalled` | an entry has been queued for 15 minutes while no check of its repository runs, or a check runs longer than three times the 95th percentile of its kind | the entry, the check |
| `runner-lost` | checks are queued and no runner was seen for 2 minutes, or a runner with a running check was not seen for twice the longest check of the week | last seen |
| `deploy-behind` | verified `main` is ahead of the serving commit for 15 minutes, the newest build failed, or a release was refused | the first landing not served |
| `reviewer-down` | the review agent is not configured, three reviews in a row failed, or a landed change has waited 30 minutes for its review | the first of them |
| `review-backlog` | the unreviewed significant changes of a repository exceed its limit, so that nothing lands there ([Workflow](workflow.md#guardrails-for-merge-first)), or the oldest undecided flag has waited 48 hours | the oldest |
| `disk-low` | less than 10 % or 5 GiB free in the data directory | the first sample below |
| `audit-broken` | the chain check, run every 5 minutes from the last checked entry, fails | the entry |
| `mirror-failing` | a repository's fetch has failed for 10 minutes | the last success |

Delivery: the firing alerts are the first section of the inbox and of the
overview, in the order of this table, and colour the mark in the top bar;
`GET /alerts`, `fy alerts` and `fy_alert{rule,repo}` give the same list. An
outside monitor that scrapes `/metrics` and `/readyz` belongs to every
installation, because an instance cannot report its own death; it also
sends the notifications. Where there is none, `FY_ALERT_WEBHOOK` makes the
instance post one JSON message when a rule starts to fire and one when it
ends, evaluated once a minute.

## What fortui gains

fortui draws a page from a tree of nodes; its browser host is the small
script that turns each node into a DOM element. Charts need no canvas. A
bar is an empty text node of a given width, drawn by CSS; a sparkline is a
text of the block characters `▁▂▃▄▅▆▇█`, which also works in the terminal
and in the HTML the server sends. Each chart prints its current, lowest and
highest value as text. The additions are the items MIF1 to MIF6 of the
plan:

1. **Keys** (MIF1). The browser host forwards key presses outside text
   fields. `ui_shortcut(chord)` registers a chord such as `j` or `mod+k`;
   the host prevents the browser's default only for registered chords.
2. **Focus** (MIF2). `ui_focus(id)`, `ui_reveal(id)` (scroll into view),
   and arrow keys and Enter delivered while a text field has the focus (the
   palette).
3. **`look=`** (MIF3) on text, link, button, row and column, emitted as
   `data-look`, so a style does not depend on an id suffix; and the tone
   `UI_TONE_ATTENTION`.
4. **`anchor=`** (MIF4), emitted as the HTML `id`, so `#L812` and `#checks`
   scroll before any script runs.
5. **Unchanged nodes are skipped** (MIF5). The host updates only the
   elements whose node changed since the last frame, and a row or column
   with an id keeps its element when rows are inserted before it, so a
   live update touches one row. `FUI.stats` reports frame time, bytes and
   nodes for the budget tests.
6. **A plot node** (MIF6), `plot(id, values, kind)`, drawn as inline SVG
   (`polyline`, `rect`) by `fortui_html` and the browser host, coloured
   through the tones, with block characters in the terminal. Diffs stay
   text, which keeps find-in-page, copy and screen readers.

A virtual list is not needed: a stored log has at most 64 KiB, large files
of a diff fold, lists load by key, and `content-visibility: auto` on code
rows bounds layout and paint.

## Budgets

Each budget is asserted by a test in headless Chromium or by a server test;
a number changes only in a change that says why.

| Quantity | Budget |
|---|---|
| Content visible | with the HTML response; links work before the WebAssembly arrives |
| Server render of a page | 30 ms typical, 100 ms at most |
| Page HTML, including the API answers embedded so that the application starts without a request | 60 KiB compressed for lists, 150 KiB for a change with its files |
| Nodes in a frame | 1500 |
| Frame after an input or an event | 8 ms at the 95th percentile |
| Event to changed row | 1 s; one request per event, none while idle |
| WebAssembly and scripts | 450 KiB and 30 KiB compressed |
| `GET /overview`, `/board`, `/inbox` | 20 ms with 10 000 landings |
| `GET /stats` over 30 days, `GET /metrics` | 50 ms, 20 ms |
| Event streams | one per tab; a worker keeps four request threads free of streams |
| Width | no sideways scroll at 390 px; light and dark both render |
