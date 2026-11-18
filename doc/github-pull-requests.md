# GitHub pull requests

GitHub is a mirror. Review a pull request there without building or running its
code, then obtain a person's approval to execute that exact head. Import it with:

```sh
fy ingest REPO PR --head FULL_SHA --approve-execution
```

`REPO` is the Fortyard repository name, `PR` the GitHub pull request number and
`FULL_SHA` its reviewed 40-character commit hash. The command needs an admin
token and a primary repository with a GitHub upstream. It fetches Git objects
without a checkout or submodules, compares the fetched and current PR heads
against `FULL_SHA`, and refuses a changed head. Approval is recorded in the
audit log with the account, source PR and hash.

The imported change preserves the Git commit authors and records a human
review for the approved version. Its branch cannot be changed by a push.
Import queues no checks and does not land the change. Review its description
and diff, then use the usual landing command to run the configured checks:

```sh
fy change view NUMBER --repo REPO
fy change diff NUMBER --repo REPO
fy change land NUMBER --repo REPO
```

Do not merge the pull request on GitHub. The GitHub backend sends the
[closing receipt](#closing-receipts).

The API is `POST /api/v1/repos/REPO/ingest` with the body
`{"pr":"PR","head":"FULL_SHA","approve_execution":true}`. Missing, false or
non-boolean approval is refused before fetching. The response identifies the
change, branch, head and source, with `checks_queued:false`. Reimporting the
same current head returns the existing change.

The MCP tool `ingest` takes `repo`, `number`, `head` and the boolean
`approve_execution`. It has the same approval and import checks as the CLI.

## Closing receipts

The GitHub backend (`fy backend github`, see
[Operating the GitHub backend](github-backend.md)) closes the pull requests of
landed branches. At its start and every ten minutes it asks the server for the
landings of the last 14 days that have no receipt yet, in primary repositories
whose upstream belongs to its `--owner`. For a landing of branch `B` as commit
`C` in `OWNER/NAME` it handles:

- every open pull request whose head is `OWNER:B`, and
- for an ingested change (branch `external/github/PR/HEAD`), pull request `PR`.

Each gets one comment `Landed as C on main through fortyard: URL.`, with `URL`
the change's page when the server has `FY_PUBLIC_URL` or `FY_URL`, and is
closed, never merged. Nothing is touched until GitHub's `main` contains `C`.
The backend then records the receipt on the server, so a restart sends nothing
twice, and a comment that already names `C` is not repeated. A failed request
is logged and retried at the next sweep. The landing never waits for its
receipt. A pull request from a fork with the same branch name stays open. The
backend's GitHub token needs Pull requests (read and write) and Contents
(read) on the repository for its receipts.

The server's routes need the runner or admin scope:
`GET /api/v1/receipts?owner=OWNER` lists the due receipts and
`POST /api/v1/receipts/N` with `{"prs":"435,436"}` records the receipt of
landing `N`. Proposal listing remains part of milestone M10.
