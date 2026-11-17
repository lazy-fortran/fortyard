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

After a successful landing and mirror push, comment on the GitHub PR with the
landed commit and close it. Do not merge it on GitHub. Proposal listing and
automatic closing receipts remain part of milestone M10.

The API is `POST /api/v1/repos/REPO/ingest` with the body
`{"pr":"PR","head":"FULL_SHA","approve_execution":true}`. Missing, false or
non-boolean approval is refused before fetching. The response identifies the
change, branch, head and source, with `checks_queued:false`. Reimporting the
same current head returns the existing change.

The MCP tool `ingest` takes `repo`, `number`, `head` and the boolean
`approve_execution`. It has the same approval and import checks as the CLI.
