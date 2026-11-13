# Replacing primary repository history

`fy admin rewrite` installs a reviewed, self-contained Git bundle in a primary
repository and remaps its source navigation. Run it on the instance's GNU/Linux
host, with the instance owner's account and the instance's data directory.

First deploy a fortyard version that supports this command. Pause automatic
deployment and incoming pushes for the maintenance window, finish or cancel
queued or running checks and landings for the target repository, then stop the
fortyard server and wait for its workers to exit. Older servers do not hold the
maintenance lock. Other services on the host can keep running. The command
refuses a running instance, an unfinished rewrite, and queued or running checks
or landings for the target repository.

Prepare these inputs from the primary repository, including its private
`refs/fortyard/changes/` histories:

- A complete expected-ref file, captured with
  `git --git-dir=REPOSITORY.git show-ref`. Every ref and object ID must still
  match when the replacement is applied.
- A complete cleaned bundle, created with `git bundle create clean.bundle --all`
  in the rewritten repository. Preserve each operational ref, including existing
  notes and change versions. Copied `refs/pull/` refs are omitted from the cleaned
  repository; `refs/replace/` refs are refused. New refs must be branches, tags or
  `refs/fortyard/` refs. The primary's `HEAD` must point directly to
  `refs/heads/main` throughout maintenance.
- A complete commit map with one `old40 new40` row per original commit, using
  lower-case, nonzero SHA-1 object IDs. The optional header is `old new`.
  Include unchanged commits as identity rows. Removed commits need a reviewed
  replacement commit; zero IDs are refused. Each destination must exist as a
  commit in the cleaned bundle, and the previous `main` must map to its replacement.

Then run:

```sh
fy admin rewrite NAME --data DATA_DIRECTORY \
  --bundle clean.bundle --expect-refs expected.refs --commit-map commit-map
```

Add `--check` to validate the bundle, ref leases, map, queue and source metadata
without changing refs, source pointers, audit or verification requests. This
prepares private backup objects and a journal. A batch controller can preflight
every repository before importing any of them. `--journal-id ID` selects a
predictable private journal name, using up to 96 lower-case hexadecimal digits
and hyphens; each attempt needs a new ID.

The command preserves the repository's configuration and hooks, backs up all
original refs and their objects in a Git bundle, and snapshots the complete
SQLite database. Its private journal is under `DATA_DIRECTORY/history-rewrites/`;
keep that backup private because it contains the original history. The journal
and repository must be on the same filesystem. Existing Git objects are never
overwritten. Ref updates are one leased Git transaction and still run native
reference-transaction hooks.

Source pointers in changes and their versions are remapped, and affected reviews
need review again. Completed checks, landings, releases, reviewed commit IDs,
audit events and discussion text keep their exact original evidence. A rewrite
adds an audit event and requests fresh verification of the cleaned `main`.
Passing results from the original history do not verify its replacement.

Full original commit IDs in source tree, blob, log and commit requests resolve
through the recorded map. Responses identify the original and resolved IDs with
`X-Fortyard-Rewritten-From` and `X-Fortyard-Resolved-Ref` headers. Verification
and build evidence continue to use exact commit IDs.

An interrupted operation blocks server startup. Recover it with the journal
named in `DATA_DIRECTORY/history-rewrite.pending`:

```sh
fy admin rewrite NAME --data DATA_DIRECTORY --recover JOURNAL_DIRECTORY
```

Recovery restores the original refs and database if no completion event was
committed. Once that audit event exists, recovery finishes the replacement and
preserves the complete audit chain. It refuses to overwrite refs changed since
the interrupted operation. An already completed journal can be verified again
with the same recovery command, even after its pending marker was cleared.
`--recover` and `--check` cannot be combined.

A batch controller may also keep `DATA_DIRECTORY/history-rewrite.batch` until
all repository imports are complete. This marker blocks server startup while
local administrative commands remain available. Resume that controller with its
exact reviewed inputs. Once every import succeeds, the controller removes the
batch marker and restarts only fortyard. Then check its health and fresh
verification result.

Publish the cleaned history to each one-way mirror with explicit expected old
object IDs and `--force-with-lease`, including each mirrored branch and tag.
The normal fast-forward mirror operation cannot publish a history replacement.
Compare primary and mirror refs before resuming writes. The command does not
publish release tags, modify external archives, expire reflogs or garbage-collect
objects. Remove unreachable original objects only after verification, with the
private recovery backups retained.
