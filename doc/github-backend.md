# Operating the GitHub backend

How to run checks of public repositories on GitHub's hosted runners. The
design is in [Check backends](backends.md#the-github-backend).

## Once per GitHub organisation

An owner of the organisation creates a GitHub App (for example
`fortyard-checks`) with the repository permissions **Actions: read and
write** and **Metadata: read**, nothing else, no webhook, and installs it for
the public repositories that use the backend. Note the App's id and download
one private key (`app.pem`); the key goes only into the backend's container.

## Once per repository

1. The repository is public in fortyard (`fy repo public NAME on`), its policy
   allows the backend (`fy repo policy NAME backends=github`), and its
   upstream is the GitHub repository, which fortyard reaches with the
   repository's deploy key (`core.sshCommand` of the mirror, as for pushes of
   `main`). The repository is primary in fortyard: a mirror's checks are not
   published. The GitHub repository belongs to `--owner` but may have another
   name; the backend takes owner and name from the upstream's address.
2. Land `deploy/github/fortyard.yml` of the running fortyard release as
   `.github/workflows/fortyard.yml`. The backend gives every check whose copy
   differs back to its home class, so the file is updated with each fortyard
   release that changes it.
3. Run the setup:

   ```sh
   fy backend github setup GITHUB-NAME --owner ORG --github-token-file owner-token
   ```

   It refuses a private repository, one with repository, environment or
   organisation secrets, and one whose `main` lacks the shipped workflow. It
   then allows only the pinned `actions/checkout` and `actions/cache`, sets the
   default workflow token to read-only and log and artifact retention to one
   day, and disables every other workflow. These settings need the
   repository's Administration (write) and Secrets (read) permissions, which
   the App deliberately lacks: run the setup with a fine-grained token of an
   organisation owner for that one repository (`--github-token-file`), used
   once and not stored. `--app-id ID --app-key app.pem` instead of the token
   works only for an App that has been given those permissions.

4. Keep `.github/workflows` to `fortyard.yml` and the workflows the setup
   disabled. GitHub runs every workflow of a pushed commit whose trigger
   matches, and a workflow may raise its own token's rights with
   `permissions:` whatever the repository's default is. The backend therefore
   gives back every check whose `.github/workflows` holds any other file
   (a new workflow, or one that is active on GitHub); such a check runs on
   fortyard's own runners. A workflow added to the repository later needs
   the setup again before checks of its commits go to GitHub.

## The backend process

```sh
fy backend github --server http://fortyard:8740 --token-file runner-token \
    --app-id ID --app-key app.pem --owner ORG \
    [--classes github-linux,github-macos-arm64,github-windows] \
    [--max-running 20] [--max-macos 5] [--max-minutes 360]
```

The runner token needs the runner scope; the process releases the checks it
held when it starts, so a restart queues them again. It notices a check withdrawn
on the server (a landing candidate that lost a member) within six polls and
cancels its run. Every ten minutes it deletes finished runs of check branches
older than an hour and cancels runs of check branches whose check no longer
runs here, such as a run GitHub started only after its check was given back.
The check's time limit is at most `--max-minutes` (the workflow's own
`timeout-minutes` is 360); a job that GitHub ends at its limit counts as a
timeout, not as trouble of the runner. `--poll` (seconds
between requests about a run, default 5) and `--start-timeout` (seconds a run
may take to start before its check is given back, default 600) rarely need
changing. Its network needs only `api.github.com` (and the fortyard server);
GitHub's log downloads redirect to `*.blob.core.windows.net` hosts, which the
backend's allowlist must therefore also name.
