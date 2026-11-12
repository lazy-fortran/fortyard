# The GitLab backend

`fy backend gitlab` runs checks of one runner class `gitlab-NAME` on the
runner of a GitLab group (see "The GitLab backend" in
[doc/backends.md](../../doc/backends.md)). It needs:

- a private project in that group, for example `GROUP/fortyard-ci`, used for
  nothing else, with one backend process per project;
- a project access token of that project with the scope `api` and the role
  Owner (deleting pipelines needs it), in a file readable only by the
  backend;
- a runner token of fortyard with the scope `runner`.

Once per project, with this file as `fortyard.yml`:

```sh
fy backend gitlab setup --gitlab https://gitlab.tugraz.at \
    --project GROUP/fortyard-ci --project-token-file gitlab-token \
    --config deploy/gitlab/fortyard.yml
```

This commits `fortyard.yml` to the branch `config` (in an empty project that
branch becomes the default branch, so no check branch ever is), protects it,
makes `fortyard.yml@GROUP/fortyard-ci:config` the project's CI/CD
configuration file, lets maintainers pass pipeline variables, and keeps one
tag per image in the container registry. Run it again after `fortyard.yml`
changed.

The backend process reaches the server over fortyard's internal network
(its allowlist names gitlab.tugraz.at and nothing else of TU Graz):

```sh
fy backend gitlab --server http://fortyard:8740 --token-file runner-token \
    --gitlab https://gitlab.tugraz.at --project GROUP/fortyard-ci \
    --project-token-file gitlab-token --class gitlab-NAME --data /var/lib/fy-gitlab
```

`--max-minutes` (default 15) bounds each check, `--start-wait` (60 s) is how
long a job may wait for the runner before it is cancelled, and `--backoff`
(600 s) the pause after that or after any other trouble of GitLab or the
runner. `--data` holds the mirrors of the repositories, the git credentials
file and the record of fo's cache archives.

Jobs run in `texlive/texlive:latest-minimal`, the only images the
university's group runners allow, and install gfortran and the build tools
with apt; fo is built from its main, which fortyard keeps building with its
dependencies' mains. fo's cache is kept per repository in the project's generic
package registry (package `fo-cache`, version REPO, one file per passing
pipeline). Every job holds a token that can write that registry, so the
backend uploads the archives itself from the passing job's artifact, records
their SHA-256 and gives the next job of the repository the name and SHA-256
of the newest one; a job uses no other archive. The backend keeps its newest
three archives per repository and deletes every other file of the package
version.
