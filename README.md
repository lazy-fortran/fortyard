# fortyard

**Retired on 2026-11-26 and no longer developed.** The repositories it hosted are developed on GitHub again, with pull requests and GitHub's merge queue. This repository is kept as a record of the experiment.

fortyard is a self-hosted forge for Fortran projects: git hosting, CI with
[fo](https://github.com/lazy-fortran/fo), code review, a merge queue, issues, a
shared binary build cache, and AI agents that implement and review changes. It
is written in modern Fortran wherever that is sensible, with C libraries for TLS
and SQLite. One binary, `fy`, is server, build runner, command-line client and
MCP server.

Status: first milestone in progress; see [PLAN.md](PLAN.md).

## Build and test

Needs gfortran with OpenMP, SQLite, [fo](https://github.com/lazy-fortran/fo) and git.
The web application also needs LFortran, an emscripten SDK in `EMSDK_PATH`
and access to [fortui](https://github.com/lazy-fortran/fortui); its browser
test needs Node and Chromium.

```sh
fo build
fo test
scripts/dev-up.sh        # throwaway local instance; prints URL and token
build/fo/bin/fy login --server http://127.0.0.1:8741 --token TOKEN
build/fo/bin/fy whoami
scripts/dev-down.sh
```

The web application is Fortran compiled to WebAssembly and drawn with fortui.
The server composes the first frame of each page with the same Fortran code and
sends it as HTML, so pages show before the WebAssembly loads:

```sh
scripts/build-web.sh . build/web               # fortui at the version fpm.toml pins
build/fo/bin/fy serve --data DIR --web build/web
node test/web_app_test.mjs build/fo/bin/fy build/web
FY_TEST_BASE=/yard node test/web_app_test.mjs build/fo/bin/fy build/web   # below a path prefix
```

See [DESIGN.md](DESIGN.md) for goals, decisions and architecture, and
[doc/testing-at-scale.md](doc/testing-at-scale.md) for testing large codes.
Instance administrators can [replace primary repository history](doc/history-rewrites.md)
with leased refs, private backups and recovery.
