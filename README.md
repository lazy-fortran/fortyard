# fortyard

fortyard is a self-hosted forge for Fortran projects: git hosting, CI with
[fo](https://github.com/lazy-fortran/fo), code review, a merge queue, issues, a
shared binary build cache, and AI agents that implement and review changes. It
is written in modern Fortran wherever that is sensible, with C libraries for TLS
and SQLite. One binary, `fy`, is server, build runner, command-line client and
MCP server.

Status: first milestone in progress; see [PLAN.md](PLAN.md).

## Build and test

Needs gfortran, SQLite, [fo](https://github.com/lazy-fortran/fo) and git.

```sh
fo build
fo test
scripts/dev-up.sh        # throwaway local instance; prints URL and token
build/fo/bin/fy login --server http://127.0.0.1:8741 --token TOKEN
build/fo/bin/fy whoami
scripts/dev-down.sh
```

See [DESIGN.md](DESIGN.md) for goals, decisions and architecture, and
[doc/testing-at-scale.md](doc/testing-at-scale.md) for testing large codes.
