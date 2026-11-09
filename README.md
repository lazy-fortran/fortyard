# fortyard

fortyard is a self-hosted forge for Fortran projects: git hosting, CI with
[fo](https://github.com/lazy-fortran/fo), code review, a merge queue, issues, a
shared binary build cache, and AI agents that implement and review changes. It
is written in modern Fortran wherever that is sensible, with C libraries for TLS
and SQLite. One binary, `fy`, is server, build runner,
command-line client and MCP server.

Status: design phase. No code yet.

See [DESIGN.md](DESIGN.md) for goals, decisions, architecture and milestones.
