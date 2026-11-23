# Every run is recorded in one history per author, in SQLite, next to the stores, not in them

Stage two needs the board to remember runs, which targets does not do: it keeps only
each target's latest state and the latest run's progress. A recorder joins the progress
file with `tar_meta()` when a run ends and writes a run record to a SQLite database. That
database is one per author, at `tools::R_user_dir("targetsboard", "data")/history.sqlite`,
shared by all their projects. It records every run of a project, including `tar_make()`
from the author's console, not only runs the board started. When a target errors, the
recorder copies its workspace into the history and keeps the last N per target, so past
problems stay debuggable after targets overwrites them. DBI and RSQLite become hard
dependencies. The recorder and the Runs tab ship as 0.3.0, before the board server.
(Amended: they were to be 0.2.0; the value preview overhaul, ADR 0004, ships first as
0.2.0, while the recorder is on hold.)

## Considered Options

- **One history per project, next to its store**: rejected. The board server and a
  cross-project view would have to find and open every project's file, and moving
  to one shared database later would mean migrating data.
- **The history inside the store** (`_targets/user/`): rejected. It would be lost with
  `tar_destroy()`, which authors run routinely.
- **Only runs started from the board**: rejected. Authors run `tar_make()` from the
  console most of the time, so the history would miss most runs.
- **Keep only the latest workspace, as targets does**: rejected. A failure from last
  week could not be debugged once a newer one replaced it.
- **DBI and RSQLite in Suggests, with the history off without them**: rejected. The
  history is what stage two's features stand on, and both packages are small and on CRAN.

## Consequences

The history outlives any one project and can hold paths to projects that no longer
exist, so the board needs a way to forget a project. Copied workspaces cost disk: N is
the author's to set. Recognising console runs depends on watching the progress file
being rewritten without a board run going. Stage three can move the history to Postgres
behind the same DBI calls. This extends ADR 0001's local board without changing its
promise: the history is written by the board's recorder, never into the author's script
or store.
