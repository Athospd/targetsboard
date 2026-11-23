# Stage two: runs that happen without the author

Status: proposal, for discussion. Nothing here is built.

ADR 0001 scopes stage one to a local, read-only board for one author and names
the vision: an orchestrator in the class of Airflow, whose steps carry
analytical content. This document designs the next stage: **pipelines that run
on a schedule or on an event, with every run kept**, still for one author or a
small team on one machine or server. Authentication, several teams and remote
workers stay in stage three.

## Where the board stands against an orchestrator

The full comparison with Airflow is in `airflow-comparison.md`.

The full comparison with Airflow is in `airflow-comparison.md`.

Airflow is a scheduler of runs. Each run is a dated record kept in a database,
started by a clock, an event or a person, and its UI is mostly a grid of runs
against tasks. targets is a build system. It keeps only the latest state of
each target and skips whatever is up to date. The board today shows that
latest state very well, and it can start runs. It does not remember them.

| Needed for stage two | targets gives | The board adds today | Missing |
|---|---|---|---|
| A record of each run | The progress file: each target's outcome in **the latest run only** | The Log tab of the board's own latest run | A run history |
| Durations, sizes, value changes over time | `tar_meta()`: `time`, `seconds`, `bytes` and the `data` hash, latest build only | n/a | History of the same fields |
| Problems of past runs | Workspaces, overwritten by the next failure | The Problem pane, latest only | Problems kept per run |
| Runs without the author | `tar_make()` | Runs started from the board | A process that outlives the browser, a scheduler and a queue |
| Being told | n/a | Browser notifications | Alerts on run outcomes |

Checked with targets 1.12.1: each `tar_make()` rewrites the progress file, with
every target's outcome in that run (`completed`, `skipped`, `errored`,
`canceled`), and `tar_meta()` changes only for the targets that were built.
Joining the two at the end of a run gives a complete record of it, without
changing targets.

## The design

Four parts, each shippable alone, in this order:

```
                 ┌──────────────────────── board server (one R process) ─────────────────────────┐
  schedules ───► │  scheduler ──► queue (one run per store) ──► runner (tar_make, callr) ──┐     │
  file events ─► │                                                                          ▼     │
  Run button ──► │                                    recorder ◄── progress file + tar_meta()     │
                 │                                        │                                      │
                 └────────────────────────────────────────┼──────────────────────────────────────┘
                                                          ▼
                                       history database (SQLite) ──► board UI: Runs tab, grid, alerts
```

### 1. The recorder and the history database (first)

Everything else needs the history, so it comes first, and it is useful on its
own: the board would remember the runs it watches, including `tar_make()` run
from the author's console.

**Where.** A SQLite file, through DBI and RSQLite, with a single writer, the
recorder. Default `tools::R_user_dir("targetsboard", "data")/history.sqlite`,
shared by every project of the author, so stage two's project list and stage
three's server need no migration of the data. Postgres replaces SQLite in stage
three behind the same DBI calls.

**What is recorded.**

```
project   id, script (absolute path), store (absolute path), name, added_at
run       id (ULID), project_id, trigger (board | console | schedule | event | api),
          mode (keep_going, stop_at_warnings), names (requested targets, or all),
          started_at, ended_at, outcome (succeeded | errored | stopped | crashed),
          r_version, targets_version, git_commit (of the project, when it is a repo)
build     run_id, name, parent (for branches), type, outcome (completed | skipped |
          errored | canceled), seconds, bytes, data (hash), warnings, error
problem   run_id, name, traceback, workspace_path (a copy, see the retention question)
log       run_id, text (the run's console output; a file for long logs)
```

`build` has a row for every target and branch of the run, skipped ones
included: the grid needs them. Patterns keep their branch rows, so the grid can
fold a pattern the way the graph already does.

**How.** At the end of a run (the progress file has settled and the process is
gone, the test the poller already uses), the recorder joins the progress file
with `tar_meta()` and writes one transaction. During a run it can update the
`run` row as progress changes, so a crash still leaves a run marked `crashed`.
Runs from the console are recognised when the progress file is rewritten
without a board run going (`targets::tar_process()` names their process).

**What is not recorded: values.** targets overwrites values, and copying them
would grow without bound. The `data` hash still tells whether a value
changed between runs. Authors who need old values can use targets' own
version-aware cloud storage, which the history can point to later.

### 2. The Runs tab and the grid

- **Runs**: one row per run (when, trigger, duration, outcome, counts of built,
  skipped and errored targets), newest first.
- **Grid**: runs against targets, each cell coloured by the target's outcome in
  that run, as in Airflow's Grid view. A cell opens that run's Problem pane or
  build details. Skipped cells are pale, so the work each run actually did shows
  at a glance, which Airflow cannot show.
- **A target's history**: the focus gains its durations and sizes over runs, and
  the runs where its value changed.
- **Compare two runs**: which targets were rebuilt, and why the values differ.

### 3. The board server, the queue and the scheduler

A long-running R process, separate from the Shiny app, started with something
like `targetsboard::board_server()` and kept alive by the operating system
(systemd, launchd, Windows Task Scheduler, or a Docker container).

- **Projects**: the server runs any number of registered projects (script plus
  store). Registering one is a function call or a form in the board.
- **Queue**: targets can run only one `tar_make()` per store at a time, so the
  queue holds at most one waiting run per project and merges repeated
  requests. This matches Airflow's `max_active_runs=1` with `catchup=False`,
  which is what analytical pipelines nearly always want.
- **Schedules**: cron expressions per project, with a timezone. A scheduled run
  is cheap when nothing changed, because targets skips everything that is up
  to date. So schedules can be frequent.
- **Events**: a run when a watched path changes (a raw data folder, a file
  target's source) or when a check function returns `TRUE` (a database table's
  row count, an API's last-modified date). This is Airflow's assets, for R.
  `tarchetypes::tar_age()` and `tar_cue_age()` already cover data that goes
  stale with time, inside the pipeline.
- **Runner**: `tar_make()` in a callr process, through the run-mode wrapper of
  ADR 0002, with `crew` controllers when the project sets them.
- **The Shiny board** becomes a client of the server: it reads the history
  database and the progress file, and asks the server for runs through a small
  local HTTP API (plumber2 or ambiorix). Stage one's board keeps working alone,
  with no server, as it does today.

### 4. Alerts

On a run's outcome (errored, back to succeeded, or slower than usual), per
project: email (blastula), a Slack or Teams webhook, or an R function the
author supplies. Each alert links to the run in the board.

## Considered options

- **Leave scheduling to Airflow, cron or GitHub Actions, and build only the
  history and UI**: still possible, and the recorder supports it, because
  console runs are recorded. Rejected as the main path, because R pipelines
  inside Airflow are opaque Bash or Docker tasks. Airflow 3.4's language SDKs
  cover Java, Go and TypeScript, experimentally, not R.
- **Keep the history in the targets store** (`_targets/user/`): rejected. It
  would be per project and lost with the store, and `tar_destroy()` is routine.
- **Store values per run**: rejected. See "What is not recorded".
- **Write the server in another language** (Go or Python): rejected for stage
  two. Every part of it calls targets, and one R process keeps installation to
  `install.packages()`.

## Decisions for the author

1. **History location**: one database per user (proposed), or one per project
   next to the store?
2. **Problem retention**: copy each failing run's workspaces into the history
   (complete debugging of past runs, more disk), or keep only the latest, as
   targets does? Proposed: copy, keep the last N per target.
3. **Console runs**: record `tar_make()` runs from the author's console
   (proposed), or only runs started from the board or the server?
4. **Dependencies**: DBI and RSQLite in Imports for stage 2.1 (proposed), or in
   Suggests, with the history switched off without them?
5. **First release**: ship the recorder and the Runs tab (parts 1 and 2) as
   0.2.0 before starting the server (proposed)?

## Terms this would add to the glossary

**Run record** (a run kept in the history), **History**, **Grid**, **Board
server**, **Project** (a script and its store, registered with the server),
**Schedule**, **Event** (a watched path or check that requests a run),
**Alert**. They go into `GLOSSARY.md` once the decisions above settle them.
