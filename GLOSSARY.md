# targetsboard

A local companion to a targets pipeline that helps its author understand it:
its structure, what is outdated and why, what each target produced, and what a
run is doing. It replaces `targets::tar_watch()` for the author, with its own design.

## Language

### People and product

**Author**:
The person who writes and iterates on a targets pipeline, on their own machine,
with the board open beside their editor. The one user the project is designed for.
_Avoid_: User (when the author is meant), viewer, operator

**Board**:
The application the author opens, in a browser tab, to look at their pipeline.
The project's product. It only looks: it never changes the script or the store.
_Avoid_: Dashboard, app, monitor, UI

**Control**:
Anything that changes the pipeline's state from the board, such as running it.
An optional layer on top of the board, never part of its core.
_Avoid_: Actions, operations

**DAG widget**:
The interactive drawing of the pipeline's graph that the board is built around,
also usable on its own in other Shiny apps without any promise of stability.
_Avoid_: targetsboard (for the widget), visNetwork, graph component

### The pipeline

**Target**:
One step of the pipeline, and what each box in the graph stands for.
Functions and global objects are not targets: they appear only as reasons.
_Avoid_: Node, step, job

**Pattern**:
A target that stands for many branches; the graph shows it folded, as one target
taking the worst status among its branches, with a count of each status.
_Avoid_: Dynamic target, map

**Branch**:
One of the targets a pattern stands for, shown only when its pattern is unfolded.
_Avoid_: Child, sub-target, iteration

**Reason**:
Why a target is outdated, as targets itself reports it: its command, its dependencies
or its settings changed, or it was never built; or else, that something upstream of
it is outdated. Names no particular dependency.
_Avoid_: Cause, trigger, root cause

**Value**:
What a target produced, as `tar_read()` returns it; for a file target, the file.
_Avoid_: Output, artifact, result

**Preview**:
How the board shows a target's value: a frame with what the store knows about the
target, and a tab for each view of the value, the last one what `print()` shows.
_Avoid_: Value viewer, output panel, render

**Renderer**:
What gives the views of a value of some class: a `tar_board_preview()` method, from
targetsboard, another package, or the project's `_targetsboard.R`.
_Avoid_: Bundle, output function, display hook

**Focus**:
The one target the author is looking at: the board shows its value and reasons
and highlights what it depends on and what depends on it.
_Avoid_: Selection (when one target is meant), active node

**Group**:
Two or more targets the author selected together, to run, invalidate or copy
at once. The board highlights the group's lineage instead of the focus's.
_Avoid_: Multi-selection, batch, set

**Debugging**:
Finding why a target errored or warned, by bringing its failure back into the
author's own R session. The board shows what went wrong and prepares those
steps; the interactive part happens in the author's console.
_Avoid_: Troubleshooting, inspecting

**Problem**:
What went wrong with a target in its last build: its error, or its warnings,
with what helps find the cause.
_Avoid_: Issue, failure (for the whole), diagnostics

**Workspace**:
What a target saw when it ran (its inputs, the globals and its seed), saved so
the author can load it into a session and run the target's command again.
_Avoid_: Snapshot, environment dump, checkpoint

**Traceback**:
The calls that led to a target's error, from its command down to where it
was raised.
_Avoid_: Stack trace, call stack

**Debug run**:
A run that stops inside one target, in the author's console, to step through
its command.
_Avoid_: Debug mode, interactive run

**Lineage**:
A target, or a group, with everything it depends on and everything that depends
on it, at every level, not only the direct neighbours.
_Avoid_: Neighbourhood, family, ancestry

### Statuses

A target on the board is always in exactly one of these four statuses.

**Up to date**:
The target's value is current: the next run would skip it.
_Avoid_: Built, completed, skipped, done

**Outdated**:
The target would be built again by the next run, for a reason it carries.
Also covers a target whose run was cancelled or that is waiting in a run.
_Avoid_: Stale, invalidated, dirty, canceled, queued

**Running**:
The target is being built right now.
_Avoid_: Dispatched, in progress, busy

**Errored**:
The last attempt to build the target failed. Takes precedence over outdated.
_Avoid_: Failed, broken

**Warning mark**:
A sign on an up-to-date target that its last build raised warnings. Not a status.
_Avoid_: Warned (as a status)

### Runs

**Run**:
One pass of the pipeline that builds outdated targets; the board can watch one live.
_Avoid_: Make, build, job

**Run record**:
A run as the history keeps it: when and why it started, its outcome, and each
target's outcome in it. Designed for stage two; not built yet.
_Avoid_: Run log, execution, job history

**History**:
Every run record of every project of the author, in one place on their machine.
Remembers what happened, never the values themselves.
_Avoid_: Audit log, archive, database (for the concept)

**Grid**:
Runs against targets, each cell showing what happened to that target in that run.
_Avoid_: Matrix, heatmap, timeline

### Running without the author

These are designed for stage two and not built yet.

**Project**:
A pipeline's script and its store, as the board server knows it.
_Avoid_: Pipeline (for the registered pair), workspace, repo

**Board server**:
The long-running process that starts runs of its projects when nobody has the board open.
_Avoid_: Daemon, scheduler (for the whole process), backend

**Schedule**:
When a project runs on its own, as a cron expression with a timezone.
_Avoid_: Cron job, timer, trigger

**Event**:
A change outside the pipeline that asks for a run: a watched path changing, or a
check returning `TRUE`.
_Avoid_: Sensor, asset, trigger, hook

**Alert**:
A message sent to the author about a run's outcome, or about a run that did not happen.
_Avoid_: Notification (the board's in-browser messages), alarm
