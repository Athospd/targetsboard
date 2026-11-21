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

**Focus**:
The one target the author is looking at: the board shows its value and reasons
and highlights what it depends on and what depends on it.
_Avoid_: Selection (when one target is meant), active node

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
