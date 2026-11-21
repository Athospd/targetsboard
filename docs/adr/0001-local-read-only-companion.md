# The board is a local, read-only companion that replaces tar_watch() for the author

targetsboard is built for one person: the author of a targets pipeline, on their own
machine, with the board in a browser tab beside their editor. Its main job is to help
them understand the pipeline (structure, what is outdated and why, what each target
produced), with watching a run in support. It replaces what authors use
`targets::tar_watch()` for, so they never need to open it, but with its own design
rather than a copy of `tar_watch()`'s panels and settings. The board only reads the
script and the store; anything that changes them (running, stopping, invalidating) is
an optional control layer added later. Deploying the board for a team, shared stores
and authentication are out of scope: `tar_board_app()` makes deploying possible
without the project supporting it.

This scope is a first stage, not the vision. The project aims to become an
orchestrator in the class of Airflow or GitHub Actions, where users register DAGs,
monitor them and rerun them, and where a workflow is run step by step. What would
set it apart is that its steps carry analytical and data content rather than
deployment and integration tasks. The narrow scope is meant to get understanding
right first. Later stages, with more users, shared DAGs, control and orchestration,
should build on this core rather than work around it.

## Considered Options

- **Complement `tar_watch()`** and leave monitoring to it: rejected, because the
  author should need only one tool.
- **Feature parity with `tar_watch()`**: rejected, because it would make monitoring
  the main job and bring back a table-first layout.
- **A shared dashboard on a server**: deferred, not rejected. It is part of the
  vision, but authentication, shared stores and concurrency would shape every other
  decision before the core exists.

## Consequences

The Run, Run selected and Stop buttons already in the code belong to the control
layer: they are not part of the board's core, and the core must work without them.
