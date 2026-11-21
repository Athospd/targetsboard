# targetsboard 0.0.0.9000

First cleanup of the 2024 prototype.

## Breaking changes

* The public API is now `tar_board()`, `tar_board_app()`, `targetsboard()`,
  `update_targetsboard()` and `update_targetsboard_status()` (plus the `f()`, `%not_in%` and `%<-%` helpers).
  `app_ui()`, `app_server()`, `tar_visnetwork_bg()`, the
  `dataframe_to_reactflow_*()` converters and `render_and_output_bundle()`
  are internal, and the non-working React component wrappers (`MiniMap()`,
  `Controls()`, ...) and htmlwidget bindings are gone.
* `targetsboard()` and `update_targetsboard()` take the `nodes` and `edges`
  data frames of `targets::tar_visnetwork()$x`; `update_targetsboard()` no
  longer takes `value` and `configuration`.
* `tar_board()`'s `metadata` argument moved after `targets_only`.
* The widget reports its selection as `input$<inputId>_selected_nodes`
  (was `input$selected_nodes`).
* The widget calls running targets "running": their node type is `running`
  (was `dispatched`) and it reports them as `input$<inputId>_running_nodes`
  (was `input$<inputId>_dispatched_nodes`; `_dispatched_nodes_incomers` is
  gone). Targets can no longer be connected by dragging between them.
* The board's value preview is driven by the focused target, not the first
  selected one.

## New features

* Clicking a target focuses it: what it depends on and what depends on it
  stay highlighted and the rest of the graph dims. The widget reports it as
  `input$<inputId>_focused_node`; clicking the background clears it.
* Targets whose last build raised warnings carry a warning mark. The widget
  functions take `warnings`, and `update_targetsboard_status()` can send
  the marks alone.
* When the pipeline changes, targets already drawn keep their place and only
  new ones are placed, next to their neighbours. The layout buttons are now
  **Tidy vertically** and **Tidy horizontally**, to lay out everything again.
* The project's language is in `GLOSSARY.md`, and its scope in
  `docs/adr/0001-local-read-only-companion.md`.
* `tar_board_app()` returns the board as a Shiny app object, to run in the
  foreground or deploy.
* The board follows changes to the pipeline itself: added, removed and
  rewired targets appear in the DAG.
* A **Load** button switches the pipeline script; the script box no longer
  reloads on every keystroke.
* The board's output goes to a log file: `readLines(app$get_output_file())`.
* `update_targetsboard_status()` updates target statuses in place, without
  resending or laying out the graph again.
* A **Refresh** button recomputes the graph on demand.
* **Run all**, **Run selected** and **Stop** buttons run the pipeline from
  the board: `tar_make()` of everything, or of the selected targets and what
  they depend on. Stop also stops a run started outside the board. The
  buttons follow whether a run is going, and the board tells how its own runs
  ended.
* The board tells a running pipeline by its process (`tar_process()`), not
  by targets marked dispatched: a killed run's targets show as canceled
  instead of running forever.

## Bug fixes and performance

* `tar_board()` errors when its port is already in use, typically by a
  board started before, instead of returning a background process that has
  already died.

* The background poller no longer keeps a CPU core busy, is shared by all
  browser sessions and stops with the app. It no longer recomputes
  `tar_visnetwork()` every second: only when the script or a file in `R/`
  changes, when a run ends, or on **Refresh**. In between, the board follows
  runs through the cheap `<store>/meta/progress` file, twice a second. A
  broken script no longer stops it.
* Every targets status has a node style: `completed`, `skipped`, `canceled`
  and others no longer fall back to React Flow's default node.
* The board no longer freezes after a while (unread output pipes) or errors
  on half-written files.
* Only the previewed HTML report's folder is served over HTTP, not the whole
  working directory.
* Per-status text colours, the initial layout and the animation of the edges
  feeding running targets work reliably; the tar_meta table keeps its sorting
  and scrolling.
* All dependencies are declared; golem is no longer needed. ggplot2, plotly
  and skimr are optional.
