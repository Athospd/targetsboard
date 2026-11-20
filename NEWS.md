# targetsboard (development version)

First cleanup of the 2024 prototype.

## Breaking changes

* The public API is now `tar_board()`, `tar_board_app()`, `targetsboard()` and
  `update_targetsboard()` (plus the `f()`, `%not_in%` and `%<-%` helpers).
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

## New features

* `tar_board_app()` returns the board as a Shiny app object, to run in the
  foreground or deploy.
* The board follows changes to the pipeline itself: added, removed and
  rewired targets appear in the DAG, which is laid out again.
* A **Load** button switches the pipeline script; the script box no longer
  reloads on every keystroke.
* The board's output goes to a log file: `readLines(app$get_output_file())`.

## Bug fixes and performance

* The background poller no longer keeps a CPU core busy; it polls once a
  second, is shared by all browser sessions and stops with the app.
* The board no longer freezes after a while (unread output pipes) or errors
  on half-written files.
* Only the previewed HTML report's folder is served over HTTP, not the whole
  working directory.
* Per-status text colours, the initial layout and the animation of the edges
  feeding running targets work reliably; the tar_meta table keeps its sorting
  and scrolling.
* All dependencies are declared; golem is no longer needed. ggplot2, plotly
  and skimr are optional.
