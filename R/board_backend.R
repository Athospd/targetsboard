#' The board's backend
#'
#' What the board keeps outside any browser session, so every session shares
#' it: the background poller that recomputes the graph (`tar_visnetwork_bg()`),
#' the run started from the board, and the value previews built last.
#' Created once per app.
#'
#' @param script Path to the targets script the poller starts on.
#' @param targets_only Show only targets, without functions and globals.
#' @param project The project of `_targets.yaml` it starts on.
#' @param filters tar_visnetwork()'s filters for `script` (visnetwork_filters()):
#'   kept on every redraw of it; another script is drawn whole.
#' @return A list: the `poller` and `runner` environments, the `previews`
#'   cache, the reactive values `rds_path` (the poller's graph file),
#'   `loaded_script` and `project` (the active project of `_targets.yaml`),
#'   and functions to start and stop the poller, switch project, ask the
#'   poller to recompute, and tell whether a run is going.
#' @noRd
board_backend <- function(script, targets_only = TRUE, project = active_project(), filters = list()) {
  # One background poller, restarted only when someone loads a different
  # script.
  poller <- new.env()
  rds_path <- shiny::reactiveVal(NULL)
  loaded_script <- shiny::reactiveVal(script) # the script the poller draws
  project <- shiny::reactiveVal(project) # its project (projects.R)
  previews <- preview_cache()
  first_script <- script

  stop_poller <- function() {
    if (inherits(poller$process, "r_process") && poller$process$is_alive())
      poller$process$kill()
    poller$process <- NULL
  }

  start_poller <- function(script) {
    stop_poller()
    # The author's filters name the opened script's targets: only for it.
    own <- identical(normalizePath(script, mustWork = FALSE), normalizePath(first_script, mustWork = FALSE))
    assets <- tar_visnetwork_bg(script = script, targets_only = targets_only, filters = if (own) filters else list())
    poller$process <- assets$process
    poller$script <- script
    loaded_script(script)
    poller$refresh_path <- assets$refresh_path
    rds_path(assets$tar_vis_path)
  }

  # Another project: this process's TAR_PROJECT, which the poller and the
  # runs inherit, then the poller on `script`, by default the project's
  # own. The previews read the old store: gone.
  switch_project <- function(new_project, script = project_script(new_project)) {
    set_active_project(new_project)
    project(new_project)
    previews$clear()
    start_poller(script)
  }

  # Started by the first session, and stopped with the app.
  start_once <- function() {
    if (!is.null(poller$process)) return(invisible())
    start_poller(script)
    shiny::onStop(stop_poller, session = NULL) # runs when the app stops
  }

  # Ask the poller to recompute the graph now.
  request_refresh <- function() {
    path <- poller$refresh_path
    if (!is.null(path)) writeLines(format(Sys.time(), "%H:%M:%OS6"), path)
  }

  # The run started from the board.
  runner <- new.env()
  run_alive <- function() {
    inherits(runner$process, "process") && runner$process$is_alive()
  }
  # Counts the board's own run from the start: targets records the process
  # only once the pipeline is loaded.
  busy <- function() !is.null(active_run()) || run_alive()

  list(
    poller = poller,
    runner = runner,
    previews = previews,
    rds_path = rds_path,
    loaded_script = loaded_script,
    project = project,
    switch_project = switch_project,
    start_poller = start_poller,
    stop_poller = stop_poller,
    start_once = start_once,
    request_refresh = request_refresh,
    run_alive = run_alive,
    busy = busy
  )
}
