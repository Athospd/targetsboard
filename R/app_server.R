#' The board's server
#'
#' A few module calls: the top bar (chrome_server()), the theme picker and
#' the dark mode button (theme_server()), and the board (board_server()), which wires the board's
#' own modules together.
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @param targets_only Show only targets, without functions and globals.
#' @param scripts Other scripts the Script field offers.
#' @param project The project of `_targets.yaml` the board starts on.
#' @noRd
app_server <- function(metadata, script = targets::tar_config_get("script"), targets_only = TRUE,
                       scripts = NULL, project = active_project(), filters = list()) {
  # One backend per app: the poller, the board's run and the value
  # previews, shared by every browser session (board_backend()).
  backend <- board_backend(script, targets_only, project, filters)

  function(input, output, session) {
    backend$start_once()
    chrome_server("chrome")
    theme <- theme_server("theme")
    board_server(
      "board", backend, metadata, script, scripts, targets_only,
      color_mode = theme$color_mode
    )
  }
}

# The board's display options: the widget's defaults, plus a right-click
# menu on each target and on a selected group, a toolbar over the focused
# target or the group, and a download button for an image of the graph. A
# group has no value to show.
board_options <- function() {
  targetsboard_options(
    node_actions = c(
      run = "Run this target",
      invalidate = "Invalidate this target",
      value = "Show its value",
      copy_name = "Copy its name",
      select_upstream = "Select what it depends on",
      select_downstream = "Select what depends on it",
      select_lineage = "Select its lineage"
    ),
    # Copy copies its name, as copy_name in the menu: for one target only.
    toolbar_actions = c(run = "Run", invalidate = "Invalidate", value = "Value", copy_name = "Copy"),
    node_group_actions = c(
      run = "Run {n} targets",
      invalidate = "Invalidate {n} targets",
      copy_name = "Copy {n} names",
      select_upstream = "Select what they depend on",
      select_downstream = "Select what depends on them",
      select_lineage = "Select their lineage"
    ),
    toolbar_group_actions = c(run = "Run {n}", invalidate = "Invalidate {n}"),
    download_image = TRUE
  )
}

# The targets among the graph's nodes (tar_visnetwork()$x$nodes): without
# the functions and globals, when those are drawn too.
board_targets <- function(nodes) {
  if (is.null(nodes) || nrow(nodes) == 0) return(character())
  if (!"type" %in% names(nodes)) return(as.character(nodes$id))
  as.character(nodes$id[nodes$type %in% c("stem", "pattern")])
}

#' Statuses of the run in progress
#'
#' @param progress `list(mtime, data)`: the progress file's modification time
#'   and `targets::tar_progress()` (with its `type` and `parent`).
#' @param since The progress file's modification time when the graph was last
#'   recomputed: older progress is already in the graph, or stale.
#' @return A named character vector of statuses, possibly empty.
#' @noRd
live_statuses <- function(progress, since) {
  data <- progress$data
  fresh <- !is.null(data) && nrow(data) > 0 && !is.na(progress$mtime) &&
    (is.null(since) || is.na(since) || progress$mtime > since)
  if (!fresh) return(stats::setNames(character(), character()))
  stats::setNames(data$progress, data$name)
}

#' Show invalidated targets as outdated, not errored
#'
#' `targets::tar_invalidate()` deletes a target's metadata but not its line in
#' the progress file, so `tar_visnetwork()` still calls an invalidated target
#' "errored". Keep that status only where the metadata still records an
#' error: for the target itself or for one of its branches (a pattern that
#' stopped on an error has no record of its own, only its failed branch has).
#'
#' @param nodes `tar_visnetwork()$x$nodes`.
#' @param meta `tar_meta(fields = c("parent", "error"))`, or `NULL` when there
#'   is no store yet.
#' @noRd
clear_invalidated_errors <- function(nodes, meta) {
  if (is.null(meta) || !nrow(nodes)) return(nodes)
  failed <- !is.na(meta$error)
  recorded <- unique(stats::na.omit(c(meta$name[failed], meta$parent[failed])))
  nodes$status[nodes$status %in% "errored" & !nodes$id %in% recorded] <- "outdated"
  nodes
}

# The errors the store records, for clear_invalidated_errors().
recorded_errors <- function() {
  tryCatch(
    targets::tar_meta(fields = c("parent", "error"), targets_only = TRUE),
    error = function(e) NULL
  )
}

# Put live statuses on the nodes of tar_visnetwork()$x$nodes.
overlay_statuses <- function(nodes, statuses) {
  hit <- match(nodes$id, names(statuses))
  nodes$status[!is.na(hit)] <- unname(statuses[hit[!is.na(hit)]])
  nodes
}

# Graphs with more targets than this ask before the board draws them, when
# loaded from the Script field: React Flow keeps every target in the page, and
# thousands of them can use up the browser's memory.
confirm_nodes_limit <- function() getOption("targetsboard.confirm_nodes", 500)

large_graph_modal <- function(script, nodes, targets_only = TRUE, ns = identity) {
  what <- if (targets_only) "targets" else "targets, functions and globals"
  shiny::modalDialog(
    title = shiny::tagList(shiny::icon("triangle-exclamation"), "A large pipeline"),
    shiny::tags$p(sprintf("%s has %s %s.", script, format(nodes, big.mark = ","), what)),
    shiny::tags$p(sprintf(
      paste(
        "Drawing more than %s can use a lot of memory, and may make the",
        "browser slow or crash the tab."
      ),
      format(confirm_nodes_limit(), big.mark = ",")
    )),
    footer = shiny::tagList(
      shiny::actionButton(ns("cancel_large"), "Cancel: keep the current graph"),
      shiny::actionButton(ns("draw_large"), "Draw it anyway", class = "btn-danger")
    ),
    easyClose = FALSE
  )
}
