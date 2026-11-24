#' The board's server
#'
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @param targets_only Show only targets, without functions and globals.
#' @param scripts Other scripts the Script field offers.
#' @noRd
app_server <- function(metadata, script = targets::tar_config_get("script"), targets_only = TRUE,
                       scripts = NULL) {
  # One backend per app: the poller, the board's run and the value
  # previews, shared by every browser session (board_backend()).
  backend <- board_backend(script, targets_only)

  function(input, output, session) {
    backend$start_once()

    # What changes on the graph, polled from the store and the poller's
    # files (graph_data_server()).
    data <- graph_data_server("graph_data", backend)
    poller_board <- data$poller_board
    script_error <- data$script_error
    progress <- data$progress
    running <- data$running
    tar_meta_data <- data$tar_meta
    warned_targets <- data$warned_targets

    # The script the board draws, Load and Refresh (script_server()): the
    # accepted graph, and when the preview methods load again.
    loaded <- script_server(
      "script", backend, metadata, script, scripts, targets_only,
      poller_board = poller_board, script_error = script_error
    )
    board <- loaded$board

    # The tabs beside the board: each a module. The tables' clicks show a
    # target on the board (show_on_board()).
    problem_clicked <- problems_server("problems", board, tar_meta_data)
    log_server("log", backend)
    visnetwork_server("visnetwork", board)
    meta_clicked <- meta_server("meta", tar_meta_data)
    internals_server("internals", board, shiny::reactive(shiny::reactiveValuesToList(input)))

    # A new graph: send it whole, with the live statuses on top.
    shiny::observeEvent(board(), {
      current <- board()
      is_running <- shiny::isolate(running())
      nodes <- clear_invalidated_errors(current$x$nodes, recorded_errors())
      nodes$status <- settle_statuses(nodes$status, is_running)
      statuses <- live_statuses(shiny::isolate(progress()), current$progress_mtime)
      update_targetsboard(
        session,
        "flow",
        nodes = overlay_statuses(nodes, settle_statuses(statuses, is_running)),
        edges = current$x$edges,
        warnings = shiny::isolate(warned_targets())
      )
    })

    # New metadata can add or clear warning marks: send just those.
    shiny::observeEvent(warned_targets(), {
      update_targetsboard_status(session, "flow", warnings = warned_targets())
    }, ignoreInit = TRUE)

    # New progress, or a run that stopped: send just the statuses.
    # Patterns show how many of their branches are done while a run goes;
    # the counts go once it stops.
    shiny::observeEvent(list(progress(), running()), {
      statuses <- live_statuses(progress(), shiny::isolate(board())$progress_mtime)
      statuses <- settle_statuses(statuses, running())
      branches <- if (running()) {
        tryCatch(targets::tar_progress_branches(), error = function(e) NULL)
      }
      update_targetsboard_status(session, "flow", statuses, branches = branches %||% data.frame())
    }, ignoreInit = TRUE)

    # The value preview (preview_server()): opened by a double-click on a
    # target, its Value, or a link to its problem, on the focused target.
    preview_request <- shiny::reactiveVal(0)
    request_preview <- function() preview_request(shiny::isolate(preview_request()) + 1)
    shiny::observeEvent(input$flow_double_clicked_node, request_preview())
    preview <- preview_server(
      "preview", backend,
      focused = shiny::reactive(input$flow_focused_node),
      open_request = shiny::reactive(if (preview_request() > 0) preview_request()),
      board = board, running = running, tar_meta = tar_meta_data,
      methods = loaded$methods
    )

    # The actions of the right-click menus and the toolbars (board_options()):
    # Run and Invalidate go to the run module, Value opens the preview.
    node_action <- shiny::reactive(input$flow_node_action)
    shiny::observeEvent(node_action(), {
      if (identical(node_action()$action, "value")) request_preview()
    })
    # Run all, Stop, Invalidate all, and the graph's runs (run_server()).
    run <- run_server("run", backend, board, running, action = node_action)

    # A target picked in the tar_meta() table: show it on the board, focused,
    # with what it depends on and what depends on it directly in view.
    show_on_board <- function(name, preview = FALSE) {
      edges <- board()$x$edges
      neighbours <- unique(c(name, edges$from[edges$to == name], edges$to[edges$from == name]))
      bslib::nav_select("view", "board")
      if (preview) request_preview()
      update_targetsboard_focus(session, "flow", name)
      targetsboard_fit_view(session, "flow", neighbours)
    }
    shiny::observeEvent(meta_clicked(), show_on_board(meta_clicked()))
    # The problem pane's links to its inputs.
    shiny::observeEvent(preview$target_link(), show_on_board(preview$target_link()))
    # A problem picked in the Problems tab, or a run's notification: on the
    # board, with its Problem in the value preview.
    shiny::observeEvent(problem_clicked(), show_on_board(problem_clicked(), preview = TRUE))
    shiny::observeEvent(run$show_problem(), show_on_board(run$show_problem(), preview = TRUE))

    # The graph follows the page's dark mode switch.
    shiny::observeEvent(input$dark_mode, {
      update_targetsboard_options(session, "flow", color_mode = input$dark_mode)
    })

    # Keep where the author left the targets, for the next time the board
    # opens: in the store's user folder, `_targets/user/`.
    shiny::observeEvent(input$flow_layout, write_board_layout(input$flow_layout))

    # The power button: stop the board. In tar_board()'s background process,
    # that ends the process too.
    shiny::observeEvent(input$turn_off, shiny::stopApp("The board is turned off."))
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
#'   and `targets::tar_progress()`.
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
    title = "A large pipeline",
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
