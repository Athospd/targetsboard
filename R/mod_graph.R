#' The board's graph: the targetsboard widget, kept up to date
#'
#' @param id The module's id.
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param layout The board's saved layout, from `read_board_layout()`, or NULL.
#' @noRd
graph_ui <- function(id, metadata, layout = NULL) {
  targetsboard(
    shiny::NS(id, "flow"), metadata$nodes, metadata$edges,
    options = board_options(), layout = layout
  )
}

#' @param board Reactive: the graph to draw (script_server()).
#' @param progress,running,warned_targets Reactives: the polled progress,
#'   whether a run goes, and the targets with a warning (graph_data_server()).
#' @param outdated Reactive: why outdated targets are (outdated_reasons()),
#'   shown on hovering them.
#' @param costs Reactive: each built target's time and size
#'   (target_costs()), for its badge.
#' @param kinds Reactive: the targets that are files or reports
#'   (target_kinds()), for their icons.
#' @param show Reactive event: `list(name)`, a target to focus and bring into
#'   view with what it depends on and what depends on it directly.
#' @param color_mode Reactive: `"light"` or `"dark"`, the page's mode.
#' @param tar_meta Reactive: `targets::tar_meta()`, for an unfolded
#'   pattern's branches.
#' @return A list of reactives, the widget's events: `focused`, the focused
#'   target; `double_clicked`, a target double-clicked; `node_action`, an
#'   action chosen in a menu or toolbar; and `branch_clicked`,
#'   `list(pattern, position)`, a branch of the unfolded pattern clicked.
#' @noRd
graph_server <- function(id, board, progress, running, warned_targets, show, color_mode,
                         outdated = shiny::reactive(list()),
                         kinds = shiny::reactive(character()),
                         costs = shiny::reactive(list()),
                         tar_meta = shiny::reactive(NULL)) {
  stopifnot(
    shiny::is.reactive(board), shiny::is.reactive(progress), shiny::is.reactive(running),
    shiny::is.reactive(warned_targets), shiny::is.reactive(show), shiny::is.reactive(color_mode),
    shiny::is.reactive(outdated), shiny::is.reactive(kinds), shiny::is.reactive(costs),
    shiny::is.reactive(tar_meta)
  )
  shiny::moduleServer(id, function(input, output, session) {
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

    # Why outdated targets are, on hovering them: sent when the reasons
    # change, and after a new graph, which comes without them.
    send_reasons <- function() {
      reasons <- vapply(outdated(), reasons_tooltip, character(1))
      update_targetsboard_status(session, "flow", reasons = reasons)
    }
    shiny::observeEvent(outdated(), send_reasons())
    shiny::observeEvent(board(), shiny::isolate(send_reasons()), priority = -1)

    # Which targets are files or reports, for their icons: the same way.
    send_kinds <- function() update_targetsboard_status(session, "flow", kinds = kinds())
    shiny::observeEvent(kinds(), send_kinds())
    shiny::observeEvent(board(), shiny::isolate(send_kinds()), priority = -1)

    # Each built target's time and size, for its badge: the same way.
    send_costs <- function() update_targetsboard_status(session, "flow", costs = costs())
    shiny::observeEvent(costs(), send_costs())
    shiny::observeEvent(board(), shiny::isolate(send_costs()), priority = -1)

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

    shiny::observeEvent(show(), {
      name <- show()$name
      edges <- shiny::isolate(board())$x$edges
      neighbours <- unique(c(name, edges$from[edges$to == name], edges$to[edges$from == name]))
      update_targetsboard_focus(session, "flow", name)
      targetsboard_fit_view(session, "flow", neighbours)
    })

    # The graph follows the page's dark mode switch.
    shiny::observeEvent(color_mode(), {
      update_targetsboard_options(session, "flow", color_mode = color_mode())
    })

    # Keep where the author left the targets, for the next time the board
    # opens: in the store's user folder, `_targets/user/`.
    shiny::observeEvent(input$flow_layout, write_board_layout(input$flow_layout))

    # One pattern unfolded at a time (#147): its toolbar's Branches button
    # unfolds it, and folds it again. Its branches follow the run.
    unfolded <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$flow_node_action, {
      action <- input$flow_node_action
      if (!identical(action$action, "unfold")) return()
      unfolded(if (!identical(unfolded(), action$node)) action$node)
    })
    shiny::observe({
      pattern <- unfolded()
      if (is.null(pattern)) return(send_command(session, "flow", "unfold"))
      shown <- pattern_branches(pattern, tar_meta(), progress()$data)
      send_command(session, "flow", "unfold", pattern = shown$pattern, branches = I(shown$branches), more = shown$more)
    })

    list(
      focused = shiny::reactive(input$flow_focused_node),
      double_clicked = shiny::reactive(input$flow_double_clicked_node),
      node_action = shiny::reactive(input$flow_node_action),
      branch_clicked = shiny::reactive(input$flow_branch_click)
    )
  })
}
