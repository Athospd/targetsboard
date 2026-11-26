#' A board: one pipeline's controls, graph and tabs
#'
#' board_sidebar_ui() holds its run controls and the run's summary, for the
#' page's sidebar;
#' board_ui() its script bar, as a browser's address bar, above its card of
#' tabs: Board, the graph with the value preview beside it;
#' Problems, what errored or warned, with a count in its title; Log, the
#' output of the board's latest run; Script, the script as it is on disk and
#' where things are; tar_visnetwork(), the graph as that
#' function draws it; and tar_meta(). With `options(targetsboard.dev =
#' TRUE)`, an Internals tab shows the board's own inputs. Each tab's `value`
#' is its snake_case id.
#' @param id The module's id.
#' @noRd
board_sidebar_ui <- function(id) {
  shiny::tagList(
    run_ui(shiny::NS(id, "run")),
    run_summary_ui(shiny::NS(id, "run_summary"))
  )
}

#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @param scripts Other scripts the script bar suggests.
#' @param layout The board's saved layout, from `read_board_layout()`, or NULL.
#' @noRd
board_ui <- function(id, metadata, script, scripts = NULL, layout = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    script_ui(ns("script"), script, scripts),
    board_tabs_ui(ns, metadata, layout)
  )
}

board_tabs_ui <- function(ns, metadata, layout) {
  bslib::navset_card_underline(
    id = ns("view"),
    full_screen = TRUE,
    bslib::nav_panel(
      "Board", value = "board",
      # The value preview belongs to the board's own graph: it follows
      # the focused target. While Load loads another script, a spinner
      # over both keeps the old graph from being used. One unpadded body
      # holds them, so the board fills the card as before.
      bslib::card_body(
        class = "tb-board-pane", padding = 0, gap = 0,
        script_loading_ui(ns("script")),
        no_script_output(ns("no_script")),
        bslib::layout_sidebar(
          class = "p-0",
          sidebar = preview_ui(ns("preview")),
          graph_ui(ns("graph"), metadata, layout)
        )
      )
    ),
    bslib::nav_panel(
      shiny::tags$span("Problems", problems_count_ui(ns("problems"))), value = "problems",
      problems_ui(ns("problems"))
    ),
    bslib::nav_panel("Log", value = "log", log_ui(ns("log"))),
    bslib::nav_panel("Script", value = "script", script_view_ui(ns("script_view"))),
    bslib::nav_panel("tar_visnetwork()", value = "tar_visnetwork", visnetwork_ui(ns("visnetwork"))),
    bslib::nav_panel("Metadata", value = "tar_meta", meta_ui(ns("meta"))),
    if (board_dev_mode()) {
      bslib::nav_panel("Internals", value = "internals", internals_ui(ns("internals")))
    }
  )
}

#' @param backend The board's backend (board_backend()).
#' @param scripts Other scripts the script bar suggests.
#' @param targets_only Whether the graph shows only targets.
#' @param color_mode Reactive: `"light"` or `"dark"`, the page's mode.
#' @return A list of reactives: `focused`, the target focused on its graph.
#' @noRd
board_server <- function(id, backend, metadata, script, scripts = NULL, targets_only = TRUE, color_mode) {
  stopifnot(!shiny::is.reactive(backend), shiny::is.reactive(color_mode))
  shiny::moduleServer(id, function(input, output, session) {
    # What changes on the graph, polled from the store and the poller's files.
    data <- graph_data_server("graph_data", backend)

    # The script the board draws, Load and Refresh: the accepted graph, and
    # when the preview methods load again.
    loaded <- script_server(
      "script", backend, metadata, script, scripts, targets_only,
      poller_board = data$poller_board, script_error = data$script_error,
      running = data$running
    )
    board <- loaded$board

    # A target to show on the graph: from a table, a problem's links, or a
    # run's notification.
    shown <- shiny::reactiveVal(NULL)
    graph <- graph_server(
      "graph", board, data$progress, data$running, data$warned_targets,
      show = shiny::reactive(shown()), color_mode = color_mode, outdated = data$outdated,
      kinds = data$kinds, costs = data$costs
    )

    # The tabs beside the board. The tables' clicks show a target on it.
    problem_clicked <- problems_server("problems", board, data$tar_meta)
    log_server("log", backend)
    script_view_server("script_view", backend$loaded_script)
    no_script_server("no_script", backend$loaded_script)
    visnetwork_server("visnetwork", board, color_mode)
    meta_clicked <- meta_server("meta", data$tar_meta, data$script_targets)
    internals_server("internals", board, shiny::reactive(shiny::reactiveValuesToList(input)))

    # The value preview: opened by a double-click on a target, its Value, or
    # a link to its problem, on the focused target.
    preview_request <- shiny::reactiveVal(0)
    request_preview <- function() preview_request(shiny::isolate(preview_request()) + 1)
    shiny::observeEvent(graph$double_clicked(), request_preview())
    shiny::observeEvent(graph$node_action(), {
      if (identical(graph$node_action()$action, "value")) request_preview()
    })
    preview <- preview_server(
      "preview", backend,
      focused = graph$focused,
      open_request = shiny::reactive(if (preview_request() > 0) preview_request()),
      board = board, running = data$running, tar_meta = data$tar_meta,
      methods = loaded$methods, outdated = data$outdated, commands = data$commands,
      rules = data$rules, skipped = data$skipped, settings = data$settings
    )

    # Run all, Stop, Invalidate all, and the graph's Run and Invalidate.
    run <- run_server("run", backend, board, data$running, action = graph$node_action)
    # Under the run buttons: the run going, or the last one.
    run_summary_server("run_summary", board, data$running, data$progress, data$tar_meta, scope = run$scope, maker = run$maker)

    # A target picked in the Metadata table or a problem's links: on the
    # Board tab, focused; a problem picked, with its Problem in the preview.
    show_on_board <- function(name, preview = FALSE) {
      bslib::nav_select("view", "board")
      if (preview) request_preview()
      # Counted, so showing the same target again brings it back into view.
      shown(list(name = name, n = (shiny::isolate(shown())$n %||% 0) + 1))
    }
    shiny::observeEvent(meta_clicked(), show_on_board(meta_clicked()))
    shiny::observeEvent(preview$target_link(), show_on_board(preview$target_link()))
    shiny::observeEvent(problem_clicked(), show_on_board(problem_clicked(), preview = TRUE))
    shiny::observeEvent(run$show_problem(), show_on_board(run$show_problem(), preview = TRUE))

    list(focused = graph$focused)
  })
}
