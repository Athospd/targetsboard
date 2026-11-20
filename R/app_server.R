#' The board's server
#'
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @param targets_only Show only targets, without functions and globals.
#' @noRd
app_server <- function(metadata, script = "_targets.R", targets_only = TRUE) {
  # One background poller per app, shared by every browser session.
  # Restarted only when someone loads a different script.
  poller <- new.env()
  poller_rds_path <- shiny::reactiveVal(NULL)

  stop_poller <- function() {
    if (inherits(poller$process, "r_process") && poller$process$is_alive())
      poller$process$kill()
    poller$process <- NULL
  }

  start_poller <- function(script) {
    stop_poller()
    assets <- tar_visnetwork_bg(script = script, targets_only = targets_only)
    poller$process <- assets$process
    poller$script <- script
    poller$refresh_path <- assets$refresh_path
    poller_rds_path(assets$tar_vis_path)
  }

  function(input, output, session) {
    if (is.null(poller$process)) {
      start_poller(script)
      shiny::onStop(stop_poller, session = NULL) # runs when the app stops
    }

    # The graph, as last recomputed by the poller: re-read only when the
    # poller rewrites its file.
    board <- shiny::reactiveVal(list(x = metadata, progress_mtime = progress_mtime()))
    poller_board <- shiny::reactivePoll(
      500, session,
      checkFunc = function() {
        path <- poller_rds_path()
        if (is.null(path)) NA else file.mtime(path)
      },
      valueFunc = function() {
        path <- poller_rds_path()
        if (is.null(path) || !file.exists(path)) return(NULL)
        tryCatch(readRDS(path), error = function(e) NULL)
      }
    )
    shiny::observeEvent(poller_board(), board(poller_board()))

    # Live statuses, from the progress file targets writes during a run:
    # cheap to check, so every half second.
    progress <- shiny::reactivePoll(
      500, session,
      checkFunc = progress_mtime,
      valueFunc = function() {
        list(
          mtime = progress_mtime(),
          data = tryCatch(targets::tar_progress(), error = function(e) NULL)
        )
      }
    )

    # Whether a run is going, whoever started it: checked twice a second.
    running <- shiny::reactivePoll(
      500, session,
      checkFunc = function() !is.null(active_run()),
      valueFunc = function() !is.null(active_run())
    )

    # A new graph: send it whole, with the live statuses on top.
    shiny::observeEvent(board(), {
      current <- board()
      is_running <- shiny::isolate(running())
      nodes <- current$x$nodes
      nodes$status <- settle_statuses(nodes$status, is_running)
      statuses <- live_statuses(shiny::isolate(progress()), current$progress_mtime)
      update_targetsboard(
        session,
        "flow",
        nodes = overlay_statuses(nodes, settle_statuses(statuses, is_running)),
        edges = current$x$edges
      )
    })

    # New progress, or a run that stopped: send just the statuses.
    shiny::observeEvent(list(progress(), running()), {
      statuses <- live_statuses(progress(), shiny::isolate(board())$progress_mtime)
      statuses <- settle_statuses(statuses, running())
      if (length(statuses)) update_targetsboard_status(session, "flow", statuses)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$refresh, {
      path <- poller$refresh_path
      if (!is.null(path)) writeLines(format(Sys.time(), "%H:%M:%OS6"), path)
    })

    observe({
      bslib::toggle_sidebar("output_preview_sidebar", open = !is.null(input$flow_selected_nodes[1]))
    })

    output$output_preview <- shiny::renderUI({
      req(input$flow_selected_nodes)
      tar_output <- tryCatch(
        targets::tar_read_raw(input$flow_selected_nodes[1]),
        error = function(e) shiny::p("No artifact available for this target.")
      )

      c(render_fun, output_ui_fun) %<-% render_and_output_bundle(tar_output)
      # Reuse one output id: assigning it again replaces the previous
      # renderer instead of piling up a new output per click.
      output$output_preview_content <- render_fun(tar_output)
      output_ui_fun("output_preview_content")
    })

    observeEvent(input$load_script, {
      new_script <- input$script
      if (!file.exists(new_script)) {
        shiny::showNotification(
          paste0("Script not found: ", new_script),
          type = "error"
        )
        return()
      }
      if (!identical(new_script, poller$script)) start_poller(new_script)
    })

    output$nodes <- reactable::renderReactable({
      reactable::reactable(board()$x$nodes)
    })

    # Re-read tar_meta() only when targets writes new metadata; checking the
    # file's modification time every second is cheap.
    tar_meta_data <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() file.mtime(store_file("meta")),
      valueFunc = function() {
        tryCatch(targets::tar_meta(targets_only = TRUE), error = function(e) NULL)
      }
    )

    output$meta <- reactable::renderReactable({
      req(tar_meta_data())
      reactable::reactable(tar_meta_data(), wrap = FALSE)
    })

    output$debug <- shiny::renderPrint({
      c(
        board = board(),
        reactiveValuesToList(input)
      )
    })
  }
}

# A file in the targets store's meta folder, e.g. "progress" or "meta".
store_file <- function(name) {
  file.path(targets::tar_config_get("store"), "meta", name)
}

progress_mtime <- function() file.mtime(store_file("progress"))

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

# Put live statuses on the nodes of tar_visnetwork()$x$nodes.
overlay_statuses <- function(nodes, statuses) {
  hit <- match(nodes$id, names(statuses))
  nodes$status[!is.na(hit)] <- unname(statuses[hit[!is.na(hit)]])
  nodes
}
