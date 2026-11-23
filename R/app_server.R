#' The board's server
#'
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @param targets_only Show only targets, without functions and globals.
#' @param scripts Other scripts the Script field offers.
#' @noRd
app_server <- function(metadata, script = targets::tar_config_get("script"), targets_only = TRUE,
                       scripts = NULL) {
  # One background poller per app, shared by every browser session.
  # Restarted only when someone loads a different script.
  poller <- new.env()
  poller_rds_path <- shiny::reactiveVal(NULL)
  loaded_script <- shiny::reactiveVal(script) # the script the poller draws

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
    loaded_script(script)
    poller$refresh_path <- assets$refresh_path
    poller_rds_path(assets$tar_vis_path)
  }

  # The run started from the board, also shared by every browser session.
  runner <- new.env()
  board_run_alive <- function() {
    inherits(runner$process, "process") && runner$process$is_alive()
  }
  # Counts the board's own run from the start: targets records the process
  # only once the pipeline is loaded.
  busy <- function() !is.null(active_run()) || board_run_alive()

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
    shiny::observeEvent(poller_board(), {
      new_board <- poller_board()
      # While the author decides on a large graph, the poller's next
      # recomputations wait with it.
      if (!is.null(pending_board())) return(pending_board(new_board))
      nodes <- nrow(new_board$x$nodes %||% data.frame())
      if (shiny::isolate(loading()) && nodes > confirm_nodes_limit()) {
        pending_board(new_board)
        shiny::showModal(large_graph_modal(shiny::isolate(poller$script), nodes, targets_only))
        return()
      }
      board(new_board)
      load_done()
    })

    # A newly loaded script's graph over the size limit is held back until
    # the author says to draw it: too many targets can use up the browser's
    # memory. Cancel keeps the current graph and goes back to its script.
    pending_board <- shiny::reactiveVal(NULL)
    previous_script <- NULL
    shiny::observeEvent(input$draw_large_graph, {
      shiny::req(pending_board())
      board(pending_board())
      pending_board(NULL)
      shiny::removeModal()
      load_done()
    })
    shiny::observeEvent(input$cancel_large_graph, {
      shiny::req(pending_board())
      pending_board(NULL)
      shiny::removeModal()
      start_poller(previous_script)
      load_done()
    })

    # Load stays busy from the click until the new script's graph arrives, or
    # it turns out not to load. The poller writes a new file for each script,
    # so the first one it writes is that script's graph.
    loading <- shiny::reactiveVal(FALSE)
    load_done <- function() {
      if (!shiny::isolate(loading())) return()
      loading(FALSE)
      bslib::update_task_button("load_script", state = "ready")
      shinycssloaders::hideSpinner("board_loading")
    }
    # Meanwhile a shinycssloaders spinner covers the Board pane, and keeps the
    # old graph and its preview from being used. board_loading is only its
    # anchor: an empty output, whose first value hides the spinner the
    # package shows when an output binds.
    output$board_loading <- shiny::renderUI(NULL)
    shiny::outputOptions(output, "board_loading", suspendWhenHidden = FALSE)
    output$board_loading_caption <- shiny::renderText({
      if (loading()) sprintf("Loading %s...", shiny::isolate(poller$script))
    })
    # The cover shows only with a caption (index.css): the caption has to
    # update while the cover is hidden.
    shiny::outputOptions(output, "board_loading_caption", suspendWhenHidden = FALSE)

    # A script that stops loading (a typo while editing it): say so, for as
    # long as it lasts, rather than silently keep the last graph.
    script_error <- shiny::reactivePoll(
      500, session,
      checkFunc = function() {
        path <- poller_rds_path()
        if (is.null(path)) NA else file.mtime(poller_error_path(path))
      },
      valueFunc = function() {
        path <- poller_rds_path()
        error_path <- if (!is.null(path)) poller_error_path(path)
        if (is.null(error_path) || !file.exists(error_path)) return(NULL)
        tryCatch(readLines(error_path, warn = FALSE), error = function(e) NULL)
      }
    )
    shiny::observeEvent(script_error(), ignoreNULL = FALSE, {
      if (!is.null(script_error())) load_done()
      text <- script_error_text(poller$script, script_error())
      if (is.null(text)) {
        shiny::removeNotification("script_error", session)
      } else {
        shiny::showNotification(text, id = "script_error", duration = NULL, type = "error", session = session)
      }
    })

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
    running <- shiny::reactivePoll(500, session, checkFunc = busy, valueFunc = busy)

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

    shiny::observe({
      is_running <- running()
      shiny::updateActionButton(session, "run_all", disabled = is_running)
      shiny::updateActionButton(session, "stop", disabled = !is_running)
      shiny::updateActionButton(
        session, "invalidate_all",
        disabled = is_running || length(board_targets(board()$x$nodes)) == 0
      )
    })

    launch_run <- function(names = NULL) {
      if (busy()) {
        shiny::showNotification("A run is already going.", type = "warning")
        return()
      }
      runner$process <- start_run(poller$script, names, modes = list(
        keep_going = isTRUE(input$keep_going),
        stop_at_warnings = isTRUE(input$stop_at_warnings)
      ))
      runner$stopped <- FALSE
    }
    shiny::observeEvent(input$run_all, launch_run())

    # Stops the run going, whoever started it.
    shiny::observeEvent(input$stop, {
      if (board_run_alive()) {
        runner$stopped <- TRUE
        runner$process$kill_tree() # the board's own run: its whole tree
      } else {
        handle <- active_run()
        if (!is.null(handle)) stop_run(handle)
      }
    })

    # Tell how the board's own run ended, once per session.
    notified <- NULL
    shiny::observeEvent(running(), {
      process <- runner$process
      if (running() || !inherits(process, "process") || identical(process, notified)) return()
      notified <<- process
      error <- run_error(process)
      # The targets that errored in this run: a run that keeps going after
      # errors finishes, but with them.
      errored <- run_errored_targets(tryCatch(
        targets::tar_progress(fields = c("type", "parent", "progress")),
        error = function(e) NULL
      ))
      show_problem <- if (length(errored)) {
        shiny::tags$a(
          href = "#", class = "tb-show-problem ms-1",
          onclick = sprintf(
            "Shiny.setInputValue('show_problem', %s, {priority: 'event'}); return false;",
            jsonlite::toJSON(errored[1], auto_unbox = TRUE)
          ),
          "Show problem"
        )
      }
      if (isTRUE(runner$stopped)) {
        shiny::showNotification("Run stopped.", type = "warning")
      } else if (is.null(error) && length(errored) == 0) {
        shiny::showNotification("Run finished.", type = "message")
      } else if (is.null(error)) {
        shiny::showNotification(
          shiny::tagList(paste0("Run finished, with errors in ", describe_names(errored), "."), show_problem),
          type = "warning", duration = NULL
        )
      } else {
        shiny::showNotification(
          shiny::tagList(
            paste0("Run failed", if (length(errored)) paste0(" in ", describe_names(errored)), ": "),
            shiny::tags$code(error), show_problem
          ),
          type = "error", duration = NULL
        )
      }
    }, ignoreInit = TRUE)

    # Ask the poller to recompute the graph now.
    request_refresh <- function() {
      path <- poller$refresh_path
      if (!is.null(path)) writeLines(format(Sys.time(), "%H:%M:%OS6"), path)
    }
    shiny::observeEvent(input$refresh, request_refresh())

    # Invalidate targets, after a confirmation: the next run builds them
    # again, which can take long. The confirmation is for the targets it
    # names, whatever is selected by then.
    to_invalidate <- shiny::reactiveVal(NULL)
    ask_to_invalidate <- function(names) {
      to_invalidate(names)
      shown <- utils::head(names, 10)
      shiny::showModal(shiny::modalDialog(
        title = "Invalidate targets?",
        shiny::p(
          "The next run will build these targets again, and what depends on them.",
          "Until then, their values can no longer be read."
        ),
        shiny::tags$ul(
          lapply(shown, \(name) shiny::tags$li(shiny::tags$code(name))),
          if (length(names) > length(shown)) {
            shiny::tags$li(paste("and", length(names) - length(shown), "more"))
          }
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("invalidate_confirm", "Invalidate", class = "btn-danger")
        )
      ))
    }
    shiny::observeEvent(input$invalidate_all, {
      names <- board_targets(board()$x$nodes)
      shiny::req(length(names) > 0)
      ask_to_invalidate(names)
    })
    shiny::observeEvent(input$invalidate_confirm, {
      shiny::removeModal()
      if (busy()) {
        shiny::showNotification("A run is going: stop it before invalidating.", type = "warning")
        return()
      }
      names <- to_invalidate()
      shiny::req(names)
      to_invalidate(NULL)
      invalidate_targets(names)
      request_refresh()
      shiny::showNotification(
        paste0("Invalidated ", length(names), if (length(names) == 1) " target." else " targets."),
        type = "message"
      )
    })

    # The value preview: opened by a double-click on a target, or its menu's
    # "Show its value". While open, it follows the focused target; it closes
    # with the focus, or by its own toggle.
    preview_open <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$flow_double_clicked_node, preview_open(TRUE))
    shiny::observeEvent(input$flow_focused_node, {
      if (is.null(input$flow_focused_node)) preview_open(FALSE)
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$value_preview_sidebar, {
      if (isFALSE(input$value_preview_sidebar)) preview_open(FALSE)
    })
    observe({
      bslib::toggle_sidebar(
        "value_preview_sidebar",
        open = preview_open() && !is.null(input$flow_focused_node)
      )
    })

    # The actions of the right-click menus and the toolbars (board_options()):
    # on the target they were chosen on, or on the selected group.
    shiny::observeEvent(input$flow_node_action, {
      action <- input$flow_node_action
      nodes <- as.character(unlist(action$nodes %||% action$node))
      switch(
        action$action,
        run = launch_run(nodes),
        invalidate = ask_to_invalidate(nodes),
        value = preview_open(TRUE)
      )
    })

    output$value_preview <- shiny::renderUI({
      req(preview_open(), input$flow_focused_node)
      name <- input$flow_focused_node
      # A target that errored or warned shows its problem first; an errored
      # one has no value to show.
      problem <- target_problem(name, poller$script)
      problem_part <- NULL
      if (has_problem(problem)) {
        edges <- board()$x$edges
        inputs <- intersect(edges$from[edges$to == name], board_targets(board()$x$nodes))
        problem_part <- problem_ui(
          problem, inputs, running = running(), script = poller$script,
          store = targets::tar_config_get("store"), dir = normalizePath(getwd(), winslash = "/")
        )
        if (!is.null(problem$error)) return(problem_part)
      }
      no_value <- new.env() # a value can be NULL itself
      value <- tryCatch(suppressWarnings(targets::tar_read_raw(name)), error = function(e) no_value)
      if (identical(value, no_value)) {
        # The problem says why there is no value: a pattern whose branches
        # errored. Without one, say it plainly.
        if (!is.null(problem_part)) return(problem_part)
        value <- shiny::p("No value stored for this target yet.")
      }

      c(render_fun, output_ui_fun) %<-% render_and_output_bundle(value)
      # Reuse one output id: assigning it again replaces the previous
      # renderer instead of piling up a new output per click.
      output$value_preview_content <- render_fun(value)
      shiny::tagList(problem_part, output_ui_fun("value_preview_content"))
    })

    # The Script field names the script the board draws. The poller is shared,
    # so a tab opened after someone loaded another script, or open while they
    # did, follows it.
    shiny::observeEvent(loaded_script(), {
      shown <- loaded_script()
      if (identical(shown, shiny::isolate(input$script))) return()
      shiny::updateSelectizeInput(
        session, "script",
        choices = script_choices(script, c(scripts, shown)), selected = shown
      )
    })

    observeEvent(input$load_script, {
      new_script <- input$script
      if (!file.exists(new_script)) {
        shiny::showNotification(
          paste0("Script not found: ", new_script),
          type = "error"
        )
        bslib::update_task_button("load_script", state = "ready")
        return()
      }
      if (identical(new_script, poller$script)) {
        bslib::update_task_button("load_script", state = "ready")
        return()
      }
      loading(TRUE)
      shinycssloaders::showSpinner("board_loading")
      previous_script <<- poller$script
      start_poller(new_script)
    })

    # The graph as tar_visnetwork() draws it, the view tar_watch() shows:
    # recomputed with the board's graph, without live statuses.
    output$vis_network <- visNetwork::renderVisNetwork({
      shiny::req(board()$widget)
      board()$widget
    })

    # A target picked in the tar_meta() table: show it on the board, focused,
    # with what it depends on and what depends on it directly in view.
    show_on_board <- function(name, preview = FALSE) {
      edges <- board()$x$edges
      neighbours <- unique(c(name, edges$from[edges$to == name], edges$to[edges$from == name]))
      bslib::nav_select("view", "board")
      if (preview) preview_open(TRUE)
      update_targetsboard_focus(session, "flow", name)
      targetsboard_fit_view(session, "flow", neighbours)
    }
    shiny::observeEvent(input$meta_row_clicked, show_on_board(input$meta_row_clicked))
    # A problem picked in the Problems tab, or a run's notification: on the
    # board, with its Problem in the value preview.
    shiny::observeEvent(input$problem_row_clicked, show_on_board(input$problem_row_clicked, preview = TRUE))
    shiny::observeEvent(input$show_problem, show_on_board(input$show_problem, preview = TRUE))

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

    # Re-read tar_meta() only when targets writes new metadata; checking the
    # file's modification time every second is cheap.
    tar_meta_data <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() file.mtime(store_file("meta")),
      valueFunc = function() {
        tryCatch(targets::tar_meta(targets_only = TRUE), error = function(e) NULL)
      }
    )

    # Targets whose last build raised warnings, for the warning marks.
    warned_targets <- shiny::reactive(warned_targets_of(tar_meta_data()))

    output$meta <- reactable::renderReactable({
      req(tar_meta_data())
      reactable::reactable(
        tar_meta_data(),
        wrap = FALSE,
        # A click on a row shows its target on the board, even twice in a row.
        onClick = reactable::JS(
          # Not the empty row of an empty table.
          "function(row) { if (row && row.values && row.values.name) Shiny.setInputValue('meta_row_clicked', row.values.name, {priority: 'event'}); }"
        ),
        rowStyle = list(cursor = "pointer")
      )
    })

    # The Problems tab: what errored or warned in the latest builds.
    problems <- shiny::reactive({
      targets <- board_targets(board()$x$nodes)
      problems_table(tar_meta_data(), if (length(targets)) targets)
    })
    output$problems_count <- shiny::renderUI({
      n <- nrow(problems())
      if (n == 0) return(NULL)
      errors <- any(problems()$problem == "Error")
      shiny::tags$span(class = paste("badge ms-1", if (errors) "text-bg-danger" else "text-bg-warning"), n)
    })
    output$problems <- reactable::renderReactable({
      reactable::reactable(
        problems(),
        columns = list(
          name = reactable::colDef(name = "Target", minWidth = 120),
          problem = reactable::colDef(name = "Problem", width = 100),
          message = reactable::colDef(name = "Message", minWidth = 300),
          branches = reactable::colDef(name = "Branches", width = 140)
        ),
        wrap = FALSE,
        defaultPageSize = 50,
        language = reactable::reactableLang(noData = "No problems: no target errored or warned in its latest build."),
        onClick = reactable::JS(
          "function(row) { if (row && row.values && row.values.name) Shiny.setInputValue('problem_row_clicked', row.values.name, {priority: 'event'}); }"
        ),
        rowStyle = list(cursor = "pointer")
      )
    })

    # The Log tab: the output of the latest run the board started, re-read
    # while it grows.
    run_log <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() {
        path <- attr(runner$process, "log")
        if (is.null(path)) NA else paste(path, file.mtime(path))
      },
      valueFunc = function() read_run_log(attr(runner$process, "log"))
    )
    output$run_log <- shiny::renderText({
      log <- run_log()
      if (is.null(log)) return("No run started from the board yet: the log of its next run shows here.")
      paste0(attr(runner$process, "header"), "

", log)
    })

    # The Internals tab, shown only to develop the board (board_dev_mode()).
    output$internals <- shiny::renderPrint({
      c(
        board = board()[c("x", "progress_mtime")], # not the visNetwork widget
        reactiveValuesToList(input)
      )
    })
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
    toolbar_actions = c(run = "Run", invalidate = "Invalidate", value = "Value"),
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

#' Targets whose last build raised warnings
#'
#' targets records a branch's warnings on the branch alone: its pattern
#' carries the mark.
#' @param meta `tar_meta(targets_only = TRUE)`.
#' @noRd
warned_targets_of <- function(meta) {
  if (is.null(meta) || !"warnings" %in% names(meta)) return(character())
  warned <- meta[!is.na(meta$warnings), ]
  names <- warned$name
  if (all(c("type", "parent") %in% names(warned))) {
    branch <- warned$type %in% "branch" & !is.na(warned$parent)
    names[branch] <- warned$parent[branch]
  }
  sort(unique(names))
}

# Graphs with more targets than this ask before the board draws them, when
# loaded from the Script field: React Flow keeps every target in the page, and
# thousands of them can use up the browser's memory.
confirm_nodes_limit <- function() getOption("targetsboard.confirm_nodes", 500)

large_graph_modal <- function(script, nodes, targets_only = TRUE) {
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
      shiny::actionButton("cancel_large_graph", "Cancel: keep the current graph"),
      shiny::actionButton("draw_large_graph", "Draw it anyway", class = "btn-danger")
    ),
    easyClose = FALSE
  )
}
