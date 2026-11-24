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
  poller <- backend$poller
  runner <- backend$runner
  previews <- backend$previews
  loaded_script <- backend$loaded_script
  start_poller <- backend$start_poller
  stop_poller <- backend$stop_poller
  board_run_alive <- backend$run_alive
  busy <- backend$busy
  request_refresh <- backend$request_refresh

  function(input, output, session) {
    backend$start_once()

    # The graph the board draws: the first render's, then each the poller
    # recomputes, once Load accepts it.
    board <- shiny::reactiveVal(list(x = metadata, progress_mtime = progress_mtime()))

    # What changes on the graph, polled from the store and the poller's
    # files (graph_data_server()).
    data <- graph_data_server("graph_data", backend)
    poller_board <- data$poller_board
    script_error <- data$script_error
    progress <- data$progress
    running <- data$running
    tar_meta_data <- data$tar_meta
    warned_targets <- data$warned_targets

    # The tabs beside the board: each a module. The tables' clicks show a
    # target on the board (show_on_board()).
    problem_clicked <- problems_server("problems", board, tar_meta_data)
    log_server("log", backend)
    visnetwork_server("visnetwork", board)
    meta_clicked <- meta_server("meta", tar_meta_data)
    internals_server("internals", board, shiny::reactive(shiny::reactiveValuesToList(input)))

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
      if (loading()) "Loading pipeline\u2026"
    })
    # The cover shows only with a caption (index.css): the caption has to
    # update while the cover is hidden.
    shiny::outputOptions(output, "board_loading_caption", suspendWhenHidden = FALSE)

    shiny::observeEvent(script_error(), ignoreNULL = FALSE, {
      if (!is.null(script_error())) load_done()
      text <- script_error_text(poller$script, script_error())
      if (is.null(text)) {
        shiny::removeNotification("script_error", session)
      } else {
        shiny::showNotification(text, id = "script_error", duration = NULL, type = "error", session = session)
      }
    })

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

    # The preview methods of the project's _targetsboard.R: at start, and
    # again on Refresh, so editing them needs no restart.
    preview_methods <- shiny::reactiveVal(0)
    reload_preview_methods <- function() {
      loaded <- tryCatch(load_preview_methods(), error = function(e) e)
      if (inherits(loaded, "error")) {
        shiny::showNotification(
          paste("_targetsboard.R didn't load:", conditionMessage(loaded)),
          type = "error", duration = NULL
        )
      }
      previews$clear()
      preview_methods(shiny::isolate(preview_methods()) + 1)
    }
    reload_preview_methods()
    shiny::observeEvent(input$refresh, {
      request_refresh()
      reload_preview_methods()
    })

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
      preview_methods()
      focused_records_changed()
      # The problem says why there is no value: a pattern whose branches
      # errored. Without one, say it plainly.
      no_value <- function() problem_part %||% shiny::p("No value stored for this target yet.")
      store <- targets::tar_config_get("store")
      target <- preview_target(name, store, branch = branch_choice())
      if (is.null(target)) return(no_value())
      key <- preview_key(target)
      # A large value is read only when asked: it can take long and fill
      # the board's memory.
      if (preview_too_large(target) && !key %in% read_anyway()) {
        return(shiny::tagList(problem_part, preview_gate(target, store = store)))
      }
      built <- previews$get(key)
      if (is.null(built)) {
        missing <- new.env() # a value can be NULL itself
        value <- tryCatch(suppressWarnings(read_preview_value(target, store)), error = function(e) missing)
        if (identical(value, missing)) return(no_value())
        built <- previews$set(key, build_preview(value, target))
      }
      # Plots are drawn by the board, at the size of the preview. Reusing
      # the ids replaces the previous renderers instead of piling them up.
      plots <- Filter(function(view) inherits(view, "tar_board_preview_plot"), built$views)
      plot_ids <- sprintf("value_preview_plot_%d", seq_along(plots))
      for (i in seq_along(plots)) {
        local({
          view <- plots[[i]]
          output[[plot_ids[[i]]]] <- render_preview_plot(view, plot_ids[[i]], session)
        })
      }
      # A file's view is built here, in the session, when its section
      # opens: the first now, the others on value_preview_file_open.
      files <- Find(function(view) inherits(view, "tar_board_preview_files"), built$views)
      preview_files(list(paths = files$paths, target = target))
      if (!is.null(files)) render_file_section(1)
      shiny::tagList(problem_part, preview_frame(target, built, plot_ids, store))
    })
    preview_files <- shiny::reactiveVal(NULL)
    render_file_section <- function(i) {
      files <- shiny::isolate(preview_files())
      if (is.null(files) || i > length(files$paths)) return()
      path <- files$paths[[i]]
      output[[file_output_id(i)]] <- shiny::renderUI(file_section_view(path, files$target))
    }
    shiny::observeEvent(input$value_preview_file_open, render_file_section(input$value_preview_file_open))
    # A pattern's preview opens on its first branch; the picker changes it.
    branch_choice <- shiny::reactiveVal(1L)
    shiny::observeEvent(input$flow_focused_node, branch_choice(1L), priority = 10, ignoreNULL = FALSE)
    shiny::observeEvent(input$value_preview_branch, branch_choice(as.integer(input$value_preview_branch)))
    read_anyway <- shiny::reactiveVal(character())
    # The preview follows the focused target's records in tar_meta(), its
    # branches' too: it changes when the target is invalidated or built
    # again. Only its own: a run writes new metadata for each target it
    # builds, which shouldn't redraw a preview whose target didn't change.
    focused_records <- function(meta, name) {
      if (is.null(meta) || is.null(name)) return(NULL)
      parent <- if ("parent" %in% names(meta)) meta$parent else NA_character_
      meta[meta$name %in% name | parent %in% name, ]
    }
    seen_records <- NULL
    shiny::observeEvent(input$flow_focused_node, {
      seen_records <<- focused_records(shiny::isolate(tar_meta_data()), input$flow_focused_node)
    }, ignoreNULL = FALSE)
    focused_records_changed <- shiny::reactiveVal(0)
    shiny::observeEvent(tar_meta_data(), {
      records <- focused_records(tar_meta_data(), shiny::isolate(input$flow_focused_node))
      if (identical(records, seen_records)) return()
      seen_records <<- records
      focused_records_changed(shiny::isolate(focused_records_changed()) + 1)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    shiny::observeEvent(input$value_preview_read, {
      target <- preview_target(input$flow_focused_node, targets::tar_config_get("store"), branch = branch_choice())
      if (!is.null(target)) read_anyway(union(read_anyway(), preview_key(target)))
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
      # Empty when a typed text that matches no script was left in the field.
      if (!nzchar(new_script %||% "")) {
        shiny::showNotification("Choose a script to load.", type = "warning")
        bslib::update_task_button("load_script", state = "ready")
        return()
      }
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
    shiny::observeEvent(meta_clicked(), show_on_board(meta_clicked()))
    # The problem pane's links to a target (problem.R) send meta_row_clicked
    # themselves, until the preview is a module.
    shiny::observeEvent(input$meta_row_clicked, show_on_board(input$meta_row_clicked))
    # A problem picked in the Problems tab, or a run's notification: on the
    # board, with its Problem in the value preview.
    shiny::observeEvent(problem_clicked(), show_on_board(problem_clicked(), preview = TRUE))
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
