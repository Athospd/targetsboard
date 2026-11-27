#' The board's runs: Run all, its modes, Stop, and Invalidate all
#'
#' The controls sit in the page's sidebar. Runs and invalidations asked from
#' the graph's menus and toolbars come in as `action`.
#' @param id The module's id.
#' @noRd
run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("all"), "Run all", icon = shiny::icon("play")),
    run_mode_switches(ns = ns),
    shiny::actionButton(ns("stop"), "Stop", icon = shiny::icon("stop"), disabled = TRUE),
    shiny::actionButton(ns("invalidate_all"), "Invalidate all", icon = shiny::icon("eraser"), disabled = TRUE)
  )
}

#' @param backend The board's backend (board_backend()): its run, poller
#'   and refresh.
#' @param board Reactive: the graph the board draws, for its targets.
#' @param running Reactive: whether the pipeline is running.
#' @param action Reactive event: an action chosen on the graph, a list of
#'   `action` and `nodes` (or `node`). It handles `run` and `invalidate`.
#' @param log `function(code)`: logs an action's R code (console_log()).
#' @return A list of reactives: `show_problem`, the target whose problem a
#'   run's notification asks to show.
#' @noRd
run_server <- function(id, backend, board, running, action, log = function(code) NULL) {
  stopifnot(
    !shiny::is.reactive(backend), shiny::is.reactive(board), shiny::is.reactive(running),
    shiny::is.reactive(action)
  )
  shiny::moduleServer(id, function(input, output, session) {
    runner <- backend$runner
    busy <- backend$busy

    shiny::observe({
      is_running <- running()
      shiny::updateActionButton(session, "all", disabled = is_running)
      shiny::updateActionButton(session, "stop", disabled = !is_running)
      shiny::updateActionButton(
        session, "invalidate_all",
        disabled = is_running || length(board_targets(board()$x$nodes)) == 0
      )
    })

    # What the board's run was asked for (NULL: the whole pipeline), for the
    # run summary's count of targets to go. A run the board didn't start,
    # from the console, is taken as the whole pipeline: its names are
    # unknown here.
    scope <- shiny::reactiveVal(NULL)
    maker <- shiny::reactiveVal(NULL) # how the board's run runs (#162)
    launched <- FALSE
    shiny::observeEvent(running(), {
      if (running() && !launched) {
        scope(NULL)
        maker(NULL)
      }
      launched <<- FALSE
    }, ignoreInit = TRUE)

    # An error here would end the session, unseen: say it instead (#253).
    # `failed` marks it: a call may well return NULL.
    last_failure <- shiny::reactiveVal(NULL)
    failed <- structure(list(), class = "tb_failed")
    fail <- function(what) function(e) {
      message <- cli::ansi_strip(conditionMessage(e))
      last_failure(message)
      shiny::showNotification(paste0("Couldn't ", what, ": ", message), type = "error", duration = NULL)
      failed
    }

    launch_run <- function(names = NULL) {
      if (busy()) {
        shiny::showNotification("A run is already going.", type = "warning")
        return()
      }
      modes <- list(keep_going = isTRUE(input$keep_going), stop_at_warnings = isTRUE(input$stop_at_warnings))
      process <- tryCatch(start_run(backend$poller$script, names, modes = modes), error = fail("start the run"))
      if (identical(process, failed)) return()
      scope(names)
      launched <<- TRUE
      runner$process <- process
      maker(attr(runner$process, "maker"))
      log(run_code(names, modes, attr(runner$process, "maker"), backend$poller$script))
      runner$stopped <- FALSE
    }
    shiny::observeEvent(input$all, launch_run())

    # Stops the run going, whoever started it.
    shiny::observeEvent(input$stop, {
      if (backend$run_alive()) {
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
        shiny::tags$a(href = "#", class = "tb-show-problem ms-1", "Show problem") |>
          send_on_click(session$ns("show_problem"), errored[1])
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

    # Invalidate targets, after a confirmation: the next run builds them
    # again, which can take long. The confirmation is for the targets it
    # names, whatever is selected by then.
    to_invalidate <- shiny::reactiveVal(NULL)
    ask_to_invalidate <- function(names) {
      to_invalidate(names)
      shiny::showModal(invalidate_modal(names, session$ns))
    }
    shiny::observeEvent(input$invalidate_all, {
      names <- board_targets(board()$x$nodes)
      shiny::req(length(names) > 0)
      attr(names, "all") <- TRUE # for the Console: tar_invalidate(everything())
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
      if (identical(tryCatch(invalidate_targets(names), error = fail("invalidate")), failed)) return()
      log(invalidate_code(names))
      backend$request_refresh()
      shiny::showNotification(
        paste0("Invalidated ", length(names), if (length(names) == 1) " target." else " targets."),
        type = "message"
      )
    })

    # The graph's menus and toolbars: on the target they were chosen on, or
    # on the selected group.
    shiny::observeEvent(action(), {
      chosen <- action()
      nodes <- as.character(unlist(chosen$nodes %||% chosen$node))
      switch(
        chosen$action,
        run = launch_run(nodes),
        invalidate = ask_to_invalidate(nodes),
        NULL
      )
    })

    list(
      show_problem = shiny::reactive(input$show_problem), scope = shiny::reactive(scope()),
      maker = shiny::reactive(maker())
    )
  })
}

#' The confirmation before invalidating targets
#' @param ns The run module's namespace, for its button.
#' @noRd
invalidate_modal <- function(names, ns = identity) {
  shown <- utils::head(names, 50)
  count <- if (length(names) == 1) "1 target" else paste(length(names), "targets")
  shiny::modalDialog(
    title = shiny::tagList(shiny::icon("eraser"), paste0("Invalidate ", count, "?")),
    shiny::p(
      class = "text-body-secondary",
      "The next run will build these targets again, and what depends on them.",
      "Until then, their values can no longer be read."
    ),
    # A list that scrolls past a dozen names.
    shiny::tags$ul(
      class = "list-group tb-modal-list",
      lapply(shown, \(name) shiny::tags$li(class = "list-group-item", shiny::tags$code(name))),
      if (length(names) > length(shown)) {
        shiny::tags$li(
          class = "list-group-item text-body-secondary",
          paste("and", length(names) - length(shown), "more")
        )
      }
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("invalidate_confirm"), "Invalidate", icon = shiny::icon("eraser"), class = "btn-danger")
    ),
    easyClose = TRUE
  )
}
