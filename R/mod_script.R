#' The script the board draws: the Script field, Load and Refresh
#'
#' Its controls sit in the page's sidebar; while Load loads another script,
#' script_loading_ui() covers the Board pane.
#' @param id The module's id.
#' @param script Path to the targets script.
#' @param scripts Other scripts the Script field offers.
#' @noRd
script_ui <- function(id, script, scripts = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    script_input(script, scripts, id = ns("path")),
    # Busy, with a spinner, until the script's graph arrives: loading a
    # pipeline can take a while.
    bslib::input_task_button(
      ns("load"), "Load", icon = shiny::icon("folder-open"), label_busy = "Loading...",
      type = "default", auto_reset = FALSE
    ),
    shiny::actionButton(ns("refresh"), "Refresh", icon = shiny::icon("rotate"))
  )
}

#' The cover over the Board pane while Load loads a script
#'
#' A shinycssloaders spinner, styled in index.css from the theme's colours
#' (custom.css). It shows only with a caption.
#' @noRd
script_loading_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "tb-board-loading tb-loading",
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("loading")),
      type = 8, custom.css = TRUE, hide.ui = FALSE, id = ns("loading_spinner"),
      caption = shiny::textOutput(ns("loading_caption"), inline = TRUE)
    )
  )
}

#' @param backend The board's backend (board_backend()): its poller, the
#'   script it reads, and the preview cache.
#' @param metadata `targets::tar_visnetwork()$x`, the first graph.
#' @param targets_only Whether the graph shows only targets.
#' @param poller_board Reactive: each graph the poller recomputes.
#' @param script_error Reactive: why the script doesn't load, or `NULL`.
#' @return A list of reactives: `board`, the graph the board draws, once
#'   accepted, and `methods`, which changes when the project's preview
#'   methods are loaded again.
#' @noRd
script_server <- function(id, backend, metadata, script, scripts = NULL, targets_only = TRUE,
                          poller_board, script_error) {
  stopifnot(
    !shiny::is.reactive(backend), !shiny::is.reactive(metadata), !shiny::is.reactive(script),
    !shiny::is.reactive(scripts), !shiny::is.reactive(targets_only),
    shiny::is.reactive(poller_board), shiny::is.reactive(script_error)
  )
  shiny::moduleServer(id, function(input, output, session) {
    poller <- backend$poller

    # The graph the board draws: the first render's, then each the poller
    # recomputes, once Load accepts it.
    board <- shiny::reactiveVal(list(x = metadata, progress_mtime = progress_mtime()))

    shiny::observeEvent(poller_board(), {
      new_board <- poller_board()
      # While the author decides on a large graph, the poller's next
      # recomputations wait with it.
      if (!is.null(pending_board())) return(pending_board(new_board))
      nodes <- nrow(new_board$x$nodes %||% data.frame())
      if (shiny::isolate(loading()) && nodes > confirm_nodes_limit()) {
        pending_board(new_board)
        shiny::showModal(large_graph_modal(shiny::isolate(poller$script), nodes, targets_only, session$ns))
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
    shiny::observeEvent(input$draw_large, {
      shiny::req(pending_board())
      board(pending_board())
      pending_board(NULL)
      shiny::removeModal()
      load_done()
    })
    shiny::observeEvent(input$cancel_large, {
      shiny::req(pending_board())
      pending_board(NULL)
      shiny::removeModal()
      backend$start_poller(previous_script)
      load_done()
    })

    # Load stays busy from the click until the new script's graph arrives, or
    # it turns out not to load. The poller writes a new file for each script,
    # so the first one it writes is that script's graph.
    loading <- shiny::reactiveVal(FALSE)
    load_done <- function() {
      if (!shiny::isolate(loading())) return()
      loading(FALSE)
      bslib::update_task_button("load", state = "ready")
      shinycssloaders::hideSpinner("loading")
    }
    # Meanwhile the spinner covers the Board pane, and keeps the old graph
    # and its preview from being used. The loading output is only its
    # anchor: an empty output, whose first value hides the spinner the
    # package shows when an output binds.
    output$loading <- shiny::renderUI(NULL)
    shiny::outputOptions(output, "loading", suspendWhenHidden = FALSE)
    output$loading_caption <- shiny::renderText({
      if (loading()) "Loading pipeline\u2026"
    })
    # The cover shows only with a caption (index.css): the caption has to
    # update while the cover is hidden.
    shiny::outputOptions(output, "loading_caption", suspendWhenHidden = FALSE)

    shiny::observeEvent(script_error(), ignoreNULL = FALSE, {
      if (!is.null(script_error())) load_done()
      text <- script_error_text(poller$script, script_error())
      if (is.null(text)) {
        shiny::removeNotification("script_error", session)
      } else {
        shiny::showNotification(text, id = "script_error", duration = NULL, type = "error", session = session)
      }
    })

    # The Script field names the script the board draws. The poller is shared,
    # so a tab opened after someone loaded another script, or open while they
    # did, follows it.
    shiny::observeEvent(backend$loaded_script(), {
      shown <- backend$loaded_script()
      if (identical(shown, shiny::isolate(input$path))) return()
      shiny::updateSelectizeInput(
        session, "path",
        choices = script_choices(script, c(scripts, shown)), selected = shown
      )
    })

    shiny::observeEvent(input$load, {
      new_script <- input$path
      # Empty when a typed text that matches no script was left in the field.
      if (!nzchar(new_script %||% "")) {
        shiny::showNotification("Choose a script to load.", type = "warning")
        bslib::update_task_button("load", state = "ready")
        return()
      }
      if (!file.exists(new_script)) {
        shiny::showNotification(paste0("Script not found: ", new_script), type = "error")
        bslib::update_task_button("load", state = "ready")
        return()
      }
      if (identical(new_script, poller$script)) {
        bslib::update_task_button("load", state = "ready")
        return()
      }
      loading(TRUE)
      shinycssloaders::showSpinner("loading")
      previous_script <<- poller$script
      backend$start_poller(new_script)
    })

    # The preview methods of the project's _targetsboard.R: at start, and
    # again on Refresh, so editing them needs no restart.
    methods <- shiny::reactiveVal(0)
    reload_preview_methods <- function() {
      loaded <- tryCatch(load_preview_methods(), error = function(e) e)
      if (inherits(loaded, "error")) {
        shiny::showNotification(
          paste("_targetsboard.R didn't load:", conditionMessage(loaded)),
          type = "error", duration = NULL
        )
      }
      backend$previews$clear()
      methods(shiny::isolate(methods()) + 1)
    }
    reload_preview_methods()
    shiny::observeEvent(input$refresh, {
      backend$request_refresh()
      reload_preview_methods()
    })

    list(board = shiny::reactive(board()), methods = shiny::reactive(methods()))
  })
}
