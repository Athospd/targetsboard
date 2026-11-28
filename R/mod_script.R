#' The script the board draws: a bar as a browser's address bar
#'
#' The project, when there are projects (project_picker()), then Refresh,
#' the script's path, and Load, above the board's tabs. The path is
#' free text, with the known scripts as suggestions under it: all of them
#' when it gets the focus, filtered as one types; Enter or a pick loads it. While Load loads another script,
#' script_loading_ui() covers the Board pane.
#' @param id The module's id.
#' @param script Path to the targets script.
#' @param scripts Other scripts the bar suggests.
#' @noRd
# The switch that draws functions and globals on the graph, or not (#305).
# Off at first; script_server() sets it as the board is.
functions_switch <- function(id) {
  shiny::tags$div(
    title = "Draw the functions and global objects the targets use, as tar_visnetwork(targets_only = FALSE) does",
    bslib::input_switch(id, "Functions and globals", value = FALSE)
  )
}

script_ui <- function(id, script, scripts = NULL) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "tb-address-bar",
    project_picker(ns("project"), sprintf("output['%s'] === 'true'", ns("show_projects"))),
    script_bar(ns, script, scripts)
  )
}

# Refresh, the path with its suggestions, and Load, in one input group.
script_bar <- function(ns, script, scripts) {
  shiny::tags$div(
    class = "input-group input-group-sm tb-address",
    shiny::actionButton(
      ns("refresh"), NULL, icon = shiny::icon("rotate"),
      class = "btn-default", title = "Refresh: read the script again"
    ),
    script_input(script, id = ns("path"), suggestions = ns("suggestions"), load = ns("load")),
    # Busy, with a spinner, until the script's graph arrives: loading a
    # pipeline can take a while.
    bslib::input_task_button(
      ns("load"), "Load", icon = shiny::icon("folder-open"), label_busy = "Loading...",
      type = "default", auto_reset = FALSE
    ),
    # The suggestions: in the page from the start, then as the server
    # updates them.
    shiny::tags$span(
      id = ns("suggestions_list"), class = "shiny-html-output", hidden = NA,
      script_suggestions(ns("suggestions"), known_scripts(script, scripts))
    )
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
#' @param poller_board Reactive: each graph the poller recomputes.
#' @param script_error Reactive: why the script doesn't load, or `NULL`.
#' @param running Reactive: whether a run is going. The project can't change
#'   then.
#' @return A list of reactives: `board`, the graph the board draws, once
#'   accepted, and `methods`, which changes when the project's preview
#'   methods are loaded again.
#' @noRd
script_server <- function(id, backend, metadata, script, scripts = NULL,
                          poller_board, script_error, running = shiny::reactive(FALSE)) {
  stopifnot(
    !shiny::is.reactive(backend), !shiny::is.reactive(metadata), !shiny::is.reactive(script),
    !shiny::is.reactive(scripts),
    shiny::is.reactive(poller_board), shiny::is.reactive(script_error), shiny::is.reactive(running)
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
        shiny::showModal(large_graph_modal(shiny::isolate(poller$script), nodes, shiny::isolate(backend$targets_only()), session$ns))
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
    previous_project <- NULL
    previous_only <- NULL
    # Back to the script, and its project, before Load or a project switch;
    # or to targets only, before the switch turned functions on.
    go_back <- function() {
      if (!identical(previous_only, shiny::isolate(backend$targets_only()))) {
        backend$targets_only(previous_only)
        bslib::update_switch("functions", value = !previous_only)
        backend$start_poller(previous_script)
      } else if (identical(previous_project, shiny::isolate(backend$project()))) {
        backend$start_poller(previous_script)
      } else {
        backend$switch_project(previous_project, previous_script)
      }
    }
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
      go_back()
      load_done()
    })

    # Load stays busy from the click until the new script's graph arrives, or
    # it turns out not to load. The poller writes a new file for each script,
    # so the first one it writes is that script's graph.
    loading <- shiny::reactiveVal(FALSE)
    # Load, or a project switch: the spinner over the Board pane, and where
    # Cancel goes back to.
    start_loading <- function() {
      loading(TRUE)
      shinycssloaders::showSpinner("loading")
      previous_script <<- poller$script
      previous_project <<- shiny::isolate(backend$project())
      previous_only <<- shiny::isolate(backend$targets_only())
    }
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
      # No script at all: the board's notice says so (no_script_ui()).
      text <- if (is.null(poller$script) || file.exists(poller$script)) {
        script_error_text(poller$script, script_error())
      }
      if (is.null(text)) {
        shiny::removeNotification("script_error", session)
      } else {
        shiny::showNotification(text, id = "script_error", duration = NULL, type = "error", session = session)
      }
    })

    # The bar names the script the board draws. The poller is shared, so a
    # tab opened after someone loaded another script, or open while they
    # did, follows it. The scripts loaded join the suggestions.
    known <- shiny::reactiveVal(known_scripts(script, scripts))
    # A document's script (tangle_script()) shows as the document (#136).
    shiny::observeEvent(backend$loaded_script(), {
      shown <- backend$loaded_script()
      shown <- tangle_document(shown) %||% shown
      known(union(shiny::isolate(known()), shown))
      if (identical(shown, shiny::isolate(input$path))) return()
      shiny::updateTextInput(session, "path", value = shown)
    })
    output$suggestions_list <- shiny::renderUI(script_suggestions(session$ns("suggestions"), known()))
    shiny::outputOptions(output, "suggestions_list", suspendWhenHidden = FALSE)

    # The scripts loaded on each store this session (#245): two pipelines on
    # one store share the records of targets with the same names.
    store_scripts <- shiny::reactiveVal(list())
    remember_script <- function(project, script) {
      key <- normalizePath(project_store(project), winslash = "/", mustWork = FALSE)
      history <- shiny::isolate(store_scripts())
      history[[key]] <- union(history[[key]], script)
      store_scripts(history)
      setdiff(history[[key]], script)
    }
    others <- shiny::reactiveVal(remember_script(shiny::isolate(backend$project()), script))

    # Functions and globals on the graph or not (#305): the poller again, on
    # the same script, its graph loading as Load's does. The switch starts
    # as the backend is: a page loaded again keeps it. Only when it differs
    # from the page's: a late update would undo a click made meanwhile.
    if (!shiny::isolate(backend$targets_only())) bslib::update_switch("functions", value = TRUE)
    shiny::observeEvent(input$functions, ignoreInit = TRUE, {
      only <- !isTRUE(input$functions)
      if (identical(only, shiny::isolate(backend$targets_only()))) return()
      start_loading()
      backend$set_targets_only(only)
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
      # A pipeline written as a document loads through a script that tangles
      # it (#136): tar_tangle() needs tarchetypes and parsermd.
      if (is_tangle_document(new_script)) {
        missing <- c("tarchetypes", "parsermd")[!vapply(c("tarchetypes", "parsermd"), rlang::is_installed, logical(1))]
        if (length(missing)) {
          shiny::showNotification(paste0("A pipeline written as a document needs ", and_list(missing),
                                         ": install ", if (length(missing) == 1) "it" else "them", " to load it."),
                                  type = "error")
          bslib::update_task_button("load", state = "ready")
          return()
        }
        new_script <- tangle_script(new_script)
      }
      if (identical(new_script, poller$script)) {
        bslib::update_task_button("load", state = "ready")
        return()
      }
      # Another project's script loads on that project (projects.R).
      current <- shiny::isolate(backend$project())
      project <- project_of_script(new_script, current)
      if (!identical(project, current) && shiny::isolate(running())) {
        shiny::showNotification(
          paste0(new_script, " is project ", project, "'s script: load it after the run."),
          type = "warning"
        )
        bslib::update_task_button("load", state = "ready")
        return()
      }
      start_loading()
      others(remember_script(project, new_script))
      if (identical(project, current) && length(others())) {
        shiny::showNotification(shared_store_ui(new_script, others(), project_store(project)),
                                type = "warning", duration = NULL)
      }
      if (identical(project, current)) {
        backend$start_poller(new_script)
      } else {
        backend$switch_project(project, new_script)
      }
    })

    # The projects of _targets.yaml, followed as the file changes, and the
    # Project field on the active one. Their scripts join the suggestions.
    listed <- watch_changes(listed_projects)
    # "true" or "false", for the field's conditionalPanel.
    output$show_projects <- shiny::renderText({
      tolower(shows_projects(board_projects(backend$project(), listed()), backend$project()))
    })
    shiny::outputOptions(output, "show_projects", suspendWhenHidden = FALSE)
    shiny::observe({
      active <- backend$project()
      shiny::updateSelectInput(session, "project", choices = project_choices(active, listed()), selected = active)
    })
    shiny::observe({
      session$sendCustomMessage("tb-disable", list(
        id = session$ns("project"), disabled = running(),
        title = if (running()) "Switch project after the run"
      ))
    })
    shiny::observeEvent(listed(), ignoreNULL = FALSE, {
      active <- shiny::isolate(backend$project())
      known(union(shiny::isolate(known()), project_scripts(board_projects(active, listed()), active)))
    })
    shiny::observeEvent(input$project, {
      new_project <- input$project
      if (identical(new_project, shiny::isolate(backend$project()))) return()
      if (shiny::isolate(running())) return() # disabled meanwhile: a race
      start_loading()
      others(remember_script(new_project, project_script(new_project)))
      backend$switch_project(new_project)
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

    list(board = shiny::reactive(board()), methods = shiny::reactive(methods()), store_others = shiny::reactive(others()))
  })
}

# The scripts the bar suggests: the board's, the ones given, and the
# projects' (projects.R).
known_scripts <- function(script, scripts = NULL) {
  unique(c(script_choices(script, scripts), project_scripts()))
}

#' The script bar's suggestions, a datalist of paths
#' @noRd
script_suggestions <- function(id, paths) {
  shiny::tags$datalist(id = id, lapply(paths, function(path) shiny::tags$option(value = path)))
}

#' The notice of a script loaded on a store other scripts used (#245)
#'
#' With a _targets.yaml project that gives the script its own store, to
#' copy: the board doesn't edit the author's files.
#' @param script The script loaded.
#' @param others The scripts loaded on the store before it, this session.
#' @param store The store.
#' @noRd
shared_store_ui <- function(script, others, store) {
  name <- gsub("[^A-Za-z0-9_]+", "_", tools::file_path_sans_ext(basename(script)))
  yaml <- paste0(name, ":
  script: ", script, "
  store: _targets_", name)
  shiny::tagList(
    shiny::tags$p(paste0(
      script, " uses the same store as ", and_list(others), " (", store, "). ",
      "Targets with the same names are shared."
    )),
    code_block(paste0("A store of its own, in _targets.yaml (TAR_PROJECT=", name, ")"), "store", yaml)
  )
}
