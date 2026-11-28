# Profile a target (#141): off by default. options(targetsboard.profile =
# TRUE) turns it on, and a switch in the Value pane's Debug section turns it
# on or off for this session. On, the section gives the profvis code to
# copy, and a Profile action that re-runs the target once, with its inputs
# from the store, in a background process, and links its flame graph.

# A build longer than this asks before profiling it again.
profile_warn_seconds <- 60

#' R code that profiles a target: its reproduce code, the command under
#' profvis
#' @inheritParams reproduce_code
#' @noRd
profile_code <- function(name, command, workspace, inputs = character(), seed = NULL,
                         script = NULL, store = NULL, dir = NULL) {
  command <- command %||% "# (the target's command)"
  wrapped <- paste0("profvis::profvis({\n", gsub("(^|\n)", "\\1  ", command), "\n})")
  reproduce_code(name, wrapped, workspace, inputs, seed, script = script, store = store, dir = dir)
}

#' Runs profile code in a background process, from the board's folder, and
#' saves its flame graph to `out`
#' @return The callr process.
#' @noRd
profile_start <- function(code, dir, out) {
  callr::r_bg(function(code, dir, out) {
    setwd(dir)
    options(keep.source = TRUE) # the flame graph shows the code's lines
    p <- eval(parse(text = code, keep.source = TRUE), envir = globalenv())
    htmlwidgets::saveWidget(p, out, selfcontained = FALSE)
  }, args = list(code = code, dir = dir, out = out), supervise = TRUE)
}

profile_ui <- function(id) shiny::uiOutput(shiny::NS(id, "block"))

#' @param target A reactive: `list(name, code, seconds, dir)` for the focused
#'   target, or NULL when it can't be profiled.
#' @param start Starts a profile: `profile_start()`, replaced in tests.
#' @return A reactive: the last profile's `list(name, url)` or
#'   `list(name, error)`, NULL before one ends.
#' @noRd
profile_server <- function(id, target, start = profile_start) {
  stopifnot(shiny::is.reactive(target))
  shiny::moduleServer(id, function(input, output, session) {
    on <- shiny::reactiveVal(isTRUE(getOption("targetsboard.profile")))
    shiny::observeEvent(input$on, on(isTRUE(input$on)), ignoreInit = TRUE)
    job <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal(NULL)

    output$block <- shiny::renderUI({
      t <- target()
      if (is.null(t)) return(NULL)
      shiny::tags$div(
        class = "tb-profile",
        shiny::tags$div(
          title = "Profile this target with profvis: re-run it once and see where its time goes.",
          bslib::input_switch(session$ns("on"), "Profile", value = on())
        ),
        if (on()) {
          shiny::tagList(
            code_block("Profile", "profile", t$code),
            if (!rlang::is_installed("profvis")) {
              shiny::tags$p(class = "text-muted small", "Install the profvis package to profile it here.")
            } else {
              shiny::actionButton(session$ns("profile"), "Profile", class = "btn-sm btn-default",
                                  disabled = if (!is.null(job())) NA)
            }
          )
        }
      )
    })

    go <- function(t) {
      out <- file.path(tempfile("targetsboard_profile_"), paste0(make.names(t$name), ".html"))
      dir.create(dirname(out), recursive = TRUE)
      job(list(name = t$name, out = out, process = start(t$code, t$dir, out)))
      shiny::showNotification(paste0("Profiling ", t$name, "..."), id = session$ns("running"), duration = NULL)
    }
    shiny::observeEvent(input$profile, {
      t <- target()
      shiny::req(t, is.null(job()))
      if (isTRUE(t$seconds > profile_warn_seconds)) {
        shiny::showModal(shiny::modalDialog(
          title = paste("Profile", t$name, "?"),
          paste0("Its last build took ", round(t$seconds), " seconds: profiling runs it again, as long, ",
                 "and its profile can be large."),
          footer = shiny::tagList(shiny::modalButton("Cancel"),
                                  shiny::actionButton(session$ns("confirm"), "Profile", class = "btn-primary"))
        ))
      } else {
        go(t)
      }
    })
    shiny::observeEvent(input$confirm, {
      shiny::removeModal()
      t <- target()
      shiny::req(t, is.null(job()))
      go(t)
    })

    shiny::observe({
      j <- job()
      shiny::req(j)
      if (j$process$is_alive()) return(shiny::invalidateLater(500))
      error <- tryCatch({
        j$process$get_result()
        NULL
      }, error = function(e) cli::ansi_strip(conditionMessage(e$parent %||% e)))
      job(NULL)
      shiny::removeNotification(session$ns("running"))
      if (is.null(error) && file.exists(j$out)) {
        url <- serve_html_report(j$out)
        result(list(name = j$name, url = url))
        shiny::showNotification(
          shiny::tags$a(href = url, target = "_blank", paste0("Open the flame graph of ", j$name)),
          duration = NULL
        )
      } else {
        error <- error %||% "it saved no flame graph."
        result(list(name = j$name, error = error))
        shiny::showNotification(paste0("Profiling ", j$name, " failed: ", error), type = "error", duration = NULL)
      }
    })

    result
  })
}
