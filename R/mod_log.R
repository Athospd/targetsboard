#' The Log tab
#'
#' The output of the latest run the board started, headed by what the run
#' was, and re-read while it grows.
#'
#' @param id The module's id.
#' @noRd
log_ui <- function(id) {
  shiny::tags$div(class = "tb-run-log", shiny::verbatimTextOutput(shiny::NS(id, "text")))
}

#' @rdname log_ui
#' @param backend `board_backend()`, whose `runner` holds the run: not
#'   reactive.
#' @noRd
log_server <- function(id, backend) {
  stopifnot(!shiny::is.reactive(backend))
  shiny::moduleServer(id, function(input, output, session) {
    runner <- backend$runner
    run_log <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() {
        path <- attr(runner$process, "log")
        if (is.null(path)) NA else paste(path, file.mtime(path))
      },
      valueFunc = function() read_run_log(attr(runner$process, "log"))
    )

    output$text <- shiny::renderText({
      log <- run_log()
      if (is.null(log)) return("No run started from the board yet: the log of its next run shows here.")
      paste0(attr(runner$process, "header"), "\n\n", log)
    })
  })
}
