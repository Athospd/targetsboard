#' The Internals tab
#'
#' The board's graph and the app's inputs, shown only to develop the board
#' (`board_dev_mode()`).
#'
#' @param id The module's id.
#' @noRd
internals_ui <- function(id) {
  shiny::verbatimTextOutput(shiny::NS(id, "text"))
}

#' @rdname internals_ui
#' @param board The board's graph: a reactive.
#' @param inputs The app's inputs, as a list: a reactive. The module's own
#'   `input` holds only its own, namespaced.
#' @noRd
internals_server <- function(id, board, inputs) {
  stopifnot(shiny::is.reactive(board), shiny::is.reactive(inputs))
  shiny::moduleServer(id, function(input, output, session) {
    output$text <- shiny::renderPrint({
      c(
        board = board()[c("x", "progress_mtime")], # not the visNetwork widget
        inputs()
      )
    })
  })
}
