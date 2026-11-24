#' The tar_visnetwork() tab
#'
#' The graph as `tar_visnetwork()` draws it, the view `tar_watch()` shows:
#' recomputed with the board's graph, without live statuses.
#'
#' @param id The module's id.
#' @noRd
visnetwork_ui <- function(id) {
  visNetwork::visNetworkOutput(shiny::NS(id, "graph"), height = "100%")
}

#' @rdname visnetwork_ui
#' @param board The board's graph, whose `widget` is `tar_visnetwork()`'s: a
#'   reactive.
#' @noRd
visnetwork_server <- function(id, board) {
  stopifnot(shiny::is.reactive(board))
  shiny::moduleServer(id, function(input, output, session) {
    output$graph <- visNetwork::renderVisNetwork({
      shiny::req(board()$widget)
      board()$widget
    })
  })
}
