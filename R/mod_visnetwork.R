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
#' @param color_mode Reactive: `"light"` or `"dark"`, the page's mode.
#' @noRd
visnetwork_server <- function(id, board, color_mode) {
  stopifnot(shiny::is.reactive(board), shiny::is.reactive(color_mode))
  shiny::moduleServer(id, function(input, output, session) {
    output$graph <- visNetwork::renderVisNetwork({
      shiny::req(board()$widget)
      visnetwork_in_mode(board()$widget, color_mode())
    })
  })
}

# visNetwork draws labels on a canvas, so they can't read the page's CSS
# variables: in dark mode, the graph's and the legend's labels get dark
# mode's text colour, Bootstrap's.
visnetwork_in_mode <- function(widget, mode) {
  if (!identical(mode, "dark")) return(widget)
  text <- "#dee2e6"
  widget$x$options$nodes$font <- list(color = text)
  if (is.data.frame(widget$x$legend$nodes)) {
    widget$x$legend$nodes$font.color <- text
  }
  widget
}
