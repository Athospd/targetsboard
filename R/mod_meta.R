#' The tar_meta() tab
#'
#' `tar_meta()` as a table. A click on a row asks to show its target on the
#' board.
#'
#' @param id The module's id.
#' @noRd
meta_ui <- function(id) {
  reactable::reactableOutput(shiny::NS(id, "table"))
}

#' @rdname meta_ui
#' @param tar_meta `tar_meta(targets_only = TRUE)`: a reactive.
#' @return The name of the target whose row was clicked, a reactive that
#'   fires on every click, even on the same row twice.
#' @noRd
meta_server <- function(id, tar_meta) {
  stopifnot(shiny::is.reactive(tar_meta))
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- reactable::renderReactable({
      shiny::req(tar_meta())
      reactable::reactable(
        tar_meta(),
        wrap = FALSE,
        onClick = row_click_js(session$ns("row_clicked")),
        rowStyle = list(cursor = "pointer")
      )
    })

    shiny::reactive(input$row_clicked)
  })
}
