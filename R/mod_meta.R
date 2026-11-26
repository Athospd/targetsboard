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
      meta <- tar_meta()
      reactable::reactable(
        meta,
        theme = board_table_theme(),
        # Readable times and sizes, sorted as numbers: slowest first.
        columns = meta_columns(meta),
        defaultSorted = if ("seconds" %in% names(meta)) list(seconds = "desc"),
        wrap = FALSE,
        onClick = row_click_js(session$ns("row_clicked")),
        rowStyle = list(cursor = "pointer")
      )
    })

    shiny::reactive(input$row_clicked)
  })
}

# The tar_meta() table's seconds and bytes, shown as "42 s" and "1.2 MB".
meta_columns <- function(meta) {
  columns <- list()
  if ("seconds" %in% names(meta)) {
    columns$seconds <- reactable::colDef(
      cell = function(value) if (is.na(value)) "" else format_seconds(value),
      align = "right", defaultSortOrder = "desc"
    )
  }
  if ("bytes" %in% names(meta)) {
    columns$bytes <- reactable::colDef(
      cell = function(value) format_bytes(value) %||% "",
      align = "right", defaultSortOrder = "desc"
    )
  }
  if (length(columns)) columns else NULL
}
