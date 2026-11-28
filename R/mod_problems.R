#' The Problems tab
#'
#' What errored or warned in the latest builds of the board's targets: a
#' table, and a count in the tab's title, red with errors, yellow with only
#' warnings. A click on a row asks to show its target on the board.
#'
#' Its UI is in two places: `problems_count_ui()` in the tab's title,
#' `problems_ui()` in the tab.
#'
#' @param id The module's id.
#' @noRd
problems_count_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "count"), inline = TRUE)
}

#' @rdname problems_count_ui
#' @noRd
problems_ui <- function(id) {
  reactable::reactableOutput(shiny::NS(id, "table"))
}

#' @rdname problems_count_ui
#' @param board The board's graph, `list(x = tar_visnetwork()$x, ...)`: a
#'   reactive.
#' @param tar_meta `tar_meta(targets_only = TRUE)`: a reactive.
#' @return The name of the target whose row was clicked, a reactive that
#'   fires on every click, even on the same row twice.
#' @noRd
problems_server <- function(id, board, tar_meta) {
  stopifnot(shiny::is.reactive(board), shiny::is.reactive(tar_meta))
  shiny::moduleServer(id, function(input, output, session) {
    problems <- shiny::reactive({
      targets <- board_targets(board()$x$nodes)
      problems_table(tar_meta(), if (length(targets)) targets)
    })

    output$count <- shiny::renderUI({
      n <- nrow(problems())
      if (n == 0) return(NULL)
      errors <- any(problems()$problem == "Error")
      shiny::tags$span(class = paste("badge ms-1", if (errors) "text-bg-danger" else "text-bg-warning"), n)
    })

    output$table <- reactable::renderReactable({
      reactable::reactable(
        problems(),
        theme = board_table_theme(),
        columns = list(
          name = reactable::colDef(name = "Target", minWidth = 120),
          problem = reactable::colDef(name = "Problem", width = 100),
          message = reactable::colDef(name = "Message", minWidth = 300),
          branches = reactable::colDef(name = "Branches", width = 140)
        ),
        wrap = FALSE,
        defaultPageSize = 50,
        language = reactable::reactableLang(noData = "No problems: no target errored or warned in its latest build."),
        onClick = row_click_js(session$ns("row_clicked")),
        rowStyle = list(cursor = "pointer")
      )
    })

    shiny::reactive(input_name(input$row_clicked))
  })
}

#' A reactable's onClick that sends the clicked row's target name to `input_id`
#'
#' As an event, so a click on the same row twice sends it twice. Not the
#' empty row of an empty table.
#' @param input_id The input's full id, namespaced.
#' @noRd
row_click_js <- function(input_id) {
  reactable::JS(sprintf(
    "function(row) { if (row && row.values && row.values.name) Shiny.setInputValue('%s', row.values.name, {priority: 'event'}); }",
    input_id
  ))
}
