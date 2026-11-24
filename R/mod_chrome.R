#' The page's top bar: its title, the dark mode switch and the power button
#'
#' @param id The module's id.
#' @noRd
chrome_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$span(class = "tb-app-title", "Targets board"),
    shiny::tags$div(
      class = "d-flex align-items-center gap-2",
      bslib::input_dark_mode(id = ns("dark_mode"), mode = "light"),
      shiny::actionButton(
        ns("turn_off"), NULL,
        icon = shiny::icon("power-off"), title = "Turn the board off", class = "btn-sm"
      )
    )
  )
}

#' @return A list of reactives: `color_mode`, `"light"` or `"dark"`.
#' @noRd
chrome_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # The power button: stop the board. In tar_board()'s background process,
    # that ends the process too.
    shiny::observeEvent(input$turn_off, shiny::stopApp("The board is turned off."))

    list(color_mode = shiny::reactive(input$dark_mode))
  })
}
