#' The page's top bar: its title, the dark mode switch and the power button
#'
#' @param id The module's id.
#' @param address What goes between the title and the switches: the board's
#'   script bar.
#' @noRd
chrome_ui <- function(id, address = NULL) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center gap-3 w-100 tb-top-bar",
    shiny::tags$span(class = "text-nowrap", "Targets board"),
    address,
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
