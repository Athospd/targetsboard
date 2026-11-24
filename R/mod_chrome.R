#' The page's top bar: its title, the dark mode switch and the power button
#'
#' A dark-only theme starts the page in dark mode, with the switch hidden: it
#' would not change the theme (actions.js shows it again on a pick).
#'
#' @param id The module's id.
#' @param ... Other controls, put before the switch: the theme picker
#'   (theme_ui()).
#' @param theme The theme the page starts with: `board_theme_option()`.
#' @noRd
chrome_ui <- function(id, ..., theme = board_theme_option()) {
  ns <- shiny::NS(id)
  dark_only <- theme_is_dark_only(theme)
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$span(class = "tb-app-title", "Targets board"),
    shiny::tags$div(
      class = "d-flex align-items-center gap-2",
      ...,
      htmltools::tagAppendAttributes(
        bslib::input_dark_mode(id = ns("dark_mode"), mode = if (dark_only) "dark" else "light"),
        hidden = if (dark_only) NA
      ),
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
