#' The page's top bar: its title, other controls and the power button
#'
#' @param id The module's id.
#' @param ... Other controls, put before the power button: the theme picker
#'   and the dark mode button (theme_ui()).
#' @noRd
chrome_ui <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$span(class = "tb-app-title", brand_wordmark()),
    shiny::tags$div(
      class = "d-flex align-items-center gap-2",
      ...,
      shiny::actionButton(
        ns("turn_off"), NULL,
        icon = shiny::icon("power-off"), title = "Turn the board off", class = "btn-sm"
      )
    )
  )
}

#' @return An empty list: the power button stops the board.
#' @noRd
chrome_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # The power button: stop the board. In tar_board()'s background process,
    # that ends the process too.
    shiny::observeEvent(input$turn_off, shiny::stopApp("The board is turned off."))

    list()
  })
}

#' The brand's mark, inline, in the theme's text colour
#'
#' The wordmark, the Pulse mark and the name (dev/logo/make_logo.py writes
#' inst/app/www/wordmark.svg, the name as outlines): inline rather than an
#' image, so it takes `currentColor` in every theme and mode. Its
#' `aria-label` is the name screen readers read.
#' @noRd
brand_wordmark <- function() {
  shiny::HTML(paste(readLines(app_sys("app/www/wordmark.svg"), warn = FALSE), collapse = "
"))
}
