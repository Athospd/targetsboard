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
      mode_button(ns("dark_mode"), dark_only),
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

# The dark mode button: a button as the power button beside it (the same
# classes, a Font Awesome icon), so a theme's button colours, which fit its
# navbar, are its colours too: the sun in light mode, the moon in dark
# (index.css). A click flips bslib's switch, hidden beside it, which sets the
# page's mode and sends it as the `dark_mode` input (srcjs/actions.js). A
# dark-only theme hides the button.
mode_button <- function(id, dark_only = FALSE) {
  shiny::tagList(
    shiny::tags$button(
      type = "button", class = "btn btn-default btn-sm tb-mode-button",
      title = "Light or dark mode", `aria-label` = "Light or dark mode",
      `data-tb-mode-toggle` = id,
      hidden = if (dark_only) NA,
      shiny::icon("sun", class = "tb-mode-light"),
      shiny::icon("moon", class = "tb-mode-dark")
    ),
    htmltools::tagAppendAttributes(
      bslib::input_dark_mode(id = id, mode = if (dark_only) "dark" else "light"),
      hidden = NA
    )
  )
}
