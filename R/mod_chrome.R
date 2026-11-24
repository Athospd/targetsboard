#' The page's top bar: its title, the theme picker, the dark mode switch and
#' the power button
#'
#' The picker lists `board_themes()`. A dark-only theme starts the page in
#' dark mode and hides the switch, which it would not change.
#'
#' @param id The module's id.
#' @param theme The theme the page starts with: `board_theme_option()`.
#' @noRd
chrome_ui <- function(id, theme = board_theme_option()) {
  ns <- shiny::NS(id)
  dark_only <- theme_is_dark_only(theme)
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$span(class = "tb-app-title", "Targets board"),
    shiny::tags$div(
      class = "d-flex align-items-center gap-2",
      theme_picker(ns("theme"), theme),
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

# A <select> of the themes, for Shiny's select binding. `data-tb-theme`: the
# page remembers the pick in the browser (actions.js), unless the
# targetsboard.theme option chose the theme, which then wins.
theme_picker <- function(id, theme) {
  themes <- board_themes()
  shiny::tags$select(
    id = id, class = "form-select form-select-sm tb-theme-picker",
    title = "Theme", `aria-label` = "Theme",
    `data-tb-theme` = if (is.null(getOption("targetsboard.theme"))) "remember" else "option",
    Map(function(name, dark_only) {
      shiny::tags$option(
        value = name, name,
        selected = if (identical(name, theme)) NA,
        `data-dark-only` = if (dark_only) NA
      )
    }, themes$name, themes$dark_only, USE.NAMES = FALSE)
  )
}

theme_is_dark_only <- function(name) {
  themes <- board_themes()
  isTRUE(themes$dark_only[themes$name == name])
}

#' @return A list of reactives: `color_mode`, `"light"` or `"dark"`.
#' @noRd
chrome_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # The power button: stop the board. In tar_board()'s background process,
    # that ends the process too.
    shiny::observeEvent(input$turn_off, shiny::stopApp("The board is turned off."))

    # A pick: the page's new theme, compiled and sent; the page then forces
    # dark mode, or gives the switch back (actions.js).
    shiny::observeEvent(input$theme, ignoreInit = TRUE, {
      shiny::req(input$theme %in% board_themes()$name)
      set_page_theme(session, input$theme)
    })

    list(color_mode = shiny::reactive(input$dark_mode))
  })
}

# Sets the page's theme, and tells the page whether it's dark-only
# (actions.js).
set_page_theme <- function(session, name) {
  session$setCurrentTheme(app_theme(name))
  session$sendCustomMessage("tb-theme", list(dark_only = theme_is_dark_only(name)))
}
