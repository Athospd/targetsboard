#' The theme picker: a select of `board_themes()`
#'
#' Placed by app_ui(), today in the top bar (chrome_ui()'s `...`); it works
#' anywhere on the page. A pick sets the page's theme; the page then forces
#' dark mode for a dark-only theme and hides the dark mode switch, or gives
#' the switch back (srcjs/actions.js, which also remembers the pick in the
#' browser).
#'
#' @param id The module's id.
#' @param theme The theme the page starts with: `board_theme_option()`.
#' @noRd
theme_ui <- function(id, theme = board_theme_option()) {
  themes <- board_themes()
  # data-tb-theme: the page remembers the pick in the browser (actions.js),
  # unless the targetsboard.theme option chose the theme, which then wins.
  shiny::tags$select(
    id = shiny::NS(id, "pick"), class = "form-select form-select-sm tb-theme-picker",
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

#' @return A list of reactives: `theme`, the picked theme's name.
#' @noRd
theme_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Not on load: the page starts with its theme.
    shiny::observeEvent(input$pick, ignoreInit = TRUE, {
      shiny::req(input$pick %in% board_themes()$name)
      set_page_theme(session, input$pick)
    })

    list(theme = shiny::reactive(input$pick))
  })
}

# Sets the page's theme, and tells the page whether it's dark-only
# (actions.js).
set_page_theme <- function(session, name) {
  session$setCurrentTheme(app_theme(name))
  session$sendCustomMessage("tb-theme", list(dark_only = theme_is_dark_only(name)))
}

theme_is_dark_only <- function(name) {
  themes <- board_themes()
  isTRUE(themes$dark_only[themes$name == name])
}
