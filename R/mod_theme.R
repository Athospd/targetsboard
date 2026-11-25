#' The theme picker and the dark mode button
#'
#' A select of `board_themes()`, and a button to switch between light and
#' dark mode, as the power button beside it. Placed by app_ui(), today in the
#' top bar (chrome_ui()'s `...`); it works anywhere on the page.
#'
#' A pick sets the page's theme (theme_server()). A dark-only theme puts the
#' page in dark mode and hides the button, which would not change it;
#' leaving it gives back the mode the page had. The browser remembers the
#' pick and the next page starts on it, unless the `targetsboard.theme`
#' option chose the theme, which then wins.
#'
#' The page's mode lives in bslib's dark mode switch, hidden: it sets the
#' page's mode and sends it as an input; the button and the server flip it
#' (bslib::toggle_dark_mode()). bslib draws the switch's icon in colours of
#' its own, which a theme's navbar can hide (black on black): the button is
#' an actionButton(), as the power button, in the theme's button colours.
#'
#' @param id The module's id.
#' @param theme The theme the page starts with: `board_theme_option()`.
#' @noRd
theme_ui <- function(id, theme = board_theme_option()) {
  ns <- shiny::NS(id)
  themes <- board_themes()
  dark_only <- themes$name[themes$dark_only]
  shiny::tags$div(
    class = "tb-theme d-flex align-items-center gap-2",
    shiny::selectInput(
      ns("pick"), "Theme",
      choices = board_theme_choices(themes), selected = theme, selectize = FALSE, width = "auto"
    ),
    # Shown unless the theme is dark-only.
    shiny::conditionalPanel(
      sprintf("![%s].includes(input.pick)", paste0("'", dark_only, "'", collapse = ", ")),
      ns = ns,
      shiny::actionButton(
        ns("mode"), NULL,
        icon = shiny::tagList(
          shiny::icon("sun", class = "tb-mode-light"),
          shiny::icon("moon", class = "tb-mode-dark")
        ),
        title = "Light or dark mode", class = "btn-sm tb-mode-button"
      )
    ),
    htmltools::tagAppendAttributes(
      bslib::input_dark_mode(id = ns("dark_mode"), mode = if (theme %in% dark_only) "dark" else "light"),
      hidden = NA
    )
  )
}

#' @return A list of reactives: `theme`, the picked theme's name;
#'   `dark_only`, whether it's dark-only; `color_mode`, the page's mode,
#'   `"light"` or `"dark"`.
#' @noRd
theme_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    themes <- board_themes()$name
    remember <- is.null(getOption("targetsboard.theme"))
    dark_only <- shiny::reactive(theme_is_dark_only(input$pick))

    # The remembered pick: the page sends it, as `saved` (actions.js).
    if (remember) ask_saved_theme(session)
    shiny::observeEvent(input$saved, {
      shiny::req(input$saved %in% themes, !identical(input$saved, input$pick))
      shiny::updateSelectInput(session, "pick", selected = input$saved)
    })

    # A pick, not on load: the page starts with its theme. A dark-only one
    # puts the page in dark mode; leaving it gives back the mode before.
    was_dark_only <- shiny::reactiveVal(theme_is_dark_only(board_theme_option()))
    mode_before <- shiny::reactiveVal("light")
    shiny::observeEvent(input$pick, ignoreInit = TRUE, {
      shiny::req(input$pick %in% themes)
      set_page_theme(session, input$pick, remember)
      if (dark_only() && !was_dark_only()) {
        mode_before(if (is.null(input$dark_mode)) "light" else input$dark_mode)
        set_page_mode(session, "dark")
      } else if (!dark_only() && was_dark_only()) {
        set_page_mode(session, mode_before())
      }
      was_dark_only(dark_only())
    })

    shiny::observeEvent(input$mode, {
      set_page_mode(session, if (identical(input$dark_mode, "dark")) "light" else "dark")
    })

    list(
      theme = shiny::reactive(input$pick),
      dark_only = dark_only,
      color_mode = shiny::reactive(input$dark_mode)
    )
  })
}

# Sets the page's theme; the page remembers it, unless the option chose
# the theme (actions.js).
set_page_theme <- function(session, name, remember = TRUE) {
  session$setCurrentTheme(app_theme(name))
  if (remember) session$sendCustomMessage("tb-theme-remember", list(name = name))
}

# Sets the page's mode: bslib's switch follows, and sends it.
set_page_mode <- function(session, mode) {
  bslib::toggle_dark_mode(mode, session = session)
}

# Asks the page for the theme it remembers, as the module's `saved` input.
ask_saved_theme <- function(session) {
  session$sendCustomMessage("tb-theme-saved", list(input = session$ns("saved")))
}

theme_is_dark_only <- function(name) {
  themes <- board_themes()
  isTRUE(themes$dark_only[themes$name == name])
}
