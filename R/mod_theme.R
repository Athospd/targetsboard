#' The theme picker and the dark mode button
#'
#' A select of `board_themes()`, and a button to switch between light and
#' dark mode, as the power button beside it. Placed by app_ui(), today in the
#' top bar (chrome_ui()'s `...`); it works anywhere on the page.
#'
#' A pick sets the page's theme (theme_server()). A dark-only theme puts the
#' page in dark mode and hides the button, which would not change it;
#' leaving it gives back the mode the user had. The browser remembers the
#' pick and the mode the user set (#216), and the next page starts on them,
#' unless the `targetsboard.theme` option chose the theme, which then wins;
#' the mode is still remembered.
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
    # The user's own mode: their last click, or the one the browser
    # remembers. A dark-only theme's dark isn't theirs: leaving the theme
    # gives theirs back.
    user_mode <- shiny::reactiveVal("light")

    # What the page remembers: `list(theme, mode)`, as `saved` (actions.js).
    # The mode first, so a remembered dark-only theme that follows doesn't
    # take dark for the user's.
    ask_saved_theme(session)
    shiny::observeEvent(input$saved, {
      theme <- input$saved$theme
      # The page's theme: the picker's value, or before it comes, the one
      # the page started with.
      theme <- if (remember && isTRUE(theme %in% themes)) theme else input$pick %||% board_theme_option()
      mode <- input$saved$mode
      if (isTRUE(mode %in% c("light", "dark"))) {
        user_mode(mode)
        if (!theme_is_dark_only(theme)) set_page_mode(session, mode)
      }
      if (!identical(theme, input$pick)) shiny::updateSelectInput(session, "pick", selected = theme)
    })

    # A pick, not on load: the page starts with its theme. A dark-only one
    # puts the page in dark mode; leaving it gives back the user's mode.
    was_dark_only <- shiny::reactiveVal(theme_is_dark_only(board_theme_option()))
    shiny::observeEvent(input$pick, ignoreInit = TRUE, {
      shiny::req(input$pick %in% themes)
      set_page_theme(session, input$pick, remember)
      if (dark_only() && !was_dark_only()) {
        set_page_mode(session, "dark")
      } else if (!dark_only() && was_dark_only()) {
        set_page_mode(session, user_mode())
      }
      was_dark_only(dark_only())
    })

    shiny::observeEvent(input$mode, {
      mode <- if (identical(input$dark_mode, "dark")) "light" else "dark"
      user_mode(mode)
      set_page_mode(session, mode, remember = TRUE)
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

# Sets the page's mode: bslib's switch follows, and sends it. The page
# remembers it when the user chose it (actions.js).
set_page_mode <- function(session, mode, remember = FALSE) {
  bslib::toggle_dark_mode(mode, session = session)
  if (remember) session$sendCustomMessage("tb-theme-remember", list(mode = mode))
}

# Asks the page for the theme and mode it remembers, as the module's `saved`
# input: `list(theme, mode)`, each `NULL` if none.
ask_saved_theme <- function(session) {
  session$sendCustomMessage("tb-theme-saved", list(input = session$ns("saved")))
}

theme_is_dark_only <- function(name) {
  themes <- board_themes()
  isTRUE(themes$dark_only[themes$name == name])
}
