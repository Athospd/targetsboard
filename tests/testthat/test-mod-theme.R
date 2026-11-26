test_that("the picker lists the themes, starting on the board's", {
  withr::local_options(targetsboard.theme = NULL)
  html <- as.character(theme_ui("theme"))
  expect_match(html, '<select class="shiny-input-select[^"]*" id="theme-pick">')
  expect_match(html, '<option value="Targets board" selected>', fixed = TRUE)
  for (name in board_themes()$name) expect_match(html, sprintf('value="%s"', name), fixed = TRUE)
  expect_match(html, '<label[^>]*for="theme-pick"[^>]*>Theme</label>') # for screen readers
})

test_that("the dark mode button is an actionButton, as the power button, over bslib's switch", {
  html <- as.character(theme_ui("theme", theme = "Targets board"))
  expect_match(html, '<button class="btn btn-default action-button btn-sm tb-mode-button" id="theme-mode"', fixed = TRUE)
  power <- as.character(chrome_ui("chrome"))
  expect_match(power, '<button class="btn btn-default action-button btn-sm" id="chrome-turn_off"', fixed = TRUE)
  expect_match(html, "fa-sun", fixed = TRUE)
  expect_match(html, "fa-moon", fixed = TRUE)
  # Shown unless the pick is dark-only.
  dark_only <- board_themes()$name[board_themes()$dark_only]
  expect_match(html, 'data-display-if="![^"]*\\.includes\\(input.pick\\)" data-ns-prefix="theme-"')
  for (name in dark_only) expect_match(html, sprintf("&#39;%s&#39;", name), fixed = TRUE)
  # bslib's switch: the mode's state, not its looks.
  expect_match(html, '<bslib-input-dark-mode id="theme-dark_mode"[^>]*mode="light"[^>]*hidden')
  expect_match(as.character(theme_ui("theme", theme = "Darkly")), '<bslib-input-dark-mode[^>]*mode="dark"')
})

test_that("the top bar holds the picker and the button before the power button", {
  html <- as.character(chrome_ui("chrome", theme_ui("theme")))
  expect_match(html, 'id="theme-pick".*id="theme-mode".*id="chrome-turn_off"')
  expect_no_match(as.character(chrome_ui("chrome")), "theme-pick", fixed = TRUE) # placed by app_ui()
  expect_match(as.character(app_ui(list(nodes = fake_nodes(), edges = fake_edges()), "_targets.R")),
               'id="theme-pick"', fixed = TRUE)
})

# theme_server() with its effects on the page recorded.
local_theme_calls <- function(env = parent.frame()) {
  calls <- new.env()
  calls$themes <- character()
  calls$modes <- character()
  calls$asked <- 0
  local_mocked_bindings(
    set_page_theme = function(session, name, remember = TRUE) calls$themes <- c(calls$themes, name),
    set_page_mode = function(session, mode, remember = FALSE) {
      calls$modes <- c(calls$modes, mode)
      calls$remembered <- c(calls$remembered, remember)
    },
    ask_saved_theme = function(session) calls$asked <- calls$asked + 1,
    .env = env
  )
  calls
}

test_that("a pick sets the page's theme; dark-only ones put the page in dark mode, and back", {
  withr::local_options(targetsboard.theme = NULL)
  calls <- local_theme_calls()
  shiny::testServer(theme_server, {
    session$setInputs(pick = "Targets board", dark_mode = "light") # the page's first values
    expect_length(calls$themes, 0)
    session$setInputs(pick = "Pulse")
    expect_false(session$returned$dark_only())
    session$setInputs(pick = "Vapor")
    expect_true(session$returned$dark_only())
    session$setInputs(dark_mode = "dark")
    session$setInputs(pick = "Darkly") # dark-only to dark-only: the mode stays
    session$setInputs(pick = "Nope") # not a theme: ignored
    session$setInputs(pick = "Targets board")
    expect_equal(calls$themes, c("Pulse", "Vapor", "Darkly", "Targets board"))
    expect_equal(calls$modes, c("dark", "light")) # into Vapor; out of Darkly, the mode before
    expect_equal(session$returned$theme(), "Targets board")
    expect_equal(session$returned$color_mode(), "dark") # the switch's, until the page sends it
  })
})

test_that("the button flips the page's mode", {
  calls <- local_theme_calls()
  shiny::testServer(theme_server, {
    session$setInputs(pick = "Targets board", dark_mode = "light")
    session$setInputs(mode = 1)
    session$setInputs(dark_mode = "dark", mode = 2)
    expect_equal(calls$modes, c("dark", "light"))
    expect_equal(calls$remembered, c(TRUE, TRUE)) # the user's: the page keeps it (#216)
  })
})

test_that("a dark-only theme's dark isn't remembered, and leaving it gives back the user's mode (#216)", {
  withr::local_options(targetsboard.theme = NULL)
  calls <- local_theme_calls()
  shiny::testServer(theme_server, {
    session$setInputs(pick = "Targets board", dark_mode = "light")
    session$setInputs(mode = 1) # the user goes dark
    session$setInputs(dark_mode = "dark", pick = "Darkly")
    session$setInputs(pick = "Targets board")
    expect_equal(calls$modes, c("dark", "dark", "dark")) # their click, Darkly's, then theirs back
    expect_equal(calls$remembered, c(TRUE, FALSE, FALSE))
  })
})

test_that("the page's remembered pick is restored, unless the option chose the theme", {
  withr::local_options(targetsboard.theme = NULL)
  calls <- local_theme_calls()
  shiny::testServer(theme_server, {
    expect_equal(calls$asked, 1)
    session$setInputs(pick = "Targets board")
    session$setInputs(saved = list(theme = "Nope")) # not a theme: ignored
    session$setInputs(saved = list(theme = "Pulse")) # updateSelectInput(): no error in the test session
    expect_length(calls$modes, 0) # no mode remembered: the page's stays
    session$setInputs(saved = list(theme = "Pulse", mode = "dark"))
    expect_equal(calls$modes, "dark")
    session$setInputs(saved = list(theme = "Vapor", mode = "light")) # dark-only: it keeps its dark
    expect_equal(calls$modes, "dark")
  })
  # The option's theme wins, but the user's mode is still theirs.
  withr::local_options(targetsboard.theme = "Cosmo")
  calls <- local_theme_calls()
  shiny::testServer(theme_server, {
    expect_equal(calls$asked, 1)
    session$setInputs(pick = "Cosmo")
    session$setInputs(saved = list(theme = "Pulse", mode = "dark"))
    expect_equal(calls$modes, "dark")
    expect_equal(session$returned$theme(), "Cosmo")
  })
})

test_that("set_page_theme() sets the theme, and has the page remember it unless asked not to", {
  calls <- list()
  session <- list(
    setCurrentTheme = function(theme) calls$theme <<- theme,
    sendCustomMessage = function(type, message) calls$message <<- list(type, message)
  )
  set_page_theme(session, "Vapor")
  expect_equal(bslib::theme_bootswatch(calls$theme), "vapor")
  expect_equal(calls$message, list("tb-theme-remember", list(name = "Vapor")))
  calls$message <- NULL
  set_page_theme(session, "Targets board", remember = FALSE)
  expect_null(bslib::theme_bootswatch(calls$theme))
  expect_null(calls$message)
})

test_that("set_page_mode() has the page remember the mode only when asked", {
  calls <- list()
  session <- list(sendCustomMessage = function(type, message) calls$message <<- list(type, message))
  local_mocked_bindings(toggle_dark_mode = function(mode, session) calls$mode <<- mode, .package = "bslib")
  set_page_mode(session, "dark")
  expect_equal(calls$mode, "dark")
  expect_null(calls$message)
  set_page_mode(session, "light", remember = TRUE)
  expect_equal(calls$message, list("tb-theme-remember", list(mode = "light")))
})
