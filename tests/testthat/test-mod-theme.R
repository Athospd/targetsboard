test_that("the picker lists the themes, starting on the board's", {
  withr::local_options(targetsboard.theme = NULL)
  html <- as.character(theme_ui("theme"))
  expect_match(html, '<select id="theme-pick"', fixed = TRUE)
  expect_match(html, 'data-tb-theme="remember"', fixed = TRUE)
  expect_match(html, '<option value="Targets board" selected>', fixed = TRUE)
  for (name in board_themes()$name) expect_match(html, sprintf('value="%s"', name), fixed = TRUE)
  expect_match(html, '<option value="Darkly" data-dark-only>', fixed = TRUE)
})

test_that("a theme set by the option starts the picker, which then doesn't remember picks", {
  withr::local_options(targetsboard.theme = "Darkly")
  html <- as.character(theme_ui("theme"))
  expect_match(html, 'data-tb-theme="option"', fixed = TRUE)
  expect_match(html, '<option value="Darkly" selected data-dark-only>', fixed = TRUE)
})

test_that("the top bar holds the picker, before its switch, and hides the switch for a dark theme", {
  withr::local_options(targetsboard.theme = NULL)
  html <- as.character(chrome_ui("chrome", theme_ui("theme")))
  expect_match(html, 'id="theme-pick".*class="btn btn-default btn-sm tb-mode-button"')
  expect_no_match(html, "<button[^>]*tb-mode-button[^>]*hidden") # the dark mode button shows
  expect_no_match(as.character(chrome_ui("chrome")), "theme-pick", fixed = TRUE) # placed by app_ui()
  expect_match(as.character(app_ui(list(nodes = fake_nodes(), edges = fake_edges()), "_targets.R")),
               'id="theme-pick"', fixed = TRUE)

  dark <- as.character(chrome_ui("chrome", theme = "Darkly"))
  expect_match(dark, '<button[^>]*tb-mode-button[^>]*hidden')
  expect_match(dark, '<bslib-input-dark-mode[^>]*mode="dark"[^>]*hidden')
})

test_that("a pick sets the page's theme, and the module returns it", {
  set <- character()
  local_mocked_bindings(set_page_theme = function(session, name) set <<- c(set, name))
  shiny::testServer(theme_server, {
    session$setInputs(pick = "Targets board") # the page's first value: no change
    expect_length(set, 0)
    session$setInputs(pick = "Pulse")
    expect_equal(session$returned$theme(), "Pulse")
    session$setInputs(pick = "Vapor")
    session$setInputs(pick = "Nope") # not a theme: ignored
    session$setInputs(pick = "Targets board")
    expect_equal(set, c("Pulse", "Vapor", "Targets board"))
  })
})

test_that("set_page_theme() sends the theme and whether it's dark-only", {
  calls <- list()
  session <- list(
    setCurrentTheme = function(theme) calls$theme <<- theme,
    sendCustomMessage = function(type, message) calls$message <<- list(type, message)
  )
  set_page_theme(session, "Vapor")
  expect_equal(bslib::theme_bootswatch(calls$theme), "vapor")
  expect_equal(calls$message, list("tb-theme", list(dark_only = TRUE)))
  set_page_theme(session, "Targets board")
  expect_null(bslib::theme_bootswatch(calls$theme))
  expect_equal(calls$message[[2]]$dark_only, FALSE)
})

test_that("the dark mode button is a button as the power button, over bslib's switch", {
  html <- as.character(chrome_ui("chrome", theme = "Targets board"))
  power <- regmatches(html, regexpr('<button[^>]*id="chrome-turn_off"[^>]*>', html))
  expect_match(power, "btn-sm", fixed = TRUE)
  expect_match(html, '<button type="button" class="btn btn-default btn-sm tb-mode-button"[^>]*data-tb-mode-toggle="chrome-dark_mode"')
  expect_match(html, '<bslib-input-dark-mode id="chrome-dark_mode"[^>]*hidden') # its state, not its looks
  expect_match(html, "fa-sun", fixed = TRUE)
  expect_match(html, "fa-moon", fixed = TRUE)
})
