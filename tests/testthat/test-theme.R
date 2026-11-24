test_that("board_themes() lists Targets board first, then Bootswatch presets", {
  themes <- board_themes()
  expect_equal(themes$name[1], "Targets board")
  expect_true(is.na(themes$preset[1]))
  expect_false(anyDuplicated(themes$name) > 0)
  expect_true(all(themes$preset[-1] %in% bslib::bootswatch_themes(5)))
  expect_type(themes$dark_only, "logical")
})

test_that("app_theme() is the board's own look by default, and a preset by name", {
  withr::local_options(targetsboard.theme = NULL)
  own <- app_theme()
  expect_s3_class(own, "bs_theme")
  expect_equal(bslib::theme_version(own), "5")
  expect_null(bslib::theme_bootswatch(own))

  cosmo <- app_theme("Cosmo")
  expect_equal(bslib::theme_bootswatch(cosmo), "cosmo")
  expect_equal(unname(bslib::bs_get_variables(cosmo, "min-contrast-ratio")), "4.5")

  withr::local_options(targetsboard.theme = "Lux")
  expect_equal(bslib::theme_bootswatch(app_theme()), "lux")
})

test_that("an unknown theme is an error that lists the themes", {
  expect_error(app_theme("Nope"), "Unknown board theme: Nope")
  expect_error(app_theme("Nope"), "Targets board, Brite")
  withr::local_options(targetsboard.theme = 3)
  expect_error(board_theme_option(), "Unknown board theme")
})

test_that("the top bar's picker lists the themes, starting on the board's", {
  withr::local_options(targetsboard.theme = NULL)
  html <- as.character(chrome_ui("chrome"))
  expect_match(html, '<select id="chrome-theme"', fixed = TRUE)
  expect_match(html, 'data-tb-theme="remember"', fixed = TRUE)
  expect_match(html, '<option value="Targets board" selected>', fixed = TRUE)
  for (name in board_themes()$name) expect_match(html, sprintf('value="%s"', name), fixed = TRUE)
  expect_match(html, '<option value="Darkly" data-dark-only>', fixed = TRUE)
  expect_no_match(html, "hidden", fixed = TRUE) # the dark mode switch shows
})

test_that("a theme set by the option starts the picker, and a dark one hides the switch", {
  withr::local_options(targetsboard.theme = "Darkly")
  html <- as.character(chrome_ui("chrome"))
  expect_match(html, 'data-tb-theme="option"', fixed = TRUE)
  expect_match(html, '<option value="Darkly" selected data-dark-only>', fixed = TRUE)
  expect_match(html, '<bslib-input-dark-mode[^>]*mode="dark"[^>]*hidden')
})

test_that("a pick sets the page's theme", {
  set <- character()
  local_mocked_bindings(set_page_theme = function(session, name) set <<- c(set, name))
  shiny::testServer(chrome_server, {
    session$setInputs(theme = "Targets board") # the page's first value: no change
    expect_length(set, 0)
    session$setInputs(theme = "Pulse")
    session$setInputs(theme = "Vapor")
    session$setInputs(theme = "Nope") # not a theme: ignored
    session$setInputs(theme = "Targets board")
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
