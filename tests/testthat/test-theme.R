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
