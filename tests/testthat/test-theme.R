test_that("board_themes() lists Targets board first, then Bootswatch presets", {
  themes <- board_themes()
  expect_equal(themes$name[1], "Targets board")
  expect_true(is.na(themes$preset[1]))
  expect_false(anyDuplicated(themes$name) > 0)
  expect_equal(unique(themes$kind), c("board", "editor", "bootswatch"))
  expect_true(all(themes$preset[themes$kind == "bootswatch"] %in% bslib::bootswatch_themes(5)))
  expect_true(all(is.na(themes$preset[themes$kind != "bootswatch"])))
  expect_equal(themes$name[themes$kind == "editor"], names(board_palettes()))
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
  expect_error(app_theme("Nope"), "Targets board, Catppuccin")
  withr::local_options(targetsboard.theme = 3)
  expect_error(board_theme_option(), "Unknown board theme")
})

test_that("board_palettes() gives each editor theme a light and a dark variant", {
  for (name in names(board_palettes())) {
    palette <- board_palettes()[[name]]
    expect_named(palette, c("light", "dark"))
    for (mode in palette) {
      expect_true(all(c("bg", "fg", "muted", "primary", "success", "info", "warning", "danger") %in% names(mode)))
      colours <- mode[names(mode) != "link"]
      expect_true(all(grepl("^#[0-9a-f]{6}$", colours)), label = name)
    }
  }
})

test_that("an editor theme's colours are the page's, in both modes", {
  theme <- app_theme("Catppuccin")
  expect_s3_class(theme, "bs_theme")
  expect_null(bslib::theme_bootswatch(theme))
  vars <- bslib::bs_get_variables(theme, c("body-bg", "body-color", "primary", "body-bg-dark", "body-color-dark", "min-contrast-ratio"))
  expect_equal(tolower(unname(vars)), c("#eff1f5", "#4c4f69", "#1e66f5", "#1e1e2e", "#cdd6f4", "4.5"))
})

test_that("the picker groups the themes, Targets board first", {
  choices <- board_theme_choices()
  expect_equal(names(choices), c("Targets board", "Editor themes", "Bootswatch"))
  expect_equal(choices[["Editor themes"]], names(board_palettes()))
  html <- as.character(theme_ui("theme", theme = "Targets board"))
  expect_match(html, '<option value="Targets board" selected>Targets board</option>\\s*<optgroup label="Editor themes">')
  expect_match(html, '<optgroup label="Bootswatch">', fixed = TRUE)
})
