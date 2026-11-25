test_that("board_themes() lists Targets board first, then Bootswatch presets", {
  themes <- board_themes()
  expect_equal(themes$name[1], "Targets board")
  expect_true(is.na(themes$preset[1]))
  expect_false(anyDuplicated(themes$name) > 0)
  expect_equal(unique(themes$kind), c("board", "own", "editor", "bootswatch"))
  expect_equal(themes$name[themes$kind == "own"], names(board_own_palettes()))
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
  expect_error(app_theme("Nope"), "Targets board, Flow, Catppuccin")
  withr::local_options(targetsboard.theme = 3)
  expect_error(board_theme_option(), "Unknown board theme")
})

test_that("a palette has a dark variant, and a light one unless the theme is dark-only", {
  palettes <- c(board_own_palettes(), board_palettes())
  themes <- board_themes()
  for (name in names(palettes)) {
    palette <- palettes[[name]]
    dark_only <- themes$dark_only[themes$name == name]
    expect_named(palette, if (dark_only) "dark" else c("light", "dark"))
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
  expect_equal(names(choices), c("Targets board", "Flow", "Editor themes", "Bootswatch"))
  expect_equal(choices[["Editor themes"]], names(board_palettes()))
  html <- as.character(theme_ui("theme", theme = "Targets board"))
  expect_match(html, '<option value="Targets board" selected>Targets board</option>\\s*<option value="Flow">')
  expect_match(html, '<optgroup label="Bootswatch">', fixed = TRUE)
})

test_that("a dark-only palette is dark in light mode too, and Flow is the board's own", {
  themes <- board_themes()
  expect_true(themes$dark_only[themes$name == "Glow Night"])
  expect_false(themes$dark_only[themes$name == "Flow"])
  vars <- bslib::bs_get_variables(app_theme("Glow Night"), c("body-bg", "body-bg-dark", "link-color-dark"))
  expect_equal(tolower(unname(vars)), c("#070425", "#070425", "#ff068b"))
  expect_equal(names(board_theme_choices())[1:3], c("Targets board", "Flow", "Editor themes"))
  html <- as.character(theme_ui("theme", theme = "Targets board"))
  expect_match(html, '<option value="Flow">Flow</option>\\s*<optgroup label="Editor themes">')
})
