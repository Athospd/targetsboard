# The board's look, in two places:
#
# - The page (sidebar, tabs, inputs, previews): app_theme(), a Bootstrap 5
#   theme. Our CSS (inst/app/www/index.css) reads its colours as `--bs-*`
#   variables, so it follows the theme and the dark mode switch.
# - The graph widget: the `--tb-*` colour tokens at the top of
#   srcjs/targetsboardCustomCSS.css, with light and dark values. The widget
#   must look right outside the app too, so it keeps its own.
#
# Text sizes: Bootstrap's body size and the --tb-font-size-* scale at the top
# of index.css. Buttons: one style, bslib's default (`btn-default`), in two
# sizes, the sidebar's and `btn-sm`; on the canvas, the toolbar's button.

# The board's themes: the picker's list, in its order. "Targets board" is
# the board's own look and the default; the others are Bootswatch presets
# that pass the contrast checks in test-browser-themes.R, in both modes, or
# in dark mode for a dark-only one (the page keeps it in dark mode): all of
# Bootswatch 5's but journal (its buttons), morph (its light mode) and
# spacelab (text just under 4.5).
# `dark_only`: the preset is dark in light mode too.
board_themes <- function() {
  presets <- c(
    "brite", "cerulean", "cosmo", "cyborg", "darkly", "flatly", "litera",
    "lumen", "lux", "materia", "minty", "pulse", "quartz", "sandstone",
    "simplex", "sketchy", "slate", "solar", "superhero", "united", "vapor",
    "yeti", "zephyr"
  )
  data.frame(
    name = c("Targets board", tools::toTitleCase(presets)),
    preset = c(NA, presets),
    dark_only = c(FALSE, presets %in% c("cyborg", "darkly", "slate", "solar", "superhero", "vapor"))
  )
}

#' The app's Bootstrap theme
#'
#' One of `board_themes()`, by name: the page's Bootstrap 5 theme, pinned to
#' Bootstrap 5 whatever bslib's default becomes, with the board's own colours
#' as rules. "Targets board" sets none of them: the widget's tokens keep
#' their own values (the beige canvas). A Bootswatch preset sets the canvas
#' from its colours, so a dark preset gets a dark canvas, asks Bootstrap for
#' readable text on its buttons, and darkens its inactive tabs' text.
#' @param name The theme's name: the `targetsboard.theme` option, or
#'   "Targets board".
#' @return A [bslib::bs_theme()].
#' @noRd
app_theme <- function(name = board_theme_option()) {
  themes <- board_themes()
  check_board_theme(name)
  preset <- themes$preset[themes$name == name]
  if (is.na(preset)) return(bslib::bs_theme(version = 5))
  bslib::bs_theme(
    version = 5, preset = preset,
    # Bootswatch lowers it, and some presets then put white text on light
    # buttons; Bootstrap's own 4.5 picks dark text there.
    "min-contrast-ratio" = 4.5
  ) |>
    bslib::bs_add_rules(preset_rules)
}

# A preset's board colours. The widget's tokens read --tb-theme-* when a
# theme sets them (targetsboardCustomCSS.css), in both modes: derived from
# --bs-*, they follow the mode themselves.
preset_rules <- "
:root {
  --tb-theme-canvas-bg: color-mix(in srgb, var(--bs-body-bg) 92%, var(--bs-primary));
  --tb-theme-canvas-marks: color-mix(in srgb, var(--bs-body-color) 35%, transparent);
  --tb-theme-minimap-bg: var(--bs-body-bg);
  --tb-theme-minimap-mask: color-mix(in srgb, var(--bs-body-color) 12%, transparent);
}
/* A preset's link colour can be faint on its page; the tabs take its text. */
#board-view .nav-link:not(.active) {
  --bs-nav-link-color: var(--bs-secondary-color);
  --bs-nav-link-hover-color: var(--bs-emphasis-color);
}
"

# The theme the board starts with: the `targetsboard.theme` option, checked.
board_theme_option <- function() {
  check_board_theme(getOption("targetsboard.theme", "Targets board"))
}

check_board_theme <- function(name) {
  themes <- board_themes()$name
  if (!rlang::is_string(name) || !name %in% themes) {
    rlang::abort(c(
      sprintf("Unknown board theme: %s.", format(name)),
      i = paste("Themes:", paste(themes, collapse = ", "))
    ), call = NULL)
  }
  name
}
