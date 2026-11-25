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

# The board's themes: the picker's list, in its order, by kind (its groups):
# - "board": "Targets board", the board's own look and the default.
# - "editor": the palettes of popular editor themes (board_palettes()), with
#   a light and a dark variant each.
# - "bootswatch": Bootswatch presets.
# All pass the contrast checks in test-browser-themes.R, in both modes, or
# in dark mode for a dark-only one (the page keeps it in dark mode). Of
# Bootswatch 5's, left out: journal (its buttons), morph (its light mode)
# and spacelab (text just under 4.5); and by choice cyborg, lumen, materia,
# sandstone, simplex, slate and superhero.
# `preset`: a Bootswatch preset's name. `dark_only`: the theme is dark in
# light mode too.
board_themes <- function() {
  presets <- c(
    "brite", "cerulean", "cosmo", "darkly", "flatly", "litera", "lux",
    "minty", "pulse", "quartz", "sketchy", "solar", "united", "vapor", "yeti",
    "zephyr"
  )
  editors <- names(board_palettes())
  data.frame(
    name = c("Targets board", editors, tools::toTitleCase(presets)),
    kind = rep(c("board", "editor", "bootswatch"), c(1, length(editors), length(presets))),
    preset = c(NA, rep(NA, length(editors)), presets),
    dark_only = c(FALSE, rep(FALSE, length(editors)), presets %in% c("darkly", "solar", "vapor"))
  )
}

# The picker's choices: the themes in groups, "Targets board" first, alone.
board_theme_choices <- function(themes = board_themes()) {
  c(
    list("Targets board" = "Targets board"),
    list(
      "Editor themes" = themes$name[themes$kind == "editor"],
      "Bootswatch" = themes$name[themes$kind == "bootswatch"]
    )
  )
}

#' The app's Bootstrap theme
#'
#' One of `board_themes()`, by name: the page's Bootstrap 5 theme, pinned to
#' Bootstrap 5 whatever bslib's default becomes, with the board's own colours
#' as rules. "Targets board" sets none of them: the widget's tokens keep
#' their own values (the beige canvas). A Bootswatch preset sets the canvas
#' from its colours, so a dark preset gets a dark canvas, asks Bootstrap for
#' readable text on its buttons, and darkens its inactive tabs' text. An
#' editor theme does the same with its palette's colours, in each mode
#' (palette_theme()).
#' @param name The theme's name: the `targetsboard.theme` option, or
#'   "Targets board".
#' @return A [bslib::bs_theme()].
#' @noRd
app_theme <- function(name = board_theme_option()) {
  themes <- board_themes()
  check_board_theme(name)
  theme <- themes[themes$name == name, ]
  if (theme$kind == "board") return(bslib::bs_theme(version = 5))
  if (theme$kind == "editor") return(palette_theme(board_palettes()[[name]]))
  preset <- theme$preset
  bslib::bs_theme(
    version = 5, preset = preset,
    # Bootswatch lowers it, and some presets then put white text on light
    # buttons; Bootstrap's own 4.5 picks dark text there.
    "min-contrast-ratio" = 4.5
  ) |>
    bslib::bs_add_rules(preset_rules)
}

# A theme's board colours, for a Bootswatch or an editor theme. The widget's
# tokens read --tb-theme-* when a theme sets them (targetsboardCustomCSS.css),
# in both modes: derived from --bs-*, they follow the mode themselves. The
# mixes were measured in every theme and mode (test-browser-themes.R checks
# them):
# - A status takes a theme colour: up to date success, outdated info,
#   errored danger, running warning. Its fill: 15% of it over Bootstrap's
#   subtle background of it; its text and outline: Bootstrap's emphasis of
#   it, 25% of the way to the page's strongest text. A solid accent with
#   the page's text read under 2 in some themes.
# - The warning mark and an errored branch's badge as the running and
#   errored targets; the spinner as a running target's text.
# - Edges: 75% of the text over the page; a selected one, the strongest
#   text. Into a running target: the running colour.
# - Selection, the focus ring: primary halfway to the strongest text (a
#   theme's primary alone vanished on Lux's dark canvas).
# - The controls: the page's surfaces, as the Tidy buttons.
preset_rules <- "
:root {
  --tb-theme-canvas-bg: color-mix(in srgb, var(--bs-body-bg) 92%, var(--bs-primary));
  --tb-theme-canvas-marks: color-mix(in srgb, var(--bs-body-color) 35%, transparent);
  --tb-theme-minimap-bg: var(--bs-body-bg);
  --tb-theme-minimap-mask: color-mix(in srgb, var(--bs-body-color) 12%, transparent);
  --tb-theme-status-uptodate: color-mix(in srgb, var(--bs-success) 15%, var(--bs-success-bg-subtle));
  --tb-theme-status-uptodate-text: color-mix(in srgb, var(--bs-success-text-emphasis) 75%, var(--bs-emphasis-color));
  --tb-theme-status-uptodate-border: var(--tb-theme-status-uptodate-text);
  --tb-theme-status-outdated: color-mix(in srgb, var(--bs-info) 15%, var(--bs-info-bg-subtle));
  --tb-theme-status-outdated-text: color-mix(in srgb, var(--bs-info-text-emphasis) 75%, var(--bs-emphasis-color));
  --tb-theme-status-outdated-border: var(--tb-theme-status-outdated-text);
  --tb-theme-status-errored: color-mix(in srgb, var(--bs-danger) 15%, var(--bs-danger-bg-subtle));
  --tb-theme-status-errored-text: color-mix(in srgb, var(--bs-danger-text-emphasis) 75%, var(--bs-emphasis-color));
  --tb-theme-status-errored-border: var(--tb-theme-status-errored-text);
  --tb-theme-status-running: color-mix(in srgb, var(--bs-warning) 15%, var(--bs-warning-bg-subtle));
  --tb-theme-status-running-text: color-mix(in srgb, var(--bs-warning-text-emphasis) 75%, var(--bs-emphasis-color));
  --tb-theme-status-running-border: var(--tb-theme-status-running-text);
  --tb-theme-spinner: var(--tb-theme-status-running-text);
  --tb-theme-warning: var(--tb-theme-status-running);
  --tb-theme-warning-text: var(--tb-theme-status-running-text);
  --tb-theme-danger: var(--tb-theme-status-errored);
  --tb-theme-danger-text: var(--tb-theme-status-errored-text);
  --tb-theme-node-border: color-mix(in srgb, var(--bs-body-color) 75%, var(--bs-body-bg));
  --tb-theme-handle: var(--tb-theme-node-border);
  --tb-theme-handle-border: var(--bs-body-bg);
  --tb-theme-edge: color-mix(in srgb, var(--bs-body-color) 75%, var(--bs-body-bg));
  --tb-theme-edge-selected: var(--bs-emphasis-color);
  --tb-theme-edge-running: var(--tb-theme-status-running-text);
  --tb-theme-edge-running-selected: var(--bs-emphasis-color);
  --tb-theme-focus-ring: color-mix(in srgb, var(--bs-primary) 50%, var(--bs-emphasis-color));
  --tb-theme-node-selected-border: var(--tb-theme-focus-ring);
  --tb-theme-selection-bg: color-mix(in srgb, var(--tb-theme-focus-ring) 10%, transparent);
  --tb-theme-selection-border: var(--tb-theme-focus-ring);
  --tb-theme-controls-bg: var(--bs-body-bg);
  --tb-theme-controls-bg-hover: var(--bs-tertiary-bg);
  --tb-theme-controls-color: var(--bs-body-color);
  --tb-theme-controls-color-hover: var(--bs-emphasis-color);
  --tb-theme-controls-border: var(--bs-border-color);
}
/* A preset's link colour can be faint on its page; the tabs take its text. */
#board-view .nav-link:not(.active) {
  --bs-nav-link-color: var(--bs-secondary-color);
  --bs-nav-link-hover-color: var(--bs-emphasis-color);
}
"

# Editor themes: four palettes of popular editor themes (vscodethemes.com,
# by installs), each with a light and a dark variant, so the board's two
# modes both get the theme's own colours. Hex values from each theme's
# official source:
# - Catppuccin: Latte and Mocha (github.com/catppuccin/palette, MIT).
# - Tokyo Night: Light and Night (github.com/enkia/tokyo-night-vscode-theme).
# - Nord: Snow Storm and Polar Night, with Frost and Aurora
#   (github.com/nordtheme/nord, MIT).
# - Gruvbox: light (faded accents) and dark (bright accents)
#   (github.com/morhetz/gruvbox, MIT/X11).
# Per mode: bg, fg (text), surface (secondary background), border, muted
# (secondary text), and the accents; a value left out is Bootstrap's own,
# from bg and fg. `link`: the light mode's link colour, a Sass expression,
# when its primary is too light for text on its bg.
board_palettes <- function() {
  list(
    "Catppuccin" = list(
      light = c(
        bg = "#eff1f5", fg = "#4c4f69", surface = "#e6e9ef", border = "#ccd0da",
        muted = "#6c6f85", primary = "#1e66f5", success = "#40a02b", info = "#209fb5",
        warning = "#df8e1d", danger = "#d20f39"
      ),
      dark = c(
        bg = "#1e1e2e", fg = "#cdd6f4", surface = "#181825", border = "#45475a",
        muted = "#a6adc8", primary = "#89b4fa", success = "#a6e3a1", info = "#74c7ec",
        warning = "#f9e2af", danger = "#f38ba8"
      )
    ),
    "Tokyo Night" = list(
      light = c(
        bg = "#e6e7ed", fg = "#343b58", surface = "#d6d8df",
        muted = "#707280", primary = "#2959aa", success = "#385f0d", info = "#006c86",
        warning = "#8f5e15", danger = "#8c4351"
      ),
      dark = c(
        bg = "#1a1b26", fg = "#c0caf5", surface = "#16161e", border = "#414868",
        muted = "#9aa5ce", primary = "#7aa2f7", success = "#9ece6a", info = "#7dcfff",
        warning = "#e0af68", danger = "#f7768e"
      )
    ),
    "Nord" = list(
      light = c(
        bg = "#eceff4", fg = "#2e3440", surface = "#e5e9f0", border = "#d8dee9",
        muted = "#4c566a", primary = "#5e81ac", success = "#a3be8c", info = "#88c0d0",
        warning = "#ebcb8b", danger = "#bf616a",
        link = "shade-color(#5e81ac, 25%)"
      ),
      dark = c(
        bg = "#2e3440", fg = "#eceff4", surface = "#3b4252", border = "#4c566a",
        muted = "#d8dee9", primary = "#88c0d0", success = "#a3be8c", info = "#8fbcbb",
        warning = "#ebcb8b", danger = "#bf616a"
      )
    ),
    "Gruvbox" = list(
      light = c(
        bg = "#fbf1c7", fg = "#3c3836", surface = "#f2e5bc", border = "#d5c4a1",
        muted = "#7c6f64", primary = "#076678", success = "#79740e", info = "#427b58",
        warning = "#b57614", danger = "#9d0006"
      ),
      dark = c(
        bg = "#282828", fg = "#ebdbb2", surface = "#32302f", border = "#504945",
        muted = "#a89984", primary = "#83a598", success = "#b8bb26", info = "#8ec07c",
        warning = "#fabd2f", danger = "#fb4934"
      )
    )
  )
}

# A palette's theme: Bootstrap 5 with its light colours as the theme's, its
# dark colours as Bootstrap 5.3's dark mode's (the *-dark Sass variables), and
# its dark accents as rules: Bootstrap compiles the accents for light mode
# only. With the presets' rules: the canvas from its own colours.
palette_theme <- function(palette) {
  light <- palette$light
  dark <- palette$dark
  accents <- c("primary", "success", "info", "warning", "danger")
  optional <- function(colours, name) if (name %in% names(colours)) colours[[name]]
  vars <- c(
    list(
      version = 5, bg = light[["bg"]], fg = light[["fg"]],
      "min-contrast-ratio" = 4.5,
      # bslib mixes the text from fg and bg: the palette's own, exactly.
      "body-color" = light[["fg"]],
      "body-secondary-color" = light[["muted"]],
      "body-secondary-bg" = light[["surface"]],
      "body-tertiary-bg" = light[["surface"]],
      "border-color" = optional(light, "border"),
      "link-color" = optional(light, "link"),
      "body-bg-dark" = dark[["bg"]],
      "body-color-dark" = dark[["fg"]],
      "body-emphasis-color-dark" = dark[["fg"]],
      "body-secondary-color-dark" = dark[["muted"]],
      "body-secondary-bg-dark" = dark[["surface"]],
      "body-tertiary-bg-dark" = dark[["surface"]],
      "border-color-dark" = optional(dark, "border"),
      "link-color-dark" = dark[["primary"]],
      "link-hover-color-dark" = dark[["fg"]]
    ),
    as.list(light[accents]),
    stats::setNames(as.list(dark[accents]), paste0(accents, "-text-emphasis-dark"))
  )
  vars <- vars[!vapply(vars, is.null, logical(1))]
  dark_accents <- vapply(accents, function(accent) {
    rgb <- paste(grDevices::col2rgb(dark[[accent]])[, 1], collapse = ", ")
    sprintf("  --bs-%s: %s;\n  --bs-%s-rgb: %s;", accent, dark[[accent]], accent, rgb)
  }, character(1))
  do.call(bslib::bs_theme, vars) |>
    bslib::bs_add_rules(c(
      preset_rules,
      paste0("[data-bs-theme=\"dark\"] {\n", paste(dark_accents, collapse = "\n"), "\n}")
    ))
}

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
