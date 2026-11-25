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
# - "own": themes of the board's own, from a palette (board_own_palettes()).
# - "editor": the palettes of popular editor themes (board_palettes()).
# - "bootswatch": Bootswatch presets.
# All pass the contrast checks in test-browser-themes.R, in both modes, or
# in dark mode for a dark-only one (the page keeps it in dark mode). Of
# Bootswatch 5's, left out: journal (its buttons), morph (its light mode)
# and spacelab (text just under 4.5); and by choice cyborg, lumen, materia,
# sandstone, simplex, slate and superhero.
# `preset`: a Bootswatch preset's name. `dark_only`: the theme is dark in
# light mode too (a palette with no light variant).
board_themes <- function() {
  presets <- c(
    "brite", "cerulean", "cosmo", "darkly", "flatly", "litera", "lux",
    "minty", "pulse", "quartz", "sketchy", "solar", "united", "vapor", "yeti",
    "zephyr"
  )
  own <- names(board_own_palettes())
  editors <- names(board_palettes())
  palettes <- c(board_own_palettes(), board_palettes())
  data.frame(
    name = c("Targets board", own, editors, tools::toTitleCase(presets)),
    kind = rep(
      c("board", "own", "editor", "bootswatch"),
      c(1, length(own), length(editors), length(presets))
    ),
    preset = c(NA, rep(NA, length(palettes)), presets),
    dark_only = c(
      FALSE, vapply(palettes, function(p) is.null(p$light), logical(1)),
      presets %in% c("darkly", "solar", "vapor")
    ),
    row.names = NULL
  )
}

# The picker's choices: the themes in groups. "Targets board" and the
# board's own themes first, not in a group.
board_theme_choices <- function(themes = board_themes()) {
  own <- themes$name[themes$kind %in% c("board", "own")]
  c(
    as.list(stats::setNames(own, own)),
    list(
      "Editor themes" = themes$name[themes$kind == "editor"],
      "Bootswatch" = themes$name[themes$kind == "bootswatch"]
    )
  )
}

# A palette theme's palette, by name: the board's own or an editor's.
theme_palette <- function(name) c(board_own_palettes(), board_palettes())[[name]]

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
  if (theme$kind %in% c("own", "editor")) return(palette_theme(theme_palette(name)))
  preset <- theme$preset
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

# The board's own palette themes:
# - Flow: React Flow's website (reactflow.dev): its pink, hsl(333 100% 50%),
#   on its white and near-black pages, with its slate text, and its
#   Tailwind accents (600 or 500 in light mode, 400 in dark), from its
#   stylesheets' --primary, --foreground, --muted-foreground, --border.
# As board_palettes(), below.
board_own_palettes <- function() {
  list(
    "Flow" = list(
      light = c(
        bg = "#ffffff", fg = "#0f172a", surface = "#f1f5f9", border = "#e2e8f0",
        muted = "#64748b", primary = "#ff0073", success = "#00a544", info = "#3080ff",
        warning = "#fe6e00", danger = "#fb2c36",
        link = "shade-color(#ff0073, 20%)"
      ),
      dark = c(
        bg = "#111111", fg = "#ffffff", surface = "#171717", border = "#333333",
        muted = "#97a3b4", primary = "#ff0073", success = "#05df72", info = "#54a2ff",
        warning = "#ff8b1a", danger = "#ff6568"
      )
    )
  )
}

# Editor themes: palettes of popular editor themes (vscodethemes.com), each
# with a light and a dark variant, so the board's two modes both get the
# theme's own colours, or a dark one only, and the theme is dark-only. Hex
# values from each theme's official source:
# - Catppuccin: Latte and Mocha (github.com/catppuccin/palette, MIT).
# - Tokyo Night: Light and Night (github.com/enkia/tokyo-night-vscode-theme).
# - Nord: Snow Storm and Polar Night, with Frost and Aurora
#   (github.com/nordtheme/nord, MIT).
# - Gruvbox: light (faded accents) and dark (bright accents)
#   (github.com/morhetz/gruvbox, MIT/X11).
# - Solarized: light and dark, Ethan Schoonover's palette, as Ryan Olson's
#   "Solarized" for VS Code (github.com/ryanolsonx/vscode-solarized-theme,
#   MIT). Its text: base01 on light, base1 on dark, a step stronger than
#   the editor's body text, for small text on the board.
# - Glow Night: dark only (genta.glow-night, the VS Code extension's theme
#   file; github.com/gentabelardi/GlowNight): its page and pink, and its
#   syntax colours as the accents.
# Per mode: bg, fg (text), surface (secondary background), border, muted
# (secondary text), and the accents; a value left out is Bootstrap's own,
# from bg and fg. `link`: the mode's link colour, a Sass expression, when
# its primary is too faint for text on its bg.
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
    ),
    "Solarized" = list(
      light = c(
        bg = "#fdf6e3", fg = "#586e75", surface = "#eee8d5", border = "#ddd6c1",
        muted = "#657b83", primary = "#268bd2", success = "#859900", info = "#2aa198",
        warning = "#b58900", danger = "#dc322f",
        link = "shade-color(#268bd2, 25%)"
      ),
      dark = c(
        bg = "#002b36", fg = "#93a1a1", surface = "#073642", border = "#586e75",
        muted = "#839496", primary = "#268bd2", success = "#859900", info = "#2aa198",
        warning = "#b58900", danger = "#dc322f",
        link = "tint-color(#268bd2, 25%)"
      )
    ),
    "Glow Night" = list(
      dark = c(
        bg = "#070425", fg = "#58c4c4", surface = "#0d0935", border = "#484291",
        muted = "#65737e", primary = "#e4097e", success = "#c3e88d", info = "#89ddff",
        warning = "#ffcb6b", danger = "#ff5370", link = "#ff068b"
      )
    )
  )
}

# A palette's theme: Bootstrap 5 with its light colours as the theme's, its
# dark colours as Bootstrap 5.3's dark mode's (the *-dark Sass variables), and
# its dark accents as rules: Bootstrap compiles the accents for light mode
# only. With the presets' rules: the canvas from its own colours. A dark-only
# palette's dark colours are its light mode's too.
palette_theme <- function(palette) {
  dark <- palette$dark
  light <- if (is.null(palette$light)) dark else palette$light
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
      "link-color-dark" = if ("link" %in% names(dark)) dark[["link"]] else dark[["primary"]],
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
