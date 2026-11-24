# The board's look, in two places:
#
# - The page (sidebar, tabs, inputs, previews): app_theme(), a Bootstrap 5
#   theme. Our CSS (inst/app/www/index.css) reads its colours as `--bs-*`
#   variables, so it follows the theme and the dark mode switch.
# - The graph widget: the `--tb-*` colour tokens at the top of
#   srcjs/targetsboardCustomCSS.css, with light and dark values. The widget
#   must look right outside the app too, so it keeps its own.

#' The app's Bootstrap theme
#'
#' Pinned to Bootstrap 5, whatever bslib's default becomes: the board's CSS
#' is written for it, and for its colour modes.
#' @return A [bslib::bs_theme()].
#' @noRd
app_theme <- function() {
  bslib::bs_theme(version = 5)
}
