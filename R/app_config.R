#' Access files in the current app
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "targetsboard")
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
add_external_resources <- function() {
  shiny::addResourcePath("www", app_sys("app/www"))

  shiny::tags$head(
    shiny::tags$title("Targets board"),
    # The brand's mark on an ink tile (made on the media branch).
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    shiny::tags$link(rel = "icon", type = "image/png", href = "www/favicon.png"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/index.css"),
    # Colours R code (srcjs/highlight.js): highlight.js, BSD-3, see
    # www/highlight.js.LICENSE.
    shiny::tags$script(src = "www/highlight.js", defer = NA),
    # What the board's buttons and links do (srcjs/actions.js).
    shiny::tags$script(src = "www/actions.js", defer = NA),
    # PDFs where the browser has no PDF viewer (srcjs/preview.js).
    shiny::tags$script(src = "www/preview.js", defer = NA)
  )
}

# Whether the board shows what only helps develop it: the Internals tab, with
# the app's own inputs. Set with `options(targetsboard.dev = TRUE)`;
# tar_board() hands the option to its background process.
board_dev_mode <- function() isTRUE(getOption("targetsboard.dev"))
