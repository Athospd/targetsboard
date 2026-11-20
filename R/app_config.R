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
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/index.css")
  )
}
