#' A notice over the board when there is no targets script yet (#190)
#'
#' The board waits for the script, as `targets::tar_watch()` does, and draws
#' it once it exists. It writes nothing: the notice links the targets manual,
#' whose walkthrough starts a pipeline with `targets::use_targets()`.
#' @param script Path to the script the board waits for.
#' @noRd
no_script_ui <- function(script) {
  shiny::tags$div(
    class = "tb-no-script alert alert-secondary", role = "status",
    shiny::tags$p(
      shiny::tags$strong("No targets pipeline file detected"), " at ",
      shiny::tags$code(script, .noWS = "after"), ". The board draws the pipeline as soon as it exists."
    ),
    shiny::tags$p(
      "To start one, see the ",
      shiny::tags$a(
        href = "https://books.ropensci.org/targets/walkthrough.html", target = "_blank",
        rel = "noopener", "targets manual's walkthrough", .noWS = "after"
      ),
      ", or run ", shiny::tags$code("targets::use_targets()", .noWS = "outside"), " in the project's folder."
    )
  )
}

#' The notice's output: shown while the loaded script doesn't exist
#'
#' Checked every second, but drawn again only when the script comes or goes.
#' @param loaded_script Reactive: the script the board draws.
#' @noRd
no_script_server <- function(id, loaded_script) {
  stopifnot(shiny::is.reactive(loaded_script))
  shiny::moduleServer(id, function(input, output, session) {
    missing <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() !file.exists(loaded_script() %||% ""),
      valueFunc = function() !file.exists(loaded_script() %||% "")
    )
    output$notice <- shiny::renderUI({
      if (missing()) no_script_ui(normalizePath(loaded_script(), winslash = "/", mustWork = FALSE))
    })
  })
}

no_script_output <- function(id) shiny::uiOutput(shiny::NS(id, "notice"))
