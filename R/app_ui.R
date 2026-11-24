#' The board's UI
#'
#' The top bar (chrome_ui()) holds only the title, the dark mode switch and
#' the power button, which turns the board off. The sidebar holds the board's
#' controls, and the page its card of tabs (board_ui()).
#'
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @param layout The board's saved layout, from `read_board_layout()`, or NULL.
#' @import shiny bslib
#' @noRd
app_ui <- function(metadata, script = targets::tar_config_get("script"), layout = NULL,
                   scripts = NULL) {
  shiny::tagList(
    add_external_resources(),
    page_sidebar(
      title = chrome_ui("chrome"),
      # Pinned: the board's CSS is written for Bootstrap 5, whatever bslib's
      # default becomes.
      theme = bslib::bs_theme(version = 5),
      fillable = TRUE,
      sidebar = sidebar(
        title = "Control Panel",
        board_sidebar_ui("board", script, scripts)
      ),
      board_ui("board", metadata, layout)
    )
  )
}

# How the board's runs differ from tar_make(): off on every start, so a mode
# left on doesn't surprise the next session. They apply to every run the
# board starts. Without the internals they rely on (run_modes_supported()),
# they are disabled, and say what to add to the script instead.
run_mode_switches <- function(supported = run_modes_supported(), ns = identity) {
  switches <- list(
    keep_going = list(
      label = "Keep going after errors",
      help = "Build the other targets after one errors.",
      line = 'tar_option_set(error = "continue")'
    ),
    stop_at_warnings = list(
      label = "Stop at warnings",
      help = "A target's first warning becomes an error, with a workspace and a traceback.",
      line = NULL
    )
  )
  tags$div(
    class = "tb-run-modes",
    lapply(names(switches), function(id) {
      mode <- switches[[id]]
      title <- if (supported) {
        mode$help
      } else if (!is.null(mode$line)) {
        paste0("Not available with this version of targets: add ", mode$line, " to the script instead.")
      } else {
        "Not available with this version of targets."
      }
      input <- bslib::input_switch(ns(id), mode$label, value = FALSE)
      if (!supported) {
        input <- htmltools::tagQuery(input)$find("input")$addAttrs(disabled = NA)$allTags()
      }
      tags$div(title = title, input)
    })
  )
}

#' The Script field: a dropdown of the launch script and `scripts`, filtered
#' as one types. Only those can be chosen: no path of one's own.
#' @noRd
script_input <- function(script, scripts = NULL, id = "script") {
  shiny::selectizeInput(
    id, "Script",
    choices = script_choices(script, scripts), selected = script,
    options = list(placeholder = "Choose a targets script")
  )
}

#' The scripts the Script field offers: the launch script first, then the
#' others, each once.
#' @noRd
script_choices <- function(script, scripts = NULL) {
  if (!is.null(scripts) && !is.character(scripts)) {
    stop("`scripts` must be a character vector of paths to targets scripts.", call. = FALSE)
  }
  unique(c(script, scripts))
}
