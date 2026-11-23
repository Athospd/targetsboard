#' The board's UI
#'
#' The top bar holds only the title, the dark mode switch and the power
#' button, which turns the board off. The page is one card of tabs: Board, the board's own graph;
#' Problems, what errored or warned, with a count in its title; Log, the
#' output of the board's latest run; tar_visnetwork(), the graph as that
#' function draws it; and tar_meta().
#' With `options(targetsboard.dev = TRUE)`, an Internals tab shows the app's
#' own inputs, to develop the board. Each tab's `value` is its snake_case id.
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
      title = tags$div(
        class = "d-flex justify-content-between align-items-center w-100",
        "Targets board",
        tags$div(
          class = "d-flex align-items-center gap-2",
          input_dark_mode(id = "dark_mode", mode = "light"),
          actionButton(
            "turn_off", NULL,
            icon = icon("power-off"), title = "Turn the board off", class = "btn-sm"
          )
        )
      ),
      fillable = TRUE,
      sidebar = sidebar(
        title = "Control Panel",
        script_input(script, scripts),
        # Busy, with a spinner, until the script's graph arrives: loading a
        # pipeline can take a while.
        bslib::input_task_button(
          "load_script", "Load", icon = icon("folder-open"), label_busy = "Loading...",
          type = "default", auto_reset = FALSE
        ),
        actionButton("refresh", "Refresh", icon = icon("rotate")),
        tags$hr(),
        actionButton("run_all", "Run all", icon = icon("play")),
        run_mode_switches(),
        actionButton("stop", "Stop", icon = icon("stop"), disabled = TRUE),
        actionButton("invalidate_all", "Invalidate all", icon = icon("eraser"), disabled = TRUE)
      ),
      navset_card_underline(
        id = "view",
        full_screen = TRUE,
        nav_panel(
          "Board", value = "board",
          # The value preview belongs to the board's own graph: it follows
          # the focused target. While Load loads another script, a spinner
          # over both keeps the old graph from being used. One unpadded body
          # holds them, so the board fills the card as before.
          card_body(
            class = "tb-board-pane", padding = 0, gap = 0,
            tags$div(
              class = "tb-board-loading",
              # Styled in index.css, from the theme's colours (custom.css).
              shinycssloaders::withSpinner(
                uiOutput("board_loading"),
                type = 8, custom.css = TRUE, hide.ui = FALSE, id = "board_loading_spinner",
                caption = textOutput("board_loading_caption", inline = TRUE)
              )
            ),
            layout_sidebar(
              class = "p-0",
              sidebar = sidebar(
                id = "value_preview_sidebar",
                width = "40%",
                open = FALSE,
                position = "right",
                uiOutput("value_preview")
              ),
              targetsboard(
                "flow", metadata$nodes, metadata$edges,
                options = board_options(), layout = layout
              )
            )
          )
        ),
        nav_panel(
          tags$span("Problems", uiOutput("problems_count", inline = TRUE)), value = "problems",
          reactable::reactableOutput("problems")
        ),
        nav_panel(
          "Log", value = "log",
          tags$div(class = "tb-run-log", verbatimTextOutput("run_log"))
        ),
        nav_panel(
          "tar_visnetwork()", value = "tar_visnetwork",
          visNetwork::visNetworkOutput("vis_network", height = "100%")
        ),
        nav_panel(
          "tar_meta()", value = "tar_meta",
          reactable::reactableOutput("meta")
        ),
        if (board_dev_mode()) {
          nav_panel(
            "Internals", value = "internals",
            verbatimTextOutput("internals")
          )
        }
      )
    )
  )
}

# How the board's runs differ from tar_make(): off on every start, so a mode
# left on doesn't surprise the next session. They apply to every run the
# board starts. Without the internals they rely on (run_modes_supported()),
# they are disabled, and say what to add to the script instead.
run_mode_switches <- function(supported = run_modes_supported()) {
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
      input <- bslib::input_switch(id, mode$label, value = FALSE)
      if (!supported) {
        input <- htmltools::tagQuery(input)$find("input")$addAttrs(disabled = NA)$allTags()
      }
      tags$div(title = title, input)
    })
  )
}

#' The Script field: a text field with a dropdown of `scripts`, filtered as
#' one types. Any other path can still be typed (`create`), and is kept when
#' the field loses focus, so that Load gets it.
#' @noRd
script_input <- function(script, scripts = NULL) {
  shiny::selectizeInput(
    "script", "Script",
    choices = script_choices(script, scripts), selected = script,
    options = list(
      create = TRUE, createOnBlur = TRUE, persist = FALSE,
      placeholder = "Path to a targets script"
    )
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
