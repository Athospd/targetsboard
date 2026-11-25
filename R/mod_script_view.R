#' The Script tab
#'
#' The script the board has loaded, as it is on disk, read only, and where
#' things are: the script, the project folder, the data store and what is in
#' it, the `_targets.yaml` in use and the active `TAR_PROJECT`. Each path
#' copies with a click. It follows Load, and the files as they change: it
#' looks again every second, and shows again only when something changed.
#'
#' @param id The module's id.
#' @noRd
script_view_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card_body(
    class = "tb-script-view",
    shiny::uiOutput(ns("paths")),
    shiny::uiOutput(ns("code"))
  )
}

#' @rdname script_view_ui
#' @param script The script the board has loaded: a reactive.
#' @return Nothing: the tab only shows.
#' @noRd
script_view_server <- function(id, script) {
  stopifnot(shiny::is.reactive(script))
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveVal(NULL)
    shiny::observe({
      shiny::invalidateLater(1000)
      new <- script_view_state(script())
      if (!identical(new, shiny::isolate(state()))) state(new)
    })

    output$paths <- shiny::renderUI({
      shiny::req(state())
      paths_table(state()$paths)
    })
    output$code <- shiny::renderUI({
      shiny::req(state())
      text <- state()$text
      if (is.null(text)) {
        return(shiny::tags$p(class = "text-muted", "The script can't be read."))
      }
      code_block(basename(state()$script), "script", text)
    })
    invisible()
  })
}

# What the tab shows, read from disk: compared with the last, so the tab
# shows again only when something changed.
script_view_state <- function(script) {
  text <- tryCatch(
    paste(readLines(script, warn = FALSE), collapse = "\n"),
    error = function(e) NULL, warning = function(w) NULL
  )
  list(script = script, text = text, paths = where_things_are(script))
}

#' Where things are: the paths the board and targets work with
#'
#' The store and the configuration as targets finds them from the board's
#' folder: `tar_config_get()` follows `_targets.yaml`, `TAR_CONFIG` and
#' `TAR_PROJECT`.
#'
#' @param script The script the board has loaded.
#' @return A data frame: `label`, `path` (absolute), `exists`, and `depth`
#'   (1 for what is inside the store).
#' @noRd
where_things_are <- function(script) {
  full <- function(path) unname(vapply(path, absolute_path, ""))
  store <- targets::tar_config_get("store")
  config <- Sys.getenv("TAR_CONFIG", "_targets.yaml")
  project <- Sys.getenv("TAR_PROJECT")
  rows <- data.frame(
    label = c(
      "Script", "Project folder", "Data store",
      "Metadata", "Progress", "Values", "Workspaces",
      "Configuration"
    ),
    path = full(c(
      script, getwd(), store,
      file.path(store, "meta", "meta"), file.path(store, "meta", "progress"),
      file.path(store, "objects"), file.path(store, "workspaces"),
      config
    )),
    depth = c(0, 0, 0, 1, 1, 1, 1, 0)
  )
  if (nzchar(project)) {
    rows <- rbind(rows, data.frame(label = "Project (TAR_PROJECT)", path = project, depth = 0))
  }
  rows$exists <- rows$label == "Project (TAR_PROJECT)" | file.exists(rows$path)
  rows
}

# The paths as a table, each whole and copied with a click, in neutral
# colours (the red copy buttons are for a target's code); one not there yet
# says so.
paths_table <- function(paths) {
  missing_note <- function(label) {
    if (label == "Configuration") "none: targets' defaults" else "not created yet"
  }
  rows <- lapply(seq_len(nrow(paths)), function(i) {
    row <- paths[i, ]
    shiny::tags$tr(
      class = if (!row$exists) "tb-path-missing",
      shiny::tags$th(scope = "row", class = if (row$depth > 0) "ps-4 fw-normal", row$label),
      shiny::tags$td(
        htmltools::tagAppendAttributes(copy_code_button(row$path), class = "tb-copy-path"),
        if (!row$exists) shiny::tags$span(class = "text-muted small ms-2", missing_note(row$label))
      )
    )
  })
  shiny::tags$table(
    class = "table table-sm align-middle tb-paths",
    shiny::tags$caption(class = "caption-top", "Where things are"),
    shiny::tags$tbody(rows)
  )
}

# A path made absolute, as the system names it, even when it doesn't exist
# yet: normalizePath() only resolves what exists (on Windows it keeps short
# names, RUNNER~1; on macOS a link, /var for /private/var; on Linux it stays
# relative). So the deepest folder that exists is resolved, and the rest
# added to it.
absolute_path <- function(path) {
  # Relative unless it starts at a root, a drive, a home (~) or a share (\\).
  if (!grepl("^([/~]|[A-Za-z]:|[\\\\])", path)) path <- file.path(getwd(), path)
  if (file.exists(path)) return(normalizePath(path, winslash = "/"))
  parent <- dirname(path)
  if (identical(parent, path)) return(path)
  file.path(absolute_path(parent), basename(path))
}
