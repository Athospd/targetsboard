#' The Script tab
#'
#' The script the board has loaded, as it is on disk, read only, and where
#' things are: the script, the project folder, the data store and what is in
#' it, the `_targets.yaml` in use and, when the board shows projects, the
#' line that puts your console on the board's (`Sys.setenv(TAR_PROJECT = )`).
#' Each copies with a click. It follows Load, and the files as they change: it
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
    state <- watch_changes(function() script_view_state(script()))

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
  # A pipeline written as a document shows the document (#136).
  shown <- tangle_document(script) %||% script
  text <- tryCatch(
    paste(readLines(shown, warn = FALSE), collapse = "\n"),
    error = function(e) NULL, warning = function(w) NULL
  )
  list(script = shown, text = text, paths = where_things_are(script))
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
  rows <- data.frame(
    label = c(
      "Script", "Folder", "Data store",
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
  rows$exists <- file.exists(rows$path)
  # The board's project, as the line for the author's console (projects.R).
  if (shows_projects()) {
    rows <- rbind(rows, data.frame(
      label = "Project", path = project_env_code(active_project()), depth = 0, exists = TRUE
    ))
  }
  rows
}

# The paths as a table: each the value preview's copy button, cut short
# with an ellipsis when narrow, whole on hover and when copied; one not
# there yet says so.
paths_table <- function(paths) {
  missing_note <- function(label) {
    if (label == "Configuration") "none: targets' defaults" else "not created yet"
  }
  rows <- lapply(seq_len(nrow(paths)), function(i) {
    row <- paths[i, ]
    shiny::tags$tr(
      shiny::tags$th(scope = "row", class = if (row$depth > 0) "ps-4 fw-normal", row$label),
      shiny::tags$td(shiny::tags$div(
        class = "tb-path",
        copy_code_button(row$path),
        if (!row$exists) shiny::tags$span(class = "tb-path-note text-muted small", missing_note(row$label))
      ))
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
