# The board's saved layout: where the author left the targets, and the view.
#
# A JSON file in the store's user folder (`_targets/user/`, which targets
# leaves to its users), so it goes with the project and survives restarts:
#
#   {"direction": "LR",
#    "viewport": {"x": 12, "y": -40, "zoom": 0.8},
#    "positions": {"raw_data": {"x": 0, "y": 0}, "model": {"x": 250, "y": 0}}}
#
# The widget reports it as `input$<inputId>_layout` and draws one given to
# targetsboard(layout = ).

board_layout_path <- function(store = targets::tar_config_get("store")) {
  file.path(store, "user", "targetsboard_layout.json")
}

# NULL when there is none, or it can't be read (with a warning: the board
# opens anyway, laid out afresh).
read_board_layout <- function(path = board_layout_path()) {
  if (!file.exists(path)) return(NULL)
  layout <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_warn(c("Could not read the board's layout at {.file {path}}.", x = "{conditionMessage(e)}"))
      NULL
    }
  )
  if (!is.null(layout) && !is_board_layout(layout)) {
    cli::cli_warn("Ignoring the board's layout at {.file {path}}: it is not one.")
    return(NULL)
  }
  layout
}

# Writes only into a store that exists: a board on a pipeline that never ran
# must not create `_targets/`.
write_board_layout <- function(layout, path = board_layout_path()) {
  if (!is_board_layout(layout)) rlang::abort("`layout` must be the widget's `<inputId>_layout` input.")
  store <- dirname(dirname(path))
  if (!dir.exists(store)) return(invisible(FALSE))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  # Whole or not at all: a page loading while the file is being written must
  # not read half of it. Written beside it, then renamed over it.
  temp <- tempfile("targetsboard_layout_", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(temp), add = TRUE)
  jsonlite::write_json(layout, temp, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  file.rename(temp, path)
  invisible(TRUE)
}

is_board_layout <- function(layout) {
  is.list(layout) && is.list(layout$positions) &&
    all(vapply(layout$positions, \(p) is.numeric(p$x) && is.numeric(p$y), logical(1)))
}
