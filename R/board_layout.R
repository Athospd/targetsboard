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
  write_user_json(layout, path)
}

# A JSON file in the store's user folder, only into a store that exists.
# Whole or not at all: a page loading while the file is being written must
# not read half of it. Written beside it, then renamed over it.
write_user_json <- function(x, path) {
  store <- dirname(dirname(path))
  if (!dir.exists(store)) return(invisible(FALSE))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  temp <- tempfile("targetsboard_", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(temp), add = TRUE)
  jsonlite::write_json(x, temp, auto_unbox = TRUE, digits = NA, pretty = TRUE)
  file.rename(temp, path)
  invisible(TRUE)
}

# The board's settings per script, beside its layout (#305):
#
#   {"functions": {"_targets.R": true}}
#
# whether the board drew the script with its functions and globals when
# last left. A script is keyed by its path from the working directory, so
# the store's folder can move with the project.
board_settings_path <- function(store = targets::tar_config_get("store")) {
  file.path(store, "user", "targetsboard_settings.json")
}

script_key <- function(script) {
  path <- normalizePath(script, winslash = "/", mustWork = FALSE)
  wd <- paste0(normalizePath(getwd(), winslash = "/"), "/")
  if (startsWith(path, wd)) substring(path, nchar(wd) + 1) else path
}

read_board_settings <- function(path = board_settings_path()) {
  if (!file.exists(path)) return(list())
  settings <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  if (is.list(settings)) settings else list()
}

# TRUE or FALSE as the switch was left for `script`, or NULL.
read_functions_shown <- function(script, path = board_settings_path()) {
  shown <- read_board_settings(path)$functions[[script_key(script)]]
  if (is.logical(shown) && length(shown) == 1 && !is.na(shown)) shown
}

write_functions_shown <- function(script, shown, path = board_settings_path()) {
  settings <- read_board_settings(path)
  settings$functions <- as.list(settings$functions)
  settings$functions[[script_key(script)]] <- isTRUE(shown)
  write_user_json(settings, path)
}

is_board_layout <- function(layout) {
  is.list(layout) && is.list(layout$positions) &&
    all(vapply(layout$positions, \(p) is.numeric(p$x) && is.numeric(p$y), logical(1)))
}
