# Small stand-ins for targets::tar_visnetwork()$x$nodes / $edges.
fake_nodes <- function(ids = c("raw", "model", "report"),
                       status = c("uptodate", "dispatched", "outdated")) {
  data.frame(
    name = ids, type = "stem", description = NA_character_, status = status,
    seconds = NA_real_, bytes = NA_real_, branches = NA_integer_,
    label = ids, color = "#78B7C5", id = ids, level = seq_along(ids),
    shape = "dot"
  )
}

fake_edges <- function(from = c("raw", "model"), to = c("model", "report")) {
  data.frame(from = from, to = to, arrows = rep("to", length(from)))
}

# Writes a tiny pipeline into a temporary directory that becomes the working
# directory until the calling test ends. Returns the directory.
local_pipeline <- function(make = FALSE, .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(
    c(
      "library(targets)",
      "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2))"
    ),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir, .local_envir = .env)
  if (make) targets::tar_make(reporter = "silent")
  dir
}

# app_server() for the pipeline in the working directory, with its shared
# poller killed when the calling test ends.
local_app_server <- function(.env = parent.frame()) {
  metadata <- targets::tar_visnetwork(targets_only = TRUE)$x
  server <- app_server(metadata, "_targets.R")
  withr::defer(environment(server)$stop_poller(), envir = .env)
  server
}

poller_of <- function(server) environment(server)$poller

wait_for <- function(condition, timeout = 30) {
  deadline <- Sys.time() + timeout
  while (!condition()) {
    if (Sys.time() > deadline) stop("Timed out waiting for condition")
    Sys.sleep(0.25)
  }
  invisible(TRUE)
}

# The real board in headless Chrome, for the pipeline in `dir`. Skips when
# Chrome or shinytest2 is missing. Uses the installed targetsboard.
local_board_app <- function(dir, name, .env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))
  app <- shinytest2::AppDriver$new(dir, name = name, load_timeout = 60 * 1000)
  withr::defer(app$stop(), envir = .env)
  app
}

# JS for the board's node of target `id`.
node_js <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s']\")", id)

# Presses a key in the page as a person would: a synthetic KeyboardEvent
# doesn't reach React Flow's key handling, a DevTools key event does.
press_key <- function(app, key, code = c(Backspace = 8, Delete = 46, Escape = 27)[[key]]) {
  chrome <- app$get_chromote_session()
  for (type in c("rawKeyDown", "keyUp")) {
    chrome$Input$dispatchKeyEvent(type = type, key = key, code = key, windowsVirtualKeyCode = code)
    Sys.sleep(0.3)
  }
}
