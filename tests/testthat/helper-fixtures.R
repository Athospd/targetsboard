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

# The real board in headless Chrome, for the pipeline in `dir`: or, with
# `app`, the lines of another app.R. Skips when Chrome or shinytest2 is
# missing. Uses the installed targetsboard.
local_board_app <- function(dir, name, app = "targetsboard::tar_board_app()", .env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")
  writeLines(app, file.path(dir, "app.R"))
  # Slow CI runners can take over a minute to start Chrome and the app.
  app <- shinytest2::AppDriver$new(dir, name = name, load_timeout = 120 * 1000)
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

# app.R lines for a bare app with just the widget, as "dag", with the
# given targetsboard() arguments: to test the widget outside the board.
widget_app <- function(args = "") {
  c(
    "library(shiny)",
    "net <- targets::tar_visnetwork(targets_only = TRUE)$x",
    sprintf(
      "ui <- fluidPage(targetsboard::targetsboard('dag', net$nodes, net$edges, height = '500px'%s))",
      if (nzchar(args)) paste0(", ", args) else ""
    ),
    "server <- function(input, output, session) {}",
    "shinyApp(ui, server)"
  )
}

# Sends a message to the widget as Shiny would, e.g. "targetsboard:options".
send_widget_message <- function(app, type, detail) {
  app$run_js(sprintf(
    "window.dispatchEvent(new CustomEvent('%s', {detail: %s}))",
    type, jsonlite::toJSON(detail, auto_unbox = TRUE)
  ))
}

# A pipeline of `n` targets in a chain, t1 to tn, in the working directory.
local_chain_pipeline <- function(n, .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  commands <- c("1", paste0("t", seq_len(n - 1)))
  writeLines(
    c(
      "library(targets)",
      "list(",
      paste0("  tar_target(t", seq_len(n), ", ", commands, ")", collapse = ",\n"),
      ")"
    ),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir, .local_envir = .env)
  dir
}

# Fires a mouse event ("dblclick", "contextmenu") on the node of target `id`,
# at its centre, as React sees a real one.
mouse_event <- function(app, id, type) {
  app$run_js(sprintf(
    "const n = %s; const r = n.getBoundingClientRect();
     n.dispatchEvent(new MouseEvent('%s', {bubbles: true, cancelable: true, button: %d,
       clientX: r.x + r.width / 2, clientY: r.y + r.height / 2}));",
    node_js(id), type, if (type == "contextmenu") 2L else 0L
  ))
}

# Closes the open modal with its Cancel button. Bootstrap ignores a hide while
# the modal is still fading in, which a test can click into: on CI's faster
# runners it did, and the modal stayed open. So click until it is gone.
dismiss_modal <- function(app, attempts = 10) {
  for (i in seq_len(attempts)) {
    app$run_js(
      "const b = document.querySelector('.modal-footer [data-bs-dismiss], .modal-footer [data-dismiss]');
       if (b) b.click();"
    )
    closed <- tryCatch({
      app$wait_for_js("document.querySelector('.modal-dialog') === null", timeout = 1000)
      TRUE
    }, error = function(e) FALSE)
    if (closed) return(invisible())
  }
  stop("The modal did not close.")
}
