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
