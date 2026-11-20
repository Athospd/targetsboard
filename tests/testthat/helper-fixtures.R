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
  if (make) targets::tar_make(reporter = "silent", callr_function = NULL)
  dir
}
