progress_of <- function(mtime, ...) {
  statuses <- c(...)
  list(mtime = mtime, data = data.frame(name = names(statuses), progress = unname(statuses)))
}

test_that("only progress newer than the last recompute is live", {
  t0 <- as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
  progress <- progress_of(t0 + 5, raw = "completed", model = "dispatched")

  expect_equal(live_statuses(progress, since = t0), c(raw = "completed", model = "dispatched"))
  expect_equal(live_statuses(progress, since = NA), c(raw = "completed", model = "dispatched"))
  expect_length(live_statuses(progress, since = t0 + 5), 0)
  expect_length(live_statuses(list(mtime = NA, data = NULL), since = t0), 0)
})

test_that("live statuses are put on the matching nodes", {
  nodes <- overlay_statuses(fake_nodes(), c(model = "completed", gone = "errored"))
  expect_equal(nodes$status, c("uptodate", "completed", "outdated"))
  expect_equal(overlay_statuses(fake_nodes(), character())$status, fake_nodes()$status)
})

test_that("an invalidated target that had errored shows as outdated", {
  nodes <- fake_nodes(c("raw", "forecast", "ozone"), c("uptodate", "errored", "errored"))
  meta <- data.frame(
    name = c("raw", "ozone_1", "ozone_2"), parent = c(NA, "ozone", "ozone"),
    error = c(NA, NA, "object 'Wind' not found")
  )
  # forecast's record is gone: invalidated. ozone stopped on a failed branch,
  # which has the only record: still errored.
  expect_equal(clear_invalidated_errors(nodes, meta)$status, c("uptodate", "outdated", "errored"))
  meta$error[3] <- NA # the branch rebuilt fine
  expect_equal(clear_invalidated_errors(nodes, meta)$status, c("uptodate", "outdated", "outdated"))
  expect_equal(clear_invalidated_errors(nodes, NULL), nodes) # no store yet
})

test_that("Invalidate clears an error from the graph, as targets keeps it in progress", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c(
    "library(targets)",
    "list(tar_target(x, 1), tar_target(y, stop('boom')))"
  ), "_targets.R")
  try(targets::tar_make(reporter = "silent", callr_function = NULL), silent = TRUE)
  status_of <- function() {
    nodes <- initial_metadata("_targets.R")$nodes
    nodes$status[nodes$id == "y"]
  }
  expect_equal(status_of(), "errored")
  invalidate_targets("y")
  expect_equal(targets::tar_progress(names = "y")$progress, "errored") # targets keeps it
  expect_equal(status_of(), "outdated")
})
