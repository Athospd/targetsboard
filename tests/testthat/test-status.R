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
