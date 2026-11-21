# The background poller recomputes tar_visnetwork() only when needed.
local_poller <- function(.env = parent.frame()) {
  poller <- tar_visnetwork_bg("_targets.R", interval = 0.2, settle = 0.5, targets_only = TRUE)
  withr::defer(poller$process$kill(), envir = .env)
  wait_for(function() file.exists(poller$tar_vis_path))
  poller
}

written_at <- function(poller) file.mtime(poller$tar_vis_path)

wait_for_rewrite <- function(poller, since) {
  wait_for(function() !identical(written_at(poller), since))
}

test_that("the poller recomputes only when the code changes or on request", {
  skip_on_cran()
  local_pipeline()
  poller <- local_poller()
  expect_setequal(readRDS(poller$tar_vis_path)$x$nodes$id, c("a", "b", "c"))
  # The whole widget too, for the tar_visnetwork tab.
  expect_s3_class(readRDS(poller$tar_vis_path)$widget, "visNetwork")

  first <- written_at(poller)
  Sys.sleep(1.5)
  expect_identical(written_at(poller), first)
  expect_true(poller$process$is_alive())

  # A new target in the script.
  Sys.sleep(1.1) # file times can have 1s resolution
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c))"),
    "_targets.R"
  )
  wait_for_rewrite(poller, first)
  expect_setequal(readRDS(poller$tar_vis_path)$x$nodes$id, c("a", "b", "c", "d"))

  # A refresh request from the app.
  second <- written_at(poller)
  Sys.sleep(1.1)
  writeLines("1", poller$refresh_path)
  wait_for_rewrite(poller, second)
})

test_that("the poller recomputes once a run ends, with the progress it reflects", {
  skip_on_cran()
  local_pipeline()
  poller <- local_poller()
  expect_true(all(readRDS(poller$tar_vis_path)$x$nodes$status == "outdated"))

  before <- written_at(poller)
  targets::tar_make(reporter = "silent")
  wait_for_rewrite(poller, before)

  board <- readRDS(poller$tar_vis_path)
  expect_true(all(board$x$nodes$status == "uptodate"))
  expect_identical(board$progress_mtime, file.mtime("_targets/meta/progress"))
})

test_that("a broken script doesn't kill the poller", {
  skip_on_cran()
  local_pipeline()
  poller <- local_poller()
  before <- written_at(poller)

  Sys.sleep(1.1)
  writeLines("stop('broken')", "_targets.R")
  Sys.sleep(2)
  expect_true(poller$process$is_alive())
  expect_identical(written_at(poller), before)

  writeLines(c("library(targets)", "list(tar_target(z, 1))"), "_targets.R")
  wait_for_rewrite(poller, before)
  expect_equal(readRDS(poller$tar_vis_path)$x$nodes$id, "z")
})

test_that("a killed run still counts as ended", {
  skip_on_cran()
  local_pipeline()
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, {Sys.sleep(60); a}))"),
    "_targets.R"
  )
  poller <- local_poller()
  process <- targets::tar_make(callr_function = callr::r_bg, reporter = "silent")
  withr::defer(process$kill_tree())
  wait_for(function() any(targets::tar_progress()$progress == "dispatched"))

  before <- written_at(poller)
  process$kill_tree()
  wait_for_rewrite(poller, before)
  expect_identical(readRDS(poller$tar_vis_path)$progress_mtime, file.mtime("_targets/meta/progress"))
})

test_that("the poller draws a script that appears after it started", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  poller <- tar_visnetwork_bg("_targets.R", interval = 0.2, settle = 0.5, targets_only = TRUE)
  withr::defer(poller$process$kill())

  Sys.sleep(1.5)
  expect_true(poller$process$is_alive())
  expect_false(file.exists(poller$tar_vis_path))

  writeLines(c("library(targets)", "list(tar_target(a, 1), tar_target(b, a))"), "_targets.R")
  wait_for(function() file.exists(poller$tar_vis_path))
  expect_setequal(readRDS(poller$tar_vis_path)$x$nodes$id, c("a", "b"))
})
