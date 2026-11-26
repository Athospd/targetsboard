# The poller redraws when a file the script sources changes, not only R/ (#152).

test_that("sourced_paths() reads the script's literal tar_source() and source() paths", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "library(targets)",
    "tar_source()",
    "targets::tar_source(c('functions', 'helpers'))",
    "source('utils.R')",
    "if (TRUE) base::source(file = 'more.R', local = TRUE)",
    "tar_source(files = 'lib')",
    "source(file.path('built', 'at_run_time.R'))",
    "x <- function() source('inside.R')",
    "list(tar_target(a, 1))"
  ), script)
  expect_equal(
    sourced_paths(script),
    c("R", "functions", "helpers", "utils.R", "more.R", "lib", "inside.R")
  )
  # Base R only: it runs in the poller.
  expect_identical(environment(sourced_paths), baseenv())
})

test_that("sourced_paths() gives R/ alone for a script without them, or broken", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines("list(targets::tar_target(a, 1))", script)
  expect_equal(sourced_paths(script), "R")
  writeLines("list(tar_target(a, ", script)
  expect_equal(sourced_paths(script), "R")
})

test_that("the poller redraws when a function in a sourced folder outside R/ changes", {
  skip_on_cran()
  local_pipeline()
  dir.create("functions")
  writeLines("f <- function(x) x + 1", "functions/f.R")
  writeLines(c("library(targets)", "tar_source('functions')", "list(tar_target(a, f(1)))"), "_targets.R")
  targets::tar_make(reporter = "silent")
  poller <- tar_visnetwork_bg("_targets.R", interval = 0.2, settle = 0.5, targets_only = TRUE)
  withr::defer(poller$process$kill())
  wait_for(function() file.exists(poller$tar_vis_path))
  status <- function() {
    nodes <- readRDS(poller$tar_vis_path)$x$nodes
    nodes$status[nodes$id == "a"]
  }
  expect_equal(status(), "uptodate")
  first <- file.mtime(poller$tar_vis_path)
  Sys.sleep(1.1)
  writeLines("f <- function(x) x + 2", "functions/f.R")
  wait_for(function() !identical(file.mtime(poller$tar_vis_path), first))
  expect_equal(status(), "outdated")
})
