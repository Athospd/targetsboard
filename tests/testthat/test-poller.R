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
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c))"),
    "_targets.R"
  )
  touch_later("_targets.R")
  wait_for_rewrite(poller, first)
  expect_setequal(readRDS(poller$tar_vis_path)$x$nodes$id, c("a", "b", "c", "d"))

  # A refresh request from the app.
  second <- written_at(poller)
  writeLines("1", poller$refresh_path) # read by its content, not its time
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

test_that("a broken script doesn't kill the poller, which reports it until it's fixed", {
  skip_on_cran()
  local_pipeline()
  poller <- local_poller()
  before <- written_at(poller)
  error_path <- poller_error_path(poller$tar_vis_path)
  expect_false(file.exists(error_path))

  writeLines("stop('broken')", "_targets.R")
  touch_later("_targets.R")
  wait_for(function() file.exists(error_path))
  expect_match(paste(readLines(error_path), collapse = " "), "broken")
  expect_true(poller$process$is_alive())
  expect_identical(written_at(poller), before)

  writeLines(c("library(targets)", "list(tar_target(z, 1))"), "_targets.R")
  wait_for_rewrite(poller, before)
  expect_equal(readRDS(poller$tar_vis_path)$x$nodes$id, "z")
  expect_false(file.exists(error_path))
})

test_that("the notification of a broken script names it, its error, and what the board shows", {
  expect_null(script_error_text("_targets.R", NULL))
  expect_null(script_error_text("_targets.R", character()))
  text <- script_error_text("_targets.R", c("object 'x' not found", "in tar_target()"))
  expect_match(text, "^_targets.R doesn't load")
  expect_match(text, "last working graph")
  expect_match(text, "object 'x' not found in tar_target()", fixed = TRUE)
  expect_lte(nchar(script_error_text("s.R", strrep("x", 1000))), 450)
  # targets' messages come with terminal colours (#190).
  expect_no_match(script_error_text("s.R", "[31mcould not find file[39m"), "", fixed = TRUE)
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

test_that("the poller keeps the board's filters on every redraw (#148)", {
  skip_on_cran()
  local_pipeline()
  poller <- tar_visnetwork_bg(
    "_targets.R", interval = 0.2, settle = 0.5, targets_only = TRUE,
    filters = list(names = quote(any_of("b")))
  )
  withr::defer(poller$process$kill())
  wait_for(function() file.exists(poller$tar_vis_path))
  # b and what it depends on.
  expect_setequal(readRDS(poller$tar_vis_path)$x$nodes$id, c("a", "b"))

  first <- written_at(poller)
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c))"),
    "_targets.R"
  )
  touch_later("_targets.R")
  wait_for_rewrite(poller, first)
  expect_setequal(readRDS(poller$tar_vis_path)$x$nodes$id, c("a", "b"))
})

test_that("poller_why() reads the reasons and the manifest in one process, apart from the script (#161)", {
  local_pipeline(make = TRUE)
  # A script whose own names clash with poller_why()'s.
  writeLines(c(
    "library(targets)",
    "errors <- 'mine'; reasons <- 1; fresh <- 2",
    "list(tar_target(a, 1), tar_target(b, a + 2), tar_target(c, b * 2))"
  ), "_targets.R")
  found <- callr::r(poller_why, args = list(script = "_targets.R"))
  expect_equal(found$errors, character())
  expect_setequal(found$manifest$name, c("a", "b", "c"))
  expect_true(all(c("sitrep", "vertices", "edges", "parents") %in% names(found$reasons)))
  expect_equal(outdated_reasons(found$reasons)$b, c(command = "Its command changed."))

  writeLines("stop('broken')", "_targets.R")
  found <- callr::r(poller_why, args = list(script = "_targets.R"))
  expect_null(found$reasons)
  expect_null(found$manifest)
  expect_match(found$errors, "broken")
})

test_that("poller_why() reads the script as tar_make() does: a closure in a global isn't a change", {
  # A global holding a closure, as torch's nn_module() and R6 classes do:
  # read in an environment of its own, its hash changes on every read.
  local_pipeline()
  writeLines(c(
    "library(targets)",
    "helpers <- list(add = function(x) x + 1)",
    "list(tar_target(a, helpers$add(1)), tar_target(b, a + 1))"
  ), "_targets.R")
  targets::tar_make(reporter = "silent")
  found <- callr::r(poller_why, args = list(script = "_targets.R"))
  expect_equal(found$errors, character())
  outdated <- found$reasons$vertices$name[found$reasons$vertices$status %in% "outdated"]
  expect_false(any(c("helpers", "a", "b") %in% outdated))
  expect_equal(outdated_reasons(found$reasons), list())
})
