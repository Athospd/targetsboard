# Run with tar_make_future() or tar_make_clustermq() for projects set up for
# them (#162): read from the script's calls, never by running it.

maker_of <- function(lines) {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(lines, script)
  run_maker(script)
}

test_that("run_maker() is tar_make() unless the script sets up future or clustermq", {
  expect_equal(maker_of("library(targets); list(tar_target(a, 1))"), list(fun = "tar_make", workers = NULL))
  expect_equal(maker_of(c("library(future)", "plan(multisession, workers = 3)", "list()")),
               list(fun = "tar_make_future", workers = 3L))
  expect_equal(maker_of(c("future::plan(future::multisession)", "list()")),
               list(fun = "tar_make_future", workers = 1L))
  expect_equal(maker_of(c("options(clustermq.scheduler = 'multicore')", "list()")),
               list(fun = "tar_make_clustermq", workers = 1L))
  # A plan inside a function the script defines isn't the script's.
  expect_equal(maker_of(c("f <- function() future::plan(future::multisession)", "list()"))$fun, "tar_make")
  # A script that doesn't parse, or isn't there: tar_make(), which says why.
  expect_equal(maker_of("list(")$fun, "tar_make")
  expect_equal(run_maker(tempfile())$fun, "tar_make")
})

test_that("options(targetsboard.workers = ) sets the workers", {
  withr::local_options(targetsboard.workers = 4)
  expect_equal(maker_of(c("future::plan(future::multisession, workers = 2)", "list()"))$workers, 4L)
  expect_null(maker_of("list()")$workers) # tar_make() takes none
})

test_that("run_maker_label() says which, for the run's header and summary", {
  expect_null(run_maker_label(list(fun = "tar_make", workers = NULL)))
  expect_equal(run_maker_label(list(fun = "tar_make_future", workers = 1L)), "tar_make_future(), 1 worker")
  expect_equal(run_maker_label(list(fun = "tar_make_clustermq", workers = 3L)), "tar_make_clustermq(), 3 workers")
  expect_match(run_header(maker = list(fun = "tar_make_future", workers = 2L)), "all targets, with tar_make_future(), 2 workers", fixed = TRUE)
})

test_that("start_run() runs a future project with tar_make_future()", {
  skip_on_cran()
  skip_if_not_installed("future")
  local_pipeline()
  writeLines(c("library(targets)", "future::plan(future::sequential)",
               "list(tar_target(a, 1), tar_target(b, a + 1))"), "_targets.R")
  process <- start_run("_targets.R")
  process$wait(120000)
  expect_null(run_error(process))
  expect_match(attr(process, "header"), "tar_make_future(), 1 worker", fixed = TRUE)
  expect_equal(targets::tar_read(b), 2)
})

test_that("the run summary says how the board's run runs, when it isn't tar_make()", {
  progress <- data.frame(name = "a", type = "stem", parent = "a", progress = "completed")
  now <- as.POSIXct("2026-11-26 10:00:10")
  with_how <- run_summary(progress, "a", started = now - 5, ended = now, how = "tar_make_future(), 2 workers")
  expect_match(as.character(run_summary_tags(with_how)), "<div class=\"tb-run-how\">tar_make_future(), 2 workers</div>", fixed = TRUE)
  expect_equal(run_summary_shape(with_how)$how, "tar_make_future(), 2 workers") # drawn again when it changes
  expect_no_match(as.character(run_summary_tags(run_summary(progress, "a", started = now - 5, ended = now))), "tb-run-how")
})
