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

test_that("start_run() runs a clustermq project with tar_make_clustermq()", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  local_pipeline()
  # multiprocess: workers as callr processes, on every OS.
  writeLines(c("library(targets)", "options(clustermq.scheduler = 'multiprocess')",
               "list(tar_target(a, 1), tar_target(b, a + 1))"), "_targets.R")
  process <- start_run("_targets.R")
  process$wait(120000)
  expect_null(run_error(process))
  expect_match(attr(process, "header"), "tar_make_clustermq(), 1 worker", fixed = TRUE)
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

test_that("run_maker() reads scripts with empty subscripts and calls of calls (#253, #238)", {
  dir <- withr::local_tempdir()
  script <- file.path(dir, "_targets.R")
  for (code in c("x[, 1]", "x[1, ]", "f(a)(b)", "pkg::f(a)(b)", "list(tar_target(a, d[, 2:3]))")) {
    writeLines(c("library(targets)", code), script)
    expect_equal(run_maker(script), list(fun = "tar_make", workers = NULL), label = code)
  }
  writeLines(c("future::plan(future::multisession, workers = 2)", "y <- m[, 1]", "g(1)(2)"), script)
  expect_equal(run_maker(script), list(fun = "tar_make_future", workers = 2L))
})

test_that("run_maker() reads every example pipeline", {
  # The installed package's: under R CMD check, tests run beside it, without inst/ (#282).
  scripts <- list.files(system.file("examples", "pipelines", package = "targetsboard"), "[.]R$", full.names = TRUE)
  expect_gt(length(scripts), 0)
  for (script in scripts) {
    expect_no_error(run_maker(script), message = basename(script))
  }
})

test_that("start_run() falls back to tar_make() when run_maker() fails", {
  local_pipeline()
  local_mocked_bindings(run_maker = function(script) stop("cannot read"))
  local_mocked_bindings(r_bg = function(...) structure(list(), class = "fake_process"), .package = "callr")
  expect_equal(attr(start_run("_targets.R"), "maker"), list(fun = "tar_make", workers = NULL))
})

test_that("on the board, Run all builds a script with an empty subscript (#253)", {
  dir <- local_pipeline()
  writeLines(c("library(targets)", "list(tar_target(m, matrix(1:4, 2)), tar_target(col, m[, 1]))"), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "run-empty-subscript")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='col']\") !== null", timeout = 30000)
  app$click(input = "board-run-all")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='col'] .base-node.tb-status-uptodate\") !== null", timeout = 90000)
  expect_true(app$get_js("!document.querySelector('#shiny-disconnected-overlay')"))
})
