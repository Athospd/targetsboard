# The Workers tab (#137): crew's controllers, live from the progress file,
# and the last run's targets and seconds from the store's crew file.

test_that("each target goes to its controller, main, or the default", {
  manifest <- data.frame(
    name = c("x", "y", "z", "w"),
    deployment = c("worker", "worker", "main", "worker"),
    resources = c(NA, "crew (controller b)", NA, "crew")
  )
  expect_equal(
    target_controllers(manifest, c("a", "b")),
    c(x = "a", y = "b", z = "main", w = "a")
  )
})

test_that("the table counts each controller's targets now, and the last run's totals", {
  manifest <- data.frame(name = c("x", "y", "z"), deployment = c("worker", "worker", "main"),
                         resources = c(NA, "crew (controller b)", NA))
  progress <- data.frame(
    name = c("x", "y", "y_1", "y_2", "y_3", "z"),
    type = c("stem", "pattern", "branch", "branch", "branch", "stem"),
    parent = c("x", "y", "y", "y", "y", "z"),
    progress = c("completed", "dispatched", "completed", "dispatched", "errored", "dispatched")
  )
  crew <- data.frame(controller = c("a", "b"), seconds = c(1.5, 12), targets = c(1L, 3L))
  table <- workers_table(progress, manifest, c("a", "b"), crew)
  expect_equal(table$controller, c("a", "b", "main"))
  expect_equal(table$running, c(0L, 1L, 1L))
  expect_equal(table$built, c(1L, 1L, 0L))
  expect_equal(table$errored, c(0L, 1L, 0L))
  expect_equal(table$targets, c(1L, 3L, NA))
  expect_equal(table$seconds, c(1.5, 12, NA))
  # No main targets: no main row. An unnamed controller's name changes each
  # time the script loads: a lone controller takes the crew file's row.
  table <- workers_table(progress[1, ], manifest[1, ], "c0ffee", crew[1, ])
  expect_equal(table$controller, "c0ffee")
  expect_equal(table$seconds, 1.5)
  expect_null(workers_table(progress, manifest, NULL, crew))
})

test_that("the poller says which controllers the script sets", {
  skip_on_cran()
  skip_if_not_installed("crew")
  local_pipeline()
  writeLines(c(
    "library(targets)",
    "tar_option_set(controller = crew::crew_controller_group(crew::crew_controller_local(name = 'a'), crew::crew_controller_local(name = 'b')))",
    "list(tar_target(x, 1))"
  ), "_targets.R")
  expect_equal(poller_why("_targets.R")$controllers, c("a", "b"))
  writeLines(c("library(targets)", "list(tar_target(x, 1))"), "_targets.R")
  expect_null(poller_why("_targets.R")$controllers)
})

test_that("on the board: the Workers tab shows only with crew, and its table", {
  skip_if_not_installed("crew")
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "tar_option_set(controller = crew::crew_controller_local(name = 'local', workers = 2))",
    "list(tar_target(x, 1:2), tar_target(y, x, pattern = map(x)), tar_target(z, sum(y), deployment = 'main'))"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  app <- local_board_app(dir, "workers")
  tab <- "document.querySelector(\"[data-value='workers']\")"
  app$wait_for_js(sprintf("%s && %s.offsetParent !== null", tab, tab), timeout = 60000)
  app$run_js(sprintf("%s.click()", tab))
  app$wait_for_js("document.querySelector('.tb-workers .rt-tbody')?.textContent.includes('main')", timeout = 30000)
  text <- app$get_text(".tb-workers .rt-tbody")
  expect_match(text, "local", fixed = TRUE)
  expect_match(text, "main", fixed = TRUE)

  # Without crew, no tab.
  dir2 <- local_pipeline()
  app2 <- local_board_app(dir2, "no-workers")
  app2$wait_for_js("document.querySelector(\".react-flow__node\") !== null", timeout = 30000)
  Sys.sleep(3) # the poller has read the script
  expect_false(app2$get_js(sprintf("!!(%s && %s.offsetParent !== null)", tab, tab)))
})

test_that("a manifest without resources sends every target to the default", {
  manifest <- data.frame(name = c("x", "z"), deployment = c("worker", "main"))
  expect_equal(target_controllers(manifest, "a"), c(x = "a", z = "main"))
})
