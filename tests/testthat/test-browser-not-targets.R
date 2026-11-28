# Functions and globals on the canvas (#305): drawn as not targets, with no
# Run or Invalidate, and outdated, with their targets, once changed. Uses
# the installed targetsboard.

test_that("a function is outlined, offers no Run, and shows outdated once changed", {
  skip_on_cran()
  dir <- local_pipeline()
  script <- file.path(dir, "_targets.R")
  writeLines(c("library(targets)", "half <- function(x) x / 2", "list(tar_target(a, half(1)))"), script)
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  app <- local_board_app(dir, "not-targets", app = "targetsboard::tar_board_app(targets_only = FALSE)")
  node <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s'] .base-node\")", id)
  app$wait_for_js(paste(node("half"), "!== null"), timeout = 30000)
  expect_true(app$get_js(paste0(node("half"), ".classList.contains('tb-not-target')")))
  expect_false(app$get_js(paste0(node("a"), ".classList.contains('tb-not-target')")))
  expect_equal(app$get_js(paste0("getComputedStyle(", node("half"), ").borderStyle")), "dashed")

  # Its toolbar: Value, no Run or Invalidate; a target's has them.
  toolbar <- "Array.from(document.querySelectorAll('.tb-node-toolbar button')).map(b => b.dataset.action)"
  mouse_event(app, "half", "click")
  app$wait_for_js("document.querySelector('.tb-node-toolbar button') !== null")
  actions <- unlist(app$get_js(toolbar))
  expect_true("value" %in% actions)
  expect_false(any(c("run", "invalidate") %in% actions))
  mouse_event(app, "a", "click")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar button[data-action='run']\") !== null")

  # Changed: it and its target are outdated.
  writeLines(c("library(targets)", "half <- function(x) x / 2 + 0", "list(tar_target(a, half(1)))"), script)
  app$wait_for_js(paste0(node("half"), ".classList.contains('tb-status-outdated')"), timeout = 60000)
  expect_true(app$get_js(paste0(node("a"), ".classList.contains('tb-status-outdated')")))
})
