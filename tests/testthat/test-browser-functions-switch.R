# The Functions and globals switch in headless Chrome (#305): on, the
# graph draws the functions the targets use; off, targets only. Uses the
# installed targetsboard.

test_that("the switch draws functions and globals, and takes them away", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writeLines(c(
    "library(targets)",
    "double_it <- function(x) 2 * x",
    "list(tar_target(data, 1:3), tar_target(doubled, double_it(data)))"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  app <- local_board_app(dir, "functions_switch")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 2", timeout = 60000)
  # The nodes come with the page: Shiny binds the switch later. A click
  # before that would be its first value, which the board ignores.
  app$wait_for_js("window.jQuery?.('#board-script-functions').data('shiny-input-binding') !== undefined", timeout = 30000)
  node <- "document.querySelector('.react-flow__node[data-id=double_it]')"
  expect_null(app$get_js(node))
  app$run_js("document.querySelector('#board-script-functions').click()")
  app$wait_for_js(paste(node, "!== null"), timeout = 60000)
  app$run_js("document.querySelector('#board-script-functions').click()")
  app$wait_for_js(paste(node, "=== null"), timeout = 60000)
  expect_equal(app$get_js("document.querySelectorAll('.react-flow__node').length"), 2)
})
