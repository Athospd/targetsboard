# A nested data frame's cells in headless Chrome (#298): a click opens a
# cell's value in a dialog, cell within cell, and Back goes up a level.
# Uses the installed targetsboard.

test_that("a list cell opens its value, and a cell in it opens the next level", {
  skip_on_cran()
  skip_if_not_installed("pillar")
  dir <- withr::local_tempdir()
  writeLines(c(
    "library(targets)",
    "list(tar_target(fits, {",
    "  x <- data.frame(cyl = c(4, 6, 8))",
    "  x$data <- lapply(c(4, 6, 8), function(n) {",
    "    d <- mtcars[mtcars$cyl == n, 1:3]",
    "    d$parts <- lapply(seq_len(nrow(d)), function(i) data.frame(part = letters[seq_len(i)]))",
    "    d",
    "  })",
    "  x$model <- lapply(x$data, function(d) lm(mpg ~ disp, d))",
    "  x",
    "}))"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  targets::tar_make(reporter = "silent")
  app <- local_board_app(dir, "cells")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 0", timeout = 60000)
  app$click(selector = ".react-flow__node[data-id='fits']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js("document.querySelector('.tb-preview .tb-cell-open') !== null", timeout = preview_timeout)
  # The Rows tab: the cells' summaries, as tibble prints them.
  cells <- app$get_js("Array.from(document.querySelectorAll('.tb-preview .tb-cell-open')).map(b => b.textContent)")
  expect_true(any(grepl("^<df \\[7 .* 4\\]>$", unlist(cells))))
  expect_true("<lm>" %in% unlist(cells))

  # data[[2]]: its own Rows, in a dialog, where it is as R code.
  cell_js <- function(scope, column, row) {
    sprintf("document.querySelector(`%s [data-tb-cell='{\"column\":\"%s\",\"row\":%d}']`).click()", scope, column, row)
  }
  app$run_js(cell_js(".tb-preview", "data", 2L))
  app$wait_for_js("document.querySelector('.modal .tb-cell-dialog') !== null", timeout = preview_timeout)
  title <- function() app$get_js("document.querySelector('.modal-title code').textContent")
  expect_equal(title(), "fits$data[[2]]")
  expect_null(app$get_js("document.querySelector('.modal #shiny-modal-wrapper [id$=cell_back], .modal [id$=cell_back]')"))

  # A cell inside it: the next level, and Back returns.
  app$run_js(cell_js(".modal .tb-cell-dialog", "parts", 1L))
  app$wait_for_js("document.querySelector('.modal-title code')?.textContent === 'fits$data[[2]]$parts[[1]]'", timeout = preview_timeout)
  app$run_js("document.querySelector('.modal [id$=cell_back]').click()")
  app$wait_for_js("document.querySelector('.modal-title code')?.textContent === 'fits$data[[2]]'", timeout = preview_timeout)

  # A model's cell: the model's own views.
  # Close: clicked until the dialog goes. Bootstrap ignores it while the
  # dialog is still in its transition, slow on CI.
  app$wait_for_js(paste(
    "(() => { const b = document.querySelector('.modal button[data-dismiss], .modal button[data-bs-dismiss]');",
    "if (b) b.click(); return document.querySelector('.modal .tb-cell-dialog') === null; })()"
  ), timeout = preview_timeout)
  app$run_js("Array.from(document.querySelectorAll('.tb-preview .tb-cell-open')).find(b => b.textContent === '<lm>').click()")
  app$wait_for_js("document.querySelector('.modal-title code')?.textContent === 'fits$model[[1]]'", timeout = preview_timeout)
  tabs <- app$get_js("Array.from(document.querySelectorAll('.modal .nav-link')).map(t => t.textContent.trim())")
  expect_true("Printed" %in% unlist(tabs))
})
