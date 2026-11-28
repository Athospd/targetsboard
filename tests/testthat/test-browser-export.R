# The Export button in headless Chrome (#309): the dialog, a real build in
# a background process, then the page to save. Uses the installed
# targetsboard.

test_that("Export opens its dialog, builds the page, and offers it to save", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writeLines(c(
    "library(targets)",
    "list(tar_target(data, data.frame(x = 1:3)), tar_target(total, sum(data$x)))"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  targets::tar_make(reporter = "silent")
  app <- local_board_app(dir, "export")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 0", timeout = 60000)
  app$click(selector = "[id$='export-open']")
  app$wait_for_js("document.querySelector('.modal [id$=export-summary]')?.textContent.includes('2 targets')", timeout = 30000)
  text <- app$get_js("document.querySelector('.modal').textContent")
  expect_match(text, "The page holds your data", fixed = TRUE)
  expect_match(text, 'targetsboard::tar_board_export("', fixed = TRUE)
  app$click(selector = "[id$='export-build']")
  app$wait_for_js("document.querySelector('.modal a[id$=export-save]') !== null", timeout = 120000)
  # Shiny gives the link its URL once the download binds: wait for it.
  app$wait_for_js("document.querySelector('.modal a[id$=export-save]').href.includes('export-save')", timeout = 30000)
  expect_match(app$get_js("document.querySelector('.modal a[id$=export-save]').href"), "export-save", fixed = TRUE)
})
