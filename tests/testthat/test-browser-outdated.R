# Why a target is outdated, on the board in headless Chrome: hovering it,
# and above its value in the preview.
# Uses the installed targetsboard (as R CMD check does), like every browser
# test: install the branch to a scratch library first to test a change.

test_that("an outdated target says why, on hover and in the preview", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- local_pipeline(make = TRUE) # a, b = a + 1, c = b * 2
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))
  app <- new_app_driver(dir, name = "outdated", load_timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  title <- function(id) {
    sprintf("document.querySelector(\".react-flow__node[data-id='%s'] .base-node\").getAttribute('title')", id)
  }
  # Up to date: nothing to say.
  expect_null(app$get_js(title("c")))

  # b's command changes: b says so, c that b is upstream.
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 2), tar_target(c, b * 2))"),
    file.path(dir, "_targets.R")
  )
  app$wait_for_js(sprintf("%s === 'Outdated: Its command changed.'", title("b")), timeout = 60 * 1000)
  expect_equal(app$get_js(title("c")), "Outdated: Upstream, b is outdated.")
  expect_null(app$get_js(title("a")))

  # In the preview, above the value: the same, with b as code.
  mouse_event(app, "c", "dblclick")
  app$wait_for_js("document.querySelector('.tb-why') !== null", timeout = preview_timeout)
  expect_equal(app$get_js("document.querySelector('.tb-why-reason').innerText.trim()"), "Upstream, b is outdated.")

  # Built again: the reasons go, once the poller has computed them again
  # (slow on CI; a new graph alone drops the hover texts for a moment).
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  app$wait_for_js("document.querySelector('.tb-why') === null", timeout = 90 * 1000)
  app$wait_for_js(sprintf("%s === null", title("b")), timeout = 30 * 1000)
  # Up to date again: why a run skips it, in its place.
  app$wait_for_js("document.querySelector('.tb-skipped')?.innerText.includes('so a run skips it') === true", timeout = 30 * 1000)
  expect_match(app$get_js("document.querySelector('.tb-skipped').innerText.trim()"), "^Up to date: its command")
})
