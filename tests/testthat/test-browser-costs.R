# Build times and stored sizes (#107), in a browser.

test_that("the legend's switches show each target's time and size; the slowest is warm", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(quick, 1),",
    "  tar_target(slow, {Sys.sleep(1.5); quick}),",
    "  tar_target(big, rnorm(2e5)),",
    "  tar_target(last, slow + 1)",
    ")"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  app <- local_board_app(dir, "costs")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 4", timeout = 30000)
  badge <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s'] .tb-cost\")", id)
  shown <- function(sel) app$get_js(sprintf("(() => { const e = %s; return !!e && getComputedStyle(e).display !== 'none'; })()", sel))

  # The badges arrive with the metadata, hidden until a switch is on.
  app$wait_for_js(paste(badge("slow"), "!== null"), timeout = 20000)
  expect_false(shown(badge("slow")))

  app$click(selector = ".tb-legend-switch input[data-part='times']")
  app$wait_for_js(sprintf("getComputedStyle(%s).display !== 'none'", badge("slow")))
  expect_match(app$get_js(paste0(badge("slow"), ".innerText")), "^[0-9.]+ s$")
  expect_false(shown(paste0(badge("slow"), ".querySelector('.tb-cost-size')")))
  # The slowest of the four is warm, the others not.
  expect_true(app$get_js(paste0(badge("slow"), ".querySelector('.tb-cost-time').classList.contains('tb-cost-hot')")))
  expect_false(app$get_js(paste0(badge("quick"), ".querySelector('.tb-cost-time').classList.contains('tb-cost-hot')")))
  expect_match(app$get_js(paste0(badge("big"), ".title")), "^Built in .* · stored .*B$")

  # Sizes too: both parts; the largest is warm.
  app$click(selector = ".tb-legend-switch input[data-part='sizes']")
  app$wait_for_js(sprintf("getComputedStyle(%s.querySelector('.tb-cost-size')).display !== 'none'", badge("big")))
  expect_true(app$get_js(paste0(badge("big"), ".querySelector('.tb-cost-size').classList.contains('tb-cost-hot')")))

  # The browser remembers the switches.
  app$run_js("location.reload()")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 4", timeout = 30000)
  app$wait_for_js(paste(badge("big"), "!== null"), timeout = 20000)
  expect_true(app$get_js("document.querySelector(\".tb-legend-switch input[data-part='times']\").checked"))
  expect_true(app$get_js("document.querySelector(\".tb-legend-switch input[data-part='sizes']\").checked"))
  expect_true(shown(badge("big")))

  # The Metadata tab: slowest first, its time readable.
  app$click(selector = "#board-view .nav-link[data-value='tar_meta']")
  first_row <- "document.querySelector('#board-meta-table .rt-tbody .rt-tr')"
  app$wait_for_js(paste(first_row, "!== null"), timeout = 20000)
  expect_match(app$get_js(paste0(first_row, ".innerText")), "slow")
  expect_match(app$get_js(paste0(first_row, ".innerText")), "[0-9.]+ s")
})
