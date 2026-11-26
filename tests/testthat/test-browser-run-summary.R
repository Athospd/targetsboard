# The run summary in the Control Panel (#140), in a browser, on a real run.

test_that("the Control Panel counts a run as it goes, then keeps the last run", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "nap <- function(x) { Sys.sleep(2); x }",
    "list(tar_target(a, nap(1)), tar_target(b, nap(a)), tar_target(c, nap(b)))"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "run_summary")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3", timeout = 30000)
  # Never run: nothing to say.
  expect_null(app$get_js("document.querySelector('.tb-run-summary')"))

  app$click(selector = "#board-run-all")
  summary <- "document.querySelector('.tb-run-summary')"
  app$wait_for_js(paste0(summary, " !== null && ", summary, ".classList.contains('tb-run-summary-running')"),
                  timeout = 30000)
  text <- app$get_js(paste0(summary, ".innerText"))
  expect_match(text, "Running")
  expect_match(text, "to go")
  expect_match(text, "[0-9]+:[0-9]{2}")

  # Done: the run's last progress can land a poll after the run ends, so
  # wait for all three.
  app$wait_for_js(paste0(summary, " !== null && !", summary, ".classList.contains('tb-run-summary-running')",
                         " && /3[^0-9a-z]*built/.test(", summary, ".innerText)"),
                  timeout = 60000)
  text <- app$get_js(paste0(summary, ".innerText"))
  expect_match(text, "Last run")
  expect_match(text, "3[[:space:]]*built")
  expect_no_match(text, "to go")
})
