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
  # The clock is an output of its own: it fills in a flush after the block.
  app$wait_for_js(paste0(summary, " !== null && ", summary, ".classList.contains('tb-run-summary-running')",
                         " && /[0-9]+:[0-9]{2}/.test(", summary, ".innerText)"),
                  timeout = 30000)
  text <- app$get_js(paste0(summary, ".innerText"))
  expect_match(text, "Running")
  expect_match(text, "to go")
  expect_match(text, "[0-9]+:[0-9]{2}")

  # Updated in place, not drawn again (#173): the block marked now is still
  # the one shown once a count and the clock have moved on.
  app$run_js(paste0(summary, ".dataset.tbMark = 'kept'"))
  clock <- app$get_js(paste0(summary, ".querySelector('.tb-run-elapsed').textContent.trim()"))
  app$wait_for_js(paste0(
    "(() => { const s = document.querySelector('.tb-run-summary-running'); if (!s) return false;",
    " return s.querySelector('.tb-run-count b').textContent.trim() !== '0'", # built, the first count
    " && s.querySelector('.tb-run-elapsed').textContent.trim() !== ", jsonlite::toJSON(clock, auto_unbox = TRUE), "; })()"
  ), timeout = 30000)
  expect_equal(app$get_js(paste0(summary, ".dataset.tbMark")), "kept")

  # Done: the run's last progress can land a poll after the run ends, so
  # wait for all three.
  app$wait_for_js(paste0(summary, " !== null && !", summary, ".classList.contains('tb-run-summary-running')",
                         " && /3[^0-9a-z]*built/.test(", summary, ".innerText)"),
                  timeout = 60000)
  text <- app$get_js(paste0(summary, ".innerText"))
  expect_match(text, "Last run")
  expect_match(text, "3[[:space:]]*built")
  expect_no_match(text, "to go")
  # A second run, right after: until targets records it, the summary showed
  # the last run's clock and counts (#183). Sampled every 100 ms from the
  # click, the clock is never ahead of the time since it, and no count is
  # the last run's "3 built" before a target could finish.
  writeLines(c(
    "library(targets)",
    "nap <- function(x) { Sys.sleep(1.5); x }",
    "list(tar_target(a, nap(1)), tar_target(b, nap(a)), tar_target(c, nap(b)))"
  ), file.path(dir, "_targets.R"))
  app$wait_for_js("!document.querySelector('#board-run-all').disabled", timeout = 30000)
  Sys.sleep(1) # the poller sees the new script
  app$run_js(paste0(
    "window.tbSamples = []; window.tbClick = Date.now();",
    "window.tbTimer = setInterval(() => { const s = document.querySelector('.tb-run-summary-running'); if (!s) return;",
    " const c = s.querySelector('.tb-run-elapsed'); const m = c && c.textContent.trim().match(/^([0-9]+):([0-9]{2})$/);",
    " window.tbSamples.push({ t: (Date.now() - window.tbClick) / 1000, clock: m ? 60 * +m[1] + +m[2] : null,",
    " built: +s.querySelector('.tb-run-count b').textContent }); }, 100);",
    "document.querySelector('#board-run-all').click();"
  ))
  Sys.sleep(4)
  app$run_js("clearInterval(window.tbTimer)")
  samples <- app$get_js("window.tbSamples")
  expect_gt(length(samples), 5)
  for (x in samples) {
    if (!is.null(x$clock)) expect_lte(x$clock, x$t + 1.5)
    if (x$t < 1.4) expect_equal(x$built, 0)
  }
})
