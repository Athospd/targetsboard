# The run summary in the Control Panel (#140).

progress_of <- function(...) {
  rows <- list(...)
  data.frame(
    name = vapply(rows, `[[`, "", 1),
    type = vapply(rows, `[[`, "", 2),
    parent = vapply(rows, \(r) if (length(r) > 3) r[[4]] else r[[1]], ""),
    progress = vapply(rows, `[[`, "", 3)
  )
}

test_that("run_summary() counts stems and branches, not patterns, as tar_progress_summary()", {
  progress <- progress_of(
    c("a", "stem", "completed"),
    c("b", "stem", "skipped"),
    c("p", "pattern", "dispatched"),
    c("p_1", "branch", "completed", "p"),
    c("p_2", "branch", "dispatched", "p"),
    c("e", "stem", "errored")
  )
  started <- as.POSIXct("2026-11-25 10:00:00")
  s <- run_summary(progress, c("a", "b", "p", "e", "later", "last"), started = started,
                   running = TRUE, now = started + 42)
  expect_equal(s[c("built", "skipped", "running_targets", "errored", "canceled", "to_go")],
               list(built = 2L, skipped = 1L, running_targets = 1L, errored = 1L, canceled = 0L, to_go = 2L))
  expect_equal(s$elapsed, 42)
  expect_true(s$running)
})

test_that("a finished run's time runs to its last progress, and an unknown start gives no time", {
  progress <- progress_of(c("a", "stem", "completed"))
  started <- as.POSIXct("2026-11-25 10:00:00")
  s <- run_summary(progress, "a", started = started, ended = started + 72, running = FALSE)
  expect_equal(s$elapsed, 72)
  expect_false(s$running)
  expect_true(is.na(run_summary(progress, "a", started = NA, ended = started)$elapsed))
  # A process that started after the last progress: not this run's.
  expect_true(is.na(run_summary(progress, "a", started = started + 5, ended = started)$elapsed))
})

test_that("no progress, no summary", {
  expect_null(run_summary(NULL))
  expect_null(run_summary(progress_of()[0, ]))
})

test_that("format_elapsed() reads as a clock", {
  expect_equal(format_elapsed(42), "0:42")
  expect_equal(format_elapsed(725), "12:05")
  expect_equal(format_elapsed(3729), "1:02:09")
  expect_true(is.na(format_elapsed(NA)))
})

test_that("run_summary_ui() says Running with the clock and what's to go, then Last run", {
  progress <- progress_of(c("a", "stem", "completed"), c("e", "stem", "errored"))
  now <- as.POSIXct("2026-11-25 10:00:42")
  going <- as.character(run_summary_ui(run_summary(progress, c("a", "e", "z"), started = now - 42, running = TRUE, now = now)))
  expect_match(going, "Running")
  expect_match(going, "0:42", fixed = TRUE)
  expect_match(going, "<b>1</b>[[:space:]]*to go")
  expect_match(going, "tb-run-count-errored", fixed = TRUE)

  done <- as.character(run_summary_ui(run_summary(progress, c("a", "e"), started = now - 42, ended = now)))
  expect_match(done, "Last run")
  expect_no_match(done, "to go")
  expect_no_match(done, "canceled")
  expect_null(run_summary_ui(NULL))
})

test_that("the Control Panel shows the last run from the store, with its time", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  args <- run_args()
  args$progress(list(
    mtime = file.mtime(file.path("_targets", "meta", "progress")),
    data = targets::tar_progress(fields = c("type", "parent", "progress"))
  ))
  shiny::testServer(run_server, args = args, {
    html <- output$summary$html
    expect_match(html, "Last run")
    expect_match(html, "built")
    expect_match(html, "tb-run-elapsed")
  })
})
