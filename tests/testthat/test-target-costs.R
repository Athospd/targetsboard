# Each target's build time and stored size (#107): target_costs(), its
# formats, and the run's time left.

meta_of <- function(name, type, seconds, bytes, children = NULL) {
  meta <- data.frame(name = name, type = type, seconds = seconds, bytes = bytes)
  meta$children <- if (is.null(children)) rep(list(NULL), length(name)) else children
  meta
}

test_that("target_costs() gives built stems and patterns their time and size, not branches", {
  meta <- meta_of(
    c("a", "p", "p_1", "never"),
    c("stem", "pattern", "branch", "stem"),
    c(42.1, 3, 1.5, NA),
    c(1.2e6, 2000, 1000, NA)
  )
  costs <- target_costs(meta)
  expect_setequal(names(costs), c("a", "p"))
  expect_equal(costs$a$time, "42 s")
  expect_equal(costs$a$size, "1.2 MB")
  expect_equal(costs$a$title, "Built in 42 s · stored 1.2 MB")
  expect_match(costs$p$title, "(all its branches)", fixed = TRUE)
  # Fewer than three: no accents.
  expect_false(costs$a$slow)
  expect_false(costs$a$large)
})

test_that("target_costs() accents the slowest and largest tenth, at least one", {
  n <- 12
  meta <- meta_of(paste0("t", 1:n), rep("stem", n), seconds = 1:n, bytes = rev(1:n) * 1000)
  costs <- target_costs(meta)
  expect_equal(names(Filter(function(x) x$slow, costs)), "t12")
  expect_equal(names(Filter(function(x) x$large, costs)), "t1")
  expect_equal(top_tenth(c(5, 0, 0, 1)), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(top_tenth(c(5, 1)), c(FALSE, FALSE)) # fewer than three: none
  expect_equal(top_tenth(c(0, 0, 0)), c(FALSE, FALSE, FALSE)) # a 0 is never marked
  expect_equal(sum(top_tenth(1:25)), 2)
})

test_that("target_costs() gives none without metadata", {
  expect_length(target_costs(NULL), 0)
  expect_length(target_costs(meta_of(character(), character(), numeric(), numeric())), 0)
  expect_length(target_costs(data.frame(name = "a")), 0)
})

test_that("format_seconds() reads well at every scale", {
  expect_equal(format_seconds(0.01), "<0.1 s")
  expect_equal(format_seconds(0.34), "0.3 s")
  expect_equal(format_seconds(4.25), "4.2 s")
  expect_equal(format_seconds(42.4), "42 s")
  expect_equal(format_seconds(185), "3m 05s")
  expect_equal(format_seconds(3725), "1h 02m")
  expect_true(is.na(format_seconds(NA)))
})

test_that("time_left() adds the last builds of what is to go and running, and a pattern's share", {
  meta <- meta_of(
    c("a", "b", "p", "new"),
    c("stem", "stem", "pattern", "stem"),
    c(10, 20, 30, NA), c(1, 1, 1, NA),
    children = list(NULL, NULL, c("p_1", "p_2", "p_3"), NULL)
  )
  progress <- data.frame(
    name = c("a", "p_1", "p_2"),
    type = c("stem", "branch", "branch"),
    parent = c("a", "p", "p"),
    progress = c("dispatched", "completed", "dispatched")
  )
  left <- time_left(progress, to_go = c("b", "new"), meta = meta)
  # a running (10) + b to go (20) + p, a third done (20); new never built.
  expect_equal(left$seconds, 50)
  expect_equal(left$unknown, 1)
  expect_null(time_left(progress, "b", NULL))
  # A first run, before any metadata: tar_meta()'s empty table is all character.
  empty <- data.frame(name = character(), type = character(), seconds = character(), children = character())
  first <- time_left(progress, "b", empty)
  expect_equal(first$seconds, 0)
  expect_equal(first$unknown, 3)
})

test_that("the run summary says the time left while running, not after", {
  meta <- meta_of(c("a", "b"), c("stem", "stem"), c(30, 60), c(1, 1))
  progress <- data.frame(name = "a", type = "stem", parent = "a", progress = "dispatched")
  now <- as.POSIXct("2026-11-26 10:00:10")
  going <- run_summary(progress, c("a", "b"), started = now - 10, running = TRUE, now = now,
                       meta = meta, outdated = c("a", "b"))
  expect_equal(going$left$seconds, 90)
  expect_match(as.character(run_summary_tags(going)), "About 1:30 left", fixed = TRUE)
  # An up-to-date target the run will skip doesn't count.
  skipping <- run_summary(progress, c("a", "b"), started = now - 10, running = TRUE, now = now,
                          meta = meta, outdated = "a")
  expect_equal(skipping$left$seconds, 30)
  done <- run_summary(progress, c("a", "b"), started = now - 10, ended = now, meta = meta, outdated = "b")
  expect_null(done$left)
  expect_no_match(as.character(run_summary_tags(done)), "left")
})

test_that("the Metadata table shows times and sizes readably, slowest first", {
  meta <- data.frame(name = c("a", "b"), seconds = c(1.5, 90), bytes = c(1500, NA))
  columns <- meta_columns(meta)
  expect_equal(columns$seconds$cell(90), "1m 30s")
  expect_equal(columns$seconds$cell(NA), "")
  expect_equal(columns$bytes$cell(1500), "1.5 kB")
  expect_equal(columns$bytes$cell(NA), "")
  expect_null(meta_columns(data.frame(name = "a")))
})

test_that("update_targetsboard_status() sends every built target's badge, or leaves them", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) sent <<- message)
  costs <- list(a = list(time = "42 s", size = "1.2 MB", title = "t", slow = TRUE, large = FALSE))
  update_targetsboard_status(session, "dag", costs = costs)
  expect_equal(sent$costs, costs)
  update_targetsboard_status(session, "dag", costs = list())
  expect_equal(as.character(jsonlite::toJSON(sent$costs, auto_unbox = TRUE)), "{}")
  update_targetsboard_status(session, "dag", c(raw = "completed"))
  expect_null(sent$costs)
})
