# tar_board()'s `...`: tar_visnetwork()'s filters, kept for every redraw (#148).

test_that("visnetwork_filters() keeps values, and expressions only tidyselect can read", {
  my_names <- c("a", "b")
  filters <- visnetwork_filters(names = my_names, shortcut = TRUE, exclude = starts_with("tmp"), allow = c(x, y))
  expect_equal(filters$names, c("a", "b"))
  expect_true(filters$shortcut)
  expect_equal(filters$exclude, quote(starts_with("tmp")))
  expect_equal(filters$allow, quote(c(x, y)))
  expect_length(visnetwork_filters(), 0)
})

test_that("the first render uses the filters, a tidyselect expression included", {
  skip_on_cran()
  local_pipeline()
  x <- initial_metadata("_targets.R", filters = list(names = quote(starts_with("b"))))
  expect_setequal(x$nodes$id, c("a", "b"))
  expect_setequal(initial_metadata("_targets.R")$nodes$id, c("a", "b", "c"))
})

test_that("a target name that is also a function stays a target name (#239)", {
  # raw and data are base functions: as values, tidyselect rejects them.
  filters <- visnetwork_filters(names = c(raw, data))
  expect_equal(filters$names, quote(c(raw, data)))
})

test_that("a session's variables in a filter come along as values, for the poller's process (#239)", {
  my_targets <- c("a", "b")
  n <- 2L
  filters <- visnetwork_filters(names = any_of(my_targets), degree_from = n)
  expect_equal(filters$names, rlang::expr(any_of(!!c("a", "b"))))
  expect_equal(filters$degree_from, 2L)
  prefix <- "mod"
  expect_equal(visnetwork_filters(exclude = starts_with(prefix))$exclude, quote(starts_with("mod")))
  # A target's name stays one, even when the session has a value by that name.
  a <- 1
  expect_equal(visnetwork_filters(names = c(a, b))$names, quote(c(a, b)))
  expect_equal(visnetwork_filters(names = c(a, any_of(my_targets)))$names, rlang::expr(c(a, any_of(!!c("a", "b")))))
})

test_that("the poller's process draws with a filter on a session variable (#239)", {
  skip_on_cran()
  local_pipeline()
  my_targets <- c("a", "b")
  filters <- visnetwork_filters(names = any_of(my_targets))
  ids <- callr::r(function(filters) {
    do.call(targets::tar_visnetwork, c(list(script = "_targets.R", targets_only = TRUE), filters))$x$nodes$id
  }, args = list(filters = filters))
  expect_setequal(ids, c("a", "b"))
})
