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
