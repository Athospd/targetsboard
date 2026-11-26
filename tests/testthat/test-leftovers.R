# Stored targets the script no longer has (#112).

test_that("leftover_targets() finds stored targets the script lost, with their branches' bytes", {
  meta <- data.frame(
    name = c("a", "old", "p", "p_1", "p_2", "gone", "gone_1"),
    type = c("stem", "stem", "pattern", "branch", "branch", "pattern", "branch"),
    parent = c(NA, NA, NA, "p", "p", NA, "gone"),
    bytes = c(10, 5, 0, 1, 1, 0, 700)
  )
  left <- leftover_targets(meta, c("a", "p"))
  expect_equal(left, data.frame(name = c("gone", "old"), bytes = c(700, 5)))
  expect_equal(nrow(leftover_targets(meta, c("a", "p", "old", "gone"))), 0)
  expect_equal(nrow(leftover_targets(meta, NULL)), 0) # the script's targets unknown yet
  expect_equal(nrow(leftover_targets(NULL, "a")), 0)
})

test_that("leftovers_ui() names them, their size, and the code that prunes them", {
  html <- as.character(leftovers_ui(data.frame(name = c("gone", "old"), bytes = c(700, 5))))
  expect_match(html, "2 stored targets are no longer in the script, 705 B: gone and old.", fixed = TRUE)
  expect_match(html, "data-copy=\"targets::tar_prune()\"", fixed = TRUE)
  expect_match(as.character(leftovers_ui(data.frame(name = "old", bytes = 5))), "1 stored target is no longer", fixed = TRUE)
  expect_null(leftovers_ui(data.frame(name = character(), bytes = numeric())))
})

test_that("the Metadata tab tells of a target removed from the script", {
  dir <- local_pipeline(make = TRUE) # a, b, c
  app <- local_board_app(dir, "leftovers")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='c']\") !== null", timeout = 30000)
  app$click(selector = "a[data-value='tar_meta']")
  app$wait_for_js("document.querySelector('.rt-table') !== null", timeout = 30000)
  expect_null(app$get_js("document.querySelector('.tb-leftovers')"))
  writeLines(c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 1))"), file.path(dir, "_targets.R"))
  app$wait_for_js("document.querySelector('.tb-leftovers') !== null", timeout = 60000)
  expect_match(app$get_js("document.querySelector('.tb-leftovers').innerText"), "1 stored target is no longer in the script, .*: c[.]")
})
