# tar_board_app(names = ) keeps its subset when the graph is redrawn (#148).

test_that("a board opened on part of the graph keeps it after the script changes", {
  dir <- local_pipeline()
  app <- local_board_app(dir, "filters", 'targetsboard::tar_board_app(names = starts_with("b"))')
  ids <- "Array.from(document.querySelectorAll('.react-flow__node')).map(e => e.dataset.id).sort().join()"
  app$wait_for_js(sprintf("%s === 'a,b'", ids), timeout = 30000)

  # A new target downstream: the poller redraws, still b and what it needs.
  Sys.sleep(1.1)
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(bb, b))"),
    file.path(dir, "_targets.R")
  )
  app$wait_for_js(sprintf("%s === 'a,b,bb'", ids), timeout = 30000)
  expect_equal(app$get_js(ids), "a,b,bb")
})
