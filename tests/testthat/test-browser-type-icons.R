# An icon per kind of target (#103), in a browser.

test_that("each kind of target but a plain one shows its icon, with its name on hover", {
  skip_if_not_installed("tarchetypes")
  dir <- local_pipeline()
  writeLines(c("---", "title: r", "---", "Hi"), file.path(dir, "report.Rmd"))
  writeLines("1", file.path(dir, "data.csv"))
  writeLines(c(
    "library(targets)",
    "twice <- function(x) 2 * x",
    "offset <- 3",
    "list(",
    "  tar_target(path, 'data.csv', format = 'file'),",
    "  tar_target(x, twice(1:2) + offset),",
    "  tar_target(y, x, pattern = map(x)),",
    "  tarchetypes::tar_render(report, 'report.Rmd')",
    ")"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "type_icons", "targetsboard::tar_board_app(targets_only = FALSE)")
  icon <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s'] .tb-type-icon\")", id)
  kind_of <- function(id) app$get_js(sprintf("(() => { const i = %s; return i ? i.dataset.kind : null; })()", icon(id)))

  # Files and reports come from the manifest, after the graph.
  app$wait_for_js(sprintf("%s !== null && %s !== null", icon("report"), icon("path")), timeout = 60000)
  expect_equal(kind_of("report"), "rmarkdown")
  expect_equal(kind_of("path"), "file")
  expect_equal(kind_of("y"), "pattern")
  expect_equal(kind_of("twice"), "function")
  expect_equal(kind_of("offset"), "object")
  expect_null(kind_of("x")) # a plain target: no icon
  expect_equal(app$get_js(paste0(icon("report"), ".title")), "R Markdown report")
  expect_equal(app$get_js(paste0(icon("y"), ".title")), "Pattern: dynamic branching")
  # In the node's text colour.
  same <- sprintf("getComputedStyle(%s).color === getComputedStyle(%s.closest('.base-node')).color", icon("y"), icon("y"))
  expect_true(app$get_js(same))
})
