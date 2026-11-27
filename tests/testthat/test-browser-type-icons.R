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

test_that("batched reps, URLs, grouped data and storage formats show their icons (#157)", {
  skip_if_not_installed("tarchetypes")
  dir <- local_pipeline()
  writeLines(c(
    "library(targets); library(tarchetypes)",
    "list(",
    "  tar_rep(r1, data.frame(x = 1), batches = 2, reps = 2),",
    "  tar_url(u, 'https://example.com'),",
    "  tar_group_by(g, data.frame(k = c(1, 2), v = 3:4), k),",
    "  tar_target(pq, data.frame(a = 1), format = 'parquet'),",
    "  tar_target(packed, 1, format = tar_format(read = function(path) readRDS(path),",
    "    write = function(object, path) saveRDS(object, path)))",
    ")"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "more_kinds")
  icon <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s'] .tb-type-icon\")", id)
  kind_of <- function(id) app$get_js(sprintf("(() => { const i = %s; return i ? i.dataset.kind : null; })()", icon(id)))
  app$wait_for_js(sprintf("%s !== null && %s !== null", icon("u"), icon("pq")), timeout = 60000)
  expect_equal(kind_of("r1"), "rep")
  expect_equal(kind_of("u"), "url")
  expect_equal(kind_of("g"), "group")
  expect_equal(kind_of("pq"), "format")
  expect_equal(app$get_js(paste0(icon("pq"), ".title")), "Stored as parquet")
  expect_equal(app$get_js(paste0(icon("r1"), ".title")), "Batched replications (tar_rep())")
  # A custom format is named as one, never by the code targets encodes in it.
  expect_equal(kind_of("packed"), "format")
  expect_equal(app$get_js(paste0(icon("packed"), ".title")), "Custom storage format (tar_format())")
  # The Value pane says what the kind means.
  mouse_event(app, "u", "dblclick")
  app$wait_for_js("document.querySelector('.tb-kind-hint') !== null", timeout = 20000)
  expect_match(app$get_text(".tb-kind-hint"), "ETag", fixed = TRUE)
})
