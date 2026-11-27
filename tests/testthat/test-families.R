# tar_map() families on the canvas (#113).

families_script <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  script <- file.path(dir, "_targets.R")
  writeLines(c(
    "library(targets)",
    "library(tarchetypes)",
    "mapped <- tar_map(",
    "  values = list(method = c('lm', 'loess'), span = c(1, 2)),",
    "  names = method,",
    "  tar_target(fit, f(method)),",
    "  tar_target_raw('score', quote(g(fit)))",
    ")",
    "list(",
    "  tar_target(data, 1),",
    "  mapped,",
    "  tar_combine(fit_all, mapped[[1]]),",
    "  tarchetypes::tar_map(values = list(k = 1:2), tar_target(x, k))",
    ")"
  ), script)
  script
}

test_that("tar_map_templates() reads the targets tar_map() copies, and the others", {
  found <- tar_map_templates(families_script())
  expect_setequal(found$templates, c("fit", "score", "x"))
  expect_setequal(found$others, c("data", "fit_all"))
  expect_equal(tar_map_templates(tempfile()), list(templates = character(), others = character()))
})

test_that("map_families() gives each copy its template, and nothing to the others", {
  found <- list(templates = c("fit", "fit_score", "x", "y"), others = c("data", "fit_all"))
  ids <- c("data", "fit_lm", "fit_loess", "fit_score_lm", "fit_score_loess", "fit_all", "x_1", "x_2", "y_1", "fit")
  expect_equal(
    map_families(ids, found),
    c(NA, "fit", "fit", "fit_score", "fit_score", NA, "x", "x", NA, NA)
  )
  # A family of one isn't one; nor is a target the script names itself.
  expect_equal(map_families(character(), found), character())
})

test_that("the board's nodes carry their family", {
  skip_on_cran()
  skip_if_not_installed("tarchetypes")
  script <- families_script()
  withr::local_dir(dirname(script))
  x <- initial_metadata(script)
  family <- stats::setNames(x$nodes$family, x$nodes$id)
  expect_equal(unname(family[c("fit_lm", "fit_loess", "score_lm", "x_1", "data", "fit_all")]),
               c("fit", "fit", "score", "x", NA, NA))
  nodes <- dataframe_to_reactflow_node_data(x$nodes)
  expect_equal(nodes[[match("fit_lm", x$nodes$id)]]$data$family, "fit")
  expect_null(nodes[[match("data", x$nodes$id)]]$data$family)
})

test_that("on the board, a family has a box that folds it into one node and back", {
  skip_if_not_installed("tarchetypes")
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "library(tarchetypes)",
    "mapped <- tar_map(values = list(k = c('a', 'b', 'c')), tar_target(fit, paste(data, k)))",
    "list(tar_target(data, 1), mapped, tar_combine(all, mapped))"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "families")
  node <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s']\")", id)
  app$wait_for_js(sprintf("%s !== null", node("fit_c")), timeout = 30000)
  app$wait_for_js("document.querySelector('.tb-family') !== null", timeout = 20000)
  expect_equal(app$get_text(".tb-family-label"), "fit")
  # The box holds the family's three targets.
  inside <- app$get_js(paste0(
    "(() => { const box = document.querySelector('.tb-family').getBoundingClientRect();",
    " return ['fit_a', 'fit_b', 'fit_c', 'data'].map((id) => {",
    " const r = document.querySelector('.react-flow__node[data-id=\"' + id + '\"]').getBoundingClientRect();",
    " return r.left >= box.left && r.right <= box.right && r.top >= box.top && r.bottom <= box.bottom; }); })()"
  ))
  expect_equal(unlist(inside), c(TRUE, TRUE, TRUE, FALSE))

  app$click(selector = ".tb-family-toggle")
  app$wait_for_js(sprintf("%s === null", node("fit_b")), timeout = 20000)
  expect_match(app$get_js(sprintf("%s.innerText", node("fit_a"))), "fit (3)", fixed = TRUE)
  # Its edges: data into it and it into all, once each.
  edges <- "[...document.querySelectorAll('.react-flow__edge')].map((e) => e.dataset.testid).sort().join(',')"
  expect_equal(app$get_js(edges), "rf__edge-data_to_fit_a,rf__edge-fit_a_to_all")
  expect_equal(app$get_js("document.querySelector('.tb-family-toggle').getAttribute('aria-expanded')"), "false")

  app$click(selector = ".tb-family-toggle")
  app$wait_for_js(sprintf("%s !== null && %s !== null", node("fit_b"), node("fit_c")), timeout = 20000)
  expect_no_match(app$get_js(sprintf("%s.innerText", node("fit_a"))), "(3)", fixed = TRUE)
})
