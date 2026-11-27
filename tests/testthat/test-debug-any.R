# Reproduce and debug any target from its Value pane, not only failing ones (#150).

test_that("debug_ui() gives Reproduce and Debug run, closed at first", {
  html <- as.character(debug_ui("b", command = "f(a)", inputs = "a", seed = 123L))
  expect_match(html, "<details class=\"tb-command tb-debug\">", fixed = TRUE)
  expect_match(html, "<summary>Debug</summary>", fixed = TRUE)
  expect_match(html, "targets::tar_load(c(a)) # its inputs", fixed = TRUE)
  expect_match(html, "set.seed(123) # its seed", fixed = TRUE)
  expect_match(html, "names = &quot;b&quot;", fixed = TRUE) # the debug run
  # Its workspace, when it has one.
  expect_match(as.character(debug_ui("b", command = "f(a)", workspace = TRUE)), "targets::tar_workspace(b)", fixed = TRUE)
})

test_that("debug_ui() gives a branch without a workspace its debug run alone, on its pattern", {
  html <- as.character(debug_ui("y_1a2b", run = "y", command = "f(x)", inputs = "x", branch = TRUE))
  expect_no_match(html, "Reproduce")
  expect_match(html, "names = &quot;y&quot;", fixed = TRUE)
  expect_match(html, "debug_run_script(&quot;y_1a2b&quot;)", fixed = TRUE)
  expect_match(as.character(debug_ui("y_1a2b", run = "y", branch = TRUE, workspace = TRUE)), "Reproduce")
  expect_match(as.character(debug_ui("b", running = TRUE)), "not while a run is going")
})

test_that("the Value pane of any target can reproduce and debug it; a failed one keeps its Problems part", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "tar_option_set(error = 'continue')", # b is built whatever runs first
    "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(bad, stop('boom')))"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, try(targets::tar_make(reporter = "silent"), silent = TRUE))
  app <- local_board_app(dir, "debug-any")
  open_value <- function(id) {
    app$wait_for_js(sprintf("document.querySelector(\".react-flow__node[data-id='%s']\") !== null", id), timeout = 30000)
    app$click(selector = sprintf(".react-flow__node[data-id='%s']", id))
    app$wait_for_js(".tb-node-toolbar [data-action='value']" |> sprintf(fmt = "document.querySelector(\"%s\") !== null"))
    app$click(selector = ".tb-node-toolbar [data-action='value']")
  }
  open_value("b")
  # The command comes with the poller's manifest.
  app$wait_for_js("(document.querySelector('.tb-debug .tb-code-reproduce .tb-copy') || {dataset: {}}).dataset.copy?.endsWith('a + 1')",
                  timeout = 20000)
  expect_false(app$get_js("document.querySelector('.tb-debug').open"))
  code <- app$get_js("document.querySelector('.tb-debug .tb-code-reproduce .tb-copy').dataset.copy")
  expect_match(code, "targets::tar_load(c(a)) # its inputs", fixed = TRUE)
  expect_true(endsWith(code, "a + 1"))
  expect_match(app$get_js("document.querySelector('.tb-debug .tb-code-debug_run .tb-copy').dataset.copy"),
               "names = \"b\"", fixed = TRUE)

  open_value("bad")
  app$wait_for_js("document.querySelector('.tb-problem') !== null", timeout = 20000)
  expect_null(app$get_js("document.querySelector('.tb-debug')"))
})

test_that("Debug is offered for targets, not for the functions and objects shown with them (#243)", {
  nodes <- data.frame(id = c("t", "f", "cfg"), type = c("stem", "function", "object"))
  expect_true(debuggable(nodes, "t"))
  expect_false(debuggable(nodes, "f"))
  expect_false(debuggable(nodes, "cfg"))
  expect_true(debuggable(data.frame(id = "t"), "t")) # targets_only: no type column
})
