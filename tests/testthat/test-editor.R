# Open a target's definition in the editor (#189).

local_definitions <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "R"))
  writeLines(c(
    "library(targets)",
    "tar_source()",
    "list(",
    "  tar_target(raw, read_data()),",
    "  tar_target(",
    "    clean,",
    "    tidy(raw)",
    "  ),",
    "  tarchetypes::tar_map(values = list(k = 1:2), tar_target(model, fit(clean, k))),",
    "  tar_target(name = 'quoted', 1)",
    ")"
  ), file.path(dir, "_targets.R"))
  writeLines(c("# helpers", "", "tidy <- function(x) {", "  x", "}", "read_data = function() 1"), file.path(dir, "R", "helpers.R"))
  dir
}

test_that("definition_location() finds a target's tar_*() call, and a function's definition", {
  dir <- local_definitions()
  script <- file.path(dir, "_targets.R")
  where <- function(name) definition_location(name, script)
  expect_equal(where("raw"), list(file = normalizePath(script, winslash = "/"), line = 4L))
  expect_equal(where("clean")$line, 5L) # the call, not the name's line
  expect_equal(where("quoted")$line, 10L)
  expect_equal(where("tidy"), list(file = normalizePath(file.path(dir, "R", "helpers.R"), winslash = "/"), line = 3L))
  expect_equal(where("read_data")$line, 6L)
})

test_that("definition_location() gives a tar_map() copy its template's line, and NULL when it can't tell", {
  dir <- local_definitions()
  script <- file.path(dir, "_targets.R")
  expect_equal(definition_location("model_1", script)$line, 9L)
  expect_null(definition_location("nowhere", script))
  expect_null(definition_location("raw", file.path(dir, "missing.R")))
  writeLines("list(tar_target(broken", file.path(dir, "bad.R"))
  expect_null(definition_location("broken", file.path(dir, "bad.R")))
})

test_that("the editor bridge opens what the board asks, in the session that has the editor", {
  skip_if_not_installed("later")
  requests <- tempfile()
  opened <- NULL
  stop_bridge <- editor_bridge(requests, open = function(file, line) opened <<- list(file, line), alive = function() TRUE)
  withr::defer(stop_bridge())
  request_editor(requests, "a.R", 3L)
  later::run_now(2) # the loop's next turn
  for (i in 1:20) if (is.null(opened)) later::run_now(0.5)
  expect_equal(opened, list("a.R", 3L))
  expect_false(file.exists(requests)) # taken
})

test_that("Open in editor is in the target menu only with an editor", {
  withr::local_options(targetsboard.editor = NULL)
  withr::local_envvar(TERM_PROGRAM = NA, POSITRON = NA) # no IDE in this test's session
  expect_false("open_editor" %in% vapply(board_options()$node_actions, `[[`, "", "id"))
  withr::local_options(targetsboard.editor = tempfile())
  expect_true("open_editor" %in% vapply(board_options()$node_actions, `[[`, "", "id"))
})

test_that("Open in editor, in the browser: the board asks the bridge for the target's line", {
  dir <- local_pipeline()
  writeLines(c("library(targets)", "list(", "  tar_target(a, 1),", "  tar_target(b, a + 1)", ")"), file.path(dir, "_targets.R"))
  requests <- normalizePath(tempfile("editor_", fileext = ".rds"), winslash = "/", mustWork = FALSE)
  app <- local_board_app(dir, "open-editor", sprintf(
    "options(targetsboard.editor = '%s'); targetsboard::tar_board_app()", requests
  ))
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='b']\") !== null", timeout = 30000)
  mouse_event(app, "b", "contextmenu")
  app$wait_for_js("document.querySelector(\".tb-node-menu [data-action='open_editor']\") !== null", timeout = 10000)
  expect_equal(app$get_text(".tb-node-menu [data-action='open_editor']"), "Open in editor")
  app$click(selector = ".tb-node-menu [data-action='open_editor']")
  for (i in 1:50) if (!file.exists(requests)) Sys.sleep(0.2)
  request <- readRDS(requests)
  expect_equal(basename(request$file), "_targets.R")
  expect_equal(request$line, 4L)
})
