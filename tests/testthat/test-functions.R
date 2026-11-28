# A function's code where the board names it (#153).

test_that("function_definitions() reads the functions of the script and what it sources, as written", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("R/sub", recursive = TRUE)
  dir.create("lib")
  writeLines(c("# cleaning", "clean <- function(x) {", "  x[!is.na(x)]", "}", "n <- 3"), "R/clean.R")
  writeLines("fit = function(d) lm(y ~ x, d)", "R/sub/fit.R")
  writeLines(c("helper <- function() 1", "obj$f <- function() 2"), "lib/helpers.R")
  writeLines("not R", "R/notes.txt")
  writeLines(c(
    "library(targets)",
    "tar_source()",
    "source('lib/helpers.R')",
    "helper <- function() 2 # wins over lib's",
    "list(tar_target(a, clean(1)))"
  ), "_targets.R")
  fns <- function_definitions("_targets.R")
  expect_setequal(fns$name, c("clean", "fit", "helper"))
  expect_equal(fns[fns$name == "clean", c("file", "line")], data.frame(file = "R/clean.R", line = 2L), ignore_attr = TRUE)
  expect_equal(fns$code[fns$name == "clean"], "clean <- function(x) {\n  x[!is.na(x)]\n}")
  expect_equal(fns$file[fns$name == "fit"], "R/sub/fit.R")
  expect_equal(fns[fns$name == "helper", c("file", "line")], data.frame(file = "_targets.R", line = 4L), ignore_attr = TRUE)
  # The other globals (#305): `n`, not the functions, nor obj$f.
  globals <- function_definitions("_targets.R", functions = FALSE)
  expect_equal(globals$name, "n")
  expect_equal(globals[, c("file", "line", "code")], data.frame(file = "R/clean.R", line = 5L, code = "n <- 3"), ignore_attr = TRUE)

  writeLines("broken <- function(", "R/broken.R") # a file that doesn't parse is skipped
  expect_setequal(function_definitions("_targets.R")$name, c("clean", "fit", "helper"))
  expect_equal(nrow(function_definitions("missing.R")), 0)
  expect_equal(nrow(function_definitions(NULL)), 0)
})

test_that("function_ui() shows the code and where it is; the Why card adds the functions it names", {
  fns <- data.frame(name = c("f", "g"), file = "R/f.R", line = c(3L, 9L), code = c("f <- function() 1", "g <- function() 2"))
  html <- as.character(function_ui(fns[1, ]))
  expect_match(html, "<details class=\"tb-command tb-function\" data-function=\"f\">", fixed = TRUE)
  expect_match(html, "R/f.R:3", fixed = TRUE)
  expect_match(html, "data-copy=\"f &lt;- function() 1\"", fixed = TRUE)
  expect_match(as.character(function_ui(fns[1, ], open = TRUE)), "open>", fixed = TRUE)
  expect_null(function_ui(fns[0, ]))

  why <- as.character(reasons_ui(c(depend = "`f` and `x` changed."), fns))
  expect_match(why, "data-function=\"f\"", fixed = TRUE)
  expect_no_match(why, "data-function=\"g\"", fixed = TRUE)
  expect_no_match(as.character(reasons_ui(c(depend = "`f` changed."))), "tb-function", fixed = TRUE)
  expect_null(reason_functions_ui(c(upstream = "Upstream, `b` is outdated."), fns))
})

test_that("a changed function shows its code under the target's reasons, on the board", {
  dir <- local_pipeline()
  dir.create(file.path(dir, "R"))
  writeLines(c("double_it <- function(x) {", "  x * 2", "}"), file.path(dir, "R", "double.R"))
  writeLines(c("library(targets)", "tar_source()", "list(tar_target(a, double_it(1)))"), file.path(dir, "_targets.R"))
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  writeLines(c("double_it <- function(x) {", "  x + x", "}"), file.path(dir, "R", "double.R"))
  app <- local_board_app(dir, "functions")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='a']\") !== null", timeout = 30000)
  mouse_event(app, "a", "dblclick")
  app$wait_for_js("document.querySelector('.tb-why .tb-function') !== null", timeout = preview_timeout)
  expect_equal(app$get_js("document.querySelector('.tb-function-where').innerText"), "R/double.R:1")
  expect_equal(app$get_js("document.querySelector('.tb-function .tb-copy').dataset.copy"), "double_it <- function(x) {\n  x + x\n}")
})

test_that("a function on the canvas shows its code, open, in the Value pane, and no debug run", {
  dir <- local_pipeline()
  writeLines(c("library(targets)", "half <- function(x) x / 2", "list(tar_target(a, half(1)))"), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "function-node", app = "targetsboard::tar_board_app(targets_only = FALSE)")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='half']\") !== null", timeout = 30000)
  mouse_event(app, "half", "dblclick")
  app$wait_for_js("document.querySelector('.tb-function[open]') !== null", timeout = preview_timeout)
  expect_equal(app$get_js("document.querySelector('.tb-function-where').innerText"), "_targets.R:2")
  expect_null(app$get_js("document.querySelector('.tb-debug')"))
})

test_that("global_ui() shows a global as written, the targets that use it and its hash", {
  def <- data.frame(name = "rate", file = "_targets.R", line = 3L, code = "rate <- 3")
  html <- as.character(global_ui(def, "rate", users = c("a", "b"), hash = "1383ef88"))
  expect_match(html, "rate &lt;- 3", fixed = TRUE)
  expect_match(html, "_targets.R:3", fixed = TRUE)
  expect_match(html, "Used by", fixed = TRUE)
  expect_match(html, "<code>1383ef88</code>", fixed = TRUE)
  expect_match(html, 'data-copy="targets::tar_load_globals()"', fixed = TRUE)
  expect_match(as.character(global_ui(def[0, ], "rate")), "wasn't found", fixed = TRUE)
  expect_no_match(html, "Its value", fixed = TRUE) # until the poller has read the script
})

test_that("global_ui() shows a global's value as the poller printed it, or why it couldn't", {
  def <- data.frame(name = "rate", file = "_targets.R", line = 3L, code = "rate <- 3")
  html <- as.character(global_ui(def, "rate", value = list(class = "numeric", bytes = 56, printed = "[1] 3")))
  expect_match(html, "Its value", fixed = TRUE)
  expect_match(html, "(a numeric, 56 B)", fixed = TRUE)
  expect_match(html, '<pre class="tb-preview-printed">[1] 3</pre>', fixed = TRUE)
  expect_match(as.character(global_ui(def, "rate", value = list(error = "no print method"))), "be printed: no print method", fixed = TRUE)
  # Shown even when its definition isn't found.
  expect_match(as.character(global_ui(def[0, ], "rate", value = list(class = "numeric", bytes = 56, printed = "[1] 3"))), "[1] 3", fixed = TRUE)
})

test_that("a global on the canvas shows its definition and what uses it in the Value pane", {
  skip_on_cran()
  dir <- local_pipeline()
  writeLines(c("library(targets)", "rate <- 3", "list(tar_target(a, rate * 2))"), file.path(dir, "_targets.R"))
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  app <- local_board_app(dir, "global-node", app = "targetsboard::tar_board_app(targets_only = FALSE)")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='rate']\") !== null", timeout = 30000)
  mouse_event(app, "rate", "dblclick")
  app$wait_for_js("document.querySelector('.tb-global[open]') !== null", timeout = preview_timeout)
  expect_equal(app$get_js("document.querySelector('.tb-global .tb-function-where').innerText"), "_targets.R:2")
  text <- app$get_js("document.querySelector('.tb-global').parentNode.innerText")
  expect_match(text, "Used by a", fixed = TRUE)
  expect_match(text, "Its hash, as targets recorded it", fixed = TRUE)
  # Its value, once the poller has read the script.
  app$wait_for_js("document.querySelector('.tb-global-value pre')?.innerText === '[1] 3'", timeout = 60000)
})
