# Which inputs a branch came from (#142).

branch_pipeline <- function(.env = parent.frame()) {
  dir <- local_pipeline(.env = .env)
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(x, c(10, 20, 30)),",
    "  tar_target(z, c('a', 'b')),",
    "  tar_target(l, list(1, 'two'), iteration = 'list'),",
    "  tar_target(d, data.frame(v = 1:3)),",
    "  tar_target(px, x * 2, pattern = map(x)),",
    "  tar_target(m, px + 1, pattern = map(px)),",
    "  tar_target(cr, paste(x, z), pattern = cross(x, z)),",
    "  tar_target(sl, x, pattern = slice(x, index = c(2, 3))),",
    "  tar_target(ml, l, pattern = map(l)),",
    "  tar_target(md, d$v, pattern = map(d)),",
    "  tar_target(g, {out <- data.frame(v = 1:3); out$tar_group <- c(1L, 1L, 2L); out}, iteration = 'group'),",
    "  tar_target(mg, sum(g$v), pattern = map(g))",
    ")"
  ), file.path(dir, "_targets.R"))
  targets::tar_make(reporter = "silent")
  dir
}

# Each input of a pattern's i-th branch, read with its code.
read_inputs <- function(name, i) {
  meta <- targets::tar_meta(fields = c("type", "children", "bytes"))
  branches <- as.data.frame(do.call(targets::tar_branches, list(name = as.symbol(name))))
  manifest <- targets::tar_manifest(fields = c("name", "iteration"))
  iterations <- stats::setNames(manifest$iteration, manifest$name)
  inputs <- branch_inputs(branches, branches[[1]][[i]], meta)
  stats::setNames(lapply(inputs, function(input) {
    eval(parse(text = slice_code(input, iterations[[input$input]])), envir = baseenv())
  }), vapply(inputs, `[[`, "", "input"))
}

test_that("each kind of pattern names the slices its branch ran on, and their code reads them", {
  branch_pipeline()
  expect_equal(read_inputs("px", 2), list(x = 20))
  expect_equal(read_inputs("m", 3), list(px = c(px_cb92331fff8ccf94 = 60)), ignore_attr = TRUE)
  expect_equal(unname(unlist(read_inputs("m", 3))), 60)
  expect_equal(read_inputs("cr", 4), list(x = 20, z = "b")) # cross: x varies slowest
  expect_equal(read_inputs("sl", 1), list(x = 20))
  expect_equal(read_inputs("ml", 2), list(l = "two"))
  expect_equal(read_inputs("md", 3)$d, data.frame(v = 3L), ignore_attr = TRUE)
  expect_equal(read_inputs("mg", 2)$g$v, 3L)
})

test_that("branch_inputs() is NULL for a branch it doesn't know, and says where a slice is gone", {
  branches <- data.frame(px = c("px_1", "px_2"), x = c("x_1", "x_gone"))
  meta <- data.frame(name = "x", type = "stem", bytes = 10)
  meta$children <- list(c("x_1", "x_2"))
  expect_null(branch_inputs(branches, "px_3", meta))
  inputs <- branch_inputs(branches, "px_2", meta)
  expect_true(is.na(inputs[[1]]$index))
  expect_match(as.character(branch_inputs_ui(inputs)), "its slice isn't stored any more", fixed = TRUE)
  expect_match(as.character(branch_inputs_ui(NULL)), "Its inputs aren't known", fixed = TRUE)
})

test_that("branch_inputs_ui() reads a slice only within the preview's size limit", {
  input <- list(input = "x", type = "stem", index = 2L, n = 3L, bytes = 100)
  html <- as.character(branch_inputs_ui(list(input), read = function(code) "read", limit = 1000))
  expect_match(html, "<code>x</code>, slice 2 of 3", fixed = TRUE)
  expect_match(html, "data-copy=\"vctrs::vec_slice(targets::tar_read(x), 2)\"", fixed = TRUE)
  expect_match(html, "tb-branch-input-value", fixed = TRUE)
  html <- as.character(branch_inputs_ui(list(input), read = function(code) "read", limit = 10))
  expect_no_match(html, "tb-branch-input-value", fixed = TRUE)
  expect_equal(slice_code(input, "list", store = "elsewhere"), "targets::tar_read(x, store = \"elsewhere\")[[2]]")
})

test_that("a branch's Inputs section, opened, names its slice and shows it", {
  dir <- branch_pipeline()
  app <- local_board_app(dir, "branch-inputs")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='px']\") !== null", timeout = 30000)
  mouse_event(app, "px", "dblclick")
  app$wait_for_js("document.querySelector('.tb-branch-inputs') !== null", timeout = preview_timeout)
  expect_null(app$get_js("document.querySelector('.tb-branch-input')")) # nothing worked out until opened
  app$run_js("document.querySelector('.tb-branch-inputs summary').click()")
  app$wait_for_js("document.querySelector('.tb-branch-input') !== null", timeout = 30000)
  expect_equal(app$get_js("document.querySelector('.tb-branch-input p').innerText"), "x, slice 1 of 3")
  expect_match(app$get_js("document.querySelector('.tb-branch-input-value').innerText"), "10")
})

test_that("problem_ui() gives each failing branch its Inputs section (#227)", {
  problem <- list(
    name = "p", type = "pattern", error = NULL, warnings = NULL, functions = list(),
    branches = list(
      list(name = "p_a", position = 1, of = 3, error = "boom", warnings = NULL, workspace = FALSE),
      list(name = "p_b", position = 2, of = 3, error = "boom", warnings = NULL, workspace = FALSE)
    )
  )
  html <- as.character(problem_ui(problem, ns = shiny::NS("x")))
  expect_equal(lengths(regmatches(html, gregexpr("tb-branch-inputs", html, fixed = TRUE))), 2)
  expect_match(html, "data-tb-value=\"&quot;p_b&quot;\"", fixed = TRUE)
})

test_that("a failing branch's Inputs open in the Problems list, one at a time (#227)", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)", "tar_option_set(error = 'continue')",
    "list(tar_target(x, c(1, 2, 3)), tar_target(p, if (x > 1) stop('too big') else x, pattern = map(x)))"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, try(targets::tar_make(reporter = "silent"), silent = TRUE))
  app <- local_board_app(dir, "problem-branch-inputs")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='p']\") !== null", timeout = 30000)
  mouse_event(app, "p", "dblclick")
  app$wait_for_js("document.querySelectorAll('.tb-problem-branch .tb-branch-inputs').length === 2", timeout = preview_timeout)
  # The branch and its Inputs open in one go: nothing can render between
  # them (#264). Text is read with textContent, which a closed section
  # doesn't hide.
  open_inputs <- function(i) app$run_js(sprintf(paste(
    "document.querySelectorAll('.tb-problem-branch')[%d].open = true;",
    "document.querySelectorAll('.tb-problem-branch .tb-branch-inputs summary')[%d].click();"
  ), i, i))
  input_shown <- function(text) sprintf(
    "[...document.querySelectorAll('.tb-problem-branch .tb-branch-input p')].some(p => p.textContent.trim() === '%s')", text
  )
  open_inputs(0)
  app$wait_for_js(input_shown("x, slice 2 of 3"), timeout = 30000)
  open_inputs(1)
  # shortcut: on a timeout, says what the page and the app held, for #264's CI-only failure; drop once found.
  tryCatch(app$wait_for_js(input_shown("x, slice 3 of 3"), timeout = 30000), error = function(e) {
    message("#264 sections: ", app$get_js(paste0(
      "[...document.querySelectorAll('.tb-problem-branch')].map((d, i) => i + ' open=' + d.open + ' inputs.open=' +",
      " d.querySelector('.tb-branch-inputs')?.open + ' output=' + !!d.querySelector('.shiny-html-output') +",
      " ' text=' + JSON.stringify(d.querySelector('.tb-branch-inputs')?.textContent.slice(0, 200))).join(' | ')"
    )))
    logs <- as.data.frame(app$get_logs())
    message("#264 app log:
", paste(utils::tail(paste(logs$location, logs$level, logs$message), 40), collapse = "
"))
    stop(e)
  })
  # One output, in the section opened last; the first one closed.
  expect_equal(app$get_js("document.querySelectorAll('[id$=\"-inputs\"]').length"), 1)
  expect_false(app$get_js("document.querySelectorAll('.tb-problem-branch .tb-branch-inputs')[0].open"))
})
