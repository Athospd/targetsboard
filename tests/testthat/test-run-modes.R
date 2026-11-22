# The run modes edit targets' internal fields (ADR 0002): this is the test
# that fails loudly when a new version of targets changes them.
test_that("the target fields the run modes rely on are still there", {
  expect_true(run_modes_supported())
})

test_that("apply_run_modes() changes nested targets in place, leaving their hash", {
  withr::defer(targets::tar_option_reset())
  a <- targets::tar_target(a, 1)
  b <- targets::tar_target(b, {warning("careful"); a})
  hash <- b$command$hash
  string <- b$command$string
  out <- apply_run_modes(list(a, list(b)), keep_going = TRUE, stop_at_warnings = TRUE)

  expect_identical(out[[2]][[1]], b)
  expect_equal(c(a$settings$error, b$settings$error), c("continue", "continue"))
  expect_match(paste(deparse(b$command$expr[[1]]), collapse = " "), "withCallingHandlers", fixed = TRUE)
  expect_equal(b$command$hash, hash) # what is outdated stays the script's
  expect_equal(b$command$string, string)
  expect_equal(b$cue$mode, "thorough")

  # A debug run: stop in one target, skip the rest.
  apply_run_modes(list(a, b), debug = "b")
  expect_equal(targets::tar_option_get("debug"), "b")
  expect_equal(c(a$cue$mode, b$cue$mode), c("never", "never"))
})

test_that("a command stopped at warnings errors with the warning's message", {
  expr <- warnings_as_errors(expression({warning("2 rows dropped"); 1}))
  expect_error(eval(expr[[1]]), "Warning, stopped at by the board: 2 rows dropped", fixed = TRUE)
  expect_equal(eval(warnings_as_errors(expression(1 + 1))[[1]]), 2)
})

test_that("the wrapper sources the script by its full path, with the modes", {
  dir <- local_pipeline()
  wrapper <- write_run_wrapper("_targets.R", keep_going = TRUE, debug = "b")
  code <- paste(readLines(wrapper), collapse = "\n")
  expect_match(code, normalizePath("_targets.R", winslash = "/"), fixed = TRUE)
  expect_match(code, "keep_going = TRUE, stop_at_warnings = FALSE, debug = \"b\"", fixed = TRUE)
  expect_false(any_run_mode(list()))
  expect_true(any_run_mode(list(keep_going = TRUE, stop_at_warnings = FALSE)))
})

# A pipeline with one target that errors, one that warns, and others.
local_problem_pipeline <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(bad, stop('boom')),",
    "  tar_target(good, 1),",
    "  tar_target(warned, {warning('2 rows dropped'); good + 1}),",
    "  tar_target(after, warned * 2)",
    ")"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir, .local_envir = .env)
  dir
}

finish <- function(process) {
  process$wait(timeout = 120 * 1000)
  expect_false(process$is_alive())
}

test_that("a run that keeps going builds everything but the errored target", {
  skip_on_cran()
  local_problem_pipeline()

  # Without the mode: the run stops at the error, before the rest.
  finish(start_run("_targets.R", names = c("bad", "good")))
  expect_false("good" %in% targets::tar_meta(targets_only = TRUE)$name)

  finish(start_run("_targets.R", modes = list(keep_going = TRUE)))
  meta <- targets::tar_meta(targets_only = TRUE)
  expect_setequal(meta$name[is.na(meta$error)], c("good", "warned", "after"))
  expect_match(meta$error[meta$name == "bad"], "boom")
  # The script's own settings and commands are untouched: a plain look at
  # the pipeline sees only the errored target as outdated.
  expect_equal(targets::tar_outdated(), "bad")
  expect_match(paste(readLines("_targets.R"), collapse = "\n"), "stop('boom')", fixed = TRUE)
})

test_that("a run that stops at warnings leaves a workspace and a traceback where it warned", {
  skip_on_cran()
  local_problem_pipeline()

  finish(start_run("_targets.R", names = "warned", modes = list(stop_at_warnings = TRUE)))
  meta <- targets::tar_meta(names = "warned", fields = c("error", "warnings"))
  expect_match(meta$error, "Warning, stopped at by the board: 2 rows dropped", fixed = TRUE)
  expect_true("warned" %in% targets::tar_workspaces())
  expect_true(any(grepl("warning(\"2 rows dropped\")", targets::tar_traceback(warned), fixed = TRUE)))

  # A plain run afterwards builds it again, with its warning, as usual.
  finish(start_run("_targets.R", names = "warned"))
  meta <- targets::tar_meta(names = "warned", fields = c("error", "warnings"))
  expect_true(is.na(meta$error))
  expect_match(meta$warnings, "2 rows dropped")
  expect_length(targets::tar_outdated(names = "warned"), 0)
})

test_that("the board's runs follow its run mode switches", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()
  seen <- list()
  local_mocked_bindings(start_run = function(script, names = NULL, modes = list()) {
    seen[[length(seen) + 1]] <<- modes
    NULL
  })

  shiny::testServer(server, {
    environment(server)$stop_poller()
    session$setInputs(run_all = 1)
    session$setInputs(keep_going = TRUE, stop_at_warnings = FALSE, run_all = 2)
    session$setInputs(stop_at_warnings = TRUE, flow_node_action = list(action = "run", node = "b", nodes = "b"))
  })
  expect_equal(seen, list(
    list(keep_going = FALSE, stop_at_warnings = FALSE),
    list(keep_going = TRUE, stop_at_warnings = FALSE),
    list(keep_going = TRUE, stop_at_warnings = TRUE)
  ))
})

test_that("the run mode switches are off on start, and disabled without the internals", {
  html <- as.character(run_mode_switches(supported = TRUE))
  expect_match(html, 'id="keep_going"', fixed = TRUE)
  expect_match(html, 'id="stop_at_warnings"', fixed = TRUE)
  expect_no_match(html, "checked", fixed = TRUE)
  expect_no_match(html, "disabled", fixed = TRUE)

  html <- as.character(run_mode_switches(supported = FALSE))
  expect_equal(lengths(regmatches(html, gregexpr("disabled", html, fixed = TRUE))), 2)
  expect_match(html, "add tar_option_set(error = &quot;continue&quot;) to the script", fixed = TRUE)

  page <- as.character(app_ui(list(nodes = fake_nodes(), edges = fake_edges()), "_targets.R"))
  expect_match(page, "Keep going after errors", fixed = TRUE)
})
