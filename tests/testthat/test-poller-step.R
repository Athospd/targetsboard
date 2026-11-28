# poller_step(), one check of the poller (#273): it runs in the background
# process, so covr can't see it there; here it runs in-process.

# Runs one check; `recomputed` says whether it wrote the graph again.
local_step <- function(env = parent.frame()) {
  paths <- list(vis = tempfile(fileext = ".rds"))
  paths$refresh <- sub("[.]rds$", ".refresh", paths$vis)
  run <- NULL
  why <- function(script) list(reasons = NULL, manifest = data.frame(name = "a"), cloud = NULL, controllers = NULL, globals = list(g = list(class = "numeric")), errors = "a why message")
  step <- function(state = NULL, settle = 0, running = FALSE) {
    unlink(paths$vis)
    state <- poller_step(
      state, "_targets.R", paths$vis, paths$refresh, settle = settle, filters = list(),
      sourced_paths = sourced_paths, active_run = function() if (running) list(pid = 1),
      why = why, vis_args = list(targets_only = TRUE)
    )
    attr(state, "recomputed") <- file.exists(paths$vis)
    state
  }
  list(step = step, paths = paths, set_why = function(f) why <<- f)
}

touch <- function(path, when = Sys.time() + 5) Sys.setFileTime(path, when)

test_that("the first check draws the graph and reads why; the next ones only when something changed", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  s <- local_step()
  expect_message(state <- s$step(), "a why message")
  expect_true(attr(state, "recomputed"))
  graph <- readRDS(s$paths$vis)
  expect_setequal(graph$x$nodes$id, c("a", "b", "c"))
  expect_equal(readRDS(poller_reasons_path(s$paths$vis))$manifest$name, "a")
  expect_equal(readRDS(poller_reasons_path(s$paths$vis))$globals$g$class, "numeric") # #305

  state <- s$step(state)
  expect_false(attr(state, "recomputed"))

  # The script changed.
  touch("_targets.R")
  state <- suppressMessages(s$step(state))
  expect_true(attr(state, "recomputed"))
  # A new file under R/, which it sources by default.
  dir.create("R")
  writeLines("f <- function() 1", "R/f.R")
  state <- suppressMessages(s$step(state))
  expect_true(attr(state, "recomputed"))
  expect_false(attr(s$step(state), "recomputed"))
  # The app asked for it.
  writeLines("now", s$paths$refresh)
  state <- suppressMessages(s$step(state))
  expect_true(attr(state, "recomputed"))
})

test_that("a run that ended draws the graph again, once its progress settled and its process is gone", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  s <- local_step()
  state <- suppressMessages(s$step())
  progress <- file.path("_targets", "meta", "progress")
  touch(progress)
  state <- s$step(state, settle = 60) # not settled yet
  expect_false(attr(state, "recomputed"))
  state <- s$step(state, running = TRUE) # settled, but still running
  expect_false(attr(state, "recomputed"))
  state <- suppressMessages(s$step(state))
  expect_true(attr(state, "recomputed"))
  expect_false(attr(s$step(state), "recomputed"))
})

test_that("a broken script writes its error, is retried only once it changes, and the fix clears it", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  s <- local_step()
  good <- readLines("_targets.R")
  writeLines("stop('broken on purpose')", "_targets.R")
  expect_message(state <- s$step(), "broken on purpose")
  error_path <- poller_error_path(s$paths$vis)
  expect_match(paste(readLines(error_path), collapse = " "), "broken on purpose", fixed = TRUE)
  expect_false(file.exists(poller_reasons_path(s$paths$vis)))
  state <- s$step(state) # not retried
  expect_false(attr(state, "recomputed"))

  writeLines(good, "_targets.R")
  touch("_targets.R")
  state <- suppressMessages(s$step(state))
  expect_true(attr(state, "recomputed"))
  expect_false(file.exists(error_path))
})

test_that("a why process that fails says so, and leaves the reasons as they were", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  s <- local_step()
  s$set_why(function(script) stop("why broke"))
  expect_message(state <- s$step(), "why broke")
  expect_true(attr(state, "recomputed"))
  expect_false(file.exists(poller_reasons_path(s$paths$vis)))
})

test_that("poller_why() gives each target's resources as text: none, or crew with its controller", {
  skip_on_cran()
  local_pipeline()
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(a, 1),",
    "  tar_target(b, 2, resources = tar_resources(crew = tar_resources_crew(controller = 'big')))",
    ")"
  ), "_targets.R")
  found <- poller_why("_targets.R")
  resources <- stats::setNames(found$manifest$resources, found$manifest$name)
  expect_true(is.na(resources[["a"]]))
  expect_equal(resources[["b"]], "crew (controller big)")
})
