# What the board takes from targets, one assumption per test (#283): a
# targets release that renames a column or rewords a message fails here, by
# name, rather than a feature going quietly dead.

test_that("tar_visnetwork()'s graph has the nodes and edges the board draws", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  x <- targets::tar_visnetwork(targets_only = TRUE)$x
  expect_true(all(c("id", "label", "status", "type") %in% names(x$nodes)))
  expect_true(all(c("from", "to") %in% names(x$edges)))
  expect_setequal(x$nodes$id, c("a", "b", "c"))
  expect_true(all(x$nodes$status %in% c("uptodate", "outdated", "errored", "dispatched", "completed", "canceled", "skipped", "none", "started", "built", "queued")))
  # Functions and globals: their own types.
  writeLines(c("library(targets)", "rate <- 3", "f <- function(x) x * rate", "list(tar_target(a, f(1)))"), "_targets.R")
  types <- targets::tar_visnetwork(targets_only = FALSE)$x$nodes
  expect_equal(types$type[types$id == "f"], "function")
  expect_equal(types$type[types$id == "rate"], "object")
})

test_that("tar_meta() has the fields the board reads", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  fields <- c("name", "type", "data", "parent", "path", "format", "repository", "iteration", "children",
              "seconds", "bytes", "time", "warnings", "error")
  meta <- targets::tar_meta() # every field
  expect_true(all(fields %in% names(meta)))
  expect_true(all(c("parent", "error") %in% names(targets::tar_meta(fields = c("parent", "error"), targets_only = TRUE))))
})

test_that("tar_progress() has the fields the run summary and statuses read", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  progress <- targets::tar_progress(fields = c("type", "parent", "progress"))
  expect_true(all(c("name", "type", "parent", "progress") %in% names(progress)))
  expect_true(all(progress$progress %in% c("built", "skipped", "errored", "canceled", "dispatched", "completed", "started", "queued")))
  expect_true(file.exists(file.path("_targets", "meta", "progress")))
})

test_that("tar_sitrep() and tar_network() give the cues and statuses why outdated reads", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  sitrep <- targets::tar_sitrep()
  expect_true(all(c("name", "meta", "always", "never", "command", "depend", "format", "repository",
                    "iteration", "file", "seed") %in% names(sitrep)))
  network <- targets::tar_network(targets_only = FALSE)
  expect_true(all(c("name", "type", "status") %in% names(network$vertices)))
  expect_true(all(c("from", "to") %in% names(network$edges)))
})

test_that("tar_manifest() has every field poller_why() asks for", {
  skip_on_cran()
  local_pipeline()
  # Its resources column comes only when a target sets some.
  writeLines(c(
    "library(targets)",
    "list(tar_target(a, 1), tar_target(b, a, resources = tar_resources(crew = tar_resources_crew(controller = 'x'))), tar_target(c, b))"
  ), "_targets.R")
  manifest <- targets::tar_manifest(fields = c(
    "name", "format", "command", "cue_mode", "cue_command", "cue_depend",
    "cue_file", "cue_format", "cue_repository", "cue_iteration",
    "pattern", "iteration", "memory", "storage", "retrieval", "deployment",
    "error", "priority", "repository", "resources"
  ))
  expect_equal(nrow(manifest), 3)
  expect_true("resources" %in% names(manifest))
})

test_that("targets records the pipeline's process in meta/process, with its pid and start time", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  expect_true(file.exists(file.path("_targets", "meta", "process")))
  info <- targets::tar_process()
  expect_true(all(c("pid", "created") %in% info$name))
})

# The lock's message ("is already running a {targets} pipeline", R/run.R):
# test-process-lock.R triggers the real one, from another R session.
