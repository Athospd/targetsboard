# A sandbox to try targets projects on the board (#114).
#
# It builds dev/projects/sandbox/ with a _targets.yaml of four projects:
#
#   main       _targets.R   3 quick targets, built         store _targets
#   fast       fast.R       2 quick targets, not built     store _targets_fast
#   slow       slow.R       4 targets of 3 s each          store _targets_slow
#   slow_copy  slow.R       inherits slow, its own store   store _targets_slow_copy
#
# plus other.R, a script no project names. Run from the repository root:
#
#   sandbox <- source("dev/projects/setup.R")$value
#
# Run it again to start over: it deletes the sandbox first.

projects_sandbox <- function(path = "dev/projects/sandbox") {
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE)
  path <- normalizePath(path, winslash = "/")
  old <- setwd(path)
  on.exit(setwd(old))

  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(data, mtcars),",
    "  tar_target(model, lm(mpg ~ wt, data)),",
    "  tar_target(summary, coef(model))",
    ")"
  ), "_targets.R")
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(fast_a, 1),",
    "  tar_target(fast_b, fast_a + 1)",
    ")"
  ), "fast.R")
  writeLines(c(
    "library(targets)",
    "nap <- function(x) { Sys.sleep(3); x }",
    "list(",
    "  tar_target(slow_1, nap(1)),",
    "  tar_target(slow_2, nap(slow_1 + 1)),",
    "  tar_target(slow_3, nap(slow_2 + 1)),",
    "  tar_target(slow_4, nap(slow_3 + 1))",
    ")"
  ), "slow.R")
  writeLines(c(
    "library(targets)",
    "list(tar_target(other, 'a script no project names'))"
  ), "other.R")

  # The projects, as targets writes them. "main" needs no entry: it is
  # targets' default (_targets.R, _targets).
  targets::tar_config_set(script = "fast.R", store = "_targets_fast", project = "fast")
  targets::tar_config_set(script = "slow.R", store = "_targets_slow", project = "slow")
  targets::tar_config_set(inherits = "slow", store = "_targets_slow_copy", project = "slow_copy")

  # main built, so the board starts on a green pipeline; the others not.
  targets::tar_make(reporter = "silent")

  message("Projects sandbox ready: ", path)
  message("Projects: ", paste(targets::tar_config_projects(), collapse = ", "), " (and main)")
  invisible(path)
}

# PROJECTS_SANDBOX: another folder, e.g. the demo video's own (demo/projects/).
projects_sandbox(Sys.getenv("PROJECTS_SANDBOX", "dev/projects/sandbox"))
