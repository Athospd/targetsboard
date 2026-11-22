#' Run modes: how a board run differs from tar_make()
#'
#' A board run can keep going after errors, or stop at a target's first
#' warning; a debug run stops inside one target and skips the rest. targets
#' sets these with `tar_option_set()` in the script, which the board never
#' edits (ADR 0001), and each target fixes its settings when the script
#' creates it, so an option set afterwards comes too late. So the run gets a
#' wrapper script: it sources the author's script and changes the targets it
#' returns, through the internal fields below. This file is the only place
#' that touches them (ADR 0002), and `run_modes_supported()` checks they are
#' still there.
#'
#' @name run_modes
#' @noRd
NULL

# The internal fields of a target that the run modes change, and how they
# must look. A new version of targets that changes them disables the modes.
run_modes_supported <- function() {
  target <- tryCatch(targets::tar_target_raw("probe", quote(1)), error = function(e) NULL)
  is.environment(target) &&
    is.environment(target$settings) && rlang::is_string(target$settings$error) &&
    is.environment(target$cue) && rlang::is_string(target$cue$mode) &&
    is.environment(target$command) && is.expression(target$command$expr) &&
    rlang::is_string(target$command$hash)
}

#' Change how the targets of a pipeline run
#'
#' Called by the wrapper script, on the targets the author's script returns
#' (any nesting of lists). The targets are changed in place and returned.
#' Their command's hash is left alone, so what is outdated stays what the
#' author's script says.
#'
#' @param targets What the author's script returns.
#' @param keep_going Run the other targets after one errors
#'   (targets' `error = "continue"`).
#' @param stop_at_warnings Turn a target's first warning into an error, so it
#'   leaves a workspace and a traceback where it warned.
#' @param debug A target to stop in, for a debug run in the author's
#'   console; the others are skipped unless they never ran
#'   (`cue = tar_cue("never")`).
#' @noRd
apply_run_modes <- function(targets, keep_going = FALSE, stop_at_warnings = FALSE, debug = NULL) {
  if (!is.null(debug)) targets::tar_option_set(debug = debug)
  each_target(targets, function(target) {
    if (keep_going) target$settings$error <- "continue"
    if (stop_at_warnings) target$command$expr <- warnings_as_errors(target$command$expr)
    if (!is.null(debug)) target$cue$mode <- "never"
  })
  targets
}

each_target <- function(x, f) {
  if (inherits(x, "tar_target")) {
    f(x)
  } else if (is.list(x)) {
    lapply(x, each_target, f = f)
  }
  invisible(x)
}

# A command whose first warning stops it, as an error that says so.
warnings_as_errors <- function(expr) {
  as.expression(lapply(expr, function(command) {
    bquote(withCallingHandlers(
      .(command),
      warning = function(w) {
        stop(simpleError(paste("Warning, stopped at by the board:", conditionMessage(w)), conditionCall(w)))
      }
    ))
  }))
}

#' Write the wrapper script of a run
#'
#' @param script The author's script.
#' @param path Where to write it.
#' @inheritParams apply_run_modes
#' @return `path`.
#' @noRd
write_run_wrapper <- function(script, keep_going = FALSE, stop_at_warnings = FALSE, debug = NULL,
                              path = tempfile("targetsboard_run_", fileext = ".R")) {
  writeLines(c(
    "# Written by targetsboard for one run: it loads the pipeline's own script",
    "# and changes how its targets run. The script itself is not changed.",
    sprintf(
      "targetsboard:::apply_run_modes(source(%s)$value, keep_going = %s, stop_at_warnings = %s, debug = %s)",
      deparse(normalizePath(script, winslash = "/", mustWork = TRUE)),
      keep_going, stop_at_warnings, deparse(debug)
    )
  ), path)
  path
}

# Whether any run mode is on.
any_run_mode <- function(modes) isTRUE(modes$keep_going) || isTRUE(modes$stop_at_warnings)
