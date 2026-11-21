# Running and stopping the pipeline from the board.

#' The running pipeline process, if any
#'
#' targets records the process of the latest `tar_make()` in the store
#' (`targets::tar_process()`), whoever started it: the board, the R console or
#' a scheduler. This finds that process if it is still running.
#'
#' @return A [ps::ps_handle()], or `NULL` when no run is going.
#' @noRd
active_run <- function() {
  info <- tryCatch(targets::tar_process(), error = function(e) NULL)
  if (is.null(info) || !all(c("pid", "created") %in% info$name)) return(NULL)
  pid <- suppressWarnings(as.integer(info$value[info$name == "pid"]))
  created <- as.POSIXct(info$value[info$name == "created"], tz = "UTC")
  if (is.na(pid) || is.na(created)) return(NULL)

  handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
  if (is.null(handle) || !isTRUE(tryCatch(ps::ps_is_running(handle), error = function(e) FALSE)))
    return(NULL)
  # The pid may have been reused by a newer process since that run ended.
  started <- tryCatch(ps::ps_create_time(handle), error = function(e) NA)
  if (is.na(started) || abs(as.numeric(difftime(started, created, units = "secs"))) > 2)
    return(NULL)
  handle
}

#' Start `tar_make()` in a background process
#'
#' The run is not tied to the board: it goes on if the board stops.
#'
#' @param script Path to the targets script.
#' @param names Targets to make, with whatever they depend on that is
#'   outdated. `NULL` makes the whole pipeline.
#' @return The [callr::r_bg()] process. Its output goes to the file
#'   `attr(, "log")`.
#' @noRd
start_run <- function(script, names = NULL) {
  log <- tempfile("targetsboard_run_", fileext = ".log")
  process <- rlang::inject(targets::tar_make(
    names = !!names, # spliced in: the run happens in another process
    script = script,
    reporter = "timestamp",
    callr_function = callr::r_bg,
    callr_arguments = list(stdout = log, stderr = "2>&1", supervise = FALSE)
  ))
  attr(process, "log") <- log
  process
}

#' Why a finished run failed
#'
#' In a background process, `tar_make()` returns the pipeline's error as a
#' `tar_condition_traced` object instead of throwing it.
#'
#' @param process A finished process from `start_run()`.
#' @return The error message, or `NULL` if the run succeeded.
#' @noRd
run_error <- function(process) {
  result <- tryCatch(process$get_result(), error = function(e) e)
  if (inherits(result, "error")) return(conditionMessage(result))
  if (inherits(result, "tar_condition_traced")) {
    return(result$condition$message %||% "the pipeline errored")
  }
  NULL
}

#' Stop a run: kill the pipeline process
#'
#' Only that process: listing its children takes seconds on Windows, and this
#' runs in the board's process. Parallel workers (e.g. crew's) exit on their
#' own once it is gone. The board kills the whole tree of the runs it started.
#'
#' @param handle A [ps::ps_handle()], from `active_run()`.
#' @noRd
stop_run <- function(handle) {
  tryCatch(ps::ps_kill(handle, grace = 0), error = function(e) NULL)
  invisible(NULL)
}

# A killed run leaves its running targets marked dispatched in the progress
# file. Once no run is going, show them as canceled; targets' own files are
# left alone.
settle_statuses <- function(statuses, running) {
  if (!running) statuses[statuses == "dispatched"] <- "canceled"
  statuses
}

#' Invalidate targets: the next run builds them again
#'
#' `targets::tar_invalidate()` deletes their metadata records, so they and
#' what depends on them are outdated. Their files stay in the store, but
#' `tar_read()` can no longer read them until they are built again.
#' Names that are no longer targets are ignored.
#'
#' @param names Target names.
#' @noRd
invalidate_targets <- function(names) {
  targets::tar_invalidate(names = dplyr::any_of(names))
  invisible(names)
}
