# Knowing whether the pipeline is running, whoever started it.

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

# A killed run leaves its running targets marked dispatched in the progress
# file. Once no run is going, show them as canceled; targets' own files are
# left alone.
settle_statuses <- function(statuses, running) {
  if (!running) statuses[statuses == "dispatched"] <- "canceled"
  statuses
}
