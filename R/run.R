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
  # A run in an R session (tar_make(callr_function = NULL), a debug run)
  # that ended without targets recording it, left with Q in the debugger:
  # its session lives on. Its end is marked (mark_run_ended(), #257).
  marker <- file.path(targets::tar_config_get("store"), "user", "targetsboard_ended")
  ended <- if (file.exists(marker)) tryCatch(readLines(marker, warn = FALSE), error = function(e) NULL)
  if (identical(ended, info$value[match(c("pid", "created"), info$name)])) return(NULL)
  handle
}
# targets and ps only: the poller runs it in its own process (tar_visnetwork_bg()).
environment(active_run) <- baseenv()

#' Start `tar_make()` in a background process
#'
#' The run is not tied to the board: it goes on if the board stops.
#'
#' @param script Path to the targets script.
#' @param names Targets to make, with whatever they depend on that is
#'   outdated. `NULL` makes the whole pipeline.
#' @param modes Run modes, `list(keep_going, stop_at_warnings)`: with any
#'   on, the run loads the script through a wrapper that applies them
#'   (`write_run_wrapper()`).
#' @return The [callr::r_bg()] process. Its output goes to the file
#'   `attr(, "log")`; `attr(, "maker")` is run_maker()'s.
#' @noRd
start_run <- function(script, names = NULL, modes = list()) {
  log <- tempfile("targetsboard_run_", fileext = ".log")
  # The author's script, not the wrapper. Reading it must never stop a run (#253).
  maker <- tryCatch(run_maker(script), error = function(e) list(fun = "tar_make", workers = NULL))
  if (any_run_mode(modes)) {
    script <- write_run_wrapper(
      script,
      keep_going = isTRUE(modes$keep_going),
      stop_at_warnings = isTRUE(modes$stop_at_warnings)
    )
  }
  make <- getExportedValue("targets", maker$fun)
  process <- rlang::inject(make(
    names = !!names, # spliced in: the run happens in another process
    script = script,
    reporter = "timestamp",
    !!!(if (!is.null(maker$workers)) list(workers = maker$workers)),
    callr_function = callr::r_bg,
    callr_arguments = list(stdout = log, stderr = "2>&1", supervise = FALSE)
  ))
  attr(process, "log") <- log
  attr(process, "maker") <- maker
  attr(process, "header") <- run_header(names, modes, maker = maker)
  process
}

#' What a run is: when it started, what it builds and its modes
#'
#' The first line of the Log tab. callr writes the log file itself, from
#' its first byte, so the line is kept with the process instead.
#' @noRd
run_header <- function(names = NULL, modes = list(), time = Sys.time(), maker = NULL) {
  what <- if (is.null(names)) "all targets" else describe_names(names)
  how <- run_maker_label(maker)
  paste0(
    "Run started ", format(time, "%Y-%m-%d %H:%M:%S"), ": ", what,
    if (!is.null(how)) paste0(", with ", how),
    if (any_run_mode(modes)) paste0(", ", run_modes_label(modes))
  )
}

# The run modes that are on, in words: "keep going after errors", ...
run_modes_label <- function(modes) {
  on <- c(
    if (isTRUE(modes$keep_going)) "keep going after errors",
    if (isTRUE(modes$stop_at_warnings)) "stop at warnings"
  )
  paste(on, collapse = " and ")
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

#' Mark the latest run as ended
#'
#' For a run in your own R session, `tar_make(callr_function = NULL)`, as
#' the board's debug run code does: leaving it with `Q` in the debugger
#' ends it without targets recording the end, and the board would take your
#' session for a run still going. The debug run code calls this on exit.
#'
#' @param store The targets data store.
#' @return Nothing, invisibly.
#' @examples
#' \dontrun{
#' local({
#'   on.exit(targetsboard::mark_run_ended())
#'   targets::tar_make(callr_function = NULL)
#' })
#' }
#' @export
mark_run_ended <- function(store = targets::tar_config_get("store")) {
  info <- tryCatch(targets::tar_process(store = store), error = function(e) NULL)
  if (is.null(info) || !all(c("pid", "created") %in% info$name)) return(invisible())
  dir <- file.path(store, "user")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(info$value[match(c("pid", "created"), info$name)], file.path(dir, "targetsboard_ended"))
  invisible()
}

#' Whether a process is an interactive R session: one never killed (#257)
#'
#' RStudio's rsession, Positron's ark, Rgui, or R or Rterm not running a
#' file (callr's processes, the board's runs, run one with `-f`). One that
#' can't be read counts as one.
#' @noRd
interactive_r <- function(handle) {
  tryCatch({
    name <- tolower(sub("[.]exe$", "", ps::ps_name(handle), ignore.case = TRUE))
    if (name %in% c("rsession", "ark", "rgui", "positron")) return(TRUE)
    if (!name %in% c("r", "rterm")) return(FALSE)
    args <- ps::ps_cmdline(handle)
    !any(args == "-f" | startsWith(args, "--file="))
  }, error = function(e) TRUE)
}

run_pid <- function(handle) tryCatch(ps::ps_pid(handle), error = function(e) NA)

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
  targets::tar_invalidate(names = targets::any_of(names))
  invisible(names)
}
