# targets::tar_visnetwork(), which the board relies on, needs visNetwork,
# but targets only suggests it: so we import it ourselves.
#' @importFrom visNetwork visNetwork
NULL

#' Recompute tar_visnetwork() in a background process, only when needed
#'
#' `tar_visnetwork()` hashes the whole pipeline to tell outdated targets from
#' up-to-date ones, which is slow. So the process only recomputes it when:
#' - the script, or an R file under `R/`, changed (new targets or functions),
#' - a run ended: the progress file changed since the last recompute, has not
#'   changed for `settle` seconds, and the pipeline process is gone,
#' - the app asked for it, by writing to `refresh_path`.
#' Between recomputes, the app follows a run through the cheap progress file.
#'
#' Each recompute is written to `tar_vis_path` as an RDS file holding
#' `list(x = tar_visnetwork()$x, widget = tar_visnetwork(), progress_mtime = )`:
#' `widget` for the `tar_visnetwork` tab, and the modification time of the
#' progress file it reflects, so the app knows which progress is newer.
#' When `tar_visnetwork()` fails (a broken script), the error message is
#' written next to it, to `poller_error_path(tar_vis_path)`, and removed by
#' the next recompute that works: the app shows it.
#'
#' @param script Path to the targets script.
#' @param interval Seconds between two checks; checks are cheap file stats.
#' @param settle Seconds without progress before a run counts as ended.
#' @param ... Passed on to [targets::tar_visnetwork()].
#' @return A list: the `process` ([callr::r_bg()]), `tar_vis_path` and
#'   `refresh_path`.
#' @noRd
tar_visnetwork_bg <- function(script, interval = 0.5, settle = 1, ...) {
  tar_vis_path <- tempfile("tar_visnetwork_", fileext = ".rds")
  refresh_path <- sub("[.]rds$", ".refresh", tar_vis_path)

  # Runs in the background process: self-contained, it can't call this
  # package's internals.
  tar_visnetwork_instance <- function(script, tar_vis_path, refresh_path, interval, settle, ...) {
    progress_file <- file.path(targets::tar_config_get("store"), "meta", "progress")

    code_signature <- function() {
      files <- c(script, sort(list.files("R", pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)))
      list(
        files = files,
        mtimes = file.mtime(files),
        refresh = if (file.exists(refresh_path)) readLines(refresh_path, warn = FALSE)
      )
    }
    # Is the pipeline process targets recorded still running? (A copy of
    # active_run(): a killed run leaves targets marked dispatched for good.)
    running <- function() {
      info <- tryCatch(targets::tar_process(), error = function(e) NULL)
      if (is.null(info)) return(FALSE)
      pid <- suppressWarnings(as.integer(info$value[info$name == "pid"]))
      created <- as.POSIXct(info$value[info$name == "created"], tz = "UTC")
      if (length(pid) != 1 || is.na(pid) || length(created) != 1 || is.na(created)) return(FALSE)
      handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
      if (is.null(handle)) return(FALSE)
      started <- tryCatch(ps::ps_create_time(handle), error = function(e) NA)
      isTRUE(tryCatch(ps::ps_is_running(handle), error = function(e) FALSE)) &&
        !is.na(started) &&
        abs(as.numeric(difftime(started, created, units = "secs"))) <= 2
    }

    computed_code <- NULL
    computed_progress <- NULL # progress mtime of the last recompute
    seen_progress <- NULL     # and the one last seen, since when
    seen_since <- Sys.time()

    while (TRUE) {
      code <- code_signature()
      progress_mtime <- file.mtime(progress_file)
      if (!identical(progress_mtime, seen_progress)) {
        seen_progress <- progress_mtime
        seen_since <- Sys.time()
      }
      run_ended <- !is.null(computed_progress) &&
        !identical(progress_mtime, computed_progress) &&
        difftime(Sys.time(), seen_since, units = "secs") >= settle &&
        !running()

      if (!identical(code, computed_code) || run_ended) {
        error_path <- sub("[.]rds$", ".error", tar_vis_path) # poller_error_path()
        widget <- tryCatch(
          targets::tar_visnetwork(script = script, ...),
          error = function(e) {
            message("tar_visnetwork() failed: ", conditionMessage(e))
            writeLines(conditionMessage(e), error_path)
            NULL
          }
        )
        if (!is.null(widget)) unlink(error_path)
        # Write to a temp file and rename it into place, so the app never
        # reads a half-written file. On Windows the rename fails while the
        # app holds the file open: then retry on the next check.
        written <- is.null(widget) || {
          tmp <- paste0(tar_vis_path, ".tmp")
          saveRDS(list(x = widget$x, widget = widget, progress_mtime = progress_mtime), file = tmp)
          file.rename(tmp, tar_vis_path) || { unlink(tmp); FALSE }
        }
        # A broken script is retried once it changes again, not every check.
        if (written) {
          computed_code <- code
          computed_progress <- progress_mtime
        }
      }
      Sys.sleep(interval)
    }
  }

  bg_process <- callr::r_bg(
    tar_visnetwork_instance,
    args = list(
      script = script, tar_vis_path = tar_vis_path, refresh_path = refresh_path,
      interval = interval, settle = settle, ...
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = sub("[.]rds$", ".log", tar_vis_path),
    stderr = "2>&1"
  )
  invisible(list(process = bg_process, tar_vis_path = tar_vis_path, refresh_path = refresh_path))
}

# Where the poller writes why its last recompute failed: see tar_visnetwork_bg().
poller_error_path <- function(tar_vis_path) sub("[.]rds$", ".error", tar_vis_path)

#' Why the script doesn't load, for the board's notification
#'
#' @param message The error message the poller wrote, or `NULL` when it loads.
#' @return A notification's text, or `NULL`.
#' @noRd
script_error_text <- function(script, message) {
  if (is.null(message) || !length(message)) return(NULL)
  message <- paste(message, collapse = " ")
  if (nchar(message) > 300) message <- paste0(substr(message, 1, 297), "...")
  paste0(
    script, " doesn't load, so the board still shows its last working graph. ",
    "The error: ", message
  )
}
