#' The graph and what changes on it, for one browser session
#'
#' Reactives polled from the poller's files and the store: the board's graph
#' (`board`, set from `poller_board` once Load accepts it), why the script
#' doesn't load (`script_error`), the run's live `progress`, whether a run
#' is going (`running`), `tar_meta()` and the targets that warned.
#'
#' @param backend `board_backend()`.
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param session The Shiny session the polls belong to.
#' @return A list of those reactives.
#' @noRd
graph_data <- function(backend, metadata, session) {
  # The graph, as last recomputed by the poller: re-read only when the
  # poller rewrites its file.
  board <- shiny::reactiveVal(list(x = metadata, progress_mtime = progress_mtime()))
  poller_board <- shiny::reactivePoll(
    500, session,
    checkFunc = function() {
      path <- backend$rds_path()
      if (is.null(path)) NA else file.mtime(path)
    },
    valueFunc = function() {
      path <- backend$rds_path()
      if (is.null(path) || !file.exists(path)) return(NULL)
      tryCatch(readRDS(path), error = function(e) NULL)
    }
  )

  # A script that stops loading (a typo while editing it): say so, for as
  # long as it lasts, rather than silently keep the last graph.
  script_error <- shiny::reactivePoll(
    500, session,
    checkFunc = function() {
      path <- backend$rds_path()
      if (is.null(path)) NA else file.mtime(poller_error_path(path))
    },
    valueFunc = function() {
      path <- backend$rds_path()
      error_path <- if (!is.null(path)) poller_error_path(path)
      if (is.null(error_path) || !file.exists(error_path)) return(NULL)
      tryCatch(readLines(error_path, warn = FALSE), error = function(e) NULL)
    }
  )

  # Live statuses, from the progress file targets writes during a run:
  # cheap to check, so every half second.
  progress <- shiny::reactivePoll(
    500, session,
    checkFunc = progress_mtime,
    valueFunc = function() {
      list(
        mtime = progress_mtime(),
        data = tryCatch(targets::tar_progress(), error = function(e) NULL)
      )
    }
  )

  # Whether a run is going, whoever started it: checked twice a second.
  running <- shiny::reactivePoll(500, session, checkFunc = backend$busy, valueFunc = backend$busy)

  # Re-read tar_meta() only when targets writes new metadata; checking the
  # file's modification time every second is cheap.
  tar_meta_data <- shiny::reactivePoll(
    1000, session,
    checkFunc = function() file.mtime(store_file("meta")),
    valueFunc = function() {
      tryCatch(targets::tar_meta(targets_only = TRUE), error = function(e) NULL)
    }
  )

  # Targets whose last build raised warnings, for the warning marks.
  warned_targets <- shiny::reactive(warned_targets_of(tar_meta_data()))

  list(
    board = board,
    poller_board = poller_board,
    script_error = script_error,
    progress = progress,
    running = running,
    tar_meta = tar_meta_data,
    warned_targets = warned_targets
  )
}

# A file in the targets store's meta folder, e.g. "progress" or "meta".
store_file <- function(name) {
  file.path(targets::tar_config_get("store"), "meta", name)
}

progress_mtime <- function() file.mtime(store_file("progress"))

#' Targets whose last build raised warnings
#'
#' targets records a branch's warnings on the branch alone: its pattern
#' carries the mark.
#' @param meta `tar_meta(targets_only = TRUE)`.
#' @noRd
warned_targets_of <- function(meta) {
  if (is.null(meta) || !"warnings" %in% names(meta)) return(character())
  warned <- meta[!is.na(meta$warnings), ]
  names <- warned$name
  if (all(c("type", "parent") %in% names(warned))) {
    branch <- warned$type %in% "branch" & !is.na(warned$parent)
    names[branch] <- warned$parent[branch]
  }
  sort(unique(names))
}
