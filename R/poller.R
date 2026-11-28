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
#' After the graph, the process writes why its outdated targets are outdated
#' to `poller_reasons_path(tar_vis_path)` (outdated_reasons()): the cues of
#' `tar_sitrep()`, and `tar_network(targets_only = FALSE)` to name what
#' changed. They hash the pipeline again, so they come after the graph, which
#' doesn't wait for them. With them goes `manifest`, each target's format and
#' command (`tar_manifest()`), for its icon (target_kinds()), and its
#' settings (target_settings()). The three run in one process of their own
#' (poller_why()).
#'
#' @param script Path to the targets script.
#' @param interval Seconds between two checks; checks are cheap file stats.
#' @param settle Seconds without progress before a run counts as ended.
#' @param filters More arguments to [targets::tar_visnetwork()], from
#'   visnetwork_filters(): values, or expressions kept for tidyselect.
#' @param ... Passed on to [targets::tar_visnetwork()].
#' @return A list: the `process` ([callr::r_bg()]), `tar_vis_path` and
#'   `refresh_path`.
#' @noRd
tar_visnetwork_bg <- function(script, interval = 0.5, settle = 1, filters = list(), ...) {
  tar_vis_path <- tempfile("tar_visnetwork_", fileext = ".rds")
  refresh_path <- sub("[.]rds$", ".refresh", tar_vis_path)

  # Runs in the background process: self-contained, it can't call this
  # package's internals.
  tar_visnetwork_instance <- function(script, tar_vis_path, refresh_path, interval, settle, filters, sourced_paths,
                                      active_run, why, ...) {
    progress_file <- file.path(targets::tar_config_get("store"), "meta", "progress")

    # What the script sources (sourced_paths()), read again when it changes;
    # their folders are listed every time, so a new file there counts too.
    sourced <- list(mtime = NULL, paths = "R")
    code_signature <- function() {
      mtime <- file.mtime(script)
      if (!identical(mtime, sourced$mtime)) sourced <<- list(mtime = mtime, paths = sourced_paths(script))
      listed <- lapply(sourced$paths, function(path) {
        if (dir.exists(path)) list.files(path, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
        else if (file.exists(path)) path
      })
      files <- c(script, sort(unique(unlist(listed))))
      list(
        files = files,
        mtimes = file.mtime(files),
        refresh = if (file.exists(refresh_path)) readLines(refresh_path, warn = FALSE)
      )
    }
    # Is the pipeline process targets recorded still running? A killed run
    # leaves targets marked dispatched for good.
    running <- function() !is.null(active_run())

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
          do.call(targets::tar_visnetwork, c(list(script = script, ...), filters)),
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
        if (!is.null(widget) && written) {
          reasons_path <- sub("[.]rds$", "_reasons.rds", tar_vis_path) # poller_reasons_path()
          # Why targets are outdated, and the manifest: one process for the
          # three, not one each (poller_why()).
          found <- tryCatch(callr::r(why, args = list(script = script)), error = function(e) {
            list(errors = c(why = conditionMessage(e)))
          })
          for (error in found$errors) message(error)
          reasons <- found$reasons
          manifest <- found$manifest
          if (!is.null(reasons)) reasons$progress_mtime <- progress_mtime
          if (!is.null(reasons) || !is.null(manifest)) {
            if (is.null(reasons)) reasons <- list(progress_mtime = progress_mtime)
            reasons$manifest <- manifest
            reasons$cloud <- found$cloud
            reasons$controllers <- found$controllers
            tmp <- paste0(reasons_path, ".tmp")
            saveRDS(reasons, file = tmp)
            if (!file.rename(tmp, reasons_path)) unlink(tmp)
          }
        }
      }
      Sys.sleep(interval)
    }
  }

  bg_process <- callr::r_bg(
    tar_visnetwork_instance,
    args = list(
      script = script, tar_vis_path = tar_vis_path, refresh_path = refresh_path,
      interval = interval, settle = settle, filters = filters, sourced_paths = sourced_paths,
      active_run = active_run, why = poller_why, ...
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = sub("[.]rds$", ".log", tar_vis_path),
    stderr = "2>&1"
  )
  invisible(list(process = bg_process, tar_vis_path = tar_vis_path, refresh_path = refresh_path))
}

#' Why targets are outdated, and the manifest, for the poller
#'
#' What outdated_reasons() reads: `tar_sitrep()`, and `tar_network()` to name
#' what changed; and `tar_manifest()`, each target's format, command, cue
#' and settings (target_kinds(), skipped_reasons(), target_settings()). The
#' poller runs it in a process of its own, where the three load the script
#' in turn (`callr_function = NULL`) instead of starting a process each. Each part fails on its own. Base R
#' only, in `baseenv()`: it runs in another process.
#' @param script Path to the targets script.
#' @return A list: `reasons` (`sitrep`, `vertices`, `edges`, `parents`) and
#'   `manifest`, each `NULL` if it failed; `cloud`, where the metadata lives
#'   when in a cloud bucket (`repository`, `bucket`, `prefix`), else `NULL`
#'   (#111); `controllers`, crew's controller names, the default first, or
#'   `NULL` (#137); and `errors`, the messages.
#' @noRd
poller_why <- function(script) {
  # The script runs in the global environment, as in tar_make(), emptied
  # before each read. In an environment of its own, a global holding a
  # closure (torch's nn_module(), an R6 class) hashes anew on each read, and
  # what uses it looks outdated.
  fresh <- function() {
    rm(list = ls(globalenv(), all.names = TRUE), envir = globalenv())
    globalenv()
  }
  targets::tar_option_reset() # options from a script read before don't carry over
  errors <- character()
  reasons <- tryCatch({
    network <- targets::tar_network(script = script, targets_only = FALSE, callr_function = NULL, envir = fresh())
    meta <- targets::tar_meta(fields = "parent", targets_only = TRUE)
    list(
      sitrep = as.data.frame(targets::tar_sitrep(script = script, callr_function = NULL, envir = fresh())),
      vertices = as.data.frame(network$vertices[, c("name", "type", "status")]),
      edges = as.data.frame(network$edges),
      parents = as.data.frame(meta[!is.na(meta$parent), c("name", "parent")])
    )
  }, error = function(e) {
    errors <<- c(errors, paste("Why outdated failed:", conditionMessage(e)))
    NULL
  })
  manifest <- tryCatch(
    as.data.frame(targets::tar_manifest(script = script, callr_function = NULL, envir = fresh(), fields = c(
      "name", "format", "command", "cue_mode", "cue_command", "cue_depend",
      "cue_file", "cue_format", "cue_repository", "cue_iteration",
      "pattern", "iteration", "memory", "storage", "retrieval", "deployment",
      "error", "priority", "repository", "resources"
    ))),
    error = function(e) {
      errors <<- c(errors, paste("tar_manifest() failed:", conditionMessage(e)))
      NULL
    }
  )
  # Resources as text, the kinds set and crew's controller: the objects
  # themselves are environments, not for the app.
  if (!is.null(manifest) && "resources" %in% names(manifest)) {
    manifest$resources <- vapply(manifest$resources, function(r) {
      if (!length(r)) return(NA_character_)
      kinds <- names(r)
      controller <- tryCatch(r$crew$controller, error = function(e) NULL)
      if (is.character(controller) && length(controller) == 1) {
        kinds[kinds == "crew"] <- sprintf("crew (controller %s)", controller)
      }
      paste(kinds, collapse = ", ")
    }, character(1))
  }
  # The script's options, set when tar_manifest() ran it above.
  cloud <- tryCatch({
    repository <- targets::tar_option_get("repository_meta")
    if (is.character(repository) && length(repository) == 1 && repository %in% c("aws", "gcp")) {
      where <- targets::tar_option_get("resources")[[repository]]
      list(repository = repository, bucket = where$bucket, prefix = where$prefix)
    }
  }, error = function(e) NULL)
  # crew's controllers, by name, the default first: tar_manifest() loaded the
  # script here, so its tar_option_set() holds (#137). NULL without crew.
  controller <- tryCatch(targets::tar_option_get("controller"), error = function(e) NULL)
  controllers <- if (inherits(controller, "crew_class_controller_group")) {
    names(controller$controllers)
  } else if (inherits(controller, "crew_class_controller")) {
    controller$launcher$name
  }
  list(reasons = reasons, manifest = manifest, cloud = cloud, controllers = controllers, errors = errors)
}
environment(poller_why) <- baseenv()

# Where the poller writes why its last recompute failed: see tar_visnetwork_bg().
poller_error_path <- function(tar_vis_path) sub("[.]rds$", ".error", tar_vis_path)

# Where the poller writes why outdated targets are: see tar_visnetwork_bg().
poller_reasons_path <- function(tar_vis_path) sub("[.]rds$", "_reasons.rds", tar_vis_path)

#' Why the script doesn't load, for the board's notification
#'
#' @param message The error message the poller wrote, or `NULL` when it loads.
#' @return A notification's text, or `NULL`.
#' @noRd
script_error_text <- function(script, message) {
  if (is.null(message) || !length(message)) return(NULL)
  message <- cli::ansi_strip(paste(message, collapse = " "))
  if (nchar(message) > 300) message <- paste0(substr(message, 1, 297), "...")
  paste0(
    script, " doesn't load, so the board still shows its last working graph. ",
    "The error: ", message
  )
}

#' The files and folders a targets script sources
#'
#' The literal paths of its `tar_source()` and `source()` calls, anywhere in
#' it, and `R/`, `tar_source()`'s default: the poller redraws when a `.R`
#' file in them changes. A path built at run time (`file.path(dir, "f.R")`)
#' isn't read. Base R only, in `baseenv()`: it runs in the poller, another
#' process, where this package's internals aren't loaded.
#' @param script Path to the targets script.
#' @return A character vector of paths, `"R"` first.
#' @noRd
sourced_paths <- function(script) {
  exprs <- tryCatch(parse(script, keep.source = FALSE), error = function(e) expression(), warning = function(w) expression())
  found <- character()
  # The literal strings of an argument: "a", or c("a", "b").
  literal <- function(arg) {
    if (is.character(arg)) return(arg)
    if (is.call(arg) && identical(arg[[1]], as.name("c"))) {
      parts <- as.list(arg)[-1]
      if (length(parts) && all(vapply(parts, is.character, logical(1)))) return(unlist(parts))
    }
    character()
  }
  visit <- function(x) {
    if (!is.call(x)) return(invisible())
    fn <- x[[1]]
    name <- if (is.name(fn)) {
      as.character(fn)
    } else if (is.call(fn) && length(fn) == 3 && is.name(fn[[1]]) && as.character(fn[[1]]) %in% c("::", ":::")) {
      as.character(fn[[3]])
    } else {
      ""
    }
    # Its arguments without the empty ones, as drop_empty() (#283): this
    # runs in the background process, which can't call it.
    given <- function(args) {
      args[vapply(seq_along(args), function(i) !identical(args[[i]], quote(expr = )), logical(1))]
    }
    # tar_tangle(path): a pipeline written as a document (#136).
    if (name == "tar_tangle") {
      args <- given(as.list(x)[-1])
      named <- if (is.null(names(args))) rep("", length(args)) else names(args)
      arg <- if ("path" %in% named) args[["path"]] else if (length(args) && !nzchar(named[[1]])) args[[1]]
      if (!is.null(arg)) found <<- c(found, literal(arg))
    }
    if (name %in% c("tar_source", "source", "sys.source")) {
      args <- given(as.list(x)[-1])
      named <- if (is.null(names(args))) rep("", length(args)) else names(args)
      arg <- if ("files" %in% named) {
        args[["files"]]
      } else if ("file" %in% named) {
        args[["file"]]
      } else if (length(args) && !nzchar(named[[1]])) {
        args[[1]]
      }
      found <<- c(found, if (is.null(arg) && name == "tar_source") "R" else if (!is.null(arg)) literal(arg))
    }
    for (part in as.list(x)[-1]) if (!missing(part)) visit(part)
  }
  for (expr in exprs) visit(expr)
  unique(c("R", found))
}
environment(sourced_paths) <- baseenv()
