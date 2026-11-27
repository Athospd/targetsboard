#' A pattern's branches, to unfold it on the canvas (#147)
#'
#' Each branch's status comes from the live progress when it has one (a run
#' going, or the last run's), else from its metadata: an error is errored,
#' a record is up to date. The failing come first, then the running, then
#' the rest, each in branch order; past `cap`, only their number.
#'
#' @param pattern The pattern's name.
#' @param meta `targets::tar_meta()`, with `name`, `type`, `children` and
#'   `error`, or NULL.
#' @param progress `targets::tar_progress()` with `name` and `progress`, or
#'   NULL.
#' @param cap The most branches to list.
#' @return `list(pattern, branches, more)`: `branches` a list of
#'   `list(name, position, status)`, with status a node type (uptodate,
#'   outdated, errored, running); `more` how many were left out.
#' @noRd
pattern_branches <- function(pattern, meta, progress = NULL, cap = 50L) {
  none <- list(pattern = pattern, branches = list(), more = 0L)
  row <- if (!is.null(meta)) meta[meta$name %in% pattern & meta$type %in% "pattern", , drop = FALSE]
  if (is.null(row) || !nrow(row)) return(none)
  names <- row$children[[1]]
  names <- names[!is.na(names)]
  if (!length(names)) return(none)
  errors <- meta$error[match(names, meta$name)]
  status <- ifelse(is.na(match(names, meta$name)), "outdated", ifelse(is.na(errors), "uptodate", "errored"))
  if (!is.null(progress) && nrow(progress)) {
    live <- progress$progress[match(names, progress$name)]
    status[!is.na(live)] <- status_to_node_type(live[!is.na(live)])
  }
  order <- order(factor(status, c("errored", "running", "outdated", "uptodate")), seq_along(names))
  shown <- utils::head(order, cap)
  list(
    pattern = pattern,
    branches = lapply(shown, function(i) list(name = names[[i]], position = i, status = status[[i]])),
    more = length(names) - length(shown)
  )
}
