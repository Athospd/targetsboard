#' How long each target took to build, and how big its value is
#'
#' From `tar_meta()`: `seconds`, the last build's time, and `bytes`, the
#' stored value's size (targets doesn't record memory). A pattern's are
#' summed over its branches. The slowest and the largest tenth of the built
#' targets, at least one each, are marked, when there are three or more.
#'
#' @param meta `targets::tar_meta(targets_only = TRUE)`, or NULL.
#' @return A named list, by target (stems and patterns): `time` and `size`
#'   as shown ("42 s", "1.2 MB"), `title` for hovering, and `slow`, `large`.
#' @noRd
target_costs <- function(meta) {
  none <- json_object()
  if (is.null(meta) || !nrow(meta) || !all(c("name", "type", "seconds", "bytes") %in% names(meta))) return(none)
  built <- meta[meta$type %in% c("stem", "pattern") & !is.na(meta$seconds), , drop = FALSE]
  if (!nrow(built)) return(none)
  slow <- top_tenth(built$seconds)
  large <- top_tenth(built$bytes)
  stats::setNames(lapply(seq_len(nrow(built)), function(i) {
    time <- format_seconds(built$seconds[i])
    size <- format_bytes(built$bytes[i]) %||% NA_character_
    list(
      time = time,
      size = size,
      title = paste0(
        "Built in ", time, if (!is.na(size)) paste0(" \u00b7 stored ", size),
        if (built$type[i] == "pattern") " (all its branches)"
      ),
      slow = slow[i],
      large = large[i]
    )
  }), built$name)
}

# The largest tenth of `x`, at least one, when there are three or more
# values; else none. A 0 is never marked.
top_tenth <- function(x) {
  x[is.na(x)] <- 0
  if (length(x) < 3) return(rep(FALSE, length(x)))
  n <- max(1, floor(length(x) / 10))
  x > 0 & rank(-x, ties.method = "first") <= n
}

# "0.3 s", "4.2 s", "42 s", "3m 05s", "1h 02m".
format_seconds <- function(seconds) {
  if (is.na(seconds)) return(NA_character_)
  if (seconds < 0.05) return("<0.1 s")
  if (seconds < 10) return(sprintf("%.1f s", seconds))
  if (seconds < 60) return(sprintf("%d s", as.integer(round(seconds))))
  s <- round(seconds)
  if (s < 3600) return(sprintf("%dm %02ds", s %/% 60, s %% 60))
  sprintf("%dh %02dm", s %/% 3600, (s %% 3600) %/% 60)
}

#' The run's time left, from the targets' last builds
#'
#' The last build times of the outdated targets the run hasn't reached and
#' of those running. A pattern that has started counts the share of its
#' branches not done yet. An estimate: a target's next build can differ.
#'
#' @param progress The run's progress (tar_progress(), with type and parent).
#' @param to_go The outdated targets with no progress yet.
#' @param meta `tar_meta(targets_only = TRUE)`, or NULL.
#' @return `list(seconds, unknown)`: the estimate, and how many targets
#'   counted have never been built (so it is a lower bound); NULL with no
#'   metadata.
#' @noRd
time_left <- function(progress, to_go, meta) {
  if (is.null(meta) || !all(c("name", "seconds") %in% names(meta))) return(NULL)
  # Before any metadata, tar_meta()'s empty table has character columns.
  last <- stats::setNames(suppressWarnings(as.numeric(meta$seconds)), meta$name)
  top <- progress[!progress$type %in% "branch", , drop = FALSE]
  running <- top$name[top$progress %in% "dispatched"]
  # Patterns: dispatched themselves, or with a branch still running.
  branches <- progress[progress$type %in% "branch", , drop = FALSE]
  patterns <- unique(c(top$name[top$type %in% "pattern" & top$progress %in% "dispatched"],
                       branches$parent[branches$progress %in% "dispatched"]))
  stems <- setdiff(running, patterns)
  known <- function(names) names[names %in% names(last) & !is.na(last[names])]
  seconds <- sum(last[known(c(to_go, stems))])
  children <- if ("children" %in% names(meta)) stats::setNames(meta$children, meta$name)
  for (p in known(patterns)) {
    total <- length(children[[p]] %||% character())
    done <- sum(branches$parent %in% p & branches$progress %in% c("completed", "skipped"))
    share <- if (total > 0) max(0, 1 - done / total) else 1
    seconds <- seconds + last[[p]] * share
  }
  list(seconds = seconds, unknown = length(setdiff(c(to_go, stems, patterns), known(c(to_go, stems, patterns)))))
}
