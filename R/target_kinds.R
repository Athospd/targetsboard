#' What kind of target each one is, beyond tar_visnetwork()'s type
#'
#' The graph knows a target's type (stem, pattern, function, object). Its
#' icon also tells files and reports apart, which only its definition says:
#' `tar_manifest()`'s `format` and `command`. A report's command runs
#' tarchetypes' renderer; a file target has `format = "file"`.
#'
#' @param manifest `targets::tar_manifest(fields = c("name", "format",
#'   "command"))`, as the poller writes it, or NULL.
#' @return A named character vector, by target: `"quarto"`, `"rmarkdown"` or
#'   `"file"`, for the targets of those kinds only.
#' @noRd
target_kinds <- function(manifest) {
  none <- stats::setNames(character(), character())
  if (is.null(manifest) || !nrow(manifest) || !"name" %in% names(manifest)) return(none)
  command <- if ("command" %in% names(manifest)) manifest$command else rep("", nrow(manifest))
  format <- if ("format" %in% names(manifest)) manifest$format else rep("", nrow(manifest))
  kind <- ifelse(
    grepl("tar_quarto(_rep)?_run", command), "quarto",
    ifelse(
      grepl("tar_render(_rep)?_run", command), "rmarkdown",
      ifelse(format %in% c("file", "file_fast"), "file", NA_character_)
    )
  )
  keep <- !is.na(kind)
  stats::setNames(kind[keep], manifest$name[keep])
}

#' Each target's command, as it runs
#'
#' From `tar_manifest()`, which the poller reads: the command after
#' metaprogramming (`tar_map()`, `tar_eval()`, `tar_combine()`, hooks), not
#' always what the author wrote.
#' @param manifest `targets::tar_manifest(fields = c("name", ..., "command"))`.
#' @return A named character vector: target name to command.
#' @noRd
target_commands <- function(manifest) {
  none <- stats::setNames(character(), character())
  if (is.null(manifest) || !nrow(manifest) || !all(c("name", "command") %in% names(manifest))) return(none)
  keep <- !is.na(manifest$command) & nzchar(manifest$command)
  stats::setNames(as.character(manifest$command[keep]), manifest$name[keep])
}
