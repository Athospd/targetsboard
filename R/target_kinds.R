#' What kind of target each one is, beyond tar_visnetwork()'s type
#'
#' The graph knows a target's type (stem, pattern, function, object). Its
#' icon also tells other kinds apart, which only its definition says:
#' `tar_manifest()`'s `format`, `iteration` and `command`. The first that
#' applies wins:
#' - a report: its command runs tarchetypes' renderer (`"quarto"`,
#'   `"rmarkdown"`);
#' - batched replications: `tar_rep()`, `tar_rep2()`, `tar_map_rep()`
#'   (`"rep"`, #157);
#' - a file: `format = "file"` (`"file"`);
#' - a URL: `format = "url"` (`"url"`);
#' - grouped data: `iteration = "group"`, as `tar_group_by()` gives
#'   (`"group"`);
#' - a storage format other than rds: `"format:<format>"`, e.g. `"format:qs"`.
#'
#' @param manifest `targets::tar_manifest()` with `name`, `format`,
#'   `iteration` and `command`, as the poller writes it, or NULL.
#' @return A named character vector, by target, for the targets of those
#'   kinds only.
#' @noRd
target_kinds <- function(manifest) {
  none <- stats::setNames(character(), character())
  if (is.null(manifest) || !nrow(manifest) || !"name" %in% names(manifest)) return(none)
  column <- function(name) if (name %in% names(manifest)) as.character(manifest[[name]]) else rep("", nrow(manifest))
  command <- column("command")
  format <- column("format")
  iteration <- column("iteration")
  kind <- rep(NA_character_, nrow(manifest))
  set <- function(which, value) kind[is.na(kind) & which] <<- value
  set(grepl("tar_quarto(_rep)?_run", command), "quarto")
  set(grepl("tar_render(_rep)?_run", command), "rmarkdown")
  set(grepl("tar_rep2?_run[(]", command), "rep")
  set(format %in% c("file", "file_fast"), "file")
  set(format %in% "url", "url")
  set(iteration %in% "group", "group")
  stored <- !is.na(format) & nzchar(format) & !format %in% c("rds", "file", "file_fast", "url")
  # tar_format()'s carries its code, encoded: named only as custom.
  format[startsWith(format, "format_custom")] <- "custom"
  kind[is.na(kind) & stored] <- paste0("format:", format[is.na(kind) & stored])
  keep <- !is.na(kind)
  stats::setNames(kind[keep], manifest$name[keep])
}

#' What a target's kind means, in a line for the Value pane (#157)
#' @param kind A target_kinds() value, or NULL.
#' @return Text, or NULL for a kind its icon says enough about.
#' @noRd
kind_hint <- function(kind) {
  if (length(kind) != 1 || is.na(kind)) return(NULL)
  if (startsWith(kind, "format:")) {
    format <- sub("^format:", "", kind)
    if (format == "custom") {
      return("Stored in a custom format (tar_format()): reading it runs the pipeline's own code.")
    }
    if (format == "torch") return("A torch model or tensor (tar_torch()): reading it needs the torch package.")
    if (format == "keras") return("A keras model (tar_keras()): reading it needs the keras package.")
    return(paste0("Stored as ", format, " (format = \"", format, "\"), not rds: reading it needs that format's package."))
  }
  switch(kind,
    rep = "Batched replications: each branch is a batch of reps, and its value has a row per rep.",
    url = "A URL: targets reruns it when the URL's ETag or last-modified time changes.",
    group = "Grouped data (iteration = \"group\"): a pattern that maps over it gets one branch per group.",
    NULL
  )
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

# targets' own defaults, as tar_option_get() gives them in a fresh session
# (targets 1.12.1); NA: none, the setting is shown only when set.
# test-target-settings.R checks them against the installed targets.
setting_defaults <- c(
  pattern = NA, iteration = "vector", memory = "auto", storage = "worker", retrieval = "auto",
  deployment = "worker", error = "stop", priority = "0", repository = "local", resources = NA
)

#' Each target's settings, as they run
#'
#' From `tar_manifest()`, which the poller reads, with `resources` as text
#' (the kinds set, and crew's controller).
#' @param manifest `targets::tar_manifest()`, with the fields of
#'   `setting_defaults`.
#' @return A data frame: `name`, then one text column per setting it has.
#' @noRd
target_settings <- function(manifest) {
  if (is.null(manifest) || !nrow(manifest) || !"name" %in% names(manifest)) return(NULL)
  fields <- intersect(names(setting_defaults), names(manifest))
  if (!length(fields)) return(NULL)
  out <- as.data.frame(manifest)[c("name", fields)]
  out[fields] <- lapply(out[fields], function(x) if (is.list(x)) NA_character_ else as.character(x))
  out
}
