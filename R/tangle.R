# Pipelines written as Quarto or R Markdown documents (#136).
# tarchetypes::tar_tangle() makes a target of each `name <- command` (or
# `name = command`) at the top of the document's R chunks, its chunk options
# the target's settings. The board watches such a document as it watches
# sourced files (sourced_paths()), opens a target at its line in it, and
# loads a document from the script bar through a script that tangles it.

#' Whether a path is a document tar_tangle() reads
#' @noRd
is_tangle_document <- function(path) tolower(tools::file_ext(path)) %in% c("qmd", "rmd")

#' The targets of a tangled document and their lines, as definitions_in()
#' gives them, read without parsermd
#' @return A data frame of name, kind ("target") and line, or NULL.
#' @noRd
tangle_definitions <- function(file) {
  if (!file.exists(file)) return(NULL)
  lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) return(NULL)
  opens <- grep("^[[:space:]]*```+[[:space:]]*[{][Rr]([[:space:],}])", lines)
  closes <- grep("^[[:space:]]*```+[[:space:]]*$", lines)
  rows <- lapply(opens, function(open) {
    close <- closes[closes > open][1]
    if (is.na(close) || close == open + 1) return(NULL)
    exprs <- tryCatch(parse(text = lines[(open + 1):(close - 1)], keep.source = TRUE), error = function(e) NULL)
    if (!length(exprs)) return(NULL)
    keep <- vapply(exprs, function(x) {
      is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% c("<-", "=") && is.name(x[[2]])
    }, logical(1))
    if (!any(keep)) return(NULL)
    data.frame(
      name = vapply(exprs[keep], function(x) as.character(x[[2]]), character(1)),
      kind = "target",
      line = open + vapply(attr(exprs, "srcref")[keep], function(ref) as.integer(ref[[1]]), integer(1))
    )
  })
  do.call(rbind, rows)
}

#' A script that tangles a document, for the board to load: in the session's
#' temporary folder, the same for the same document
#' @return The script's path.
#' @noRd
tangle_script <- function(document) {
  document <- normalizePath(document, winslash = "/")
  name <- gsub("[^A-Za-z0-9_]+", "_", tools::file_path_sans_ext(basename(document)))
  script <- file.path(tempdir(), paste0("targetsboard_tangle_", name, "_", substr(rlang::hash(document), 1, 8), ".R"))
  code <- c(
    paste0("# targetsboard's script for ", document, ": tar_tangle() makes a target"),
    "# of each `name <- command` in its R chunks.",
    "library(targets)",
    paste0("tarchetypes::tar_tangle(", deparse(document), ")")
  )
  if (!file.exists(script) || !identical(readLines(script, warn = FALSE), code)) writeLines(code, script)
  normalizePath(script, winslash = "/")
}

#' The document a tangle_script() tangles, or NULL for any other script
#' @noRd
tangle_document <- function(script) {
  if (!startsWith(basename(script), "targetsboard_tangle_")) return(NULL)
  if (!file.exists(script)) return(NULL)
  first <- readLines(script, n = 1, warn = FALSE)
  prefix <- "# targetsboard's script for "
  if (!length(first) || !startsWith(first, prefix)) return(NULL)
  document <- sub(": tar_tangle[(][)] makes a target$", "", substring(first, nchar(prefix) + 1))
  # Relative to the board's folder, as a script in the bar.
  here <- paste0(normalizePath(getwd(), winslash = "/"), "/")
  if (startsWith(document, here)) substring(document, nchar(here) + 1) else document
}
