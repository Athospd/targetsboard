#' The pipeline's functions, as written: name, file, line and code
#'
#' Parsed, not run, from the files the script sources (sourced_paths()) and
#' the script itself: each top-level `name <- function(...)`, or with `=`.
#' A folder is read as tar_source() reads it: its `.R` files, recursively.
#' A later definition wins, as when sourced; the script comes last.
#' @param script Path to the targets script.
#' @return A data frame: `name`, `file`, `line`, `code`.
#' @noRd
function_definitions <- function(script) {
  none <- data.frame(name = character(), file = character(), line = integer(), code = character())
  if (is.null(script) || !file.exists(script)) return(none)
  files <- unlist(lapply(sourced_paths(script), function(path) {
    if (dir.exists(path)) {
      sort(list.files(path, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE))
    } else if (file.exists(path)) {
      path
    }
  }))
  is_definition <- function(x) {
    is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% c("<-", "=", "<<-") &&
      is.name(x[[2]]) && is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function"))
  }
  rows <- lapply(unique(c(files, script)), function(file) {
    exprs <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
    keep <- vapply(exprs, is_definition, logical(1))
    if (!any(keep)) return(NULL)
    refs <- attr(exprs, "srcref")[keep]
    data.frame(
      name = vapply(exprs[keep], function(x) as.character(x[[2]]), character(1)),
      file = file,
      line = vapply(refs, function(ref) as.integer(ref[[1]]), integer(1)),
      code = vapply(refs, function(ref) paste(as.character(ref), collapse = "\n"), character(1))
    )
  })
  out <- do.call(rbind, c(list(none), rows))
  out <- out[!duplicated(out$name, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' A function's code and where it is, in a section, or nothing
#'
#' @param fn Its row of function_definitions().
#' @param open Whether the section starts open.
#' @noRd
function_ui <- function(fn, open = FALSE) {
  if (is.null(fn) || nrow(fn) != 1) return(NULL)
  where <- sprintf("%s:%d", fn$file, fn$line)
  shiny::tags$details(
    class = "tb-command tb-function", `data-function` = fn$name, open = if (open) NA,
    shiny::tags$summary(shiny::tags$code(fn$name), " ", shiny::tags$span(class = "tb-function-where", where)),
    code_block("As written", "function", fn$code)
  )
}

#' The functions a target's reasons name, as function_ui() sections
#'
#' @param reasons The target's sentences, from outdated_reasons(): names
#'   are in backticks.
#' @param functions function_definitions().
#' @noRd
reason_functions_ui <- function(reasons, functions) {
  if (!length(reasons) || is.null(functions) || !nrow(functions)) return(NULL)
  named <- gsub("`", "", unlist(regmatches(reasons, gregexpr("`[^`]+`", reasons))), fixed = TRUE)
  named <- intersect(named, functions$name)
  if (!length(named)) return(NULL)
  shiny::tags$div(
    class = "tb-why-functions",
    lapply(named, function(name) function_ui(functions[functions$name == name, , drop = FALSE]))
  )
}
