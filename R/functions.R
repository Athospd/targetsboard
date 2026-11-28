#' The pipeline's functions, or its other globals, as written: name, file,
#' line and code
#'
#' Parsed, not run, from the files the script sources (sourced_paths()) and
#' the script itself: each top-level `name <- function(...)`, or with `=`;
#' with `functions = FALSE`, each other top-level `name <- value` (#305).
#' A folder is read as tar_source() reads it: its `.R` files, recursively.
#' A later definition wins, as when sourced; the script comes last.
#' @param script Path to the targets script.
#' @param functions Functions, or the other globals.
#' @return A data frame: `name`, `file`, `line`, `code`.
#' @noRd
function_definitions <- function(script, functions = TRUE) {
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
      is.name(x[[2]]) && identical(is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")), functions)
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

#' A global object, as the script writes it, in a section: where it is, the
#' targets that use it, and its hash as targets recorded it (#305)
#'
#' Its value isn't read: the board never runs the script in its own process.
#' The code that loads it in the R session comes with it.
#' @param def Its row of `function_definitions(script, functions = FALSE)`,
#'   or none.
#' @param name Its name.
#' @param users The targets that use it.
#' @param hash Its hash in the metadata, or NULL.
#' @param value Its value from the poller (poller_why()): `class`, `bytes`
#'   and `printed`, or `error`; NULL until the poller has read the script.
#' @noRd
global_ui <- function(def, name, users = character(), hash = NULL, value = NULL) {
  value_part <- if (!is.null(value$error)) {
    shiny::tags$p(class = "text-muted", paste0("Its value couldn't be printed: ", value$error))
  } else if (!is.null(value$printed)) {
    shiny::tags$details(
      class = "tb-command tb-global-value", open = NA,
      shiny::tags$summary(
        "Its value ",
        shiny::tags$span(class = "tb-function-where", paste0("(a ", value$class, ", ", format_bytes(value$bytes), ")"))
      ),
      shiny::tags$pre(class = "tb-preview-printed", value$printed),
      shiny::tags$p(class = "text-muted small", "As the script made it, when the board last read the script.")
    )
  }
  facts <- shiny::tagList(
    if (length(users)) shiny::tags$p(paste0("Used by ", describe_names(users), ".")),
    if (length(hash) && !is.na(hash)) shiny::tags$p(class = "text-muted", "Its hash, as targets recorded it: ", shiny::tags$code(hash)),
    shiny::tags$p(
      class = "text-muted",
      "A global of the pipeline: targets::tar_load_globals() loads it, with the others, in your R session.",
      copy_code_button("targets::tar_load_globals()")
    )
  )
  if (is.null(def) || nrow(def) != 1) {
    return(shiny::tagList(value_part, shiny::p("Its definition wasn't found in the script or the files it sources."), facts))
  }
  shiny::tagList(
    shiny::tags$details(
      class = "tb-command tb-function tb-global", `data-global` = name, open = NA,
      shiny::tags$summary(shiny::tags$code(name), " ", shiny::tags$span(class = "tb-function-where", sprintf("%s:%d", def$file, def$line))),
      code_block("As written", "global", def$code)
    ),
    value_part,
    facts
  )
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
