#' Where a target or a function is defined (#189)
#'
#' Reads the script and the files in the `R/` folder beside it, never
#' running them. A target is the `tar_*()` call that names it first (or with
#' `name = `), as `tar_target()` and tarchetypes' factories do; a function,
#' its `name <- function` or `name = function`. A copy that `tar_map()`
#' makes, `model_1`, has no call of its own: it gets its template's, the
#' longest target name it starts with followed by `_`.
#'
#' @param name A target's or a function's name.
#' @param script The targets script.
#' @return `list(file, line)`, the file's full path; NULL when it can't tell.
#' @noRd
definition_location <- function(name, script) {
  if (!file.exists(script)) return(NULL)
  r_dir <- file.path(dirname(script), "R")
  files <- c(script, list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE, recursive = TRUE))
  found <- do.call(rbind, lapply(files, definitions_in))
  if (is.null(found) || !nrow(found)) return(NULL)
  hit <- found[found$name == name, , drop = FALSE]
  if (!nrow(hit)) {
    templates <- found[found$kind == "target" & startsWith(name, paste0(found$name, "_")), , drop = FALSE]
    hit <- templates[order(-nchar(templates$name)), , drop = FALSE]
  }
  if (!nrow(hit)) return(NULL)
  list(file = normalizePath(hit$file[1], winslash = "/"), line = hit$line[1])
}

# The targets and functions one file defines: name, kind, line.
definitions_in <- function(file) {
  exprs <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
  data <- if (!is.null(exprs)) utils::getParseData(exprs, includeText = TRUE)
  if (is.null(data) || !nrow(data)) return(NULL)
  data <- data[order(data$line1, data$col1), ]
  children <- function(id) data[data$parent == id, , drop = FALSE]
  parent_of <- function(id) data$parent[data$id == id]
  unquote <- function(text) gsub("^[\"'`]|[\"'`]$", "", text)
  rows <- list()

  # tar_*(name, ...) or tar_*(name = name, ...).
  for (id in data$id[data$token == "SYMBOL_FUNCTION_CALL" & grepl("^tar_", data$text)]) {
    call <- parent_of(parent_of(id))
    args <- children(call)
    args <- args[seq_len(nrow(args)) > match("'('", args$token), , drop = FALSE]
    named <- which(args$token == "SYMBOL_SUB" & args$text == "name")
    first <- if (length(named)) {
      args[named[1] + 2, , drop = FALSE]
    } else if (nrow(args) && args$token[1] == "expr") {
      args[1, , drop = FALSE]
    }
    if (is.null(first) || !nrow(first) || !nzchar(first$text)) next
    rows[[length(rows) + 1]] <- data.frame(name = unquote(first$text), kind = "target", line = data$line1[data$id == call])
  }

  # name <- function(...) or name = function(...).
  for (id in data$id[data$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")]) {
    sides <- children(parent_of(id))
    sides <- sides[sides$token == "expr", , drop = FALSE]
    if (nrow(sides) != 2 || !"FUNCTION" %in% children(sides$id[2])$token) next
    rows[[length(rows) + 1]] <- data.frame(name = unquote(sides$text[1]), kind = "function", line = sides$line1[1])
  }
  if (!length(rows)) return(NULL)
  cbind(do.call(rbind, rows), file = file)
}

#' How to open a file at a line in the IDE this session runs in, or NULL
#'
#' RStudio and Positron through rstudioapi (Positron's console answers
#' `isAvailable()` too); VS Code through its `code` command. Only in an
#' interactive session: a script started from an IDE's terminal inherits
#' its variables, `POSITRON` or `TERM_PROGRAM`, but has no IDE (#263).
#' Without one (a plain browser and terminal), there is none.
#' @noRd
editor_opener <- function() {
  if (!interactive()) return(NULL)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(function(file, line) rstudioapi::navigateToFile(file, line = line))
  }
  code <- Sys.which("code")
  if (identical(Sys.getenv("TERM_PROGRAM"), "vscode") && nzchar(code)) {
    return(function(file, line) system2(code, c("-g", shQuote(paste0(file, ":", line))), wait = FALSE))
  }
  NULL
}

#' The board's way to open a definition: through the bridge to the session
#' that started it (tar_board()), or the IDE of its own session; NULL hides
#' the menu's "Open in editor".
#' @noRd
board_editor <- function() {
  requests <- getOption("targetsboard.editor")
  if (!is.null(requests)) return(function(file, line) request_editor(requests, file, line))
  editor_opener()
}

#' The bridge from the board to the editor
#'
#' tar_board() runs the board in a background process, which can't reach
#' the IDE. The board leaves its request in a file; the session that started
#' it, which has the IDE, checks the file twice a second while idle (later)
#' and opens what it asks.
#'
#' @param requests The request file's path.
#' @param open `function(file, line)`, as editor_opener() gives.
#' @param alive Whether the board still runs: the bridge stops with it.
#' @return A function that stops the bridge.
#' @noRd
editor_bridge <- function(requests, open, alive) {
  active <- TRUE
  tick <- function() {
    # A board whose process can't be asked (#263) is no longer running.
    if (!active || !isTRUE(tryCatch(alive(), error = function(e) FALSE))) return(invisible())
    if (file.exists(requests)) {
      request <- tryCatch(readRDS(requests), error = function(e) NULL)
      unlink(requests)
      if (!is.null(request)) tryCatch(open(request$file, request$line), error = function(e) NULL)
    }
    later::later(tick, 0.5)
  }
  later::later(tick, 0.5)
  function() active <<- FALSE
}

# Written whole, then renamed: the bridge never reads half a request.
request_editor <- function(requests, file, line) {
  partial <- paste0(requests, ".partial")
  saveRDS(list(file = file, line = line), partial)
  file.rename(partial, requests)
  invisible()
}
