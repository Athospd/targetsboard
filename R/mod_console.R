#' The Console tab: the board's actions as R code (#185)
#'
#' Every run, invalidation and value read from the board, as the call that
#' does the same in the R console, in the order of this browser session:
#' each to copy, or all of them as a script. What the board does stays
#' something the author can do, and learn to do, without it.
#'
#' @param id The module's id.
#' @noRd
console_ui <- function(id) {
  shiny::tags$div(class = "tb-console", shiny::uiOutput(shiny::NS(id, "entries")))
}

#' @rdname console_ui
#' @param entries Reactive: the log's entries, `console_log()$entries`.
#' @noRd
console_server <- function(id, entries) {
  stopifnot(shiny::is.reactive(entries))
  shiny::moduleServer(id, function(input, output, session) {
    output$entries <- shiny::renderUI(console_entries_ui(entries()))
  })
}

#' The session's log of R code
#'
#' Not a module: the board's modules add to it as they act.
#' @return A list: `entries`, a reactive of `list(time, code)` in order; and
#'   `add(code)`, which logs one, unless it repeats the last.
#' @noRd
console_log <- function() {
  entries <- shiny::reactiveVal(list())
  add <- function(code) {
    now <- shiny::isolate(entries())
    if (length(now) && identical(now[[length(now)]]$code, code)) return(invisible())
    entries(c(now, list(list(time = Sys.time(), code = code))))
  }
  list(entries = shiny::reactive(entries()), add = add)
}

# Each entry, with its time and a Copy button; Copy all on top.
console_entries_ui <- function(entries) {
  if (!length(entries)) {
    return(shiny::tags$p(
      class = "text-muted",
      "Nothing yet: what you do on the board shows here as R code, in order, to run in the console."
    ))
  }
  shiny::tagList(
    shiny::tags$div(
      class = "d-flex justify-content-between align-items-center mb-2",
      shiny::tags$span(class = "text-muted", "This session's board actions, as R code"),
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-default tb-copy tb-console-copy-all",
        `data-copy` = console_script(entries), "Copy all"
      )
    ),
    lapply(entries, function(entry) code_block(format(entry$time, "%H:%M:%S"), "console", entry$code))
  )
}

#' The session as a script: each call under its time
#' @noRd
console_script <- function(entries) {
  paste0(vapply(entries, function(entry) {
    paste0("# ", format(entry$time, "%H:%M:%S"), "\n", entry$code, "\n")
  }, character(1)), collapse = "\n")
}

# Target names as tidyselect takes them: a, or c(a, `my target`).
names_code <- function(names) {
  symbols <- vapply(names, function(name) deparse(as.symbol(name), backtick = TRUE), character(1))
  if (length(symbols) == 1) unname(symbols) else paste0("c(", paste(symbols, collapse = ", "), ")")
}

#' The call a board run makes, as start_run() makes it
#' @param names,modes,maker As start_run() and run_maker() take and give.
#' @param script The script, named when it isn't the default.
#' @noRd
run_code <- function(names = NULL, modes = list(), maker = NULL, script = "_targets.R") {
  args <- c(
    if (!is.null(names)) paste("names =", names_code(names)),
    if (!is.null(maker$workers)) paste("workers =", maker$workers),
    if (!identical(script, "_targets.R")) paste("script =", deparse(script))
  )
  call <- paste0("targets::", maker$fun %||% "tar_make", "(", paste(args, collapse = ", "), ")")
  # The modes change the targets for the run only (R/run_modes.R): no argument does that.
  if (any_run_mode(modes)) {
    call <- paste0("# Run from the board to ", run_modes_label(modes), ": it changes the targets for that run only.\n", call)
  }
  call
}

# Every target (Invalidate all) as everything().
invalidate_code <- function(names) {
  which <- if (isTRUE(attr(names, "all"))) "everything()" else names_code(names)
  paste0("targets::tar_invalidate(names = ", which, ")")
}
