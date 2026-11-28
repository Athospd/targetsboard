#' The Metadata tab: tar_meta()'s table
#'
#' `tar_meta()` as a table. A click on a row asks to show its target on the
#' board.
#'
#' @param id The module's id.
#' @noRd
meta_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("leftovers")),
    reactable::reactableOutput(ns("table"))
  )
}

#' @rdname meta_ui
#' @param tar_meta `tar_meta(targets_only = TRUE)`: a reactive.
#' @param script_targets Reactive: the script's targets, for the stored ones
#'   it no longer has (leftover_targets()); `NULL` until known.
#' @return The name of the target whose row was clicked, a reactive that
#'   fires on every click, even on the same row twice.
#' @noRd
meta_server <- function(id, tar_meta, script_targets = shiny::reactive(NULL), store_others = shiny::reactive(character())) {
  stopifnot(shiny::is.reactive(tar_meta), shiny::is.reactive(script_targets), shiny::is.reactive(store_others))
  shiny::moduleServer(id, function(input, output, session) {
    # Stored targets the script no longer has (#112).
    # Once per change of the leftovers, not on every metadata write (#243).
    leftovers <- shiny::reactiveVal(NULL)
    shiny::observe(leftovers(leftover_targets(tar_meta(), script_targets())), priority = 1)
    output$leftovers <- shiny::renderUI(leftovers_ui(leftovers(), store_others()))

    output$table <- reactable::renderReactable({
      shiny::req(tar_meta())
      meta <- tar_meta()
      reactable::reactable(
        meta,
        theme = board_table_theme(),
        # Readable times and sizes, sorted as numbers: slowest first.
        columns = meta_columns(meta),
        defaultSorted = if ("seconds" %in% names(meta)) list(seconds = "desc"),
        wrap = FALSE,
        onClick = row_click_js(session$ns("row_clicked")),
        rowStyle = list(cursor = "pointer")
      )
    })

    shiny::reactive(input$row_clicked)
  })
}

# The tar_meta() table's seconds and bytes, shown as "42 s" and "1.2 MB".
meta_columns <- function(meta) {
  columns <- list()
  if ("seconds" %in% names(meta)) {
    columns$seconds <- reactable::colDef(
      cell = function(value) if (is.na(value)) "" else format_seconds(value),
      align = "right", defaultSortOrder = "desc"
    )
  }
  if ("bytes" %in% names(meta)) {
    columns$bytes <- reactable::colDef(
      cell = function(value) format_bytes(value) %||% "",
      align = "right", defaultSortOrder = "desc"
    )
  }
  if (length(columns)) columns else NULL
}

#' Stored targets the script no longer has
#'
#' They stay in the store until `tar_prune()` or `tar_delete()`, unseen on
#' the board. A pattern's branches count with it.
#' @param meta `tar_meta(targets_only = TRUE)`.
#' @param targets The script's targets (`tar_manifest()`'s names), or
#'   `NULL` when unknown: then there are none to tell.
#' @return A data frame: `name` and `bytes`, its branches' included,
#'   biggest first.
#' @noRd
leftover_targets <- function(meta, targets) {
  none <- data.frame(name = character(), bytes = numeric())
  if (is.null(meta) || !nrow(meta) || is.null(targets) || !"name" %in% names(meta)) return(none)
  parent <- if ("parent" %in% names(meta)) meta$parent else rep(NA_character_, nrow(meta))
  top <- ifelse(is.na(parent), meta$name, parent)
  gone <- !top %in% targets
  if (!any(gone)) return(none)
  bytes <- if ("bytes" %in% names(meta)) meta$bytes else rep(NA_real_, nrow(meta))
  sums <- tapply(bytes[gone], top[gone], function(b) sum(b, na.rm = TRUE))
  out <- data.frame(name = names(sums), bytes = as.numeric(sums))
  out <- out[order(-out$bytes, out$name), , drop = FALSE] # the biggest named first
  rownames(out) <- NULL
  out
}

#' A notice of the stored targets the script no longer has, or nothing
#'
#' With the code that removes them, to copy: the board doesn't prune.
#' @param leftovers leftover_targets().
#' @param others The other scripts loaded on this store this session (#245).
#' @noRd
leftovers_ui <- function(leftovers, others = character()) {
  n <- nrow(leftovers)
  if (!n) return(NULL)
  shiny::tags$div(
    class = "alert alert-secondary tb-leftovers d-flex align-items-center gap-2",
    role = "status",
    shiny::tags$span(sprintf(
      "%d stored target%s %s no longer in the script, %s: %s. tar_prune() removes %s from the store.",
      n, if (n == 1) "" else "s", if (n == 1) "is" else "are",
      format_bytes(sum(leftovers$bytes)) %||% "size unknown",
      describe_names(leftovers$name), if (n == 1) "it" else "them"
    ), if (length(others)) {
      paste0(" They may be another pipeline's: this store also held ", and_list(others), "'s targets this session.")
    }),
    copy_code_button("targets::tar_prune()")
  )
}
