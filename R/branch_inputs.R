#' Which slices of which upstream targets a branch ran on
#'
#' `tar_branches()` names, for each branch of a pattern, the branch or slice
#' of each target it maps: a hash for a slice of a stem, like the branch's
#' own. `tar_meta()` keeps every target's `children` in order, so a name's
#' place there is the slice's number.
#' @param branches `tar_branches(name)`: a data frame, the pattern's own
#'   column first, then one per upstream target.
#' @param branch The branch's name.
#' @param meta `tar_meta(fields = c("type", "children", "bytes"))`.
#' @return A list per upstream target: `input`, `type` (`"pattern"` or
#'   `"stem"`), `index` and `n`, the slice's number and how many there are,
#'   and `bytes`, what reading it takes (the branch, or the whole stem);
#'   `NULL` if the branch isn't in `branches`.
#' @noRd
branch_inputs <- function(branches, branch, meta) {
  if (is.null(branches) || ncol(branches) < 2 || !branch %in% branches[[1]]) return(NULL)
  row <- branches[branches[[1]] == branch, , drop = FALSE][1, ]
  lapply(names(branches)[-1], function(input) {
    at <- match(input, meta$name)
    children <- if (!is.na(at) && "children" %in% names(meta)) meta$children[[at]] else NULL
    type <- if (!is.na(at) && "type" %in% names(meta)) meta$type[[at]] else NA_character_
    read <- if (identical(type, "pattern")) match(row[[input]], meta$name) else at
    list(
      input = input,
      type = type,
      index = match(row[[input]], children),
      n = length(children[!is.na(children)]),
      bytes = if (!is.na(read) && "bytes" %in% names(meta)) meta$bytes[[read]] else NA_real_
    )
  })
}

#' The R code that reads one slice of a target
#'
#' A pattern's branch reads as such; a stem's slice is cut as targets cuts
#' it for its `iteration`: an element, a data frame's or matrix's row, a
#' list's element, or a group (`tar_group`).
#' @param input An element of branch_inputs().
#' @param iteration The upstream target's iteration.
#' @param store The store, if not the default.
#' @noRd
slice_code <- function(input, iteration = "vector", store = NULL) {
  read <- paste0("targets::tar_read(", paste(c(input$input, store_arg(store)), collapse = ", "), ")")
  i <- input$index
  if (identical(input$type, "pattern")) {
    return(paste0("targets::tar_read(", paste(c(input$input, paste("branches =", i), store_arg(store)), collapse = ", "), ")"))
  }
  if (is.null(iteration) || is.na(iteration)) iteration <- "vector"
  switch(iteration,
    list = sprintf("%s[[%d]]", read, i),
    group = sprintf("subset(%s, tar_group == sort(unique(tar_group))[%d])", read, i),
    # As targets slices it: an element, or a data frame's or matrix's row.
    # vctrs comes with targets.
    sprintf("vctrs::vec_slice(%s, %d)", read, i)
  )
}

#' The Inputs section's content: each input's slice, with its code and a
#' few printed lines
#'
#' @param inputs branch_inputs(), or `NULL`.
#' @param iterations Named: each target's iteration (target_settings()).
#' @param read A function of R code, giving its value, or `NULL` not to
#'   read: only what fits the preview's size limit is read.
#' @noRd
branch_inputs_ui <- function(inputs, iterations = character(), store = NULL, read = NULL,
                             limit = preview_bytes_limit()) {
  if (is.null(inputs)) return(shiny::p(class = "tb-preview-note", "Its inputs aren't known: the pipeline changed since it ran."))
  if (!length(inputs)) return(shiny::p(class = "tb-preview-note", "It maps no other target."))
  shiny::tagList(lapply(inputs, function(input) {
    what <- if (identical(input$type, "pattern")) "branch" else "slice"
    if (is.na(input$index)) {
      return(shiny::tags$p(shiny::tags$code(input$input, .noWS = "after"), sprintf(": its %s isn't stored any more.", what)))
    }
    code <- slice_code(input, unname(iterations[input$input]), store)
    printed <- if (!is.null(read) && !is.na(input$bytes) && input$bytes <= limit) {
      tryCatch(printed_text(read(code), max_lines = 8), error = function(e) paste("Not read:", conditionMessage(e)))
    }
    shiny::tags$div(
      class = "tb-branch-input",
      shiny::tags$p(shiny::tags$code(input$input, .noWS = "after"), sprintf(", %s %d of %d", what, input$index, input$n)),
      code_block("Its value", "slice", code),
      if (!is.null(printed)) shiny::tags$pre(class = "tb-branch-input-value", printed)
    )
  }))
}

#' The Inputs section of a branch, closed: filled when opened (srcjs/actions.js)
#' @noRd
branch_inputs_section <- function(branch, ns = identity) {
  if (is.null(branch)) return(NULL)
  shiny::tags$details(
    class = "tb-command tb-branch-inputs", `data-tb-output` = ns("inputs"),
    shiny::tags$summary("Inputs")
  ) |>
    send_on_click(ns("inputs_open"), branch)
}
