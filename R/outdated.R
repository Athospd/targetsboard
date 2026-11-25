#' Why each outdated target is outdated
#'
#' targets tells why a target must run again with its cues, in
#' `targets::tar_sitrep()`: one row per target, or per branch of a pattern,
#' and a column per cue, TRUE when it calls for a run. Two things it doesn't
#' say, which this adds: what changed when a dependency did (the target's
#' direct dependencies that are outdated, from `targets::tar_network()`:
#' functions and objects too), and that a target is outdated only because
#' something upstream is (every cue FALSE, an outdated target upstream).
#'
#' @param reasons What the poller wrote (tar_visnetwork_bg()): `list(sitrep,
#'   vertices, edges, parents)`, with `parents` the branches' patterns
#'   (`tar_meta()`'s `name` and `parent`).
#' @return A named list: for each outdated target, its reasons, sentences
#'   with names in backticks. Empty when nothing is outdated, or the poller
#'   hasn't written them yet.
#' @noRd
outdated_reasons <- function(reasons) {
  if (is.null(reasons) || is.null(reasons$vertices)) return(list())
  vertices <- reasons$vertices
  edges <- reasons$edges
  sitrep <- reasons$sitrep
  parents <- reasons$parents
  targets <- vertices$name[vertices$type %in% c("stem", "pattern")]
  outdated <- vertices$name[vertices$status %in% "outdated"]
  result <- list()
  for (name in intersect(targets, outdated)) {
    # The target's own rows: itself, or its branches.
    branches <- if (!is.null(parents)) parents$name[parents$parent %in% name] else character()
    rows <- if (!is.null(sitrep)) sitrep[sitrep$name %in% c(name, branches), , drop = FALSE]
    cue <- function(field) {
      !is.null(rows) && field %in% names(rows) && any(rows[[field]] %in% TRUE)
    }
    inputs <- edges$from[edges$to == name]
    changed <- setdiff(intersect(inputs, outdated), targets) # functions, objects
    upstream <- intersect(intersect(inputs, outdated), targets)
    why <- c(
      if (cue("meta")) "It has no record in the store: it is new, was invalidated, or never finished.",
      if (cue("always")) "Its cue is \"always\": it runs every time.",
      if (cue("command")) "Its command changed.",
      if (cue("depend")) {
        if (length(changed)) {
          sprintf("%s changed.", backticked(changed))
        } else {
          "Something it depends on changed."
        }
      },
      if (cue("format")) "Its storage format changed.",
      if (cue("repository")) "Its repository changed.",
      if (cue("iteration")) "Its iteration changed.",
      if (cue("file")) "Its file changed, or is missing.",
      if (cue("seed")) "Its seed changed.",
      if (length(upstream)) {
        sprintf("Upstream, %s %s outdated.", backticked(upstream), if (length(upstream) == 1) "is" else "are")
      }
    )
    if (!length(why)) why <- "Something upstream changed."
    result[[name]] <- why
  }
  result
}

# Names as `a`, `b` and `c`.
backticked <- function(names) {
  names <- sprintf("`%s`", names)
  if (length(names) <= 1) return(names)
  paste(paste(names[-length(names)], collapse = ", "), "and", names[length(names)])
}

#' A target's reasons, in one line for the canvas's tooltip
#' @noRd
reasons_tooltip <- function(reasons) {
  paste("Outdated:", gsub("`", "", paste(reasons, collapse = " ")))
}

#' A target's reasons, for the value preview
#'
#' @param reasons The target's sentences, from outdated_reasons().
#' @return A tag, or `NULL` without reasons.
#' @noRd
reasons_ui <- function(reasons) {
  if (!length(reasons)) return(NULL)
  # Names in backticks become code, inline in the escaped sentence.
  sentence <- function(text) {
    html <- gsub("`([^`]*)`", "<code>\\1</code>", htmltools::htmlEscape(text))
    shiny::tags$li(shiny::HTML(html))
  }
  shiny::tags$div(
    class = "tb-why",
    shiny::tags$p(class = "tb-why-title", "Why it is outdated"),
    shiny::tags$ul(lapply(reasons, sentence))
  )
}
