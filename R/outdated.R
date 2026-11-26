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
#'   with names in backticks, named by their kind (the cue, or `upstream`),
#'   which picks their icon (reason_icons). Empty when nothing is outdated,
#'   or the poller hasn't written them yet.
#' @noRd
outdated_reasons <- function(reasons, rules = NULL) {
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
    # tarchetypes' rules (cue_rules()): their cues and helpers say it better.
    rule <- target_rule(rules, name)
    helpers <- if (!is.null(rules) && nrow(rules)) rules$name[rules$rule == "helper" & rules$`for` %in% name]
    by_helper <- any(upstream %in% helpers)
    upstream <- setdiff(upstream, helpers)
    format <- if (!is.null(reasons$manifest) && all(c("name", "format") %in% names(reasons$manifest))) {
      reasons$manifest$format[reasons$manifest$name %in% name][1]
    }
    why <- c(
      meta = if (cue("meta")) "It has no record in the store: it is new, was invalidated, or never finished.",
      always = if (cue("always") && is.null(rule)) "Its cue is \"always\": it runs every time.",
      age = if (identical(rule$rule, "age")) {
        age <- rule_age(rule$arg)
        sprintf("Its last build is older than %s.", if (is.null(age)) sprintf("`%s`", rule$arg) else format_age(age))
      },
      force = if (identical(rule$rule, "force") && (cue("always") || by_helper)) {
        sprintf("Forced: `%s` is true.", rule$arg)
      },
      change = if (identical(rule$rule, "change") && by_helper) {
        sprintf("`%s` is checked on every run: it runs again if its value changed.", rule$arg)
      },
      skip = if (identical(rule$rule, "skip")) sprintf("While `%s` is true, a run skips it.", rule$arg),
      helper = if (identical(rule$rule, "helper")) rule_sentence(rule),
      command = if (cue("command")) "Its command changed.",
      depend = if (cue("depend")) {
        if (length(changed)) {
          sprintf("%s changed.", backticked(changed))
        } else {
          "Something it depends on changed."
        }
      },
      format = if (cue("format")) "Its storage format changed.",
      repository = if (cue("repository")) "Its repository changed.",
      iteration = if (cue("iteration")) "Its iteration changed.",
      file = if (cue("file")) {
        if (identical(format, "url")) "Its URL's remote file changed, or can't be reached." else "Its file changed, or is missing."
      },
      seed = if (cue("seed")) "Its seed changed.",
      upstream = if (length(upstream)) {
        sprintf("Upstream, %s %s outdated.", backticked(upstream), if (length(upstream) == 1) "is" else "are")
      }
    )
    if (!length(why)) why <- c(upstream = "Something upstream changed.")
    result[[name]] <- why
  }
  result
}

#' Why each up-to-date target is up to date: why a run skips it
#'
#' Nothing it depends on changed since its last build: its command, the
#' functions, objects and targets it uses, and its settings. Its cue can
#' say otherwise, and is named then: `tar_cue(mode = "never")` skips it
#' whatever changed, and a cue that doesn't check, say, its command ignores
#' a change there. A tarchetypes skip rule has its own line (rule_ui()).
#'
#' @param reasons What the poller wrote: its `vertices` (`status`) and
#'   `manifest` (`cue_*`).
#' @param rules cue_rules().
#' @return A named list: for each up-to-date target, its sentences.
#' @noRd
skipped_reasons <- function(reasons, rules = NULL) {
  vertices <- reasons$vertices
  if (is.null(vertices) || !all(c("name", "type", "status") %in% names(vertices))) return(list())
  names <- vertices$name[vertices$type %in% c("stem", "pattern") & vertices$status %in% "uptodate"]
  skip_rules <- if (!is.null(rules) && nrow(rules)) rules$name[rules$rule == "skip"]
  manifest <- reasons$manifest
  cue <- function(name, field) {
    if (is.null(manifest) || !field %in% names(manifest)) return(NA)
    manifest[[field]][manifest$name %in% name][1]
  }
  checks <- c(
    cue_command = "its command", cue_depend = "what it uses", cue_format = "its storage format",
    cue_repository = "its repository", cue_iteration = "its iteration", cue_file = "its file"
  )
  result <- list()
  for (name in setdiff(names, skip_rules)) {
    if (identical(cue(name, "cue_mode"), "never")) {
      result[[name]] <- "Its cue is \"never\": a run skips it, even when something it uses changes."
      next
    }
    off <- names(checks)[vapply(names(checks), function(f) isFALSE(as.logical(cue(name, f))), logical(1))]
    result[[name]] <- if (length(off)) {
      c(
        "Up to date: nothing its cue checks changed since its last build, so a run skips it.",
        sprintf("Its cue doesn't check %s: a change there doesn't run it again.", and_list(unname(checks[off]), "or"))
      )
    } else {
      paste(
        "Up to date: its command, the functions, objects and targets it uses, and its settings",
        "haven't changed since its last build, so a run skips it."
      )
    }
  }
  result
}

#' Why a target is up to date, in the Value pane: a line per sentence, as
#' the rule's (rule_ui()), with when it last built
#' @param sentences From skipped_reasons().
#' @param built Its last build's time, or NA.
#' @noRd
skipped_ui <- function(sentences, built = NA) {
  if (!length(sentences)) return(NULL)
  if (length(built) == 1 && !is.na(built)) {
    when <- format(as.POSIXct(built), "%Y-%m-%d %H:%M")
    sentences[[1]] <- sub("since its last build", paste0("since its last build, ", when), sentences[[1]], fixed = TRUE)
  }
  icons <- c("circle-check", rep("filter", length(sentences) - 1))
  shiny::tags$div(
    class = "tb-skipped",
    unname(Map(function(text, icon) shiny::tags$p(class = "tb-rule", shiny::icon(icon), text), sentences, icons))
  )
}

# Words as "a", "a and b", "a, b and c"; or with "or".
and_list <- function(words, conjunction = "and") {
  if (length(words) <= 1) return(words)
  paste(paste(words[-length(words)], collapse = ", "), conjunction, words[length(words)])
}

# Names as `a`, `b` and `c`.
backticked <- function(names) and_list(sprintf("`%s`", names))

#' A target's reasons, in one line for the canvas's tooltip
#' @noRd
reasons_tooltip <- function(reasons) {
  paste("Outdated:", gsub("`", "", paste(reasons, collapse = " ")))
}

# The Font Awesome icon of each kind of reason (outdated_reasons()).
reason_icons <- c(
  meta = "database", always = "rotate", command = "code", depend = "pen",
  format = "box-archive", repository = "box-archive", iteration = "layer-group",
  file = "file", seed = "dice", upstream = "arrow-turn-up",
  age = "hourglass-half", force = "bolt", change = "rotate", skip = "forward", helper = "rotate"
)

#' A target's reasons, for the value preview
#'
#' A card in the outdated status's colour, info, above the value's: a
#' reason per row, with its kind's icon.
#' @param reasons The target's sentences, from outdated_reasons().
#' @param functions function_definitions(): the functions the reasons name
#'   get their code, below them (reason_functions_ui()).
#' @return A tag, or `NULL` without reasons.
#' @noRd
reasons_ui <- function(reasons, functions = NULL) {
  if (!length(reasons)) return(NULL)
  kinds <- names(reasons) %||% rep("", length(reasons))
  # Names in backticks become code, inline in the escaped sentence.
  row <- function(text, kind) {
    html <- gsub("`([^`]*)`", "<code>\\1</code>", htmltools::htmlEscape(text))
    icon <- if (kind %in% names(reason_icons)) reason_icons[[kind]] else "circle-info"
    shiny::tags$li(
      class = "list-group-item tb-why-reason", `data-kind` = kind,
      shiny::tags$span(class = "tb-why-icon", shiny::icon(icon)),
      shiny::tags$span(shiny::HTML(html))
    )
  }
  bslib::card(
    class = "tb-why border-info-subtle",
    bslib::card_header(
      class = "tb-why-title bg-info-subtle text-info-emphasis border-info-subtle",
      shiny::icon("clock-rotate-left"), "Why it is outdated"
    ),
    bslib::card_body(
      padding = 0,
      shiny::tags$ul(class = "list-group list-group-flush", unname(Map(row, unname(reasons), kinds))),
      reason_functions_ui(reasons, functions)
    )
  )
}
