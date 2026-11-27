#' tarchetypes' rerun rules, read from the script
#'
#' targets evaluates `tar_cue_age()`, `tar_cue_force()` and `tar_cue_skip()`
#' when it reads the script and keeps only the cue mode they give
#' (`"always"`, `"never"`, `"thorough"`); `tar_force()` and `tar_change()`
#' add a helper target, `<name>_change`, that runs every time. The rule
#' itself is in the script's text alone, so it is parsed, not run: the
#' factories `tar_age()`, `tar_force()`, `tar_skip()`, `tar_change()`, and
#' the cues `tar_cue_age()`, `tar_cue_force()`, `tar_cue_skip()` in a
#' target's `cue`. Names are read as written: a target renamed by
#' `tar_map()` isn't matched.
#' @param script Path to the targets script.
#' @return A data frame: `name`, `rule` (`"age"`, `"force"`, `"skip"`,
#'   `"change"`, or `"helper"` for a `_change` target), `arg` (the age,
#'   condition or value, as code) and `for` (a helper's target).
#' @noRd
cue_rules <- function(script) {
  none <- data.frame(name = character(), rule = character(), arg = character(), `for` = character(),
                     check.names = FALSE)
  if (is.null(script) || !file.exists(script)) return(none)
  exprs <- tryCatch(parse(script, keep.source = FALSE), error = function(e) expression())
  rows <- list()
  add <- function(name, rule, arg, target = NA_character_) {
    rows[[length(rows) + 1]] <<- list(name = name, rule = rule, arg = arg, `for` = target)
  }
  # A call's function name, with or without tarchetypes::.
  fname <- function(x) {
    fn <- x[[1]]
    if (is.name(fn)) return(as.character(fn))
    if (is.call(fn) && length(fn) == 3 && is.name(fn[[1]]) && as.character(fn[[1]]) %in% c("::", ":::")) return(as.character(fn[[3]]))
    ""
  }
  # An argument by name, or else by position among the unnamed ones.
  arg_of <- function(x, name, position) {
    args <- as.list(x)[-1]
    named <- names(args) %||% rep("", length(args))
    if (name %in% named) return(args[[name]])
    unnamed <- args[!nzchar(named)]
    if (length(unnamed) >= position) unnamed[[position]]
  }
  target_name <- function(x) {
    name <- arg_of(x, "name", 1)
    if (is.name(name)) as.character(name) else if (is.character(name) && length(name) == 1) name
  }
  code <- function(arg) if (!is.null(arg)) paste(deparse(arg, width.cutoff = 500L), collapse = " ")
  factories <- c(tar_age = "age", tar_force = "force", tar_skip = "skip", tar_change = "change")
  cues <- c(tar_cue_age = "age", tar_cue_force = "force", tar_cue_skip = "skip")
  visit <- function(x) {
    if (!is.call(x)) return(invisible())
    fn <- fname(x)
    if (fn %in% names(factories)) {
      rule <- factories[[fn]]
      name <- target_name(x)
      arg <- code(arg_of(x, rule, 3))
      if (!is.null(name) && !is.null(arg)) {
        add(name, rule, arg)
        if (rule %in% c("force", "change")) add(paste0(name, "_change"), "helper", arg, name)
      }
    } else {
      cue <- arg_of_named(x, "cue")
      if (is.call(cue) && fname(cue) %in% names(cues)) {
        rule <- cues[[fname(cue)]]
        name <- target_name(x)
        arg <- code(if (rule == "age") arg_of(cue, "age", 2) else arg_of(cue, "condition", 1))
        if (!is.null(name) && !is.null(arg)) add(name, rule, arg)
      }
    }
    for (part in as.list(x)[-1]) if (!missing(part)) visit(part)
  }
  # Only a named cue = : a call's first unnamed argument is never its cue.
  arg_of_named <- function(x, name) {
    args <- as.list(x)[-1]
    if (!is.null(names(args)) && name %in% names(args)) args[[name]]
  }
  for (expr in exprs) visit(expr)
  if (!length(rows)) return(none)
  out <- do.call(rbind, lapply(rows, as.data.frame, check.names = FALSE, stringsAsFactors = FALSE))
  out[!duplicated(out$name), , drop = FALSE]
}

#' An age, from its code, when it is a plain difftime
#'
#' `as.difftime(1, units = "days")` and the like, evaluated with base R
#' only: an age built from the script's own objects stays code.
#' @return A difftime, or NULL.
#' @noRd
rule_age <- function(arg) {
  value <- tryCatch(eval(str2lang(arg), envir = new.env(parent = baseenv())), error = function(e) NULL)
  if (inherits(value, "difftime") && length(value) == 1 && !is.na(value)) value
}

# "1 day", "3 hours", "90 secs": a difftime in its own units, singular for 1.
format_age <- function(age) {
  n <- as.numeric(age)
  unit <- units(age)
  if (unit == "secs") unit <- "seconds"
  if (unit == "mins") unit <- "minutes"
  if (n == 1) unit <- sub("s$", "", unit)
  paste(format(n, trim = TRUE), unit)
}

# "3 days", "5 hours", "12 minutes", "40 seconds": a span, rounded.
format_span <- function(seconds) {
  seconds <- abs(as.numeric(seconds))
  span <- if (seconds >= 86400) {
    c(round(seconds / 86400), "day")
  } else if (seconds >= 3600) {
    c(round(seconds / 3600), "hour")
  } else if (seconds >= 60) {
    c(round(seconds / 60), "minute")
  } else {
    c(round(seconds), "second")
  }
  paste(span[[1]], if (span[[1]] == "1") span[[2]] else paste0(span[[2]], "s"))
}

#' A target's rerun rule, in a sentence
#'
#' @param rule One row of cue_rules().
#' @param built When the target last built (`tar_meta()`'s `time`), or NA.
#' @param now For tests.
#' @noRd
rule_sentence <- function(rule, built = NA, now = Sys.time()) {
  arg <- sprintf("`%s`", rule$arg)
  switch(rule$rule,
    age = {
      age <- rule_age(rule$arg)
      if (is.null(age)) return(sprintf("Runs again once its last build is older than %s.", arg))
      text <- sprintf("Runs again once its last build is older than %s.", format_age(age))
      if (length(built) == 1 && !is.na(built)) {
        stale <- built + age
        text <- paste(text, if (stale <= now) {
          sprintf("Built %s ago: it is due.", format_span(difftime(now, built, units = "secs")))
        } else {
          sprintf("Due in %s.", format_span(difftime(stale, now, units = "secs")))
        })
      }
      text
    },
    force = if (identical(rule$arg, "TRUE")) "Runs every time." else sprintf("Runs every time while %s is true.", arg),
    skip = if (identical(rule$arg, "TRUE")) {
      "Always skipped, even when what it depends on changes."
    } else {
      sprintf("Skipped while %s is true, even when what it depends on changes.", arg)
    },
    change = sprintf("Runs again when the value of %s changes, checked on every run.", arg),
    helper = sprintf("Checks %s for `%s` on every run.", arg, rule$`for`)
  )
}

# The rule of a target, a row of cue_rules(), or NULL.
target_rule <- function(rules, name) {
  if (is.null(rules) || !nrow(rules) || is.null(name)) return(NULL)
  row <- rules[rules$name %in% name, , drop = FALSE]
  if (nrow(row)) row[1, , drop = FALSE]
}

#' The rule's line in the Value pane, or nothing
#' @noRd
rule_ui <- function(sentence) {
  if (is.null(sentence)) return(NULL)
  html <- gsub("`([^`]*)`", "<code>\\1</code>", htmltools::htmlEscape(sentence))
  shiny::tags$p(class = "tb-rule", shiny::icon("scale-balanced"), shiny::HTML(html))
}
