#' A target's problem: what went wrong in its last build
#'
#' Read from the store: its error or warnings (`tar_meta()`), its seed, its
#' traceback when it left a workspace, and, for a pattern, the problems of
#' its branches. The command comes from the script (`tar_manifest()`).
#'
#' @param name A target.
#' @param script The targets script, for the command.
#' @return A list, or NULL when the target has no metadata.
#' @noRd
target_problem <- function(name, script = targets::tar_config_get("script")) {
  meta <- tryCatch(
    targets::tar_meta(fields = c("type", "parent", "error", "warnings", "seed")),
    error = function(e) NULL
  )
  if (is.null(meta) || !name %in% meta$name) return(NULL)
  row <- meta[meta$name == name, ]
  workspaces <- tryCatch(targets::tar_workspaces(), error = function(e) character())
  functions <- meta$name[meta$type %in% "function"]

  branches <- list()
  if (identical(row$type, "pattern")) {
    rows <- meta[meta$parent %in% name & (!is.na(meta$error) | !is.na(meta$warnings)), ]
    rows <- rows[order(is.na(rows$error)), ] # errored first
    positions <- branch_positions(name)
    branches <- lapply(seq_len(nrow(rows)), function(i) {
      branch <- rows$name[i]
      list(
        name = branch,
        error = na_null(rows$error[i]),
        warnings = na_null(rows$warnings[i]),
        seed = na_null(rows$seed[i]),
        workspace = branch %in% workspaces,
        traceback = if (branch %in% workspaces) target_traceback(branch),
        position = positions[[branch]] %||% NA_integer_,
        of = length(positions)
      )
    })
  }

  list(
    name = name,
    type = row$type,
    error = na_null(row$error),
    warnings = na_null(row$warnings),
    seed = na_null(row$seed),
    workspace = name %in% workspaces,
    traceback = if (name %in% workspaces) target_traceback(name),
    command = target_command(name, script),
    functions = functions,
    branches = branches
  )
}

#' Whether a problem has anything to show
#' @noRd
has_problem <- function(problem) {
  !is.null(problem) &&
    (!is.null(problem$error) || !is.null(problem$warnings) || length(problem$branches) > 0)
}

na_null <- function(x) if (length(x) == 0 || is.na(x)) NULL else x

target_traceback <- function(name) {
  tryCatch(rlang::inject(targets::tar_traceback(!!as.symbol(name))), error = function(e) NULL)
}

target_command <- function(name, script) {
  manifest <- tryCatch(
    targets::tar_manifest(names = dplyr::any_of(name), fields = "command", script = script),
    error = function(e) NULL
  )
  if (is.null(manifest) || nrow(manifest) == 0) return(NULL)
  manifest$command[[1]]
}

# Where each branch of a pattern sits among its branches, by branch name.
branch_positions <- function(name) {
  branches <- tryCatch(rlang::inject(targets::tar_branches(!!as.symbol(name))), error = function(e) NULL)
  if (is.null(branches) || nrow(branches) == 0) return(list())
  as.list(stats::setNames(seq_len(nrow(branches)), branches[[1]]))
}

#' The traceback, split into the calls of the pipeline's own functions and
#' runs of calls in packages
#'
#' The first call is the target's command. targets' own error handling, and
#' the board's stop at warnings, are left out.
#'
#' @param calls `tar_traceback()`.
#' @param functions The pipeline's functions, from `tar_meta()`.
#' @return A list of `list(own = TRUE/FALSE, calls = character())`.
#' @noRd
fold_traceback <- function(calls, functions = character()) {
  noise <- paste(
    c(
      "^\\.handleSimpleError\\(", "^h\\(simple(Error|Warning|Condition)\\(",
      "^\\.signalSimpleWarning\\(", "^withRestarts\\(", "^withOneRestart\\(",
      "^doWithOneRestart\\(", "^stop\\(simpleError\\(paste\\(\"Warning, stopped at by the board",
      "^warning\\(\"", "^withCallingHandlers\\("
    ),
    collapse = "|"
  )
  # The board's stop at warnings wraps the command: the call of `warning()`
  # itself stays, as the line that warned.
  warned <- grep("^warning\\(", calls)
  keep <- !grepl(noise, calls) | seq_along(calls) %in% utils::tail(warned, 1)
  calls <- calls[keep]
  if (length(calls) == 0) return(list())
  head <- sub("^([[:alnum:]._]+:::?)?([[:alnum:]._]+)\\(.*$", "\\2", calls)
  own <- seq_along(calls) == 1 | head %in% functions | grepl("^warning\\(", calls)
  runs <- cumsum(c(TRUE, diff(own) != 0))
  unname(lapply(split(seq_along(calls), runs), function(i) list(own = own[i[1]], calls = calls[i])))
}

#' R code that reproduces a target's problem in the author's session
#'
#' With a workspace, loading it is enough: its inputs, the globals and its
#' seed. Without one (a target that only warned), the code loads the same.
#'
#' @noRd
reproduce_code <- function(name, command, workspace, inputs = character(), seed = NULL) {
  command <- command %||% "# (the target's command)"
  if (workspace) {
    return(paste0(
      "targets::tar_workspace(", name, ") # its inputs, the globals and its seed\n",
      command
    ))
  }
  paste0(
    "targets::tar_load_globals()\n",
    if (length(inputs)) {
      paste0("targets::tar_load(c(", paste(inputs, collapse = ", "), ")) # its inputs\n")
    },
    if (!is.null(seed)) paste0("set.seed(", seed, ") # its seed\n"),
    command
  )
}

#' R code for a debug run of one target, in the author's console
#'
#' The run builds only `run` (the target, or a branch's pattern) and what it
#' needs: a cue of "never" alone would still run other targets that errored.
#' @noRd
debug_run_code <- function(name, script = NULL, run = name) {
  script_arg <- if (!is.null(script) && !identical(script, "_targets.R")) {
    paste0(", script = ", deparse(script))
  } else {
    ""
  }
  paste0(
    "# Restart R first: a debug run uses this session, and objects left in it\n",
    "# can change what the pipeline builds.\n",
    "targets::tar_make(\n",
    "  names = \"", run, "\",\n",
    "  script =targetsboard::debug_run_script(\"", name, "\"", script_arg, "),\n",
    "  callr_function = NULL, use_crew = FALSE, as_job = FALSE\n",
    ")"
  )
}

#' The pane of a target's problem
#'
#' @param problem `target_problem()`.
#' @param inputs The ids of the targets it depends on directly.
#' @param running Whether a run is going: no debug run then.
#' @param script The targets script, for the debug run.
#' @noRd
problem_ui <- function(problem, inputs = character(), running = FALSE, script = NULL) {
  shiny::tags$div(
    class = "tb-problem",
    `data-target` = problem$name,
    if (!is.null(problem$error) || !is.null(problem$warnings)) {
      shiny::tags$h5(class = "tb-problem-title", if (!is.null(problem$error)) "Error" else "Warnings")
    },
    problem_details(problem, problem$functions),
    if (length(inputs)) {
      shiny::tags$div(
        class = "tb-problem-inputs",
        shiny::tags$span("Inputs: "),
        lapply(inputs, function(id) {
          shiny::tags$button(
            class = "tb-input-link btn btn-link btn-sm p-0 me-2", `data-target` = id,
            onclick = sprintf(
              "Shiny.setInputValue('meta_row_clicked', %s, {priority: 'event'})",
              jsonlite::toJSON(id, auto_unbox = TRUE)
            ),
            id
          )
        })
      )
    },
    if (problem$type != "pattern") {
      shiny::tagList(
        code_block(
          "Reproduce", "reproduce",
          reproduce_code(problem$name, problem$command, problem$workspace, inputs, problem$seed)
        ),
        debug_run_block(problem$name, running, script)
      )
    },
    if (length(problem$branches)) {
      shiny::tags$div(
        class = "tb-problem-branches",
        shiny::tags$h6(sprintf("Branches with problems: %d", length(problem$branches))),
        lapply(problem$branches, function(branch) {
          shiny::tags$details(
            class = "tb-problem-branch", `data-branch` = branch$name,
            open = if (identical(branch, problem$branches[[1]])) NA,
            shiny::tags$summary(
              shiny::tags$code(branch$name),
              if (!is.na(branch$position)) {
                shiny::tags$span(class = "text-muted ms-2", sprintf("branch %d of %d", branch$position, branch$of))
              }
            ),
            problem_details(branch, problem$functions),
            if (branch$workspace) {
              code_block(
                "Reproduce", "reproduce",
                reproduce_code(branch$name, problem$command, TRUE)
              )
            },
            debug_run_block(branch$name, running, script, run = problem$name)
          )
        })
      )
    }
  )
}

problem_details <- function(problem, functions) {
  shiny::tagList(
    if (!is.null(problem$error)) shiny::tags$pre(class = "tb-problem-error", problem$error),
    if (!is.null(problem$warnings)) {
      shiny::tags$ul(
        class = "tb-problem-warnings",
        lapply(strsplit(problem$warnings, ". ", fixed = TRUE)[[1]], shiny::tags$li)
      )
    },
    if (length(problem$traceback)) traceback_ui(fold_traceback(problem$traceback, functions)),
    if (!is.null(problem$seed)) shiny::tags$div(class = "tb-problem-seed text-muted", paste("Seed:", problem$seed))
  )
}

traceback_ui <- function(segments) {
  shiny::tags$div(
    class = "tb-traceback",
    shiny::tags$div(class = "text-muted", "Traceback"),
    lapply(segments, function(segment) {
      lines <- shiny::tags$pre(paste(segment$calls, collapse = "\n"))
      if (segment$own) {
        shiny::tags$div(class = "tb-traceback-own", lines)
      } else {
        shiny::tags$details(
          class = "tb-traceback-packages",
          shiny::tags$summary(sprintf(
            "%d call%s in packages", length(segment$calls), if (length(segment$calls) == 1) "" else "s"
          )),
          lines
        )
      }
    })
  )
}

debug_run_block <- function(name, running, script, run = name) {
  if (running) {
    return(shiny::tags$p(class = "tb-debug-run text-muted", "Debug run: not while a run is going."))
  }
  code_block("Debug run", "debug_run", debug_run_code(name, script, run))
}

# Copies the button's code, then says so for a moment (or that it couldn't:
# the clipboard needs a secure page, which localhost is).
copy_button_js <- paste(
  "const b = this;",
  "const say = t => { b.textContent = t; clearTimeout(b._tb); b._tb = setTimeout(() => b.textContent = 'Copy', 1500); };",
  "if (!navigator.clipboard) { say('Copy failed'); return; }",
  "navigator.clipboard.writeText(b.dataset.copy).then(() => say('Copied'), () => say('Copy failed'));"
)

# R code to copy into the console, shown in full.
code_block <- function(title, kind, code) {
  shiny::tags$div(
    class = paste0("tb-code tb-code-", kind),
    shiny::tags$div(
      class = "d-flex justify-content-between align-items-center",
      shiny::tags$span(class = "text-muted", title),
      shiny::tags$button(
        class = "btn btn-sm btn-outline-secondary tb-copy", `data-copy` = code,
        onclick = copy_button_js,
        "Copy"
      )
    ),
    # Text alone in <pre>: a nested tag would indent the first line.
    shiny::tags$pre(code)
  )
}

#' Write a script for a debug run of one target
#'
#' For a debug run in your own console: the run stops inside `name`, in the
#' debugger, and skips every other target unless it never ran. The script
#' loads yours and changes its targets for this run only; yours is not
#' changed. The board's Problem pane gives the code to use it.
#'
#' @param name The target, or branch, to stop in.
#' @param script Your targets script.
#' @return The path of the script, for `targets::tar_make(script = )`.
#' @examples
#' \dontrun{
#' # Restart R first, then:
#' targets::tar_make(
#'   names = "model",
#'   script = debug_run_script("model"),
#'   callr_function = NULL, use_crew = FALSE, as_job = FALSE
#' )
#' }
#' @export
debug_run_script <- function(name, script = targets::tar_config_get("script")) {
  if (!rlang::is_string(name)) rlang::abort("`name` must be one target name.")
  if (!run_modes_supported()) {
    rlang::abort(c(
      "This version of targets is not supported for debug runs.",
      i = sprintf('Add `tar_option_set(debug = "%s", cue = tar_cue(mode = "never"))` to the script instead.', name)
    ))
  }
  write_run_wrapper(script, debug = name)
}

#' The pipeline's problems: one row per target that errored or warned
#'
#' A pattern stands for its branches: its row counts those with problems.
#' Errors come first.
#'
#' @param meta `tar_meta(targets_only = TRUE)`.
#' @param targets The pipeline's targets: metadata of removed ones is left
#'   out. `NULL` keeps all.
#' @return A data frame: `name`, `problem` ("Error" or "Warnings"),
#'   `message` and `branches` (e.g. "1 of 3 errored", or "").
#' @noRd
problems_table <- function(meta, targets = NULL) {
  empty <- data.frame(name = character(), problem = character(), message = character(), branches = character())
  if (is.null(meta) || nrow(meta) == 0) return(empty)
  for (field in c("type", "parent", "error", "warnings")) {
    if (!field %in% names(meta)) meta[[field]] <- NA_character_
  }
  meta$name <- ifelse(meta$type %in% "branch" & !is.na(meta$parent), meta$parent, meta$name)
  if (!is.null(targets)) meta <- meta[meta$name %in% targets, ]
  rows <- lapply(split(meta, factor(meta$name, unique(meta$name))), function(m) {
    branches <- m[m$type %in% "branch", ]
    errors <- m$error[!is.na(m$error)]
    warnings <- m$warnings[!is.na(m$warnings)]
    if (length(errors) == 0 && length(warnings) == 0) return(NULL)
    data.frame(
      name = m$name[1],
      problem = if (length(errors)) "Error" else "Warnings",
      message = if (length(errors)) errors[1] else warnings[1],
      branches = if (nrow(branches) == 0) "" else {
        n <- sum(!is.na(branches[[if (length(errors)) "error" else "warnings"]]))
        sprintf("%d of %d %s", n, nrow(branches), if (length(errors)) "errored" else "warned")
      }
    )
  })
  out <- do.call(rbind, c(list(empty), rows))
  rownames(out) <- NULL
  out[order(out$problem != "Error", out$name), ]
}

#' The targets that errored in the latest run, from its progress
#'
#' A branch counts as its pattern.
#' @param progress `tar_progress(fields = c("type", "parent", "progress"))`.
#' @noRd
run_errored_targets <- function(progress) {
  if (is.null(progress) || nrow(progress) == 0) return(character())
  errored <- progress[progress$progress %in% "errored", ]
  if ("type" %in% names(errored) && "parent" %in% names(errored)) {
    errored$name <- ifelse(errored$type %in% "branch" & !is.na(errored$parent), errored$parent, errored$name)
  }
  as.character(unique(errored$name))
}

# "a", "a and b", "a, b and 3 more".
describe_names <- function(names, max = 3) {
  shown <- utils::head(names, max)
  rest <- length(names) - length(shown)
  if (rest > 0) return(paste0(paste(shown, collapse = ", "), " and ", rest, " more"))
  if (length(shown) <= 1) return(paste(shown))
  paste0(paste(utils::head(shown, -1), collapse = ", "), " and ", utils::tail(shown, 1))
}

#' The end of a run's log, for the Log tab
#' @noRd
read_run_log <- function(path, lines = 500) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  log <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  # Without the colours of the console.
  cli::ansi_strip(paste(utils::tail(log, lines), collapse = "\n"))
}
