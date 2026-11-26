#' The run summary: the run going, or the last one
#'
#' Under the run buttons in the Control Panel: while a run goes, its clock
#' and how many targets it built, skipped, is running, saw error and has
#' still to go; after it, the last run's time and counts, until the next
#' run. Counted as `tar_progress_summary()` does, from targets' own files,
#' so a run started from the console shows too.
#'
#' @param id The module's id.
#' @noRd
run_summary_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "summary"), class = "tb-run-summary-output")
}

#' @rdname run_summary_ui
#' @param board Reactive: the graph the board draws, for its targets.
#' @param running Reactive: whether the pipeline is running.
#' @param progress Reactive: the progress file's `list(mtime, data)`
#'   (graph_data_server()).
#' @param tar_meta Reactive: `tar_meta(targets_only = TRUE)`, for the time
#'   left (time_left()).
#' @param scope Reactive: the targets the board's run was asked for, or NULL
#'   for the whole pipeline (run_server()). Only they and what they depend
#'   on count as still to go (run_targets()).
#' @return A reactive: run_summary(), or NULL before any run. It ticks every
#'   second while a run goes.
#' @noRd
run_summary_server <- function(id, board, running, progress, tar_meta = shiny::reactive(NULL),
                               scope = shiny::reactive(NULL)) {
  stopifnot(
    shiny::is.reactive(board), shiny::is.reactive(running), shiny::is.reactive(progress),
    shiny::is.reactive(tar_meta), shiny::is.reactive(scope)
  )
  shiny::moduleServer(id, function(input, output, session) {
    # When the run's process started: tar_process(), read again when the
    # progress file changes or a run starts or stops (a new run rewrites both).
    started <- shiny::reactive({
      progress()$mtime
      running()
      run_started()
    })
    summary <- shiny::reactive({
      if (running()) shiny::invalidateLater(1000)
      current <- progress()
      graph <- board()$x
      nodes <- graph$nodes
      run_summary(
        current$data, run_targets(board_targets(nodes), graph$edges, scope()),
        started = started(), ended = current$mtime, running = running(),
        meta = tar_meta(), outdated = outdated_targets(nodes)
      )
    })
    # The block is drawn again only when what it shows changes: the clock,
    # which ticks every second, is an output of its own inside it. A
    # reactiveVal ignores a value equal to the one it holds.
    shown <- shiny::reactiveVal()
    shiny::observe({
      current <- summary()
      if (!is.null(current)) current$elapsed <- !is.na(current$elapsed)
      shown(current)
    })
    output$summary <- shiny::renderUI({
      run_summary_tags(shown(), elapsed = shiny::textOutput(session$ns("elapsed"), inline = TRUE))
    })
    output$elapsed <- shiny::renderText(format_elapsed(summary()$elapsed))
    summary
  })
}

#' The targets a run reaches: those asked for and all they depend on
#'
#' As `tar_make(names = )` does: it checks what the targets asked for depend
#' on, and builds what isn't up to date. So a run of three targets doesn't
#' count the rest of the pipeline as still to go (#181).
#' @param targets The graph's targets (board_targets()).
#' @param edges The graph's edges: `from` and `to`.
#' @param names The targets asked for, or NULL for the whole pipeline.
#' @noRd
run_targets <- function(targets, edges, names = NULL) {
  if (is.null(names)) return(targets)
  reached <- unique(as.character(names))
  repeat {
    more <- setdiff(as.character(edges$from[edges$to %in% reached]), reached)
    if (!length(more)) break
    reached <- c(reached, more)
  }
  targets[targets %in% reached]
}

#' The run as a whole: its counts and how long it took
#'
#' As `tar_progress_summary()` counts: stems and branches, not patterns.
#'
#' @param progress `targets::tar_progress(fields = c("type", "parent",
#'   "progress"))`, or NULL.
#' @param targets The graph's targets (board_targets()), for those still to run.
#' @param started When the run's process started (`tar_process()`), or NA.
#' @param ended The progress file's modification time: the run's last news.
#' @param running Whether the run is going.
#' @param now The time, for the clock of a run going.
#' @param meta `tar_meta(targets_only = TRUE)`, for the time left, or NULL.
#' @param outdated The graph's targets that aren't up to date: of those the
#'   run hasn't reached, only these count for the time left.
#' @return NULL when there is no run to tell of, or a list of `running`,
#'   `elapsed` (seconds, NA when unknown), the counts `built`, `skipped`,
#'   `running_targets`, `errored`, `canceled` and `to_go`, and while running
#'   `left`, time_left()'s estimate (or NULL).
#' @noRd
run_summary <- function(progress, targets = character(), started = NA, ended = NA,
                        running = FALSE, now = Sys.time(), meta = NULL, outdated = character()) {
  if (is.null(progress) || nrow(progress) == 0) return(NULL)
  built <- progress[!progress$type %in% "pattern", , drop = FALSE]
  count <- function(status) sum(built$progress %in% status)
  end <- if (running) now else ended
  elapsed <- as.numeric(difftime(end, started, units = "secs"))
  if (length(elapsed) != 1 || is.na(elapsed) || elapsed < 0) elapsed <- NA_real_
  list(
    running = running,
    elapsed = elapsed,
    built = count("completed"),
    skipped = count("skipped"),
    running_targets = count("dispatched"),
    errored = count("errored"),
    canceled = count("canceled"),
    # The targets the run hasn't reached: no line in the progress file yet.
    to_go = sum(!targets %in% c(progress$name, progress$parent)),
    left = if (running) {
      to_go <- intersect(targets[!targets %in% c(progress$name, progress$parent)], outdated)
      time_left(progress, to_go, meta)
    }
  )
}

# "0:42", "12:05", "1:02:09".
format_elapsed <- function(seconds) {
  if (is.na(seconds)) return(NA_character_)
  s <- round(seconds)
  h <- s %/% 3600
  m <- (s %% 3600) %/% 60
  if (h > 0) sprintf("%d:%02d:%02d", h, m, s %% 60) else sprintf("%d:%02d", m, s %% 60)
}

#' The summary as the Control Panel shows it
#' @param summary run_summary(), or NULL.
#' @noRd
run_summary_tags <- function(summary, elapsed = NULL) {
  if (is.null(summary)) return(NULL)
  # The clock: an output given by the server, or the time as text.
  if (is.null(elapsed) && is.numeric(summary$elapsed) && !is.na(summary$elapsed)) {
    elapsed <- format_elapsed(summary$elapsed)
  }
  if (isFALSE(summary$elapsed)) elapsed <- NULL
  state <- if (summary$running) "running" else if (summary$errored > 0) "errored" else "done"
  item <- function(n, what, class = NULL) {
    shiny::tags$span(class = c("tb-run-count", if (n > 0) class), shiny::tags$b(n), what)
  }
  counts <- list(
    item(summary$built, "built"),
    item(summary$skipped, "skipped"),
    if (summary$running) item(summary$running_targets, "running", "tb-run-count-running"),
    # Errored: during a run, once there is one.
    if (!summary$running || summary$errored > 0) item(summary$errored, "errored", "tb-run-count-errored"),
    if (summary$canceled > 0) item(summary$canceled, "canceled"),
    if (summary$running) item(summary$to_go, "to go")
  )
  # Where the run is: each part's share of the targets, in a thin bar.
  parts <- c(
    built = summary$built, skipped = summary$skipped, errored = summary$errored + summary$canceled,
    running = if (summary$running) summary$running_targets else 0
  )
  total <- sum(parts) + if (summary$running) summary$to_go else 0
  bar <- if (total > 0) {
    shiny::tags$div(
      class = "tb-run-bar", role = "progressbar",
      `aria-valuenow` = sum(parts[c("built", "skipped", "errored")]), `aria-valuemin` = 0, `aria-valuemax` = total,
      lapply(names(parts)[parts > 0], function(part) {
        shiny::tags$span(class = paste0("tb-run-bar-", part), style = sprintf("width: %.2f%%", 100 * parts[[part]] / total))
      })
    )
  }
  left <- summary$left
  shiny::tags$div(
    class = c("tb-run-summary", paste0("tb-run-summary-", state)),
    shiny::tags$div(
      class = "tb-run-summary-title",
      shiny::tags$span(class = "tb-run-dot", `aria-hidden` = "true"),
      if (summary$running) "Running" else "Last run",
      if (!is.null(elapsed)) shiny::tags$span(class = "tb-run-elapsed", elapsed)
    ),
    bar,
    shiny::tags$div(class = "tb-run-counts", Filter(Negate(is.null), counts)),
    if (!is.null(left) && (left$seconds > 0 || left$unknown > 0)) {
      shiny::tags$div(
        class = "tb-run-left",
        title = "From the targets' last builds",
        if (left$seconds > 0) {
          paste0("About ", format_elapsed(left$seconds), if (left$unknown > 0) "+", " left")
        } else {
          "Time left unknown: never built before"
        }
      )
    }
  )
}

# When the run's process started, from tar_process(), or NA.
run_started <- function() {
  process <- tryCatch(targets::tar_process(), error = function(e) NULL)
  created <- if (!is.null(process)) process$value[process$name == "created"]
  if (length(created) != 1) return(NA)
  # targets writes it in UTC.
  as.POSIXct(created, tz = "UTC")
}

# The graph's targets that aren't up to date.
outdated_targets <- function(nodes) {
  targets <- board_targets(nodes)
  if (!"status" %in% names(nodes)) return(targets)
  intersect(targets, as.character(nodes$id[!nodes$status %in% c("uptodate", "completed", "skipped")]))
}
