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
  shiny::uiOutput(shiny::NS(id, "summary"))
}

#' @rdname run_summary_ui
#' @param board Reactive: the graph the board draws, for its targets.
#' @param running Reactive: whether the pipeline is running.
#' @param progress Reactive: the progress file's `list(mtime, data)`
#'   (graph_data_server()).
#' @return A reactive: run_summary(), or NULL before any run. It ticks every
#'   second while a run goes.
#' @noRd
run_summary_server <- function(id, board, running, progress) {
  stopifnot(shiny::is.reactive(board), shiny::is.reactive(running), shiny::is.reactive(progress))
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
      run_summary(
        current$data, board_targets(board()$x$nodes),
        started = started(), ended = current$mtime, running = running()
      )
    })
    output$summary <- shiny::renderUI(run_summary_tags(summary()))
    summary
  })
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
#' @return NULL when there is no run to tell of, or a list of `running`,
#'   `elapsed` (seconds, NA when unknown) and the counts `built`, `skipped`,
#'   `running_targets`, `errored`, `canceled` and `to_go`.
#' @noRd
run_summary <- function(progress, targets = character(), started = NA, ended = NA,
                        running = FALSE, now = Sys.time()) {
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
    to_go = sum(!targets %in% c(progress$name, progress$parent))
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
run_summary_tags <- function(summary) {
  if (is.null(summary)) return(NULL)
  title <- if (summary$running) "Running" else "Last run"
  elapsed <- format_elapsed(summary$elapsed)
  item <- function(n, what, class = NULL) {
    shiny::tags$span(class = c("tb-run-count", if (n > 0) class), shiny::tags$b(n), what)
  }
  counts <- list(
    item(summary$built, "built"),
    item(summary$skipped, "skipped"),
    if (summary$running) item(summary$running_targets, "running", "tb-run-count-running"),
    item(summary$errored, "errored", "tb-run-count-errored"),
    if (summary$canceled > 0) item(summary$canceled, "canceled"),
    if (summary$running) item(summary$to_go, "to go")
  )
  shiny::tags$div(
    class = c("tb-run-summary", if (summary$running) "tb-run-summary-running"),
    shiny::tags$div(
      class = "tb-run-summary-title",
      title,
      if (!is.na(elapsed)) shiny::tags$span(class = "tb-run-elapsed", elapsed)
    ),
    shiny::tags$div(class = "tb-run-counts", Filter(Negate(is.null), counts))
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
