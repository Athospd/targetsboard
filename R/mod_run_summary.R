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
#' @param maker Reactive: run_maker() of the board's run, or NULL (#162).
#' @return A reactive: run_summary(), or NULL before any run. It ticks every
#'   second while a run goes.
#' @noRd
run_summary_server <- function(id, board, running, progress, tar_meta = shiny::reactive(NULL),
                               scope = shiny::reactive(NULL), maker = shiny::reactive(NULL)) {
  stopifnot(
    shiny::is.reactive(board), shiny::is.reactive(running), shiny::is.reactive(progress),
    shiny::is.reactive(tar_meta), shiny::is.reactive(scope), shiny::is.reactive(maker)
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
      is_running <- running()
      if (is_running) shiny::invalidateLater(1000)
      current <- progress()
      # A run just started: until targets records it, the process and
      # progress files are still the last run's (#183).
      this <- if (is_running) {
        current_run(current$data, current$mtime, run_started(), recorded = !is.null(active_run()))
      } else {
        list(progress = current$data, started = started())
      }
      graph <- board()$x
      nodes <- graph$nodes
      run_summary(
        this$progress, run_targets(board_targets(nodes), graph$edges, scope()),
        started = this$started, ended = current$mtime, running = is_running,
        meta = tar_meta(), outdated = outdated_targets(nodes), how = run_maker_label(maker())
      )
    })
    # The block is drawn once per shape (run_summary_shape()): Running or
    # Last run, which counts it lists, whether it tells the time left. A
    # reactiveVal ignores a value equal to the one it holds, so a new count
    # or the clock's tick don't draw it again: they are sent to the block in
    # place (run_summary_values(), srcjs/actions.js), so its nodes, the dot's
    # animation and the bar's widths carry on (#173).
    shape <- shiny::reactiveVal()
    shiny::observe(shape(run_summary_shape(summary())))
    output$summary <- shiny::renderUI({
      shape()
      run_summary_tags(shiny::isolate(summary()))
    })
    shiny::observe({
      current <- summary()
      if (is.null(current)) return()
      session$sendCustomMessage("tb-run-summary", c(list(id = session$ns("summary")), run_summary_values(current)))
    })
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
                        running = FALSE, now = Sys.time(), meta = NULL, outdated = character(),
                        how = NULL) {
  if (is.null(progress) || nrow(progress) == 0) {
    if (!running) return(NULL)
    # A run starting: nothing done yet, all to go.
    progress <- data.frame(name = character(), type = character(), parent = character(), progress = character())
  }
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
    },
    how = how # "tar_make_future(), 2 workers", or NULL for tar_make()
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
  # The clock: the time as text, which the server then updates in place
  # (run_summary_values()), or a tag given in its place.
  if (is.null(elapsed) && is.numeric(summary$elapsed) && !is.na(summary$elapsed)) {
    elapsed <- format_elapsed(summary$elapsed)
  }
  state <- run_state(summary)
  # A count, marked for run_summary_values()'s updates; `class` while above 0.
  item <- function(n, what, key, class = NULL) {
    shiny::tags$span(
      class = c("tb-run-count", if (n > 0) class), `data-count` = key, `data-active-class` = class,
      shiny::tags$b(n), what
    )
  }
  shown <- run_summary_shape(summary)$counts
  counts <- lapply(shown, function(key) {
    switch(key,
      built = item(summary$built, "built", key),
      skipped = item(summary$skipped, "skipped", key),
      running = item(summary$running_targets, "running", key, "tb-run-count-running"),
      errored = item(summary$errored, "errored", key, "tb-run-count-errored"),
      canceled = item(summary$canceled, "canceled", key),
      to_go = item(summary$to_go, "to go", key)
    )
  })
  # Where the run is: each part's share of the targets, in a thin bar. Every
  # part is there, at 0% while empty, so updates only change widths.
  values <- run_summary_values(summary)
  bar <- if (values$total > 0) {
    shiny::tags$div(
      class = "tb-run-bar", role = "progressbar",
      `aria-valuenow` = values$done, `aria-valuemin` = 0, `aria-valuemax` = values$total,
      lapply(names(values$bar), function(part) {
        shiny::tags$span(class = paste0("tb-run-bar-", part), style = sprintf("width: %.2f%%", values$bar[[part]]))
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
    shiny::tags$div(class = "tb-run-counts", counts),
    if (!is.null(values$left)) {
      shiny::tags$div(class = "tb-run-left", title = "From the targets' last builds", values$left)
    },
    if (!is.null(summary$how)) shiny::tags$div(class = "tb-run-how", summary$how)
  )
}

#' What the summary block is made of: drawn again only when this changes
#'
#' Whether a run goes and how it ended (the title and the dot), which counts
#' it lists, whether it has a bar, a clock and a time left. The numbers in
#' them are run_summary_values(), updated in place.
#' @param summary run_summary(), or NULL.
#' @noRd
run_summary_shape <- function(summary) {
  if (is.null(summary)) return(NULL)
  running <- isTRUE(summary$running)
  values <- run_summary_values(summary)
  list(
    state = run_state(summary),
    counts = c(
      "built", "skipped", if (running) "running",
      # Errored: during a run, once there is one.
      if (!running || summary$errored > 0) "errored",
      if (summary$canceled > 0) "canceled",
      if (running) "to_go"
    ),
    bar = values$total > 0,
    clock = !is.null(values$elapsed),
    left = !is.null(values$left),
    how = summary$how
  )
}

# How a run goes or ended: an error says more than a stop (canceled
# targets), which isn't a success (#204).
run_state <- function(summary) {
  if (isTRUE(summary$running)) "running"
  else if (summary$errored > 0) "errored"
  else if (summary$canceled > 0) "stopped"
  else "done"
}

#' The numbers of the summary block, as sent to update it in place
#' @param summary run_summary().
#' @return `counts`, `bar` (each part's width, in %), `done` and `total`
#'   for the bar's aria values, `elapsed` (the clock's text, or NULL) and
#'   `left` (the time left's text, or NULL).
#' @noRd
run_summary_values <- function(summary) {
  running <- isTRUE(summary$running)
  parts <- c(
    built = summary$built, skipped = summary$skipped, errored = summary$errored, canceled = summary$canceled,
    running = if (running) summary$running_targets else 0
  )
  total <- sum(parts) + if (running) summary$to_go else 0
  left <- summary$left
  list(
    counts = list(
      built = summary$built, skipped = summary$skipped, running = summary$running_targets,
      errored = summary$errored, canceled = summary$canceled, to_go = summary$to_go
    ),
    bar = as.list(if (total > 0) round(100 * parts / total, 2) else parts),
    done = sum(parts[c("built", "skipped", "errored", "canceled")]),
    total = total,
    elapsed = if (is.numeric(summary$elapsed) && !is.na(summary$elapsed)) format_elapsed(summary$elapsed),
    left = if (!is.null(left) && (left$seconds > 0 || left$unknown > 0)) {
      if (left$seconds > 0) {
        paste0("About ", format_elapsed(left$seconds), if (left$unknown > 0) "+", " left")
      } else {
        "Time left unknown: never built before"
      }
    }
  )
}

#' What a run going has told so far: none of the last run's records
#'
#' The board sees a run it starts before targets records it: until then,
#' tar_process() and the progress file are the last run's, so the summary
#' showed its clock and counts for a moment (#183).
#' @param progress The progress file's rows, or NULL.
#' @param mtime The progress file's modification time.
#' @param started When the recorded run's process started (run_started()).
#' @param recorded Whether the recorded process is the one running
#'   (active_run()).
#' @return `progress` (NULL while none is this run's) and `started` (NA while
#'   unknown).
#' @noRd
current_run <- function(progress, mtime, started, recorded) {
  if (!recorded || is.na(started)) return(list(progress = NULL, started = NA))
  # Written before this run started: the last run's lines.
  stale <- length(mtime) != 1 || is.na(mtime) || as.numeric(difftime(mtime, started, units = "secs")) < -1
  list(progress = if (!stale) progress, started = started)
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
