# What the board does with a pipeline, one operation at a time, each run by
# run_guarded() in its own process from the pipeline's directory. Each
# returns the sizes it produced; run_guarded() adds time and memory.
# They call targetsboard's internals as the board does (installed package).

scale_operations <- list(

  # targets itself: build the store. Not the board's cost, but the context.
  build = function() {
    targets::tar_make(callr_function = NULL, reporter = "silent", use_crew = FALSE, as_job = FALSE)
    files <- list.files("_targets", recursive = TRUE, full.names = TRUE)
    list(store_mb = sum(file.size(files)) / 2^20, store_files = length(files))
  },

  # The poller: tar_visnetwork() on every change of the code or a finished
  # run, then the graph the widget gets, as JSON.
  graph = function() {
    x <- targets::tar_visnetwork(targets_only = TRUE)$x
    config <- targetsboard:::board_configuration(
      "flow", x$nodes, x$edges, "100%", "100%", options = targetsboard:::board_options()
    )
    json <- jsonlite::toJSON(config, auto_unbox = TRUE, dataframe = "rows")
    list(nodes = nrow(x$nodes), edges = nrow(x$edges), payload_mb = nchar(json, "bytes") / 2^20)
  },

  # The tar_meta() poll, on every change of the metadata file, and the table
  # the tar_meta() tab sends to the browser (all rows, client side).
  meta = function() {
    meta <- targets::tar_meta(targets_only = TRUE)
    json <- jsonlite::toJSON(as.data.frame(meta), dataframe = "columns")
    list(rows = nrow(meta), payload_mb = nchar(json, "bytes") / 2^20)
  },

  # The progress polls, every 0.5 s during a run: statuses and branch counts.
  progress = function() {
    time <- function(f) median(vapply(1:5, function(i) system.time(f())[["elapsed"]], numeric(1)))
    list(
      progress_s = time(function() targets::tar_progress()),
      branches_s = time(function() targets::tar_progress_branches()),
      progress_rows = nrow(targets::tar_progress())
    )
  },

  # The Problems tab and the warning marks, from tar_meta().
  problems = function() {
    meta <- targets::tar_meta(targets_only = TRUE)
    # The board has the graph's targets already: here, from the metadata.
    pipeline_targets <- meta$name[meta$type %in% c("stem", "pattern")]
    start <- Sys.time()
    table <- targetsboard:::problems_table(meta, pipeline_targets)
    warned <- targetsboard:::warned_targets_of(meta)
    list(
      compute_s = as.numeric(difftime(Sys.time(), start, units = "secs")),
      problem_rows = nrow(table), warned = length(warned)
    )
  },

  # The value preview of one target: read whole, then rendered as the board
  # renders it (skimr for a data frame, print() otherwise).
  preview = function(name) {
    value <- targets::tar_read_raw(name)
    bundle <- targetsboard:::render_and_output_bundle(value)
    text <- if (is.data.frame(value) && rlang::is_installed("skimr")) {
      utils::capture.output(print(skimr::skim(value)))
    } else {
      utils::capture.output(print(value))
    }
    list(
      name = name, class = class(value)[1],
      object_mb = as.numeric(utils::object.size(value)) / 2^20,
      rendered_mb = sum(nchar(text, "bytes")) / 2^20, rendered_lines = length(text)
    )
  },

  # The Problem pane of one target: for a pattern, every branch with a
  # problem, with its traceback.
  problem = function(name) {
    problem <- targetsboard:::target_problem(name, "_targets.R")
    html <- as.character(targetsboard:::problem_ui(problem))
    list(name = name, branches = length(problem$branches), html_mb = nchar(html, "bytes") / 2^20)
  }
)
