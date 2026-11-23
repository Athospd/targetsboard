# Scale budgets: what the board should still do with a huge pipeline.
#
# A test-driven exercise: these tests state the budgets first; the ones that
# fail are the limitations to work on. They are slow and heavy, so they only
# run when asked:
#
#   TARGETSBOARD_SCALE_DIR  where the pipelines and their stores live (kept
#                           between runs: a 10,000-rep build takes minutes)
#   TARGETSBOARD_SCALE_N    sizes to try, e.g. "100,1000,10000" (default "100")
#   TARGETSBOARD_SCALE_CAP  memory cap per operation, in GB (default 8)
#   TARGETSBOARD_SCALE_BROWSER_N  sizes to open in the browser (default: the
#                           same): a 20,000-target page is the riskiest step
#
# Every operation runs in its own process, killed over the cap, and is also
# written to <dir>/results.csv. Uses the installed package, as the board does.
# The pipelines and operations are in dev/scale/.

scale_dir <- Sys.getenv("TARGETSBOARD_SCALE_DIR")
skip_if(scale_dir == "", "TARGETSBOARD_SCALE_DIR is not set: scale tests not asked for")
skip_on_cran()

dev_scale <- normalizePath(test_path("..", "..", "dev", "scale"), mustWork = TRUE)
source(file.path(dev_scale, "pipelines.R"), local = TRUE)
source(file.path(dev_scale, "guard.R"), local = TRUE)
source(file.path(dev_scale, "operations.R"), local = TRUE)

sizes <- as.integer(strsplit(Sys.getenv("TARGETSBOARD_SCALE_N", "100"), ",")[[1]])
cap_gb <- as.numeric(Sys.getenv("TARGETSBOARD_SCALE_CAP", "8"))
results_csv <- file.path(scale_dir, "results.csv")

# Run an operation on a pipeline, guarded, and record it.
measure <- function(kind, n, op, ...) {
  dir <- file.path(scale_dir, sprintf("%s-%d", kind, n))
  # Long enough to measure, short enough not to wait for the hopeless: the
  # budgets are seconds, the build minutes.
  timeout <- switch(op, build = 3600, graph = 1800, 300)
  result <- run_guarded(scale_operations[[op]], list(...), dir = dir, cap_gb = cap_gb,
                        timeout = timeout, label = op)
  row <- data.frame(
    time = format(Sys.time()), kind = kind, n = n, op = op,
    target = if (length(list(...))) ..1 else NA_character_,
    seconds = result$seconds %||% NA, peak_rss_mb = result$peak_rss_mb, heap_mb = result$heap_mb %||% NA,
    killed = result$killed, error = result$error %||% NA_character_,
    details = jsonlite::toJSON(result[setdiff(names(result), c(
      "label", "seconds", "peak_rss_mb", "heap_mb", "killed", "error"
    ))], auto_unbox = TRUE, digits = 4)
  )
  utils::write.table(row, results_csv, sep = ",", row.names = FALSE,
                      col.names = !file.exists(results_csv), append = file.exists(results_csv))
  result
}

# Within budget: not killed, no error, and under the time and memory given.
expect_within <- function(result, seconds = Inf, rss_mb = Inf) {
  expect_true(is.na(result$killed), info = result$killed)
  expect_null(result$error)
  expect_lt(result$seconds %||% Inf, seconds)
  expect_lt(result$peak_rss_mb, rss_mb)
}

for (n in sizes) for (kind in c("dynamic", "static")) {
  dir <- file.path(scale_dir, sprintf("%s-%d", kind, n))

  test_that(sprintf("%s, %d reps: the pipeline builds (context, no budget)", kind, n), {
    write_scale_pipeline(dir, kind, n)
    if (!dir.exists(file.path(dir, "_targets"))) {
      result <- measure(kind, n, "build")
      expect_true(is.na(result$killed), info = result$killed)
    }
    expect_true(dir.exists(file.path(dir, "_targets", "meta")))
  })
  if (!dir.exists(file.path(dir, "_targets", "meta"))) next # the build test failed

  test_that(sprintf("%s, %d reps: the graph is recomputed in 10 s and 1 GB, and sent in 5 MB", kind, n), {
    result <- measure(kind, n, "graph")
    expect_within(result, seconds = 10, rss_mb = 1024)
    expect_lt(result$payload_mb %||% Inf, 5)
  })

  test_that(sprintf("%s, %d reps: tar_meta() is re-read in 1 s, and its tab sends 5 MB at most", kind, n), {
    result <- measure(kind, n, "meta")
    expect_within(result, seconds = 1, rss_mb = 1024)
    expect_lt(result$payload_mb %||% Inf, 5)
  })

  test_that(sprintf("%s, %d reps: a progress poll takes 0.2 s at most (it runs every 0.5 s)", kind, n), {
    result <- measure(kind, n, "progress")
    expect_within(result)
    expect_lt(result$progress_s %||% Inf, 0.2)
    expect_lt(result$branches_s %||% Inf, 0.2)
  })

  test_that(sprintf("%s, %d reps: the Problems tab and warning marks take 0.5 s", kind, n), {
    result <- measure(kind, n, "problems")
    expect_within(result, rss_mb = 1024)
    expect_lt(result$compute_s %||% Inf, 0.5)
  })

  # The biggest values: the aggregate of a pattern, and its combined result.
  previews <- if (kind == "dynamic") c("boot", "fit") else c("boot_1", "estimates")
  for (name in previews) {
    test_that(sprintf("%s, %d reps: the preview of %s shows in 2 s, 500 MB and 1 MB of text", kind, n, name), {
      result <- measure(kind, n, "preview", name)
      expect_within(result, seconds = 2, rss_mb = 500)
      expect_lt(result$rendered_mb %||% Inf, 1)
    })
  }

  problem <- if (kind == "dynamic") "fit" else "fit_100"
  test_that(sprintf("%s, %d reps: the Problem pane of %s shows in 2 s", kind, n, problem), {
    result <- measure(kind, n, "problem", problem)
    expect_within(result, seconds = 2, rss_mb = 1024)
  })
}

# The page: the board opened on each built pipeline, in headless Chrome.
# Runs after the R-side budgets, so they don't compete for the CPU.
skip_if_not_installed("shinytest2")
skip_if(is.null(chromote::find_chrome()), "Chrome not found")

browser_sizes <- as.integer(strsplit(Sys.getenv("TARGETSBOARD_SCALE_BROWSER_N", paste(sizes, collapse = ",")), ",")[[1]])
for (n in browser_sizes) for (kind in c("dynamic", "static")) {
  dir <- file.path(scale_dir, sprintf("%s-%d", kind, n))
  if (!dir.exists(file.path(dir, "_targets", "meta"))) next
  expected <- if (kind == "dynamic") 6 else 2 * n + 3 # data, the mapped targets, estimates, ci

  test_that(sprintf("%s, %d reps: the board draws its %d targets in 60 s, under 1 GB of JS heap, and answers a click", kind, n, expected), {
    writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))
    start <- Sys.time()
    app <- tryCatch(
      shinytest2::AppDriver$new(dir, name = "scale", load_timeout = 300 * 1000, timeout = 60 * 1000),
      error = function(e) e
    )
    if (inherits(app, "error")) fail(paste("The board did not start:", conditionMessage(app)))
    withr::defer(app$stop())
    drawn <- tryCatch({
      app$wait_for_js(sprintf("document.querySelectorAll('.react-flow__node').length === %d", expected),
                      timeout = 300 * 1000, interval = 1000)
      TRUE
    }, error = function(e) FALSE)
    seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    heap_mb <- app$get_js("performance.memory ? performance.memory.usedJSHeapSize / 1048576 : null")
    # A click on a target: the time until it is focused, the page's answer.
    click_start <- Sys.time()
    focused <- tryCatch({
      app$run_js("document.querySelector('.react-flow__node').click()")
      app$wait_for_js("document.querySelector('.react-flow__node.tb-focused') !== null", timeout = 60 * 1000)
      TRUE
    }, error = function(e) FALSE)
    click_s <- as.numeric(difftime(Sys.time(), click_start, units = "secs"))
    utils::write.table(
      data.frame(time = format(Sys.time()), kind = kind, n = n, op = "browser", target = NA,
                 seconds = seconds, peak_rss_mb = NA, heap_mb = heap_mb %||% NA, killed = NA, error = NA,
                 details = jsonlite::toJSON(list(drawn = drawn, click_s = click_s, focused = focused), auto_unbox = TRUE)),
      results_csv, sep = ",", row.names = FALSE, col.names = !file.exists(results_csv), append = TRUE
    )
    expect_true(drawn)
    expect_lt(seconds, 60)
    expect_lt(heap_mb %||% Inf, 1024)
    expect_true(focused)
    expect_lt(click_s, 1)
  })
}
