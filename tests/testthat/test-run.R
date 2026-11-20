# A pipeline whose second target runs long enough to be caught running.
local_slow_pipeline <- function(.env = parent.frame()) {
  dir <- local_pipeline(.env = .env)
  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, {Sys.sleep(60); a}), tar_target(c, b))"),
    "_targets.R"
  )
  dir
}

# Starts the slow pipeline outside the board, as from an R console, and
# waits until b runs.
local_console_run <- function(.env = parent.frame()) {
  process <- targets::tar_make(callr_function = callr::r_bg, reporter = "silent")
  withr::defer(process$kill_tree(), envir = .env)
  wait_for(function() {
    progress <- targets::tar_progress()
    any(progress$name == "b" & progress$progress == "dispatched")
  })
  process
}

test_that("no store, or a finished run, is no active run", {
  local_pipeline()
  expect_null(active_run())
  targets::tar_make(reporter = "silent")
  expect_null(active_run())
})

test_that("a run started elsewhere is found, and not once it is killed", {
  skip_on_cran()
  local_slow_pipeline()
  process <- local_console_run()

  handle <- active_run()
  expect_s3_class(handle, "ps_handle")
  expect_equal(ps::ps_pid(handle), process$get_pid())

  process$kill_tree()
  wait_for(function() is.null(active_run()))
  expect_equal(targets::tar_progress()$progress[targets::tar_progress()$name == "b"], "dispatched")
})

test_that("a recycled pid is not taken for the run", {
  local_pipeline()
  dir.create("_targets/meta", recursive = TRUE)
  # This R process, but recorded as created long before it started.
  writeLines(
    c("name|value", paste0("pid|", Sys.getpid()), "created|2001-01-01 00:00:00.000000"),
    "_targets/meta/process"
  )
  expect_null(active_run())
})

test_that("dispatched statuses settle to canceled once nothing runs", {
  statuses <- c(a = "completed", b = "dispatched")
  expect_equal(settle_statuses(statuses, running = TRUE), statuses)
  expect_equal(settle_statuses(statuses, running = FALSE), c(a = "completed", b = "canceled"))
})

test_that("the board shows a killed run's targets as canceled", {
  skip_on_cran()
  local_slow_pipeline()
  server <- local_app_server()

  sent <- list()
  local_mocked_bindings(update_targetsboard_status = function(session, inputId, statuses) {
    sent[[length(sent) + 1]] <<- statuses
  })

  shiny::testServer(server, {
    environment(server)$stop_poller() # keep the graph as it is
    session$flushReact()
    process <- local_console_run()
    session$elapse(600)
    expect_equal(sent[[length(sent)]][["b"]], "dispatched")

    process$kill_tree()
    wait_for(function() is.null(active_run()))
    session$elapse(600)
    expect_equal(sent[[length(sent)]][["b"]], "canceled")
  })
})
