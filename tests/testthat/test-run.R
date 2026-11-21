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
  # Windows can keep a finished process visible for a moment.
  expect_no_error(wait_for(function() is.null(active_run()), timeout = 10))
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
  local_mocked_bindings(
    update_targetsboard_status = function(session, inputId, statuses = character(), warnings = NULL) {
      if (length(statuses)) sent[[length(sent) + 1]] <<- statuses # not the warning marks
    }
  )

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

test_that("start_run() makes the whole pipeline or the selected targets", {
  skip_on_cran()
  local_pipeline()

  process <- start_run("_targets.R", names = "b")
  process$wait(60000)
  expect_setequal(targets::tar_progress()$name, c("a", "b"))
  expect_true(file.exists(attr(process, "log")))
  expect_null(run_error(process))

  process <- start_run("_targets.R")
  process$wait(60000)
  expect_true(all(targets::tar_visnetwork(targets_only = TRUE)$x$nodes$status == "uptodate"))
})

test_that("stop_run() kills a run", {
  skip_on_cran()
  local_slow_pipeline()
  process <- local_console_run()
  took <- system.time(stop_run(active_run()))[["elapsed"]]
  expect_lt(took, 2) # it runs in the board's process: it must not block
  wait_for(function() !process$is_alive())
  wait_for(function() is.null(active_run())) # Windows lets go of it a moment later
  expect_false(process$is_alive())
})

test_that("the board runs, refuses a second run, stops and tells how it went", {
  skip_on_cran()
  local_slow_pipeline()
  server <- local_app_server()

  notes <- character()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes <<- c(notes, paste(type, paste(as.character(ui), collapse = " ")))
    },
    .package = "shiny"
  )

  shiny::testServer(server, {
    environment(server)$stop_poller()
    session$flushReact()

    session$setInputs(run_all = 1)
    runner <- environment(server)$runner
    withr::defer(if (runner$process$is_alive()) runner$process$kill_tree())
    wait_for(function() {
      progress <- targets::tar_progress()
      any(progress$name == "b" & progress$progress == "dispatched")
    })
    session$elapse(600)
    expect_true(running())

    session$setInputs(run_selected = 1, flow_selected_nodes = "c")
    expect_match(notes, "already going", all = FALSE)

    session$setInputs(stop = 1)
    wait_for(function() !runner$process$is_alive())
    session$elapse(600)
    expect_false(running())
    expect_match(notes, "warning Run stopped", all = FALSE)
  })
})

test_that("a failed run is reported with its error", {
  skip_on_cran()
  local_pipeline()
  writeLines(c("library(targets)", "list(tar_target(a, stop('boom')))"), "_targets.R")
  server <- local_app_server()

  notes <- character()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes <<- c(notes, paste(type, paste(as.character(ui), collapse = " ")))
    },
    .package = "shiny"
  )

  shiny::testServer(server, {
    environment(server)$stop_poller()
    session$flushReact()
    session$setInputs(run_all = 1)
    session$elapse(600) # the board sees the run start
    expect_true(running())
    wait_for(function() !environment(server)$runner$process$is_alive(), timeout = 60)
    session$elapse(600)
    expect_match(notes, "error Run failed:[[:space:]]*<code>boom</code>", all = FALSE)
  })
})

test_that("invalidate_targets() makes targets outdated, leaving their files", {
  skip_on_cran()
  local_pipeline(make = TRUE)

  invalidate_targets(c("b", "not_a_target"))

  expect_false("b" %in% targets::tar_meta(targets_only = TRUE)$name)
  expect_setequal(targets::tar_outdated(), c("b", "c"))
  expect_true(file.exists("_targets/objects/b"))
  expect_error(targets::tar_read(b)) # but without metadata, it can't be read
})

test_that("the board invalidates the selected targets once confirmed", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  notes <- character()
  modal <- NULL
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes <<- c(notes, paste(type, paste(as.character(ui), collapse = " ")))
    },
    showModal = function(ui, ...) modal <<- as.character(ui),
    removeModal = function(...) NULL,
    .package = "shiny"
  )

  shiny::testServer(server, {
    session$setInputs(flow_selected_nodes = c("b", "c"), invalidate = 1)
    expect_match(modal, "Invalidate targets?", fixed = TRUE)
    expect_match(modal, "<code>b</code>", fixed = TRUE)
    expect_true(all(c("b", "c") %in% targets::tar_meta(targets_only = TRUE)$name)) # not yet

    session$setInputs(invalidate_confirm = 1)
    expect_false(any(c("b", "c") %in% targets::tar_meta(targets_only = TRUE)$name))
    expect_true(file.exists(poller_of(server)$refresh_path)) # the graph is recomputed
    expect_match(notes, "Invalidated 2 targets", all = FALSE)
  })
})

test_that("the board refuses to invalidate while a run is going", {
  skip_on_cran()
  local_slow_pipeline()
  server <- local_app_server()

  notes <- character()
  invalidated <- NULL
  local_mocked_bindings(invalidate_targets = function(names) invalidated <<- names)
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes <<- c(notes, paste(type, paste(as.character(ui), collapse = " ")))
    },
    removeModal = function(...) NULL,
    .package = "shiny"
  )

  process <- local_console_run()
  shiny::testServer(server, {
    environment(server)$stop_poller()
    session$setInputs(flow_selected_nodes = "b", invalidate_confirm = 1)
    expect_null(invalidated)
    expect_match(notes, "stop it before invalidating", all = FALSE)
  })
})
