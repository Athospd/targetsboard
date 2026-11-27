# targets' process lock (<store>/meta/process): a run in an R session that
# lives on, a debug run left with Q, blocks every later run on the store
# (#279). And targets' messages reach the page without colour codes (#280).

test_that("mark_run_ended() unblocks the store this session ran in, and only that", {
  skip_on_cran()
  local_pipeline()
  targets::tar_make(callr_function = NULL, reporter = "silent")
  process <- file.path("_targets", "meta", "process")
  expect_true(file.exists(process)) # this session's pid, alive: targets would refuse a run
  mark_run_ended()
  expect_false(file.exists(process))
  expect_null(active_run())

  # Another process's run: not this session's to unblock.
  targets::tar_make(reporter = "silent") # in a callr process, which has ended
  expect_true(file.exists(process))
  mark_run_ended()
  expect_true(file.exists(process))
})

test_that("a store blocked by an R session is told apart from other run errors", {
  lock <- "Process ID 153332 is already running a {targets} pipeline with the _targets folder ..."
  local_mocked_bindings(
    active_run = function() NULL,
    recorded_handle = function(store = NULL) structure(list(), class = "fake_handle"),
    interactive_r = function(handle) TRUE,
    run_pid = function(handle) 153332L
  )
  expect_equal(session_lock(lock), 153332L)
  expect_null(session_lock("object 'x' not found"))
  expect_null(session_lock(NULL))
  # A pipeline process, not an R session: it may well be running.
  local_mocked_bindings(interactive_r = function(handle) FALSE)
  expect_null(session_lock(lock))
})

test_that("run_error() says targets' error without colour codes (#280)", {
  red <- "\033[31mProcess ID 1 is already running\033[39m"
  process <- list(get_result = function() stop(red))
  expect_identical(run_error(process), "Process ID 1 is already running")
  traced <- structure(list(condition = simpleError(red)), class = "tar_condition_traced")
  process <- list(get_result = function() traced)
  expect_identical(run_error(process), "Process ID 1 is already running")
})

test_that("the metadata's errors and warnings come without colour codes (#280)", {
  meta <- data.frame(name = c("a", "b"), error = c("\033[31mboom\033[39m", NA), warnings = c(NA, "\033[33mhm\033[39m"))
  plain <- plain_meta(meta)
  expect_identical(plain$error, c("boom", NA))
  expect_identical(plain$warnings, c(NA, "hm"))
  expect_null(plain_meta(NULL))
  expect_identical(plain_meta(data.frame(name = "a")), data.frame(name = "a"))
})

test_that("a Run that an R session's lock refuses offers to unblock, and runs once asked (#279)", {
  skip_on_cran()
  local_pipeline()
  # A debug run left without mark_run_ended() (an older copied snippet): an
  # R session ran the pipeline in itself, and lives on. Not this process:
  # targets exempts the parent of the run it refuses.
  session <- callr::r_bg(function(dir) {
    setwd(dir)
    targets::tar_make(callr_function = NULL, reporter = "silent")
    Sys.sleep(300)
  }, args = list(getwd()))
  withr::defer(session$kill())
  wait_for(function() identical(
    tryCatch(targets::tar_process()$value[1], error = function(e) NULL), as.character(session$get_pid())
  ) && length(targets::tar_outdated()) == 0, timeout = 60)
  targets::tar_invalidate(everything())
  args <- local_board_args()
  backend <- args$backend
  notes <- character()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes <<- c(notes, paste(type, paste(as.character(ui), collapse = " ")))
    },
    .package = "shiny"
  )
  # The lock's session is an R session; the board would see it as a run
  # going without the marker an older mark_run_ended() wrote.
  local_mocked_bindings(interactive_r = function(handle) TRUE, active_run = function() NULL)
  shiny::testServer(board_server, args = args, {
    backend$stop_poller()
    session$flushReact()
    session$setInputs(`run-all` = 1)
    session$elapse(600) # the board sees the run start
    wait_for(function() !backend$runner$process$is_alive(), timeout = 60)
    session$elapse(600)
    note <- notes[grepl("already running", notes)]
    expect_length(note, 1)
    expect_match(note, "your R session", fixed = TRUE)
    expect_match(note, "Unblock and run", fixed = TRUE)
    expect_match(note, session$ns("run-unblock"), fixed = TRUE)
    expect_no_match(note, "\033", fixed = TRUE)
    expect_true(file.exists(file.path("_targets", "meta", "process"))) # not without asking

    session$setInputs(`run-unblock` = 1)
    session$elapse(600)
    wait_for(function() !backend$runner$process$is_alive(), timeout = 60)
    session$elapse(600)
    expect_match(notes, "Run finished.", fixed = TRUE, all = FALSE)
    expect_equal(targets::tar_outdated(), character())
  })
})

test_that("the debug run's code, left with Q in an interactive R, unblocks the store (#279)", {
  skip_on_cran()
  local_pipeline()
  writeLines(c("library(targets)", "list(tar_target(a, {browser(); 1}))"), "_targets.R")
  # The board's code, as copied into a console, then Q in the debugger. It
  # runs the installed targetsboard, like the author's session.
  input <- tempfile(fileext = ".R")
  writeLines(c(
    debug_run_code("a", script = "_targets.R"),
    "Q",
    "cat('LOCKED:', file.exists('_targets/meta/process'), '\n')",
    "q('no')"
  ), input)
  # An interactive R reading stdin: Rterm --ess on Windows, R --interactive elsewhere.
  windows <- .Platform$OS.type == "windows"
  r <- file.path(R.home("bin"), if (windows) "Rterm.exe" else "R")
  out <- system2(r, c(if (windows) "--ess" else "--interactive", "--vanilla", "--quiet"),
                 stdin = input, stdout = TRUE, stderr = TRUE)
  expect_match(out, "Browse[", fixed = TRUE, all = FALSE) # it stopped in the debugger
  expect_match(out, "LOCKED: FALSE", fixed = TRUE, all = FALSE)
})
