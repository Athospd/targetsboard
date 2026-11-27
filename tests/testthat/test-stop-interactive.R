# Stop never kills the author's own R session (#257).

test_that("interactive_r() tells an R session from a script's process", {
  is_r <- function(name, cmdline) {
    local_mocked_bindings(ps_name = function(p) name, ps_cmdline = function(p) cmdline, .package = "ps")
    interactive_r("handle")
  }
  expect_true(is_r("rsession.exe", "rsession"))
  expect_true(is_r("ark", c("ark", "--connection_file", "x.json")))
  expect_true(is_r("Rgui.exe", "Rgui"))
  expect_true(is_r("Rterm.exe", c("Rterm.exe", "--interactive")))
  expect_true(is_r("R", "R"))
  # callr's processes run a file: a run's, the board's own.
  expect_false(is_r("Rterm.exe", c("Rterm.exe", "--no-readline", "--slave", "--no-save", "--no-restore", "-f", "C:/tmp/callr-scr")))
  expect_false(is_r("R", c("R", "--file=x.R")))
  expect_false(is_r("Rscript", c("Rscript", "x.R")))
  # A process that can't be read: not killed either.
  local_mocked_bindings(ps_name = function(p) stop("gone"), .package = "ps")
  expect_true(interactive_r("handle"))
})

test_that("a run in this R session is no longer going once it marks its end", {
  skip_on_cran()
  local_pipeline()
  targets::tar_make(callr_function = NULL, reporter = "silent")
  # targets recorded this session, which is alive: the run looks to be going.
  expect_false(is.null(active_run()))
  mark_run_ended()
  expect_null(active_run())
  # A store marked by mark_run_ended() before #279, its lock left: still ended.
  targets::tar_make(callr_function = NULL, reporter = "silent")
  info <- targets::tar_process()
  dir.create(file.path("_targets", "user"), showWarnings = FALSE)
  writeLines(info$value[match(c("pid", "created"), info$name)], file.path("_targets", "user", "targetsboard_ended"))
  expect_null(active_run())
})

test_that("the debug run's code marks its end, even when left with Q", {
  code <- debug_run_code("b", script = "_targets.R")
  expect_match(code, "local({", fixed = TRUE)
  expect_match(code, "on.exit(targetsboard::mark_run_ended(", fixed = TRUE)
  expect_true(is.expression(parse(text = code)))
  code <- debug_run_code("b", script = "_targets.R", store = "other_store")
  expect_match(code, "mark_run_ended(store = \"other_store\")", fixed = TRUE)
})

test_that("Stop refuses a run in an R session, and says where to stop it", {
  skip_on_cran()
  local_pipeline()
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  killed <- 0
  local_mocked_bindings(
    active_run = function() structure(list(), class = "fake_handle"),
    interactive_r = function(handle) TRUE,
    stop_run = function(handle) killed <<- killed + 1,
    run_pid = function(handle) 1234L
  )
  unblocked <- 0
  local_mocked_bindings(tar_unblock_process = function(...) unblocked <<- unblocked + 1, .package = "targets")
  shiny::testServer(run_server, args = list(
    backend = backend, board = shiny::reactiveVal(NULL), running = shiny::reactiveVal(TRUE),
    action = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(stop = 1)
    expect_equal(killed, 0)
    expect_match(last_failure(), "your R session (pid 1234)", fixed = TRUE)
    # Its lock may be a debug run's, left with Q: unblocked when asked (#279).
    expect_false(is.null(blocked()))
    session$setInputs(unblock = 1)
    expect_equal(unblocked, 1)
    expect_null(blocked())
  })
})
