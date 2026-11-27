# The run queue (#106) in the run module and on the board: runs asked for
# during a run wait, merged, and start when it ends. The module alone:
# test-mod-run-queue.R.

test_that("Run during a run queues it; Run on a queued target unqueues it; the queue starts when the run ends", {
  skip_on_cran()
  local_pipeline()
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  is_busy <- TRUE
  backend$busy <- function() is_busy
  started <- list()
  local_mocked_bindings(start_run = function(script, names = NULL, ...) {
    started[[length(started) + 1]] <<- names
    structure(list(), class = "fake_run")
  })
  running <- shiny::reactiveVal(TRUE)
  action <- shiny::reactiveVal(NULL)
  board <- shiny::reactiveVal(list(x = targets::tar_visnetwork(targets_only = TRUE)$x))
  shiny::testServer(run_server, args = list(backend = backend, board = board, running = running, action = action), {
    action(list(action = "run", nodes = list("a", "b")))
    session$flushReact()
    action(list(action = "run", nodes = list("c")))
    session$flushReact()
    expect_length(started, 0)
    expect_equal(session$returned$queued(), c("a", "b", "c"))
    # Run on queued targets takes them out.
    action(list(action = "run", nodes = list("b")))
    session$flushReact()
    expect_equal(session$returned$queued(), c("a", "c"))

    # The run ends: the queue starts, as one run, and empties.
    is_busy <<- FALSE
    running(FALSE)
    session$flushReact()
    expect_equal(started, list(c("a", "c")))
    expect_equal(session$returned$queued(), character())

    # Stop clears it; so does Clear queue.
    is_busy <<- TRUE
    running(TRUE)
    action(list(action = "run", nodes = list("a")))
    session$flushReact()
    session$setInputs(stop = 1)
    expect_equal(session$returned$queued(), character())
    action(list(action = "run", nodes = list("a")))
    session$flushReact()
    session$setInputs(`queue-clear_queue` = 1)
    expect_equal(session$returned$queued(), character())
  })
})

test_that("on the board, Run during a run queues the target with a clock, and the queued run builds it", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "list(tar_target(slow, {Sys.sleep(15); 1}), tar_target(quick, 2))" # long enough for a slow runner
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "run-queue")
  node <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s']\")", id)
  app$wait_for_js(sprintf("%s !== null", node("quick")), timeout = 30000)
  mouse_event(app, "slow", "contextmenu")
  app$wait_for_js("document.querySelector(\".tb-node-menu [data-action='run']\") !== null", timeout = 10000)
  app$click(selector = ".tb-node-menu [data-action='run']")
  app$wait_for_js(sprintf("%s.querySelector('.base-node.tb-status-running, .spinner') !== null", node("slow")), timeout = 30000)

  # Run on quick while slow runs: queued, with a clock; its Run reads Unqueue.
  mouse_event(app, "quick", "contextmenu")
  app$wait_for_js("document.querySelector(\".tb-node-menu [data-action='run']\") !== null", timeout = 10000)
  app$click(selector = ".tb-node-menu [data-action='run']")
  app$wait_for_js(sprintf("%s.querySelector('.queued-mark') !== null", node("quick")), timeout = 10000)
  # The line is its own output: it can come a moment after the clock (macOS CI).
  app$wait_for_js("(document.querySelector('.tb-run-queue') || {}).innerText?.includes('Queued: quick') === true", timeout = 10000)
  mouse_event(app, "quick", "contextmenu")
  app$wait_for_js("document.querySelector(\".tb-node-menu [data-action='run']\") !== null", timeout = 10000)
  expect_equal(app$get_text(".tb-node-menu [data-action='run']"), "Unqueue")
  app$run_js("document.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape'}))")

  # slow ends: the queued run builds quick, and the clock goes.
  app$wait_for_js(sprintf("%s.querySelector('.base-node.tb-status-uptodate') !== null", node("quick")), timeout = 90000)
  expect_true(app$get_js(sprintf("%s.querySelector('.queued-mark') === null", node("quick"))))
  expect_true(app$get_js("document.querySelector('.tb-run-queue') === null"))
})
