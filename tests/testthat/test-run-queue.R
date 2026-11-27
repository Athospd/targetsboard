# The run queue (#106): runs asked for during a run wait, merged, and start
# when it ends.

test_that("the queue merges requests, takes them back, and says what it holds", {
  q <- queue_add(empty_queue(), c("a", "b"))
  q <- queue_add(q, c("b", "c"))
  expect_equal(queued_targets(q, c("a", "b", "c", "d")), c("a", "b", "c"))
  expect_equal(queue_remove(q, "b")$names, c("a", "c"))
  # Run all: every target, whatever the pipeline is by then.
  all <- queue_add(q, NULL)
  expect_true(all$all)
  expect_equal(queued_targets(all, c("a", "b", "c", "d")), c("a", "b", "c", "d"))
  expect_equal(queue_run_names(all, c("a", "b")), NULL)
  # Taking one out of "all" leaves the others, by name.
  expect_equal(queue_remove(all, "b", targets = c("a", "b", "c", "d")), list(all = FALSE, names = c("a", "c", "d")))
  expect_true(queue_empty(empty_queue()))
  expect_false(queue_empty(q))
  expect_equal(queue_run_names(q), c("a", "b", "c"))
})

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
    session$setInputs(clear_queue = 1)
    expect_equal(session$returned$queued(), character())
  })
})

test_that("the queue's line names what waits, with Clear queue", {
  html <- as.character(run_queue_ui(c("a", "b"), "clear"))
  expect_match(html, "Queued: a, b", fixed = TRUE)
  expect_match(html, "Clear queue", fixed = TRUE)
  expect_null(run_queue_ui(character(), "clear"))
})

test_that("on the board, Run during a run queues the target with a clock, and the queued run builds it", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "list(tar_target(slow, {Sys.sleep(8); 1}), tar_target(quick, 2))"
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
  expect_match(app$get_text(".tb-run-queue"), "Queued: quick", fixed = TRUE)
  mouse_event(app, "quick", "contextmenu")
  app$wait_for_js("document.querySelector(\".tb-node-menu [data-action='run']\") !== null", timeout = 10000)
  expect_equal(app$get_text(".tb-node-menu [data-action='run']"), "Unqueue")
  app$run_js("document.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape'}))")

  # slow ends: the queued run builds quick, and the clock goes.
  app$wait_for_js(sprintf("%s.querySelector('.base-node.tb-status-uptodate') !== null", node("quick")), timeout = 90000)
  expect_true(app$get_js(sprintf("%s.querySelector('.queued-mark') === null", node("quick"))))
  expect_true(app$get_js("document.querySelector('.tb-run-queue') === null"))
})
