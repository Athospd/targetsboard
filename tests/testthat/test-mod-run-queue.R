# The run queue module (#106), on its own: requests in, `queued` and `due`
# out.

test_that("the queue merges requests, takes them back, and says what it holds", {
  q <- queue_add(empty_queue(), c("a", "b"))
  q <- queue_add(q, c("b", "c"))
  expect_equal(queued_targets(q, c("a", "b", "c", "d")), c("a", "b", "c"))
  expect_equal(queue_remove(q, "b")$names, c("a", "c"))
  # Run all: every target, whatever the pipeline is by then.
  all <- queue_add(q, NULL)
  expect_true(all$all)
  expect_equal(queued_targets(all, c("a", "b", "c", "d")), c("a", "b", "c", "d"))
  expect_equal(queue_run_names(all), NULL)
  # Taking one out of "all" leaves the others, by name.
  expect_equal(queue_remove(all, "b", targets = c("a", "b", "c", "d")), list(all = FALSE, names = c("a", "c", "d")))
  expect_true(queue_empty(empty_queue()))
  expect_false(queue_empty(q))
  expect_equal(queue_run_names(q), c("a", "b", "c"))
})

test_that("run_queue_server() queues, unqueues, clears, and says when the queue is due", {
  is_busy <- TRUE
  request <- shiny::reactiveVal(NULL)
  unqueue <- shiny::reactiveVal(NULL)
  clear <- shiny::reactiveVal(NULL)
  running <- shiny::reactiveVal(TRUE)
  targets <- shiny::reactiveVal(c("a", "b", "c"))
  shiny::testServer(run_queue_server, args = list(
    request = request, unqueue = unqueue, clear = clear, running = running,
    busy = function() is_busy, targets = targets
  ), {
    request(list(names = c("a", "b"), n = 1))
    session$flushReact()
    request(list(names = "c", n = 2))
    session$flushReact()
    expect_equal(session$returned$queued(), c("a", "b", "c"))
    unqueue(list(names = "b", n = 1))
    session$flushReact()
    expect_equal(session$returned$queued(), c("a", "c"))
    expect_null(session$returned$due())

    # The run ends: the queue is due, as one run, and empties.
    is_busy <<- FALSE
    running(FALSE)
    session$flushReact()
    expect_equal(session$returned$due()$names, c("a", "c"))
    expect_equal(session$returned$queued(), character())

    # Clear (Stop) and Clear queue empty it.
    is_busy <<- TRUE
    running(TRUE)
    request(list(names = "a", n = 3))
    session$flushReact()
    clear(1)
    session$flushReact()
    expect_equal(session$returned$queued(), character())
    request(list(names = "a", n = 4))
    session$flushReact()
    session$setInputs(clear_queue = 1)
    expect_equal(session$returned$queued(), character())
    # Run all queues every target.
    request(list(names = NULL, n = 5))
    session$flushReact()
    expect_equal(session$returned$queued(), c("a", "b", "c"))
  })
})

test_that("the queue's line names what waits, with Clear queue", {
  html <- as.character(run_queue_line(c("a", "b"), "clear"))
  expect_match(html, "Queued: a, b", fixed = TRUE)
  expect_match(html, "Clear queue", fixed = TRUE)
  expect_null(run_queue_line(character(), "clear"))
})
