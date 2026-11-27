# The run module, on its own: run_server() with its inputs as reactiveVals.

run_args <- function(.env = parent.frame()) {
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller(), envir = .env)
  list(
    backend = backend,
    board = shiny::reactiveVal(list(x = targets::tar_visnetwork(targets_only = TRUE)$x)),
    running = shiny::reactiveVal(FALSE),
    action = shiny::reactiveVal(NULL)
  )
}

test_that("run_server() checks its arguments", {
  args <- list(backend = list(), board = shiny::reactive(NULL), running = shiny::reactive(FALSE), action = shiny::reactive(NULL))
  expect_error(do.call(run_server, c("r", utils::modifyList(args, list(action = list())))), "is.reactive")
  expect_error(do.call(run_server, c("r", utils::modifyList(args, list(backend = shiny::reactive(1))))), "is.reactive")
})

test_that("Run all and the graph's Run start a run with the modes; Value is not the run's", {
  skip_on_cran()
  local_pipeline()
  args <- run_args()
  started <- list()
  local_mocked_bindings(start_run = function(script, names = NULL, modes = list(), ...) {
    started[[length(started) + 1]] <<- list(names = names, modes = modes)
    NULL
  })
  shiny::testServer(run_server, args = args, {
    session$setInputs(keep_going = TRUE, all = 1)
    expect_null(started[[1]]$names)
    expect_true(started[[1]]$modes$keep_going)

    args$action(list(action = "run", nodes = list("a", "b")))
    session$flushReact()
    expect_equal(started[[2]]$names, c("a", "b"))

    args$action(list(action = "value", node = "a"))
    session$flushReact()
    expect_length(started, 2)
  })
})

test_that("the run's scope: what the board's run was asked for, none for a run from the console (#181)", {
  skip_on_cran()
  local_pipeline()
  args <- run_args()
  local_mocked_bindings(start_run = function(...) NULL)
  alive <- FALSE # the board's run process
  args$backend$run_alive <- function() alive
  shiny::testServer(run_server, args = args, {
    args$action(list(action = "run", nodes = list("a", "b")))
    session$flushReact()
    alive <<- TRUE
    args$running(TRUE)
    session$flushReact()
    expect_equal(session$returned$scope(), c("a", "b"))
    alive <<- FALSE
    args$running(FALSE)
    session$flushReact()
    expect_equal(session$returned$scope(), c("a", "b")) # the last run's, until the next

    args$running(TRUE) # one the board didn't start
    session$flushReact()
    expect_null(session$returned$scope())
    args$running(FALSE)
    session$flushReact()

    # A board run over between two polls: running() never saw it. The next
    # run, from the console, is still not the board's (#243).
    args$action(list(action = "run", nodes = list("c")))
    session$flushReact()
    args$running(TRUE)
    session$flushReact()
    expect_null(session$returned$scope())
  })
})

test_that("the graph's Invalidate asks first, with the run's namespace on its button", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  args <- run_args()
  modal <- NULL
  invalidated <- NULL
  local_mocked_bindings(showModal = function(ui, ...) modal <<- as.character(ui), .package = "shiny")
  local_mocked_bindings(invalidate_targets = function(names) invalidated <<- names)
  shiny::testServer(run_server, args = args, {
    args$action(list(action = "invalidate", node = "b"))
    session$flushReact()
    expect_match(modal, "<code>b</code>", fixed = TRUE)
    expect_match(modal, 'id="proxy1-invalidate_confirm"', fixed = TRUE)

    # A long list is cut short in the confirmation, but all are invalidated.
    many <- paste0("t", 1:55)
    args$action(list(action = "invalidate", node = "t1", nodes = many))
    session$flushReact()
    expect_match(modal, "and 5 more", fixed = TRUE)
    session$setInputs(invalidate_confirm = 1)
    expect_equal(invalidated, many)
  })
})

test_that("a run notification's Show problem comes back as show_problem", {
  skip_on_cran()
  local_pipeline()
  args <- run_args()
  shiny::testServer(run_server, args = args, {
    expect_null(session$returned$show_problem())
    session$setInputs(show_problem = "b")
    expect_equal(session$returned$show_problem(), "b")
  })
})

test_that("a run or an invalidation that errors says so, and the session lives on (#253)", {
  skip_on_cran()
  local_pipeline()
  args <- run_args()
  local_mocked_bindings(
    start_run = function(...) stop("the run broke"),
    invalidate_targets = function(names) stop("the store broke")
  )
  shiny::testServer(run_server, args = args, {
    session$setInputs(all = 1)
    expect_match(last_failure(), "the run broke", fixed = TRUE)
    args$action(list(action = "invalidate", nodes = list("a")))
    session$flushReact()
    session$setInputs(invalidate_confirm = 1)
    expect_match(last_failure(), "the store broke", fixed = TRUE)
    session$setInputs(all = 2) # still answering
    expect_match(last_failure(), "the run broke", fixed = TRUE)
  })
})
