# The run module, on its own: run_server() with its inputs as reactiveVals.

test_that("run_server() checks its arguments", {
  args <- list(backend = list(), board = shiny::reactive(NULL), running = shiny::reactive(FALSE),
               progress = shiny::reactive(NULL), action = shiny::reactive(NULL))
  expect_error(do.call(run_server, c("r", utils::modifyList(args, list(action = list())))), "is.reactive")
  expect_error(do.call(run_server, c("r", utils::modifyList(args, list(progress = list())))), "is.reactive")
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
