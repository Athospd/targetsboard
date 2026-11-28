# The script module, on its own: script_server() with the poller's graphs
# as reactiveVals (script_args(), helper-fixtures.R).

test_that("script_server() checks its arguments", {
  args <- list(
    backend = list(), metadata = list(), script = "_targets.R",
    poller_board = shiny::reactive(NULL), script_error = shiny::reactive(NULL)
  )
  expect_error(do.call(script_server, c("s", utils::modifyList(args, list(poller_board = "graph")))), "is.reactive")
  expect_error(do.call(script_server, c("s", utils::modifyList(args, list(script = shiny::reactive("a"))))), "is.reactive")
})

test_that("the board starts on the first graph and takes each the poller sends", {
  skip_on_cran()
  local_pipeline()
  args <- script_args()
  shiny::testServer(script_server, args = args, {
    expect_equal(session$returned$board()$x, args$metadata)
    new_board <- list(x = list(nodes = fake_nodes()[1, ], edges = fake_edges()[0, ]))
    args$poller_board(new_board)
    session$flushReact()
    expect_equal(session$returned$board(), new_board)
  })
})

test_that("Refresh loads the preview methods again", {
  skip_on_cran()
  local_pipeline()
  args <- script_args()
  shiny::testServer(script_server, args = args, {
    before <- session$returned$methods()
    session$setInputs(refresh = 1)
    expect_equal(session$returned$methods(), before + 1)
  })
})

test_that("its ids are in its namespace", {
  html <- as.character(script_ui("script", "_targets.R", "other.R"))
  expect_match(html, 'id="script-suggestions"', fixed = TRUE)
  expect_match(html, 'id="script-path"', fixed = TRUE)
  expect_match(html, 'id="script-load"', fixed = TRUE)
  expect_match(html, 'id="script-refresh"', fixed = TRUE)
  expect_match(as.character(script_loading_ui("script")), 'id="script-loading_caption"', fixed = TRUE)
  expect_match(as.character(run_ui("run")), 'id="run-keep_going"', fixed = TRUE)
  expect_match(as.character(board_sidebar_ui("board")), 'id="board-script-functions"', fixed = TRUE)
})

test_that("the Functions and globals switch draws them, loading as Load does; Cancel on a large graph turns it back", {
  skip_on_cran()
  local_pipeline()
  args <- script_args()
  args$backend$start_once()
  shiny::testServer(script_server, args = args, {
    expect_true(args$backend$targets_only())
    session$setInputs(functions = FALSE) # as the page sends it, on connecting: ignored
    expect_false(loading())
    session$setInputs(functions = TRUE)
    expect_false(args$backend$targets_only())
    expect_true(loading())
    # The graph it brings is large: Cancel keeps targets only.
    withr::local_options(targetsboard.confirm_nodes = 1)
    args$poller_board(list(x = list(nodes = fake_nodes(), edges = fake_edges())))
    session$flushReact()
    expect_false(is.null(pending_board()))
    session$setInputs(cancel_large = 1)
    expect_true(args$backend$targets_only())
    expect_false(loading())
  })
})

test_that("the backend draws functions and globals on a new poller, on the same script", {
  skip_on_cran()
  local_pipeline()
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  backend$set_targets_only(FALSE) # before the poller starts: the setting only
  expect_null(backend$poller$process)
  backend$start_once()
  first <- backend$poller$process
  backend$set_targets_only(TRUE)
  expect_false(identical(backend$poller$process, first))
  expect_false(first$is_alive())
  expect_equal(backend$poller$script, "_targets.R")
})
