# The script module, on its own: script_server() with the poller's graphs
# as reactiveVals.

script_args <- function(.env = parent.frame()) {
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller(), envir = .env)
  list(
    backend = backend,
    metadata = list(nodes = fake_nodes(), edges = fake_edges()),
    script = "_targets.R",
    poller_board = shiny::reactiveVal(NULL),
    script_error = shiny::reactiveVal(NULL)
  )
}

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
})
