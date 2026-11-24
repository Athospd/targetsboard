# The graph and chrome modules, on their own.

test_that("graph_server() checks its arguments", {
  r <- shiny::reactive(NULL)
  expect_error(graph_server("g", r, r, r, r, show = NULL, color_mode = r), "is.reactive")
})

test_that("the graph shows a target asked for, each time, and follows the page's mode", {
  sent <- list()
  local_mocked_bindings(
    update_targetsboard = function(...) NULL,
    update_targetsboard_status = function(...) NULL,
    update_targetsboard_focus = function(session, inputId, node) sent[[length(sent) + 1]] <<- list("focus", inputId, node),
    targetsboard_fit_view = function(session, inputId, nodes) sent[[length(sent) + 1]] <<- list("fit", nodes),
    update_targetsboard_options = function(session, inputId, ...) sent[[length(sent) + 1]] <<- list("options", ...)
  )
  board <- shiny::reactiveVal(list(x = list(nodes = fake_nodes(), edges = fake_edges())))
  show <- shiny::reactiveVal(NULL)
  mode <- shiny::reactiveVal("light")
  r <- shiny::reactiveVal(NULL)
  shiny::testServer(graph_server, args = list(
    board = board, progress = r, running = shiny::reactiveVal(FALSE), warned_targets = r,
    show = show, color_mode = mode
  ), {
    session$flushReact()
    expect_equal(sent[[1]], list("options", color_mode = "light"))
    show(list(name = "model", n = 1))
    session$flushReact()
    expect_equal(sent[[2]], list("focus", "flow", "model"))
    expect_equal(sent[[3]], list("fit", c("model", "raw", "report")))
    show(list(name = "model", n = 2))
    session$flushReact()
    expect_length(sent, 5)
    mode("dark")
    session$flushReact()
    expect_equal(sent[[6]], list("options", color_mode = "dark"))

    session$setInputs(flow_focused_node = "raw")
    expect_equal(session$returned$focused(), "raw")
  })
})

test_that("the top bar's switch is the page's mode", {
  shiny::testServer(chrome_server, {
    session$setInputs(dark_mode = "dark")
    expect_equal(session$returned$color_mode(), "dark")
  })
  html <- as.character(chrome_ui("chrome"))
  expect_match(html, 'id="chrome-turn_off"', fixed = TRUE)
})
