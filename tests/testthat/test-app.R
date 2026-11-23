test_that("tar_board_app() returns a Shiny app for given metadata", {
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())
  app <- tar_board_app(metadata = metadata)
  expect_s3_class(app, "shiny.appobj")
})

test_that("tar_board_app() reads the pipeline when metadata is missing", {
  skip_on_cran()
  local_pipeline()
  app <- tar_board_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("tar_board() refuses a port that is already in use", {
  port <- NULL
  for (p in sample(20000:40000, 20)) {
    server <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(server)) { port <- p; break }
  }
  skip_if(is.null(port), "found no free port to listen on")
  on.exit(close(server))

  expect_true(port_in_use(port))
  expect_error(tar_board(metadata = list(), port = port), "already in use")
})

test_that("port_in_use() is FALSE when nothing listens", {
  expect_false(port_in_use(1L)) # reserved: nothing listens there
})

test_that("with no arguments, the board shows the project's configured script", {
  skip_on_cran()
  local_pipeline()
  file.rename("_targets.R", "pipeline.R")
  targets::tar_config_set(script = "pipeline.R")

  seen <- NULL
  local_mocked_bindings(initial_metadata = function(script, ...) {
    seen <<- script
    empty_metadata()
  })
  tar_board_app()
  expect_equal(seen, "pipeline.R")

  # tar_board() hands the same script to the board's background process.
  local_mocked_bindings(port_in_use = function(port, host) FALSE)
  local_mocked_bindings(r_bg = function(func, args, ...) args, .package = "callr")
  args <- suppressMessages(tar_board(browse = FALSE))
  expect_equal(args$script, "pipeline.R")
})

test_that("without a script yet, the board starts empty", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_message(metadata <- initial_metadata("_targets.R"), "No targets script")
  expect_equal(nrow(metadata$nodes), 0)
  expect_equal(nrow(metadata$edges), 0)
  expect_length(dataframe_to_reactflow_node_data(metadata$nodes), 0)
  expect_length(dataframe_to_reactflow_edge_data(metadata$edges), 0)

  app <- suppressMessages(tar_board_app())
  expect_s3_class(app, "shiny.appobj")
})

test_that("a script that exists but is broken still errors", {
  skip_on_cran()
  local_pipeline()
  writeLines("stop('broken pipeline')", "_targets.R")
  expect_error(initial_metadata("_targets.R"), "broken pipeline")
})

test_that("tar_board() picks its port and host like tar_watch() and keeps the url", {
  local_mocked_bindings(port_in_use = function(port, host) FALSE)
  local_mocked_bindings(r_bg = function(func, args, ...) args, .package = "callr")
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())

  args <- tar_board(metadata = metadata, browse = FALSE, verbose = FALSE)
  expect_true(is.numeric(args$port))
  expect_equal(args$host, "127.0.0.1")
  expect_equal(attr(args, "url"), paste0("http://127.0.0.1:", args$port))

  withr::local_options(shiny.port = 4321, shiny.host = "0.0.0.0")
  args <- tar_board(metadata = metadata, browse = FALSE, verbose = FALSE)
  expect_equal(args$port, 4321)
  expect_equal(args$host, "0.0.0.0")
  expect_equal(attr(args, "url"), "http://127.0.0.1:4321") # browsable
})

test_that("wait_for_board() returns once the port answers", {
  port <- NULL
  for (p in sample(20000:40000, 20)) {
    server <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(server)) { port <- p; break }
  }
  skip_if(is.null(port), "found no free port to listen on")
  on.exit(close(server))

  alive <- list(is_alive = function() TRUE)
  expect_true(wait_for_board(alive, "127.0.0.1", port, tempfile(), verbose = FALSE))
})

test_that("wait_for_board() fails with the board's log if the board dies", {
  log <- withr::local_tempfile(lines = c("Listening on http://127.0.0.1:1", "Error: the board broke"))
  dead <- list(is_alive = function() FALSE)
  expect_error(
    wait_for_board(dead, "127.0.0.1", 1L, log, verbose = FALSE),
    "stopped before it was up.*the board broke"
  )
})

test_that("tar_board() waits for the board, then opens it in the browser", {
  skip_on_cran()
  local_pipeline()
  opened <- NULL
  local_mocked_bindings(open_url = function(url) opened <<- url)

  app <- suppressMessages(tar_board())
  withr::defer(app$kill())

  url <- attr(app, "url")
  expect_equal(opened, url)
  port <- as.integer(sub(".*:", "", url))
  expect_true(port_in_use(port)) # up as soon as tar_board() returns
  expect_match(paste(readLines(url, warn = FALSE), collapse = ""), "Targets board")
})

test_that("each page load draws the board with the layout saved last", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  app <- tar_board_app()
  page <- function() {
    request <- list(
      REQUEST_METHOD = "GET", PATH_INFO = "/", QUERY_STRING = "",
      HTTP_HOST = "127.0.0.1", rook.input = NULL
    )
    response <- app$httpHandler(list2env(request))
    paste(response$content, collapse = "")
  }
  expect_no_match(page(), "\"layout\":", fixed = TRUE)
  # Saved after the app started: the next page load has it.
  write_board_layout(list(direction = "TB", positions = list(a = list(x = 7, y = 8))))
  expect_match(page(), "\"layout\":{\"direction\":\"TB\"", fixed = TRUE)
})

test_that("the Internals tab is only there to develop the board", {
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())
  html <- as.character(app_ui(metadata, "_targets.R"))
  expect_no_match(html, 'data-value="internals"', fixed = TRUE)
  expect_no_match(html, ">Debug<", fixed = TRUE) # "debug" is for pipelines now

  withr::local_options(targetsboard.dev = TRUE)
  html <- as.character(app_ui(metadata, "_targets.R"))
  expect_match(html, 'data-value="internals"', fixed = TRUE)
  expect_match(html, 'id="internals"', fixed = TRUE)
})

test_that("tar_board() hands the dev, confirm_nodes and preview_bytes options to its background process", {
  local_mocked_bindings(port_in_use = function(port, host) FALSE)
  local_mocked_bindings(r_bg = function(func, args, ...) list(func = func, args = args), .package = "callr")
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())
  bg <- suppressMessages(tar_board(metadata = metadata, browse = FALSE))
  expect_false(bg$args$dev)
  expect_equal(bg$args$confirm_nodes, 500)
  expect_equal(bg$args$preview_bytes, 100e6)

  withr::local_options(
    targetsboard.dev = TRUE, targetsboard.confirm_nodes = 2000, targetsboard.preview_bytes = 5e6
  )
  bg <- suppressMessages(tar_board(metadata = metadata, browse = FALSE))
  expect_true(bg$args$dev)
  expect_equal(bg$args$confirm_nodes, 2000)
  expect_equal(bg$args$preview_bytes, 5e6)
  # The background process sets them before building the app.
  body <- paste(deparse(bg$func), collapse = " ")
  for (option in c("targetsboard.dev = dev", "targetsboard.confirm_nodes = confirm_nodes",
                   "targetsboard.preview_bytes = preview_bytes")) {
    expect_match(body, option, fixed = TRUE)
  }
})

test_that("the Script field offers the scripts, with the launch script first and chosen", {
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())
  html <- as.character(app_ui(metadata, "b.R", scripts = c("a.R", "b.R", "c.R")))
  expect_match(html, 'id="script"><option', fixed = TRUE)
  expect_match(html, '<option value="b.R" selected>b.R</option>', fixed = TRUE)
  options <- regmatches(html, gregexpr('<option value="[^"]*"', html))[[1]]
  expect_equal(options, paste0('<option value="', c("b.R", "a.R", "c.R"), '"'))
  # Only the listed scripts can be chosen: no typed path, no "Add ..." entry.
  # Typing still filters the list.
  expect_no_match(html, '"create":true', fixed = TRUE)
  expect_no_match(html, '"createOnBlur"', fixed = TRUE)

  # Without scripts, the field offers just the launch script, as before.
  html <- as.character(app_ui(metadata, "_targets.R"))
  options <- regmatches(html, gregexpr('<option value="[^"]*"', html))[[1]]
  expect_equal(options, '<option value="_targets.R"')
})

test_that("script_choices() puts the launch script first, once", {
  expect_equal(script_choices("b.R", c("a.R", "b.R")), c("b.R", "a.R"))
  expect_equal(script_choices("a.R", NULL), "a.R")
  expect_error(script_choices("a.R", 1:2), "character vector")
})

test_that("tar_board() hands the scripts to the board's background process", {
  local_mocked_bindings(port_in_use = function(port, host) FALSE)
  local_mocked_bindings(r_bg = function(func, args, ...) args, .package = "callr")
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())
  args <- suppressMessages(tar_board("b.R", metadata = metadata, browse = FALSE, scripts = c("a.R", "b.R")))
  expect_equal(args$script, "b.R")
  expect_equal(args$scripts, c("a.R", "b.R"))
})
