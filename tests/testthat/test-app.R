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
