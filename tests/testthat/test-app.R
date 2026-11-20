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
