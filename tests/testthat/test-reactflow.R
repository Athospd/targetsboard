test_that("nodes become React Flow nodes typed by status", {
  nodes <- dataframe_to_reactflow_node_data(fake_nodes())

  expect_length(nodes, 3)
  expect_equal(nodes[[1]]$id, "raw")
  expect_equal(nodes[[2]]$type, "dispatched")
  expect_equal(nodes[[1]]$position, list(x = 0, y = 0))
  expect_equal(nodes[[3]]$data$label, "report")
  expect_equal(nodes[[3]]$data$target_status, "outdated")
  expect_equal(nodes[[3]]$data$target_type, "stem")
})

test_that("edges get stable ids and source/target", {
  edges <- dataframe_to_reactflow_edge_data(fake_edges())

  expect_length(edges, 2)
  expect_equal(edges[[1]]$id, "raw_to_model")
  expect_equal(edges[[2]]$source, "model")
  expect_equal(edges[[2]]$target, "report")
})

test_that("a pipeline without edges converts to an empty edge list", {
  edges <- dataframe_to_reactflow_edge_data(fake_edges(character(), character()))
  expect_length(edges, 0)
})

test_that("every targets status maps to one of the widget's node types", {
  expect_equal(
    status_to_node_type(c(
      "uptodate", "completed", "skipped", "outdated", "canceled",
      "errored", "dispatched", "something new", NA
    )),
    c(
      "uptodate", "uptodate", "uptodate", "outdated", "outdated",
      "errored", "dispatched", "outdated", "outdated"
    )
  )

  nodes <- dataframe_to_reactflow_node_data(fake_nodes(status = c("completed", "canceled", "errored")))
  expect_equal(vapply(nodes, `[[`, "", "type"), c("uptodate", "outdated", "errored"))
  expect_equal(nodes[[1]]$data$target_status, "completed")
})
