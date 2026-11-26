test_that("nodes become React Flow nodes typed by status", {
  nodes <- dataframe_to_reactflow_node_data(fake_nodes())

  expect_length(nodes, 3)
  expect_equal(nodes[[1]]$id, "raw")
  expect_equal(nodes[[2]]$type, "running")
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

test_that("every targets status maps to one of the four statuses the board shows", {
  expect_equal(
    status_to_node_type(c(
      "uptodate", "completed", "skipped", "outdated", "canceled",
      "errored", "dispatched", "warned", "something new", NA
    )),
    c(
      "uptodate", "uptodate", "uptodate", "outdated", "outdated",
      "errored", "running", "uptodate", "outdated", "outdated"
    )
  )

  nodes <- dataframe_to_reactflow_node_data(fake_nodes(status = c("completed", "canceled", "errored")))
  expect_equal(vapply(nodes, `[[`, "", "type"), c("uptodate", "outdated", "errored"))
  expect_equal(nodes[[1]]$data$target_status, "completed")
})

test_that("the nodes' and edges' JSON keeps its shape (#160: base R, as dplyr gave it)", {
  nodes <- fake_nodes(c("raw", "model"), c("uptodate", "errored"))
  nodes$description[1] <- "Raw NIH data"
  json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"))
  expect_equal(
    json(dataframe_to_reactflow_node_data(nodes, warnings = "model")),
    paste0(
      '[{"id":"raw","position":{"x":0,"y":0},"data":{"label":"raw","level":1,"description":"Raw NIH data",',
      '"target_type":"stem","target_status":"uptodate","warning":false},"type":"uptodate","deletable":false},',
      '{"id":"model","position":{"x":0,"y":0},"data":{"label":"model","level":2,"description":null,',
      '"target_type":"stem","target_status":"errored","warning":true},"type":"errored","deletable":false}]'
    )
  )
  expect_equal(
    json(dataframe_to_reactflow_edge_data(fake_edges())),
    paste0(
      '[{"id":"raw_to_model","source":"raw","target":"model","deletable":false},',
      '{"id":"model_to_report","source":"model","target":"report","deletable":false}]'
    )
  )
})

test_that("json_object() is {} when empty, the list itself otherwise", {
  expect_equal(as.character(jsonlite::toJSON(json_object(), auto_unbox = TRUE)), "{}")
  expect_equal(as.character(jsonlite::toJSON(json_object(list()), auto_unbox = TRUE)), "{}")
  expect_equal(json_object(list(a = 1)), list(a = 1))
})
