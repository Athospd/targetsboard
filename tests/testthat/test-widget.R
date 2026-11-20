test_that("board_configuration() carries the id and converted data", {
  config <- board_configuration("dag", fake_nodes(), fake_edges(), "500px", "100%")

  expect_equal(config$id, "dag")
  expect_equal(config$height, "500px")
  expect_length(config$nodes, 3)
  expect_length(config$edges, 2)
  expect_equal(config$nodes[[1]]$id, "raw")
})

test_that("targetsboard() is a reactR Shiny input with the JS bundle", {
  tag <- targetsboard("dag", fake_nodes(), fake_edges())
  html <- as.character(tag)
  deps <- htmltools::findDependencies(tag)

  expect_match(html, "targetsboard")
  expect_match(html, '"id":"dag"', fixed = TRUE)
  expect_true("targetsboard" %in% vapply(deps, `[[`, "", "name"))
})

test_that("update_targetsboard() sends the full configuration", {
  sent <- NULL
  session <- list(sendInputMessage = function(inputId, message) {
    sent <<- list(inputId = inputId, message = message)
  })

  update_targetsboard(session, "dag", fake_nodes(), fake_edges())

  expect_equal(sent$inputId, "dag")
  expect_equal(sent$message$configuration$id, "dag")
  expect_length(sent$message$configuration$nodes, 3)
})

test_that("update_targetsboard_status() sends only ids, node types and statuses", {
  sent <- NULL
  session <- list(sendCustomMessage = function(type, message) {
    sent <<- list(type = type, message = message)
  })

  update_targetsboard_status(session, "dag", c(raw = "completed", model = "dispatched"))

  expect_equal(sent$type, "targetsboard:status")
  expect_equal(sent$message$id, "dag")
  expect_equal(sent$message$nodes, list(
    list(id = "raw", type = "uptodate", target_status = "completed"),
    list(id = "model", type = "dispatched", target_status = "dispatched")
  ))

  update_targetsboard_status(session, "dag", character())
  expect_equal(sent$message$nodes, list())
})
