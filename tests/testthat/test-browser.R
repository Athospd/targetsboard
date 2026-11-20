# Runs the real board in headless Chrome: the only test that exercises the
# React widget. Uses the installed targetsboard (as R CMD check does).
test_that("the board renders the DAG and reports node clicks", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- local_pipeline(make = TRUE)
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))

  app <- shinytest2::AppDriver$new(dir, name = "board", load_timeout = 60 * 1000)
  withr::defer(app$stop())

  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  labels <- app$get_js(
    "Array.from(document.querySelectorAll('.react-flow__node')).map(n => n.innerText.trim())"
  )
  expect_setequal(unlist(labels), c("a", "b", "c"))

  # The automatic layout ran: nodes no longer all sit at the origin.
  positions <- app$get_js(
    "Array.from(document.querySelectorAll('.react-flow__node')).map(n => n.style.transform)"
  )
  expect_length(unique(unlist(positions)), 3)

  app$click(selector = ".react-flow__node[data-id='c']")
  app$wait_for_value(input = "flow_selected_nodes")
  expect_equal(app$get_value(input = "flow_selected_nodes"), "c")
  app$wait_for_value(output = "output_preview_content")
  expect_match(app$get_value(output = "output_preview_content"), "4")
})
