# A target's description (tar_target(description = )) on hover and in the
# Value pane (#143).

test_that("target_description() reads a node's description, or none", {
  nodes <- data.frame(id = c("a", "b", "c"), description = c("Raw NIH data", NA, "  "))
  expect_equal(target_description(nodes, "a"), "Raw NIH data")
  expect_null(target_description(nodes, "b"))
  expect_null(target_description(nodes, "c"))
  expect_null(target_description(nodes, "zz"))
  expect_null(target_description(data.frame(id = "a"), "a"))
  expect_match(as.character(description_ui("Raw NIH data")), "tb-description", fixed = TRUE)
  expect_null(description_ui(NULL))
})

test_that("a target's description shows on hover and in its Value pane", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "list(tar_target(a, 1, description = 'Raw NIH data'), tar_target(b, a + 1))"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "descriptions")
  node <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s'] .base-node\")", id)
  app$wait_for_js(paste(node("b"), "!== null"), timeout = 30000)
  expect_equal(app$get_js(paste0(node("a"), ".title")), "Raw NIH data")
  expect_null(app$get_js(paste0(node("b"), ".getAttribute('title')")))

  app$click(selector = ".react-flow__node[data-id='a']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js("document.querySelector('.tb-description') !== null", timeout = 20000)
  expect_equal(app$get_js("document.querySelector('.tb-description').innerText"), "Raw NIH data")
})
