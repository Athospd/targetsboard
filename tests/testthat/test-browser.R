# Runs the real board in headless Chrome: the only test that exercises the
# React widget. Uses the installed targetsboard (as R CMD check does).
test_that("the board renders the DAG, focuses targets and keeps its layout", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- local_pipeline(make = TRUE)
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))

  app <- shinytest2::AppDriver$new(dir, name = "board", load_timeout = 60 * 1000)
  withr::defer(app$stop())

  node <- function(id) sprintf("document.querySelector(\".react-flow__node[data-id='%s']\")", id)
  transform <- function(id) app$get_js(paste0(node(id), ".style.transform"))
  has_class <- function(id, class) app$get_js(sprintf("%s.classList.contains('%s')", node(id), class))

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

  # Clicking a target focuses it and previews its value.
  app$click(selector = ".react-flow__node[data-id='c']")
  app$wait_for_value(input = "flow_focused_node")
  expect_equal(app$get_value(input = "flow_focused_node"), "c")
  app$wait_for_value(output = "value_preview_content")
  expect_match(app$get_value(output = "value_preview_content"), "4")
  expect_true(has_class("c", "tb-focused"))
  expect_false(has_class("a", "tb-dimmed")) # upstream of c: in its lineage

  # A status-only update retypes the node in place, without moving it, and
  # warning marks come and go without changing the status.
  before <- transform("a")
  app$run_js(
    "window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:
       {id: 'flow', nodes: [{id: 'a', type: 'running', target_status: 'dispatched'}]}}))"
  )
  app$wait_for_js(paste0(node("a"), ".classList.contains('react-flow__node-running')"))
  expect_equal(transform("a"), before)

  app$run_js(
    "window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:
       {id: 'flow', nodes: [], warnings: ['b']}}))"
  )
  app$wait_for_js(paste0(node("b"), ".querySelector('.warning-mark') !== null"))
  expect_true(has_class("a", "react-flow__node-running"))
  app$run_js(
    "window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:
       {id: 'flow', nodes: [], warnings: []}}))"
  )
  app$wait_for_js(paste0(node("b"), ".querySelector('.warning-mark') === null"))

  # New targets appear; the targets already drawn stay where they were.
  placed <- list(a = transform("a"), b = transform("b"), c = transform("c"))
  writeLines(
    c(
      "library(targets)",
      "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c + 1),",
      "     tar_target(e, 0))"
    ),
    file.path(dir, "_targets.R")
  )
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 5", timeout = 30 * 1000)
  app$wait_for_js(paste0(node("d"), ".style.transform !== 'translate(0px, 0px)'"))
  expect_equal(list(a = transform("a"), b = transform("b"), c = transform("c")), placed)
  expect_false(transform("d") %in% unlist(placed))
  expect_false(has_class("d", "tb-dimmed")) # downstream of the focused c
  expect_true(has_class("e", "tb-dimmed")) # unrelated to c

  # Clicking the background clears the focus.
  app$click(selector = ".react-flow__pane")
  app$wait_for_js(paste0("!", node("a"), ".classList.contains('tb-dimmed') && !",
                         node("c"), ".classList.contains('tb-focused')"))
})

test_that("a board started before its script draws the pipeline once it appears", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- withr::local_tempdir()
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))

  app <- shinytest2::AppDriver$new(dir, name = "empty", load_timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_js("document.querySelector('.react-flow') !== null")
  expect_equal(app$get_js("document.querySelectorAll('.react-flow__node').length"), 0)

  writeLines(
    c("library(targets)", "list(tar_target(a, 1), tar_target(b, a))"),
    file.path(dir, "_targets.R")
  )
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 2", timeout = 30 * 1000)
  # Laid out as a whole: the two targets don't sit on top of each other.
  positions <- app$get_js(
    "Array.from(document.querySelectorAll('.react-flow__node')).map(n => n.style.transform)"
  )
  expect_length(unique(unlist(positions)), 2)
})
