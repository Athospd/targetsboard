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

test_that("dense layers stay compact, in the columns of tar_visnetwork()'s levels", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  # Three layers of three targets, each feeding all of the next: the shape
  # that dagre used to spread into a widening diamond.
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "library(targets)",
      "list(",
      "  tar_target(a1, 1), tar_target(a2, 2), tar_target(a3, 3),",
      "  tar_target(b1, a1 + a2 + a3), tar_target(b2, a1 + a2 + a3), tar_target(b3, a1 + a2 + a3),",
      "  tar_target(c1, b1 + b2 + b3), tar_target(c2, b1 + b2 + b3), tar_target(c3, b1 + b2 + b3)",
      ")"
    ),
    file.path(dir, "_targets.R")
  )
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))

  app <- shinytest2::AppDriver$new(dir, name = "dense", load_timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 9")
  app$wait_for_js(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node')).map(n => n.style.transform)).size === 9"
  )

  xy <- app$get_js(
    "Object.fromEntries(Array.from(document.querySelectorAll('.react-flow__node')).map(n => {
       // style.transform is 'translate(<x>px, <y>px)'
       const xy = n.style.transform.slice('translate('.length, -1).split(', ').map(parseFloat);
       return [n.dataset.id, xy];
     }))"
  )
  x <- vapply(xy, `[[`, 0, 1)
  y <- vapply(xy, `[[`, 0, 2)
  layer <- substr(names(xy), 1, 1)

  # One column per level, left to right.
  expect_equal(as.vector(tapply(x, layer, function(v) length(unique(v)))), c(1, 1, 1))
  expect_true(all(diff(tapply(x, layer, min)) > 0))
  # Every column has the same compact span: no layer spreads wider.
  spans <- tapply(y, layer, function(v) diff(range(v)))
  expect_equal(as.vector(spans), rep(spans[[1]], 3))
  expect_lt(spans[[1]], 3 * 60) # three targets, packed
})

test_that("the delete keys don't remove targets from the read-only board", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "delete")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")

  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_value(input = "flow_selected_nodes")
  for (key in c("Backspace", "Delete")) press_key(app, key)
  Sys.sleep(1)

  expect_equal(app$get_js("document.querySelectorAll('.react-flow__node').length"), 3)
  expect_equal(app$get_js("document.querySelectorAll('.react-flow__edge').length"), 2)
})

test_that("the minimap colours targets by status", {
  dir <- local_pipeline()
  targets::tar_make(names = "a", reporter = "silent") # a up to date, b and c outdated
  app <- local_board_app(dir, "minimap")
  app$wait_for_js("document.querySelectorAll('.react-flow__minimap-node').length === 3")
  fills <- app$get_js(
    "Array.from(document.querySelectorAll('.react-flow__minimap-node')).map(n => getComputedStyle(n).fill)"
  )
  # #354823 (up to date) and #78B7C5 (outdated)
  expect_setequal(unlist(fills), c("rgb(53, 72, 35)", "rgb(120, 183, 197)"))
})

test_that("the widget's display options take effect, and change from R", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "options", widget_app(paste(
    "options = targetsboard::targetsboard_options(color_mode = 'dark', minimap = FALSE,",
    "controls = FALSE, background = 'lines', edge_type = 'step')"
  )))
  app$wait_for_js("document.querySelectorAll('.react-flow__edge').length === 2")
  count <- function(selector) app$get_js(sprintf("document.querySelectorAll('%s').length", selector))
  has_class <- function(class) {
    app$get_js(sprintf("document.querySelector('.react-flow').classList.contains('%s')", class))
  }

  expect_true(has_class("dark"))
  expect_equal(count(".react-flow__minimap"), 0)
  expect_equal(count(".react-flow__controls"), 0)
  expect_equal(count(".react-flow__background path"), 1) # lines, not dots
  expect_equal(count(".react-flow__edge-step"), 2)

  # update_targetsboard_options() changes only what it names.
  send_widget_message(app, "targetsboard:options", list(id = "dag", options = list(
    color_mode = "light", minimap = TRUE, background = "none"
  )))
  app$wait_for_js("document.querySelector('.react-flow__minimap') !== null")
  expect_true(has_class("light"))
  expect_equal(count(".react-flow__background"), 0)
  expect_equal(count(".react-flow__controls"), 0) # unchanged
  expect_equal(count(".react-flow__edge-step"), 2) # unchanged

  # Another widget's message is not for this one.
  send_widget_message(app, "targetsboard:options", list(id = "other", options = list(minimap = FALSE)))
  Sys.sleep(0.5)
  expect_equal(count(".react-flow__minimap"), 1)
})

test_that("with only visible targets rendered, the layout runs and off-screen targets drop out", {
  dir <- local_chain_pipeline(12)
  app <- local_board_app(dir, "visible", widget_app(
    "options = targetsboard::targetsboard_options(only_render_visible = TRUE)"
  ))
  count <- "document.querySelectorAll('.react-flow__node').length"
  app$wait_for_js(paste(count, "=== 12"))
  # Laid out: twelve targets, twelve places.
  app$wait_for_js(paste(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
    ".map(n => n.style.transform)).size === 12"
  ))
  for (i in 1:12) app$click(selector = ".react-flow__controls-zoomin")
  app$wait_for_js(paste(count, "< 12"))
  expect_lt(app$get_js(count), 12)
})
