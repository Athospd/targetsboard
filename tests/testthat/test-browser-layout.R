# The board's layout in headless Chrome: columns, fitting the view, and
# saved positions.
# Uses the installed targetsboard (as R CMD check does), like every browser
# test: install the branch to a scratch library first to test a change.

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

  app <- new_app_driver(dir, name = "dense", load_timeout = 60 * 1000)
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

test_that("the first layout fits the whole graph in view, every time", {
  dir <- local_chain_pipeline(6)
  # The fit used to race the layout and, now and then, zoom to the maximum
  # on targets still stacked at the origin: so load the board a few times.
  for (i in 1:3) {
    app <- local_board_app(dir, paste0("fit", i))
    app$wait_for_js(paste(
      "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
      ".map(n => n.style.transform)).size === 6"
    ))
    Sys.sleep(1)
    outside <- app$get_js(
      "(() => { const w = document.querySelector('.react-flow').getBoundingClientRect();
         return Array.from(document.querySelectorAll('.react-flow__node')).filter(n => {
           const r = n.getBoundingClientRect();
           return r.left < w.left || r.right > w.right || r.top < w.top || r.bottom > w.bottom;
         }).map(n => n.dataset.id); })()"
    )
    expect_length(unlist(outside), 0)
    app$stop()
  }
})

test_that("the board saves where targets were left, and opens with them there", {
  dir <- local_pipeline(make = TRUE)
  path <- file.path(dir, "_targets", "user", "targetsboard_layout.json")
  app <- local_board_app(dir, "layout_save")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  # The first layout is saved once drawn.
  app$wait_for_js("Shiny.shinyapp.$inputValues['board-graph-flow_layout'] !== undefined")
  before <- node_position(app, "b")
  drag_node(app, "b", 0, 120)
  app$wait_for_js(paste0(
    "Shiny.shinyapp.$inputValues['board-graph-flow_layout'].positions.b.y > ", before[["y"]] + 50
  ))
  moved <- node_position(app, "b")
  app$click(selector = ".react-flow__controls-zoomout")
  Sys.sleep(1) # the zoom's animation
  view <- viewport_numbers(app)
  # Saved once the board writes it: later under load, so wait for it. On
  # Windows, a read can also hit the file while the board writes it.
  read_saved <- function() tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  wait_for(function() {
    saved <- read_saved()
    !is.null(saved) && isTRUE(abs(saved$positions$b$y - moved[["y"]]) < 0.5) && isTRUE(saved$viewport$zoom < 1.5)
  }, timeout = 15)
  saved <- read_saved()
  expect_equal(saved$direction, "LR")
  expect_equal(c(saved$positions$b$x, saved$positions$b$y), unname(moved), tolerance = 0.5)
  expect_lt(saved$viewport$zoom, 1.5)

  # A reload puts the targets back where they were left, and fits the view
  # to them, as the Fit View button does: not the zoomed-out view.
  app$run_js("location.reload()")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$wait_for_js(paste0(
    "(() => { const m = ", node_js("b"), ".style.transform.match(/translate\\(([-\\d.]+)px, ([-\\d.]+)px\\)/);",
    " return m && Math.abs(Number(m[2]) - ", moved[["y"]], ") < 0.5; })()"
  ))
  Sys.sleep(0.5)
  expect_equal(node_position(app, "b"), moved, tolerance = 0.5)
  expect_false(isTRUE(all.equal(viewport_numbers(app), view, tolerance = 1e-3)))
  expect_fitted(app)

  # Tidy lays the graph out again, and that is saved too.
  app$click(selector = ".tb-layout-buttons button:first-child") # Tidy vertically
  app$wait_for_js("Shiny.shinyapp.$inputValues['board-graph-flow_layout'].direction === 'TB'")
  app$wait_for_js(paste(
    "(() => { const p = Shiny.shinyapp.$inputValues['board-graph-flow_layout'].positions;",
    "return Math.abs(p.a.x - p.b.x) < 1 && p.b.y > p.a.y; })()"
  ))
  # On Windows, a read can hit the file while the board writes it: try again.
  wait_for(function() {
    identical(tryCatch(jsonlite::read_json(path)$direction, error = function(e) NULL), "TB")
  }, timeout = 10)
})

test_that("a saved layout places new targets next to their neighbours, not on top of others", {
  dir <- local_pipeline(make = TRUE)
  dir.create(file.path(dir, "_targets", "user"), showWarnings = FALSE)
  # Saved before `c` was in the pipeline, far from where a layout would put them.
  jsonlite::write_json(
    list(direction = "LR", viewport = list(x = 30, y = 20, zoom = 0.5),
         positions = list(a = list(x = 1000, y = 1000), b = list(x = 1000, y = 1300))),
    file.path(dir, "_targets", "user", "targetsboard_layout.json"),
    auto_unbox = TRUE
  )
  app <- local_board_app(dir, "layout_restore")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$wait_for_js(paste0(
    "(() => { const m = ", node_js("a"), ".style.transform.match(/translate\\(([-\\d.]+)px, ([-\\d.]+)px\\)/);",
    " return m && Number(m[1]) === 1000; })()"
  ))
  Sys.sleep(0.5)
  expect_equal(node_position(app, "a"), c(x = 1000, y = 1000))
  expect_equal(node_position(app, "b"), c(x = 1000, y = 1300))
  c_position <- node_position(app, "c")
  expect_false(isTRUE(all.equal(c_position, c(x = 0, y = 0)))) # not left at the origin
  expect_gt(c_position[["x"]], 1000) # downstream of b, to its right
  # Fitted to them all, not the saved view.
  expect_false(identical(viewport_transform(app), "translate(30px, 20px) scale(0.5)"))
  expect_fitted(app)
})

test_that("a saved layout with every target puts them back, and fits the view to them", {
  dir <- local_pipeline(make = TRUE)
  dir.create(file.path(dir, "_targets", "user"), showWarnings = FALSE)
  jsonlite::write_json(
    list(direction = "TB", viewport = list(x = 30, y = 20, zoom = 0.5),
         positions = list(a = list(x = 0, y = 0), b = list(x = 0, y = 200), c = list(x = 0, y = 400))),
    file.path(dir, "_targets", "user", "targetsboard_layout.json"),
    auto_unbox = TRUE
  )
  app <- local_board_app(dir, "layout_view")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$wait_for_js(paste0(
    "(() => { const m = ", node_js("c"), ".style.transform.match(/translate\\(([-\\d.]+)px, ([-\\d.]+)px\\)/);",
    " return m && Number(m[2]) === 400; })()"
  ))
  expect_equal(node_position(app, "c"), c(x = 0, y = 400))
  # Top to bottom, as saved: handles at the top and bottom.
  expect_true(app$get_js(paste0(node_js("b"), ".querySelector('.react-flow__handle-bottom') !== null")))
  # The view as the Fit View button makes it, not the saved one.
  expect_false(identical(viewport_transform(app), "translate(30px, 20px) scale(0.5)"))
  expect_fitted(app)
})

test_that("loading another pipeline lays it out afresh and fits the view to it", {
  dir <- local_pipeline(make = TRUE)
  writeLines(
    c("library(targets)", "list(tar_target(x, 1), tar_target(y, x + 1))"),
    file.path(dir, "other.R")
  )
  app <- local_board_app(dir, "layout_load", app = "targetsboard::tar_board_app(scripts = 'other.R')")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  # Away from the fitted view first.
  app$click(selector = ".react-flow__controls-zoomout")
  app$click(selector = ".react-flow__controls-zoomout")
  Sys.sleep(1)
  app$set_inputs(`board-script-path` = "other.R")
  app$click("board-script-load")
  app$wait_for_js(
    "document.querySelectorAll('.react-flow__node').length === 2 && document.querySelector(\".react-flow__node[data-id='x']\") !== null",
    timeout = 30 * 1000
  )
  # Laid out as a pipeline of its own: x then y, left to right.
  app$wait_for_js(paste0(node_js("y"), ".style.transform !== 'translate(0px, 0px)'"))
  Sys.sleep(0.5)
  expect_gt(node_position(app, "y")[["x"]], node_position(app, "x")[["x"]])
  expect_fitted(app)
})
