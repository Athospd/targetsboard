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

  # Clicking a target focuses it; double-clicking previews its value.
  app$click(selector = ".react-flow__node[data-id='c']")
  app$wait_for_value(input = "flow_focused_node")
  expect_equal(app$get_value(input = "flow_focused_node"), "c")
  mouse_event(app, "c", "dblclick")
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
  # Checked together: the board's own status updates may arrive later.
  app$wait_for_js(paste0(
    node("b"), ".querySelector('.warning-mark') !== null && ",
    node("a"), ".classList.contains('react-flow__node-running')"
  ))
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
  # Laid out as a whole: the two targets don't sit on top of each other,
  # once the layout has run (it waits for them to be measured).
  app$wait_for_js(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node')).map(n => n.style.transform)).size === 2",
    timeout = 10 * 1000
  )
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

test_that("double-click, right-click menu and viewport reach the server", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "events")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  menu_items <- function() {
    unlist(app$get_js(
      "Array.from(document.querySelectorAll('.tb-node-menu [role=menuitem]')).map(b => b.dataset.action)"
    ))
  }
  menu_open <- "document.querySelector('.tb-node-menu') !== null"

  mouse_event(app, "a", "dblclick")
  app$wait_for_value(input = "flow_double_clicked_node")
  expect_equal(app$get_value(input = "flow_double_clicked_node"), "a")
  # It opens the value preview: let its sidebar finish sliding in, which
  # narrows the graph.
  app$wait_for_value(output = "value_preview_content")
  Sys.sleep(1)

  # Right-click: reports and focuses the target, and opens its menu.
  mouse_event(app, "b", "contextmenu")
  app$wait_for_js(menu_open)
  expect_equal(menu_items(), c(
    "run", "invalidate", "value", "copy_name",
    "select_upstream", "select_downstream", "select_lineage"
  ))
  app$wait_for_value(input = "flow_context_node")
  expect_equal(app$get_value(input = "flow_context_node"), "b")
  expect_true(app$get_js(paste0(node_js("b"), ".classList.contains('tb-focused')")))

  # Inside the widget, however far right the click.
  box <- app$get_js(
    "(() => { const m = document.querySelector('.tb-node-menu').getBoundingClientRect();
       const w = document.querySelector('.reactflow-wrapper').getBoundingClientRect();
       return {menu_left: m.left, menu_right: m.right, left: w.left, right: w.right}; })()"
  )
  expect_gte(box$menu_left, box$left)
  expect_lte(box$menu_right, box$right + 1)

  press_key(app, "Escape")
  app$wait_for_js(paste0("!(", menu_open, ")"))

  # Copy its name: done in the browser.
  app$run_js(
    "window.__copied = null;
     Object.defineProperty(navigator, 'clipboard', {configurable: true,
       value: {writeText: t => { window.__copied = t; return Promise.resolve(); }}});"
  )
  mouse_event(app, "b", "contextmenu")
  app$wait_for_js(menu_open)
  app$click(selector = ".tb-node-menu [data-action='copy_name']")
  app$wait_for_js("window.__copied === '\"b\"'") # quoted, as an R string
  app$wait_for_js(paste0("!(", menu_open, ")"))

  # Another action goes to the server: here, the invalidate confirmation.
  mouse_event(app, "c", "contextmenu")
  app$wait_for_js(menu_open)
  app$click(selector = ".tb-node-menu [data-action='invalidate']")
  app$wait_for_js("document.querySelector('.modal-dialog') !== null")
  expect_match(app$get_js("document.querySelector('.modal-dialog').innerHTML"), "<code>c</code>", fixed = TRUE)
  dismiss_modal(app)

  # The viewport, after a zoom.
  app$wait_for_value(input = "flow_viewport")
  zoom <- app$get_value(input = "flow_viewport")$zoom
  app$click(selector = ".react-flow__controls-zoomin")
  app$wait_for_js(sprintf("Shiny.shinyapp.$inputValues['flow_viewport'].zoom > %f", zoom))
  expect_gt(app$get_value(input = "flow_viewport")$zoom, zoom)
})

test_that("a widget without node actions has no menu, but reports right-clicks", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "nomenu", widget_app())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  mouse_event(app, "b", "contextmenu")
  app$wait_for_value(input = "dag_context_node")
  expect_equal(app$get_value(input = "dag_context_node"), "b")
  expect_false(app$get_js("document.querySelector('.tb-node-menu') !== null"))
})

test_that("the widget focuses targets and fits the view on command", {
  dir <- local_chain_pipeline(8)
  app <- local_board_app(dir, "commands", widget_app())
  app$wait_for_js(paste(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
    ".map(n => n.style.transform)).size === 8"
  ))
  zoom <- function() {
    app$get_js("parseFloat(document.querySelector('.react-flow__viewport').style.transform.split('scale(')[1])")
  }
  in_view <- function(id) {
    app$get_js(sprintf(
      "(() => { const w = document.querySelector('.react-flow').getBoundingClientRect();
         const r = %s.getBoundingClientRect();
         return r.left >= w.left && r.right <= w.right && r.top >= w.top && r.bottom <= w.bottom; })()",
      node_js(id)
    ))
  }
  command <- function(...) send_widget_message(app, "targetsboard:command", list(id = "dag", ...))

  whole <- zoom()
  command(command = "focus", node = "t4")
  app$wait_for_js(paste0(node_js("t4"), ".classList.contains('tb-focused')"))
  app$wait_for_value(input = "dag_focused_node")
  expect_equal(app$get_value(input = "dag_focused_node"), "t4")

  # Fit to two targets: closer in than the whole chain, both in view.
  command(command = "fit_view", nodes = c("t4", "t5"), padding = 0.2, duration = 0, max_zoom = 1.5)
  app$wait_for_js(sprintf(
    "parseFloat(document.querySelector('.react-flow__viewport').style.transform.split('scale(')[1]) > %f",
    whole
  ))
  expect_true(in_view("t4") && in_view("t5"))
  expect_false(in_view("t1"))
  expect_lte(zoom(), 1.5)

  # Unknown targets: nothing moves. All targets: back to the whole chain.
  before <- zoom()
  command(command = "fit_view", nodes = list("nope"), duration = 0)
  Sys.sleep(0.5)
  expect_equal(zoom(), before)
  command(command = "fit_view", duration = 0)
  app$wait_for_js(paste0(
    "Array.from(document.querySelectorAll('.react-flow__node')).every(n => {",
    " const w = document.querySelector('.react-flow').getBoundingClientRect();",
    " const r = n.getBoundingClientRect(); return r.left >= w.left && r.right <= w.right; })"
  ))

  command(command = "focus")
  app$wait_for_js("document.querySelector('.tb-focused') === null")
})

test_that("a click on a tar_meta() row shows its target on the board", {
  dir <- local_chain_pipeline(8)
  targets::tar_make(reporter = "silent")
  app <- local_board_app(dir, "metajump")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 8")
  # A group selected before: the row's target replaces it.
  send_widget_message(app, "targetsboard:command",
                      list(id = "flow", command = "select", nodes = c("t1", "t2")))
  app$wait_for_js("document.querySelector('.tb-group-toolbar') !== null")

  app$click(selector = "a[data-value='tar_meta']")
  app$wait_for_js("document.querySelectorAll('#meta .rt-tbody .rt-tr').length === 8")
  app$run_js(
    "Array.from(document.querySelectorAll('#meta .rt-tbody .rt-tr'))
       .find(r => r.querySelector('.rt-td').innerText.trim() === 't6')
       .querySelector('.rt-td').click()"
  )
  app$wait_for_js("document.querySelector(\"a[data-value='board']\").classList.contains('active')")
  app$wait_for_js(paste0(node_js("t6"), ".classList.contains('tb-focused')"))
  app$wait_for_value(input = "flow_focused_node")
  expect_equal(app$get_value(input = "flow_focused_node"), "t6")
  # Alone: selected, ringed, with its own toolbar.
  app$wait_for_js("document.querySelector('.tb-group-toolbar') === null")
  expect_equal(unlist(app$get_value(input = "flow_selected_nodes")), "t6")
  expect_equal(
    unlist(app$get_js("Array.from(document.querySelectorAll('.react-flow__node.tb-focused')).map(n => n.dataset.id)")),
    "t6"
  )
  # In view, with its neighbours, and the start of the chain out of it, once
  # the zoom has finished: mid-way, both can be in view.
  app$wait_for_js(paste0(
    "(() => { const w = document.querySelector('.react-flow').getBoundingClientRect();",
    " const inside = r => r.left >= w.left && r.right <= w.right;",
    " return w.width > 0 && inside(", node_js("t6"), ".getBoundingClientRect())",
    " && !inside(", node_js("t1"), ".getBoundingClientRect()); })()"
  ), timeout = 10 * 1000)
})

test_that("the focused target's toolbar runs the board's actions", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "toolbar")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  toolbar <- "document.querySelector('.tb-node-toolbar')"

  expect_false(app$get_js(paste(toolbar, "!== null")))
  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_js(paste(toolbar, "!== null"))
  expect_equal(
    unlist(app$get_js(
      "Array.from(document.querySelectorAll('.tb-node-toolbar button')).map(b => b.dataset.action)"
    )),
    c("run", "invalidate", "value")
  )
  # Above its target.
  expect_true(app$get_js(paste0(
    "(() => { const t = ", toolbar, ".getBoundingClientRect();",
    " const n = ", node_js("b"), ".getBoundingClientRect();",
    " return t.bottom <= n.top + 1 && t.right > n.left && t.left < n.right; })()"
  )))

  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_value(output = "value_preview_content")
  expect_match(app$get_value(output = "value_preview_content"), "2")

  app$click(selector = ".tb-node-toolbar [data-action='invalidate']")
  app$wait_for_js("document.querySelector('.modal-dialog') !== null")
  expect_match(app$get_js("document.querySelector('.modal-dialog').innerHTML"), "<code>b</code>", fixed = TRUE)
  dismiss_modal(app)

  # It follows the focus, and goes with it.
  app$click(selector = ".react-flow__node[data-id='c']")
  app$wait_for_js(paste0(
    "(() => { const t = ", toolbar, "; if (!t) return false;",
    " const r = t.getBoundingClientRect(), n = ", node_js("c"), ".getBoundingClientRect();",
    " return r.right > n.left && r.left < n.right; })()"
  ))
  app$click(selector = ".react-flow__pane")
  app$wait_for_js(paste(toolbar, "=== null"))
})

test_that("a widget without toolbar actions has no toolbar", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "notoolbar", widget_app())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_js(paste0(node_js("b"), ".classList.contains('tb-focused')"))
  expect_false(app$get_js("document.querySelector('.tb-node-toolbar') !== null"))
})

# The width and height in a PNG file's header.
png_size <- function(path) {
  header <- readBin(path, "raw", 24)
  expect_equal(header[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  int <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  c(width = int(header[17:20]), height = int(header[21:24]))
}

test_that("the widget draws the whole graph as an image, for R and for download", {
  dir <- local_chain_pipeline(8)
  targets::tar_make(reporter = "silent")
  app <- local_board_app(dir, "image")
  app$wait_for_js(paste(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
    ".map(n => n.style.transform)).size === 8"
  ))
  # Zoomed in on one target: the image is still of the whole chain.
  app$click(selector = ".react-flow__node[data-id='t3']")
  for (i in 1:6) app$click(selector = ".react-flow__controls-zoomin")
  Sys.sleep(0.5)

  # From R: PNG, at twice the resolution, wider than tall like the chain.
  send_widget_message(app, "targetsboard:command", list(id = "flow", command = "export_image", format = "png"))
  app$wait_for_value(input = "flow_image")
  image <- app$get_value(input = "flow_image")
  expect_null(image$error)
  png <- withr::local_tempfile(fileext = ".png")
  size <- png_size(save_targetsboard_image(image, png))
  expect_gt(size[["width"]], 4 * size[["height"]])
  expect_gt(size[["width"]], 8 * 150 * 2) # eight targets, at twice the size

  send_widget_message(app, "targetsboard:command", list(id = "flow", command = "export_image", format = "svg"))
  app$wait_for_js("Shiny.shinyapp.$inputValues['flow_image:shiny.event']?.format === 'svg' ||
                   Shiny.shinyapp.$inputValues['flow_image']?.format === 'svg'")
  svg <- withr::local_tempfile(fileext = ".svg")
  save_targetsboard_image(app$get_value(input = "flow_image"), svg)
  text <- paste(readLines(svg, warn = FALSE), collapse = "\n")
  expect_match(text, "<svg", fixed = TRUE)
  for (id in paste0("t", 1:8)) expect_match(text, paste0(">", id, "<"), fixed = TRUE)
  expect_no_match(text, "tb-node-toolbar", fixed = TRUE) # the toolbar is left out

  # The download button: a PNG named after the graph.
  app$run_js(
    "window.__download = null;
     HTMLAnchorElement.prototype.click = function() {
       window.__download = {name: this.download, href: this.href};
     };"
  )
  app$click(selector = ".tb-download-image")
  app$wait_for_js("window.__download !== null", timeout = 15 * 1000)
  download <- app$get_js("window.__download")
  expect_equal(download$name, "targets-graph.png")
  expect_match(download$href, "^data:image/png;base64,")
})

test_that("a widget without download_image has no download button", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "nodownload", widget_app())
  app$wait_for_js("document.querySelector('.react-flow__controls') !== null")
  expect_false(app$get_js("document.querySelector('.tb-download-image') !== null"))
})

# A target's position in the graph, as React Flow places it.
node_position <- function(app, id) {
  unlist(app$get_js(paste0(
    "(() => { const m = ", node_js(id), ".style.transform.match(/translate\\(([-\\d.]+)px, ([-\\d.]+)px\\)/);",
    " return {x: Number(m[1]), y: Number(m[2])}; })()"
  )))
}
viewport_transform <- function(app) {
  app$get_js("document.querySelector('.react-flow__viewport').style.transform")
}
# The view's x, y and zoom, from its transform.
viewport_numbers <- function(app) {
  as.numeric(regmatches(viewport_transform(app), gregexpr("-?[0-9.]+", viewport_transform(app)))[[1]])
}
# Drag a target by (dx, dy) screen pixels, with real mouse events.
drag_node <- function(app, id, dx, dy) {
  box <- app$get_js(paste0(
    "(() => { const r = ", node_js(id), ".getBoundingClientRect();",
    " return {x: r.left + r.width / 2, y: r.top + r.height / 2}; })()"
  ))
  input <- app$get_chromote_session()$Input
  input$dispatchMouseEvent(type = "mousePressed", x = box$x, y = box$y, button = "left", clickCount = 1)
  for (step in 1:5) {
    input$dispatchMouseEvent(type = "mouseMoved", x = box$x + dx * step / 5, y = box$y + dy * step / 5, button = "left")
  }
  input$dispatchMouseEvent(type = "mouseReleased", x = box$x + dx, y = box$y + dy, button = "left", clickCount = 1)
}

test_that("the board saves where targets were left, and opens with them there", {
  dir <- local_pipeline(make = TRUE)
  path <- file.path(dir, "_targets", "user", "targetsboard_layout.json")
  app <- local_board_app(dir, "layout_save")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  # The first layout is saved once drawn.
  app$wait_for_js("Shiny.shinyapp.$inputValues['flow_layout'] !== undefined")
  before <- node_position(app, "b")
  drag_node(app, "b", 0, 120)
  app$wait_for_js(paste0(
    "Shiny.shinyapp.$inputValues['flow_layout'].positions.b.y > ", before[["y"]] + 50
  ))
  moved <- node_position(app, "b")
  app$click(selector = ".react-flow__controls-zoomout")
  Sys.sleep(1)
  view <- viewport_numbers(app)
  expect_true(file.exists(path))
  saved <- jsonlite::read_json(path)
  expect_equal(saved$direction, "LR")
  expect_equal(c(saved$positions$b$x, saved$positions$b$y), unname(moved), tolerance = 0.5)
  expect_lt(saved$viewport$zoom, 1.5)

  # A reload opens the board as it was left: targets and view.
  app$run_js("location.reload()")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$wait_for_js(paste0(
    "(() => { const m = ", node_js("b"), ".style.transform.match(/translate\\(([-\\d.]+)px, ([-\\d.]+)px\\)/);",
    " return m && Math.abs(Number(m[2]) - ", moved[["y"]], ") < 0.5; })()"
  ))
  Sys.sleep(0.5)
  expect_equal(node_position(app, "b"), moved, tolerance = 0.5)
  # The zoom comes back to 4 decimals, as the widget's configuration is sent.
  expect_equal(viewport_numbers(app), view, tolerance = 1e-3)

  # Tidy lays the graph out again, and that is saved too.
  app$click(selector = ".button-container button:first-child") # Tidy vertically
  app$wait_for_js("Shiny.shinyapp.$inputValues['flow_layout'].direction === 'TB'")
  app$wait_for_js(paste(
    "(() => { const p = Shiny.shinyapp.$inputValues['flow_layout'].positions;",
    "return Math.abs(p.a.x - p.b.x) < 1 && p.b.y > p.a.y; })()"
  ))
  wait_for(function() identical(jsonlite::read_json(path)$direction, "TB"), timeout = 10)
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
  # Not every target was saved: fit them all into view instead of the old view.
  expect_false(identical(viewport_transform(app), "translate(30px, 20px) scale(0.5)"))
})

test_that("a saved layout with every target restores the saved view", {
  dir <- local_pipeline(make = TRUE)
  dir.create(file.path(dir, "_targets", "user"), showWarnings = FALSE)
  jsonlite::write_json(
    list(direction = "TB", viewport = list(x = 30, y = 20, zoom = 0.5),
         positions = list(a = list(x = 0, y = 0), b = list(x = 0, y = 200), c = list(x = 0, y = 400))),
    file.path(dir, "_targets", "user", "targetsboard_layout.json"),
    auto_unbox = TRUE
  )
  app <- local_board_app(dir, "layout_view")
  app$wait_for_js("document.querySelector('.react-flow__viewport').style.transform === 'translate(30px, 20px) scale(0.5)'")
  expect_equal(node_position(app, "c"), c(x = 0, y = 400))
  # Top to bottom, as saved: handles at the top and bottom.
  expect_true(app$get_js(paste0(node_js("b"), ".querySelector('.react-flow__handle-bottom') !== null")))
})

# Clicks the node of target `id` with the multi-selection key held, as a
# person would: React Flow adds to the selection while it sees the key down.
# The key is Cmd on a Mac (where Ctrl-click is a right-click), else Ctrl, as
# React Flow decides. The events the page saw are kept in window.__events,
# to tell why if the click didn't add the target.
ctrl_click <- function(app, id) {
  mac <- isTRUE(app$get_js("navigator.userAgent.indexOf('Mac') >= 0"))
  key <- if (mac) {
    list(key = "Meta", code = "MetaLeft", vk = 91, modifiers = 4)
  } else {
    list(key = "Control", code = "ControlLeft", vk = 17, modifiers = 2)
  }
  chrome <- app$get_chromote_session()
  chrome$Emulation$setFocusEmulationEnabled(enabled = TRUE)
  app$run_js(
    "window.__events = [];
     if (!window.__eventSpy) {
       window.__eventSpy = true;
       ['keydown', 'keyup', 'pointerdown', 'click'].forEach(t => document.addEventListener(t, e => {
         const n = e.target.closest && e.target.closest('.react-flow__node');
         window.__events.push([t, e.key || '', e.ctrlKey || e.metaKey, n ? n.dataset.id : ''].join(':'));
       }, true));
     }"
  )
  box <- app$get_js(paste0(
    "(() => { const r = ", node_js(id), ".getBoundingClientRect();",
    " return {x: r.left + r.width / 2, y: r.top + r.height / 2}; })()"
  ))
  chrome$Input$dispatchKeyEvent(type = "rawKeyDown", key = key$key, code = key$code,
                                windowsVirtualKeyCode = key$vk, modifiers = key$modifiers)
  chrome$Input$dispatchMouseEvent(type = "mousePressed", x = box$x, y = box$y, button = "left",
                                  clickCount = 1, modifiers = key$modifiers)
  chrome$Input$dispatchMouseEvent(type = "mouseReleased", x = box$x, y = box$y, button = "left",
                                  clickCount = 1, modifiers = key$modifiers)
  chrome$Input$dispatchKeyEvent(type = "keyUp", key = key$key, code = key$code,
                                windowsVirtualKeyCode = key$vk)
  Sys.sleep(0.3)
}

# Waits for `js` to be true, or fails with the events ctrl_click() saw.
wait_after_ctrl_click <- function(app, js) {
  tryCatch(
    app$wait_for_js(js, timeout = 8000),
    error = function(e) {
      stop(
        "Not true after the Ctrl-click: ", js,
        "
Events: ", paste(unlist(app$get_js("window.__events || []")), collapse = " | "),
        "
Selected: ", paste(unlist(app$get_js(
          "Array.from(document.querySelectorAll('.react-flow__node.selected')).map(n => n.dataset.id)"
        )), collapse = " "),
        "
Window: ", paste(unlist(app$get_js("[window.innerWidth, window.innerHeight]")), collapse = "x"),
        call. = FALSE
      )
    }
  )
}

# Drags a box around targets `ids` with Shift held: React Flow's box selection.
shift_drag_around <- function(app, ids) {
  box <- app$get_js(sprintf(
    "(() => { const rs = %s.map(id => document.querySelector(`.react-flow__node[data-id='${id}']`).getBoundingClientRect());
       return {left: Math.min(...rs.map(r => r.left)) - 15, top: Math.min(...rs.map(r => r.top)) - 15,
               right: Math.max(...rs.map(r => r.right)) + 15, bottom: Math.max(...rs.map(r => r.bottom)) + 15}; })()",
    jsonlite::toJSON(ids)
  ))
  input <- app$get_chromote_session()$Input
  input$dispatchKeyEvent(type = "rawKeyDown", key = "Shift", code = "ShiftLeft",
                         windowsVirtualKeyCode = 16, modifiers = 8)
  input$dispatchMouseEvent(type = "mousePressed", x = box$left, y = box$top, button = "left",
                           clickCount = 1, modifiers = 8)
  for (step in 1:5) {
    input$dispatchMouseEvent(type = "mouseMoved", button = "left", modifiers = 8,
                             x = box$left + (box$right - box$left) * step / 5,
                             y = box$top + (box$bottom - box$top) * step / 5)
  }
  input$dispatchMouseEvent(type = "mouseReleased", x = box$right, y = box$bottom, button = "left",
                           clickCount = 1, modifiers = 8)
  input$dispatchKeyEvent(type = "keyUp", key = "Shift", code = "ShiftLeft", windowsVirtualKeyCode = 16)
  Sys.sleep(0.3)
}

# Two chains and a loner: a -> b -> c, d -> e, and f.
local_group_pipeline <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(
    c(
      "library(targets)",
      "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2),",
      "     tar_target(d, 10), tar_target(e, d + 1), tar_target(f, 0))"
    ),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir, .local_envir = .env)
  targets::tar_make(reporter = "silent")
  dir
}

test_that("a selected group has its own toolbar, menu and highlighting", {
  dir <- local_group_pipeline()
  app <- local_board_app(dir, "group")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 6")
  app$wait_for_js(paste(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
    ".map(n => n.style.transform)).size === 6"
  ))
  selected <- function() sort(unlist(app$get_value(input = "flow_selected_nodes")))
  has_class <- function(id, class) app$get_js(sprintf("%s.classList.contains('%s')", node_js(id), class))
  buttons <- function(selector, what = "dataset.action") {
    unlist(app$get_js(sprintf(
      "Array.from(document.querySelectorAll('%s')).map(b => b.%s)", selector, what
    )))
  }
  menu_open <- "document.querySelector('.tb-node-menu') !== null"
  group_toolbar <- "document.querySelector('.tb-group-toolbar')"

  # Click b, Ctrl-click d: a group of two.
  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_js(paste(group_toolbar, "=== null && document.querySelector('.tb-node-toolbar') !== null"))
  if (isTRUE(app$get_js("navigator.userAgent.indexOf('Mac') >= 0"))) {
    # A Cmd-click sent through Chrome's DevTools doesn't reach React Flow's
    # key handling on macOS runners: the group is selected by command there,
    # and the Ctrl-click is tested on Linux and Windows.
    send_widget_message(app, "targetsboard:command", list(id = "flow", command = "select", nodes = c("b", "d")))
    app$wait_for_js(paste(group_toolbar, "!== null"))
  } else {
    ctrl_click(app, "d")
    wait_after_ctrl_click(app, paste(group_toolbar, "!== null"))
  }
  expect_equal(selected(), c("b", "d"))

  # One toolbar, over the group: without Value, counting the targets.
  expect_equal(app$get_js("document.querySelectorAll('.tb-node-toolbar').length"), 1)
  expect_equal(buttons(".tb-group-toolbar button"), c("run", "invalidate"))
  expect_equal(buttons(".tb-group-toolbar button", "innerText.trim()"), c("Run 2", "Invalidate 2"))

  # The group and its lineages stay bright; the loner f dims.
  for (id in c("b", "d")) expect_true(has_class(id, "tb-focused"))
  for (id in c("a", "c", "e")) expect_false(has_class(id, "tb-dimmed"))
  expect_true(has_class("f", "tb-dimmed"))

  # The toolbar reports the whole group.
  app$click(selector = ".tb-group-toolbar [data-action='invalidate']")
  app$wait_for_js("document.querySelector('.modal-dialog') !== null")
  modal <- app$get_js("document.querySelector('.modal-dialog').innerHTML")
  expect_match(modal, "<code>b</code>", fixed = TRUE)
  expect_match(modal, "<code>d</code>", fixed = TRUE)
  expect_equal(sort(unlist(app$get_value(input = "flow_node_action")$nodes)), c("b", "d"))
  dismiss_modal(app)

  # Right-click on one of the group: the group's menu, for all of it.
  app$run_js(
    "window.__copied = null;
     Object.defineProperty(navigator, 'clipboard', {configurable: true,
       value: {writeText: t => { window.__copied = t; return Promise.resolve(); }}});"
  )
  mouse_event(app, "d", "contextmenu")
  app$wait_for_js(menu_open)
  expect_equal(buttons(".tb-node-menu [role=menuitem]"), c(
    "run", "invalidate", "copy_name", "select_upstream", "select_downstream", "select_lineage"
  ))
  expect_equal(
    buttons(".tb-node-menu [role=menuitem]", "innerText.trim()")[1:3],
    c("Run 2 targets", "Invalidate 2 targets", "Copy 2 names")
  )
  app$click(selector = ".tb-node-menu [data-action='copy_name']")
  app$wait_for_js("window.__copied === 'c(\"b\", \"d\")'")
  expect_equal(selected(), c("b", "d")) # still the group

  # "Select what they depend on", from the group: all levels up.
  mouse_event(app, "b", "contextmenu")
  app$wait_for_js(menu_open)
  app$click(selector = ".tb-node-menu [data-action='select_upstream']")
  app$wait_for_js("document.querySelectorAll('.react-flow__node.selected').length === 3")
  expect_equal(selected(), c("a", "b", "d"))

  # Right-click outside the group: that target alone, with its own menu.
  mouse_event(app, "f", "contextmenu")
  app$wait_for_js(menu_open)
  app$wait_for_js("document.querySelectorAll('.react-flow__node.selected').length === 1")
  expect_equal(selected(), "f")
  expect_true("value" %in% buttons(".tb-node-menu [role=menuitem]"))
  expect_equal(app$get_js(group_toolbar), NULL)

  # Escape closes the menu first, then clears the selection and the focus.
  press_key(app, "Escape")
  app$wait_for_js(paste0("!(", menu_open, ")"))
  expect_equal(selected(), "f")
  press_key(app, "Escape")
  app$wait_for_js("document.querySelectorAll('.react-flow__node.selected, .react-flow__node.tb-focused').length === 0")
  app$wait_for_value(input = "flow_focused_node", ignore = list("f"))
  expect_null(app$get_value(input = "flow_selected_nodes"))
})

test_that("the menu selects what a target depends on, what depends on it, or both", {
  dir <- local_group_pipeline()
  app <- local_board_app(dir, "select")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 6")
  selected <- function() sort(unlist(app$get_value(input = "flow_selected_nodes")))
  choose <- function(id, action, n) {
    mouse_event(app, id, "contextmenu")
    app$wait_for_js("document.querySelector('.tb-node-menu') !== null")
    app$click(selector = sprintf(".tb-node-menu [data-action='%s']", action))
    app$wait_for_js(sprintf("document.querySelectorAll('.react-flow__node.selected').length === %d", n))
    selected()
  }
  expect_equal(choose("c", "select_upstream", 3), c("a", "b", "c"))
  app$click(selector = ".react-flow__pane")
  expect_equal(choose("a", "select_downstream", 3), c("a", "b", "c"))
  app$click(selector = ".react-flow__pane")
  expect_equal(choose("b", "select_lineage", 3), c("a", "b", "c"))
  app$click(selector = ".react-flow__pane")
  expect_equal(choose("e", "select_lineage", 2), c("d", "e"))
})

test_that("a Shift-drag box selects a group, and its box opens the group's menu", {
  dir <- local_group_pipeline()
  app <- local_board_app(dir, "box")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 6")
  app$wait_for_js(paste(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
    ".map(n => n.style.transform)).size === 6"
  ))
  Sys.sleep(1) # the first fit
  shift_drag_around(app, c("d", "e"))
  app$wait_for_value(input = "flow_selected_nodes")
  chosen <- unlist(app$get_value(input = "flow_selected_nodes"))
  expect_true(all(c("d", "e") %in% chosen))
  app$wait_for_js("document.querySelector('.tb-group-toolbar') !== null")

  app$run_js(
    "const box = document.querySelector('.react-flow__nodesselection-rect');
     const r = box.getBoundingClientRect();
     box.dispatchEvent(new MouseEvent('contextmenu', {bubbles: true, cancelable: true, button: 2,
       clientX: r.x + r.width / 2, clientY: r.y + r.height / 2}));"
  )
  app$wait_for_js("document.querySelector('.tb-node-menu') !== null")
  expect_equal(
    app$get_js("document.querySelector('.tb-node-menu [data-action=run]').innerText.trim()"),
    sprintf("Run %d targets", length(chosen))
  )
})

test_that("update_targetsboard_selection() selects targets from the server", {
  dir <- local_group_pipeline()
  app <- local_board_app(dir, "selectcmd", widget_app())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 6")
  selected <- function() sort(unlist(app$get_value(input = "dag_selected_nodes")))

  send_widget_message(app, "targetsboard:command",
                      list(id = "dag", command = "select", nodes = c("a", "f", "not_a_target")))
  app$wait_for_js("document.querySelectorAll('.react-flow__node.selected').length === 2")
  expect_equal(selected(), c("a", "f"))
  # A group without group actions: no toolbar, and no menu.
  expect_equal(app$get_js("document.querySelector('.tb-node-toolbar')"), NULL)
  mouse_event(app, "a", "contextmenu")
  app$wait_for_value(input = "dag_context_node")
  expect_equal(app$get_js("document.querySelector('.tb-node-menu')"), NULL)

  send_widget_message(app, "targetsboard:command", list(id = "dag", command = "select", nodes = I(character())))
  app$wait_for_js("document.querySelectorAll('.react-flow__node.selected').length === 0")
})
