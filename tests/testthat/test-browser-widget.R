# The widget in headless Chrome, in the board and alone: options, menus,
# commands from R, and its image.
# Uses the installed targetsboard (as R CMD check does), like every browser
# test: install the branch to a scratch library first to test a change.

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
  app$wait_for_js(preview_of("a"), timeout = preview_timeout)
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

test_that("a widget without toolbar actions has no toolbar", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "notoolbar", widget_app())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_js(paste0(node_js("b"), ".classList.contains('tb-focused')"))
  expect_false(app$get_js("document.querySelector('.tb-node-toolbar') !== null"))
})

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
