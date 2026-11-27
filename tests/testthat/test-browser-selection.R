# Selecting groups of targets in headless Chrome: Ctrl-click, Shift-drag,
# the group's menu, and selections from the server.
# Uses the installed targetsboard (as R CMD check does), like every browser
# test: install the branch to a scratch library first to test a change.

test_that("a selected group has its own toolbar, menu and highlighting", {
  dir <- local_group_pipeline()
  app <- local_board_app(dir, "group")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 6")
  app$wait_for_js(paste(
    "new Set(Array.from(document.querySelectorAll('.react-flow__node'))",
    ".map(n => n.style.transform)).size === 6"
  ))
  selected <- function() sort(unlist(app$get_value(input = "board-graph-flow_selected_nodes")))
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
    send_widget_message(app, "targetsboard:command", list(id = "board-graph-flow", command = "select", nodes = c("b", "d")))
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

  # The group and its direct neighbours stay bright; the loner f dims.
  for (id in c("b", "d")) expect_true(has_class(id, "tb-focused"))
  for (id in c("a", "c", "e")) expect_false(has_class(id, "tb-dimmed"))
  expect_true(has_class("f", "tb-dimmed"))

  # The toolbar reports the whole group.
  app$click(selector = ".tb-group-toolbar [data-action='invalidate']")
  app$wait_for_js("document.querySelector('.modal-dialog') !== null")
  modal <- app$get_js("document.querySelector('.modal-dialog').innerHTML")
  expect_match(modal, "<code>b</code>", fixed = TRUE)
  expect_match(modal, "<code>d</code>", fixed = TRUE)
  expect_equal(sort(unlist(app$get_value(input = "board-graph-flow_node_action")$nodes)), c("b", "d"))
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
  app$wait_for_value(input = "board-graph-flow_focused_node", ignore = list("f"))
  expect_null(app$get_value(input = "board-graph-flow_selected_nodes"))
})

test_that("the menu selects what a target depends on, what depends on it, or both", {
  dir <- local_group_pipeline()
  app <- local_board_app(dir, "select")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 6")
  selected <- function() sort(unlist(app$get_value(input = "board-graph-flow_selected_nodes")))
  choose <- function(id, action, n) {
    mouse_event(app, id, "contextmenu")
    app$wait_for_js("document.querySelector('.tb-node-menu') !== null")
    app$click(selector = sprintf(".tb-node-menu [data-action='%s']", action))
    app$wait_for_js(sprintf("document.querySelectorAll('.react-flow__node.selected').length === %d", n))
    # The input reaches the server a moment after the page shows it (R-devel CI).
    wait_for(function() length(selected()) == n, timeout = 10)
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
  app$wait_for_value(input = "board-graph-flow_selected_nodes")
  chosen <- unlist(app$get_value(input = "board-graph-flow_selected_nodes"))
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
