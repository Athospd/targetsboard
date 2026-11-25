# The board app in headless Chrome: drawing, the Load spinner, the Script
# field, and its actions reaching the server.
# Uses the installed targetsboard (as R CMD check does), like every browser
# test: install the branch to a scratch library first to test a change.

test_that("the board renders the DAG, focuses targets and keeps its layout", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- local_pipeline(make = TRUE)
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))

  app <- new_app_driver(dir, name = "board", load_timeout = 60 * 1000)
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
  app$wait_for_value(input = "board-graph-flow_focused_node")
  expect_equal(app$get_value(input = "board-graph-flow_focused_node"), "c")
  mouse_event(app, "c", "dblclick")
  app$wait_for_js(preview_of("c"), timeout = preview_timeout)
  expect_match(app$get_js("document.querySelector('.tb-preview-printed').textContent"), "4")
  # The page's R code highlighter (www/highlight.js) loaded, with R.
  expect_true(app$get_js("!!window.targetsboardHighlight && !!window.targetsboardHighlight.getLanguage('r')"))
  expect_true(has_class("c", "tb-focused"))
  expect_false(has_class("a", "tb-dimmed")) # upstream of c: in its lineage

  # Escape in the preview, as when leaving its full-screen, keeps the focus
  # and the preview: only the board's own Escape clears them.
  app$run_js("document.querySelector('.tb-preview-name').setAttribute('tabindex', '-1'); document.querySelector('.tb-preview-name').focus();")
  press_key(app, "Escape")
  expect_equal(app$get_value(input = "board-graph-flow_focused_node"), "c")
  expect_true(app$get_js("!!document.querySelector('.tb-preview-printed')"))

  # A status-only update retypes the node in place, without moving it, and
  # warning marks come and go without changing the status.
  before <- transform("a")
  app$run_js(
    "window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:
       {id: 'board-graph-flow', nodes: [{id: 'a', type: 'running', target_status: 'dispatched'}]}}))"
  )
  app$wait_for_js(paste0(node("a"), ".classList.contains('react-flow__node-running')"))
  expect_equal(transform("a"), before)

  # The board's own status updates can retype a again in between: send both
  # until they show, and read them together.
  app$wait_for_js(paste0(
    "(() => { const send = (detail) => window.dispatchEvent(new CustomEvent('targetsboard:status', {detail}));",
    " send({id: 'board-graph-flow', nodes: [{id: 'a', type: 'running', target_status: 'dispatched'}]});",
    " send({id: 'board-graph-flow', nodes: [], warnings: ['b']});",
    " return ", node("b"), ".querySelector('.warning-mark') !== null && ",
    node("a"), ".classList.contains('react-flow__node-running'); })()"
  ))
  app$run_js(
    "window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:
       {id: 'board-graph-flow', nodes: [], warnings: []}}))"
  )
  app$wait_for_js(paste0(node("b"), ".querySelector('.warning-mark') === null"))

  # A pattern's branch counts, during a run: shown, red with errors, cleared.
  # The board's own status updates can clear the counts again before they
  # are read: send them until they show, and read them in the same check.
  app$wait_for_js(paste0(
    "(() => { const p = ", node("b"), ".querySelector('.branch-progress');",
    " if (p && p.innerText === '3/6' && p.classList.contains('branch-progress-errored')) return true;",
    " window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:",
    " {id: 'board-graph-flow', nodes: [], branches: {b: {total: 6, done: 3, errored: 1}}}}));",
    " return false; })()"
  ))
  expect_false(app$get_js(paste0(node("a"), ".querySelector('.branch-progress') !== null")))
  app$run_js(
    "window.dispatchEvent(new CustomEvent('targetsboard:status', {detail:
       {id: 'board-graph-flow', nodes: [], branches: []}}))"
  )
  app$wait_for_js(paste0(node("b"), ".querySelector('.branch-progress') === null"))

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
  # Placed once the layout has run: slow under coverage's instrumentation.
  app$wait_for_js(paste0(node("d"), ".style.transform !== 'translate(0px, 0px)'"), timeout = 30 * 1000)
  expect_equal(list(a = transform("a"), b = transform("b"), c = transform("c")), placed)
  expect_false(transform("d") %in% unlist(placed))
  expect_false(has_class("d", "tb-dimmed")) # downstream of the focused c
  expect_true(has_class("e", "tb-dimmed")) # unrelated to c

  # Clicking the background clears the focus.
  app$click(selector = ".react-flow__pane")
  app$wait_for_js(paste0("!", node("a"), ".classList.contains('tb-dimmed') && !",
                         node("c"), ".classList.contains('tb-focused')"))
})

test_that("the Load spinner covers the board only while Load loads, not at the start", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "Sys.sleep(3)",
    "list(tar_target(slow_a, 1), tar_target(slow_b, slow_a))"
  ), file.path(dir, "slow.R"))
  app <- local_board_app(dir, "spinner_only_while_loading", "targetsboard::tar_board_app(scripts = 'slow.R')")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 0")
  # Straight after the graph is drawn, before the server's first update: a
  # click in a target's middle reaches the target, not a cover.
  hit <- paste0(
    "(() => { const r = document.querySelector('.react-flow__node').getBoundingClientRect();",
    " const e = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);",
    " return e.closest('.react-flow__node') !== null; })()"
  )
  expect_true(app$get_js(hit))
  expect_equal(app$get_js("getComputedStyle(document.querySelector('.tb-board-loading .load-container')).display"), "none")
  # And still none once the server has answered.
  app$wait_for_js("Shiny.shinyapp.$values['board-script-loading'] !== undefined")
  expect_true(app$get_js(hit))

  # Load a slow script: the spinner covers the pane, captioned, and takes the
  # click; then it goes once the new graph is drawn.
  app$set_inputs(`board-script-path` = "slow.R")
  app$click(selector = "#board-script-load")
  app$wait_for_js(paste(
    "getComputedStyle(document.querySelector('.tb-board-loading .load-container')).display !== 'none'",
    "&& document.querySelector('#board-script-loading_caption').innerText === 'Loading pipeline\u2026'"
  ), timeout = 10000)
  expect_false(app$get_js(hit))
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='slow_a']\") !== null", timeout = 30000)
  app$wait_for_js("getComputedStyle(document.querySelector('.tb-board-loading .load-container')).display === 'none'")
  # The new graph is still being laid out and fitted: a click reaches its
  # targets once they settle, with no cover in the way.
  app$wait_for_js(hit)
})

test_that("a board started before its script draws the pipeline once it appears", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- withr::local_tempdir()
  writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))

  app <- new_app_driver(dir, name = "empty", load_timeout = 60 * 1000)
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

test_that("the delete keys don't remove targets from the read-only board", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "delete")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")

  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_value(input = "board-graph-flow_selected_nodes")
  for (key in c("Backspace", "Delete")) press_key(app, key)
  Sys.sleep(1)

  expect_equal(app$get_js("document.querySelectorAll('.react-flow__node').length"), 3)
  expect_equal(app$get_js("document.querySelectorAll('.react-flow__edge').length"), 2)
})

test_that("a click on a tar_meta() row shows its target on the board", {
  dir <- local_chain_pipeline(8)
  targets::tar_make(reporter = "silent")
  app <- local_board_app(dir, "metajump")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 8")
  # A group selected before: the row's target replaces it.
  send_widget_message(app, "targetsboard:command",
                      list(id = "board-graph-flow", command = "select", nodes = c("t1", "t2")))
  app$wait_for_js("document.querySelector('.tb-group-toolbar') !== null")
  # A group's toolbar has no Copy: that's for a single target.
  expect_equal(
    unlist(app$get_js("Array.from(document.querySelectorAll('.tb-group-toolbar button')).map(b => b.dataset.action)")),
    c("run", "invalidate")
  )

  app$click(selector = "a[data-value='tar_meta']")
  app$wait_for_js("document.querySelectorAll('#board-meta-table .rt-tbody .rt-tr').length === 8")
  app$run_js(
    "Array.from(document.querySelectorAll('#board-meta-table .rt-tbody .rt-tr'))
       .find(r => r.querySelector('.rt-td').innerText.trim() === 't6')
       .querySelector('.rt-td').click()"
  )
  app$wait_for_js("document.querySelector(\"a[data-value='board']\").classList.contains('active')")
  app$wait_for_js(paste0(node_js("t6"), ".classList.contains('tb-focused')"))
  app$wait_for_value(input = "board-graph-flow_focused_node")
  expect_equal(app$get_value(input = "board-graph-flow_focused_node"), "t6")
  # Alone: selected, ringed, with its own toolbar.
  app$wait_for_js("document.querySelector('.tb-group-toolbar') === null")
  expect_equal(unlist(app$get_value(input = "board-graph-flow_selected_nodes")), "t6")
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
    c("run", "invalidate", "value", "copy_name")
  )
  expect_equal(app$get_js("document.querySelector(\".tb-node-toolbar [data-action='copy_name']\").textContent"), "Copy")
  # Above its target.
  expect_true(app$get_js(paste0(
    "(() => { const t = ", toolbar, ".getBoundingClientRect();",
    " const n = ", node_js("b"), ".getBoundingClientRect();",
    " return t.bottom <= n.top + 1 && t.right > n.left && t.left < n.right; })()"
  )))

  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js(preview_of("b"), timeout = preview_timeout)
  expect_match(app$get_js("document.querySelector('.tb-preview-printed').textContent"), "2")

  app$click(selector = ".tb-node-toolbar [data-action='invalidate']")
  app$wait_for_js("document.querySelector('.modal-dialog') !== null")
  expect_match(app$get_js("document.querySelector('.modal-dialog').innerHTML"), "<code>b</code>", fixed = TRUE)
  dismiss_modal(app)

  # Copy copies its name, quoted as an R string, in the browser.
  app$run_js(
    "window.__copied = null;
     Object.defineProperty(navigator, 'clipboard', {configurable: true,
       value: {writeText: t => { window.__copied = t; return Promise.resolve(); }}});"
  )
  app$click(selector = ".tb-node-toolbar [data-action='copy_name']")
  app$wait_for_js("window.__copied === '\"b\"'")

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

test_that("the script bar takes a typed path, Enter or Load loads it, and other tabs follow", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome not found")

  dir <- withr::local_tempdir()
  writeLines(c("library(targets)", "list(tar_target(a, 1), tar_target(b, a))"), file.path(dir, "_targets.R"))
  writeLines(c("library(targets)", "list(tar_target(x, 1))"), file.path(dir, "other.R"))
  dir.create(file.path(dir, "pipelines"))
  writeLines(c("library(targets)", "list(tar_target(y, 1))"), file.path(dir, "pipelines", "not_suggested.R"))
  writeLines("targetsboard::tar_board_app(scripts = 'other.R')", file.path(dir, "app.R"))
  app <- new_app_driver(dir, name = "scripts", load_timeout = 60 * 1000)
  withr::defer(app$stop())
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 2", timeout = 30 * 1000)
  expect_equal(app$get_value(input = "board-script-path"), "_targets.R")
  suggestions <- "Array.from(document.querySelectorAll('#board-script-suggestions option')).map(o => o.value)"
  expect_equal(unlist(app$get_js(suggestions)), c("_targets.R", "other.R"))
  shown <- "Array.from(document.querySelectorAll('.tb-suggestion')).map(i => i.textContent)"
  chrome <- app$get_chromote_session()
  press <- function(key, code) {
    for (type in c("rawKeyDown", "keyUp")) {
      chrome$Input$dispatchKeyEvent(type = type, key = key, code = key, windowsVirtualKeyCode = code)
    }
  }

  # Focused, the field lists every script, whatever is in it already.
  app$click(selector = "#board-script-path")
  app$wait_for_js("document.querySelectorAll('.tb-suggestion').length === 2")
  expect_equal(unlist(app$get_js(shown)), c("_targets.R", "other.R"))
  # Typing filters them; Escape closes the list.
  app$run_js("document.getElementById('board-script-path').select()")
  chrome$Input$insertText(text = "oth")
  app$wait_for_js("document.querySelectorAll('.tb-suggestion').length === 1")
  expect_equal(unlist(app$get_js(shown)), "other.R")
  press("Escape", 27)
  app$wait_for_js("document.querySelector('.tb-suggestions') === null")

  # A path typed as a person would, not among the suggestions, and Enter.
  app$run_js("const i = document.getElementById('board-script-path'); i.focus(); i.select();")
  chrome$Input$insertText(text = "pipelines/not_suggested.R")
  press("Enter", 13)
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='y']\") !== null", timeout = 30 * 1000)
  expect_equal(app$get_js("document.querySelectorAll('.react-flow__node').length"), 1)
  # Loaded, it joins the suggestions.
  app$wait_for_js(paste0(suggestions, ".includes('pipelines/not_suggested.R')"))

  # A suggestion picked with the arrow keys and Enter loads it.
  app$run_js("document.getElementById('board-script-path').select()")
  chrome$Input$insertText(text = "other")
  app$wait_for_js("document.querySelectorAll('.tb-suggestion').length === 1")
  press("ArrowDown", 40)
  press("Enter", 13)
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='x']\") !== null", timeout = 30 * 1000)
  expect_equal(app$get_value(input = "board-script-path"), "other.R")
  # One clicked loads it too; so does the Load button.
  app$click(selector = "#board-script-path")
  app$wait_for_js("document.querySelector(\".tb-suggestion[data-value='_targets.R']\") !== null")
  app$click(selector = ".tb-suggestion[data-value='_targets.R']")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='a']\") !== null", timeout = 30 * 1000)
  app$set_inputs(`board-script-path` = "other.R")
  app$click("board-script-load")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='x']\") !== null", timeout = 30 * 1000)

  # A tab opened afterwards names the script it draws, not the launch script.
  second <- new_app_driver(app$get_url(), name = "scripts-second", load_timeout = 60 * 1000)
  withr::defer(second$stop())
  second$wait_for_js(
    "document.querySelector(\".react-flow__node[data-id='x']\") !== null", timeout = 30 * 1000
  )
  second$wait_for_value(input = "board-script-path", ignore = list(NULL, "_targets.R"), timeout = 10 * 1000)
  expect_equal(second$get_value(input = "board-script-path"), "other.R")
})

test_that("in dark mode, the tables, the toolbar and the buttons stay readable", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "dark")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  # WCAG's contrast ratio of an element's text on the background it shows on.
  app$run_js(paste(
    "const rgba = c => { const v = c.match(/[0-9.]+/g).map(Number); return v.length < 4 ? [...v, 1] : v; };",
    "const lum = c => { const [r, g, b] = c.slice(0, 3).map(v => { v /= 255;",
    "  return v <= 0.03928 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4; });",
    "  return 0.2126 * r + 0.7152 * g + 0.0722 * b; };",
    "const bg = el => { for (; el; el = el.parentElement) { const c = rgba(getComputedStyle(el).backgroundColor);",
    "  if (c[3] > 0.5) return c; } return [255, 255, 255, 1]; };",
    "window.tbContrast = sel => { const el = document.querySelector(sel); const b = bg(el);",
    "  const f = rgba(getComputedStyle(el).color); const t = f.slice(0, 3).map((v, i) => v * f[3] + b[i] * (1 - f[3]));",
    "  const [x, y] = [lum(t), lum(b)]; return (Math.max(x, y) + 0.05) / (Math.min(x, y) + 0.05); };"
  ))
  contrast <- function(selector) app$get_js(sprintf("tbContrast(\"%s\")", selector))
  tab <- function(value) {
    app$click(selector = sprintf("#board-view .nav-link[data-value='%s']", value))
    Sys.sleep(0.5)
  }

  app$run_js("document.querySelector('bslib-input-dark-mode').setAttribute('mode', 'dark')")
  app$wait_for_js("document.documentElement.dataset.bsTheme === 'dark'")
  app$wait_for_js("document.querySelector('.reactflow-wrapper').classList.contains('tb-dark')")

  app$click(selector = ".react-flow__node[data-id='b']")
  app$wait_for_js("document.querySelector('.tb-node-toolbar button') !== null")
  wait_for_settled(app)
  expect_gte(contrast(".tb-node-toolbar button"), 4.5)
  expect_false(identical( # dark, as the page around it
    app$get_js("getComputedStyle(document.querySelector('.tb-node-toolbar button')).backgroundColor"),
    "rgb(255, 255, 255)"
  ))
  refresh <- contrast("#board-script-refresh")
  expect_gte(refresh, 4.5, label = paste( # with its colours, to tell a fade from a real change
    "Refresh's contrast",
    app$get_js("(s => [s.color, s.backgroundColor, s.borderColor, s.transitionDuration].join(' / '))(getComputedStyle(document.querySelector('#board-script-refresh')))")
  ))

  tab("problems")
  app$wait_for_js("document.querySelector('#board-problems-table .rt-no-data') !== null")
  expect_gte(contrast("#board-problems-table .rt-no-data"), 4.5)
  tab("tar_meta")
  app$wait_for_js("document.querySelector('#board-meta-table .rt-td') !== null")
  expect_gte(contrast("#board-meta-table .rt-td"), 4.5)
  expect_gte(contrast("#board-meta-table .rt-th"), 4.5)
  # The Script tab: the script coloured, where things are readable.
  tab("script")
  app$wait_for_js("document.querySelector('.tb-script-view pre.language-r[data-highlighted]') !== null")
  expect_match(app$get_js("document.querySelector('.tb-script-view pre').innerText"), "tar_target", fixed = TRUE)
  expect_gte(contrast(".tb-script-view .tb-paths th"), 4.5)
  expect_gte(contrast(".tb-script-view .tb-paths caption"), 4.5)
  expect_gte(contrast(".tb-script-view .tb-paths .tb-copy-path code"), 4.5)
  expect_gte(contrast(".tb-script-view .tb-path-missing .tb-copy-path code"), 4.5)
})
