# The canvas's theme hooks in headless Chrome: each --tb-theme-* a theme of
# the app may set reaches its part of the canvas, and unset, the part looks
# as it always has.
# Uses the installed targetsboard (as R CMD check does), like every browser
# test: install the branch to a scratch library first to test a change.

test_that("a theme's --tb-theme-* colours reach the canvas, and default to the board's own", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "canvas-tokens")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$run_js("localStorage.removeItem('targetsboard.theme')")
  # b selected, and so focused; a not.
  app$click(selector = ".react-flow__node[data-id='b'] .base-node")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='b']\").classList.contains('selected')")
  parts <- "(() => {
    const cs = (el, p) => getComputedStyle(el).getPropertyValue(p);
    const node = (id) => document.querySelector(`.react-flow__node[data-id='${id}'] .base-node`);
    const edge = document.querySelector('.react-flow__edge');
    const path = edge.querySelector('.react-flow__edge-path');
    const r = {
      node_border: cs(node('a'), 'border-color'),
      node_glow: cs(node('a'), 'box-shadow'),
      selected_border: cs(node('b'), 'border-color'),
      selected_tint: cs(node('b'), 'background-image'),
      focus_shadow: cs(node('b'), 'box-shadow'),
      edge: cs(path, 'stroke'),
      edge_glow: cs(path, 'filter')
    };
    edge.classList.add('animated'); r.edge_running = cs(path, 'stroke');
    edge.classList.add('selected'); r.edge_running_selected = cs(path, 'stroke');
    edge.classList.remove('animated'); r.edge_selected = cs(path, 'stroke');
    edge.classList.remove('selected');
    const button = document.querySelector('.react-flow__controls-button');
    r.controls_bg = cs(button, 'background-color');
    r.controls_color = cs(button, 'color');
    r.controls_border = cs(button, 'border-bottom-color');
    r.controls_shadow = cs(document.querySelector('.react-flow__controls'), 'box-shadow');
    return r; })()"

  # Unset: as the board has always drawn them.
  got <- app$get_js(parts)
  expect_equal(got$selected_border, got$node_border)
  expect_match(got$selected_tint, "rgba(0, 0, 0, 0)", fixed = TRUE)
  expect_equal(got$focus_shadow, "rgb(47, 34, 61) 0px 0px 0px 3px")
  expect_equal(got$node_glow, "none")
  expect_equal(got$edge_glow, "none")
  expect_equal(got$edge_running, got$edge)
  expect_equal(got$edge_running_selected, got$edge_selected)
  expect_equal(got$controls_bg, "rgb(254, 254, 254)") # React Flow's own

  # Set by a theme, on the page's root as R/theme.R's rules do.
  app$run_js("const s = document.documentElement.style;
    s.setProperty('--tb-theme-node-border', 'rgb(1, 2, 3)');
    s.setProperty('--tb-theme-node-selected-border', 'rgb(4, 5, 6)');
    s.setProperty('--tb-theme-node-selected-tint', 'rgba(7, 8, 9, 0.5)');
    s.setProperty('--tb-theme-node-focus-shadow', 'rgb(10, 11, 12) 0px 0px 8px 2px');
    s.setProperty('--tb-theme-node-glow', 'rgb(13, 14, 15) 0px 0px 4px 0px');
    s.setProperty('--tb-theme-edge', 'rgb(16, 17, 18)');
    s.setProperty('--tb-theme-edge-selected', 'rgb(19, 20, 21)');
    s.setProperty('--tb-theme-edge-running', 'rgb(22, 23, 24)');
    s.setProperty('--tb-theme-edge-running-selected', 'rgb(25, 26, 27)');
    s.setProperty('--tb-theme-edge-glow', 'drop-shadow(rgb(28, 29, 30) 0px 0px 2px)');
    s.setProperty('--tb-theme-controls-bg', 'rgb(31, 32, 33)');
    s.setProperty('--tb-theme-controls-color', 'rgb(34, 35, 36)');
    s.setProperty('--tb-theme-controls-border', 'rgb(37, 38, 39)');
    s.setProperty('--tb-theme-controls-shadow', 'rgb(40, 41, 42) 0px 0px 2px 1px');")
  got <- app$get_js(parts)
  expect_equal(got$node_border, "rgb(1, 2, 3)")
  expect_equal(got$selected_border, "rgb(4, 5, 6)")
  expect_match(got$selected_tint, "rgba(7, 8, 9, 0.5)", fixed = TRUE)
  expect_equal(got$focus_shadow, "rgb(10, 11, 12) 0px 0px 8px 2px")
  expect_equal(got$node_glow, "rgb(13, 14, 15) 0px 0px 4px 0px")
  expect_equal(got$edge, "rgb(16, 17, 18)")
  expect_equal(got$edge_selected, "rgb(19, 20, 21)")
  expect_equal(got$edge_running, "rgb(22, 23, 24)")
  expect_equal(got$edge_running_selected, "rgb(25, 26, 27)")
  expect_equal(got$edge_glow, "drop-shadow(rgb(28, 29, 30) 0px 0px 2px)")
  expect_equal(got$controls_bg, "rgb(31, 32, 33)")
  expect_equal(got$controls_color, "rgb(34, 35, 36)")
  expect_equal(got$controls_border, "rgb(37, 38, 39)")
  expect_equal(got$controls_shadow, "rgb(40, 41, 42) 0px 0px 2px 1px")

  # The edges' own: a theme's edge colour is the running edges' too.
  app$run_js("document.documentElement.style.removeProperty('--tb-theme-edge-running');")
  expect_equal(app$get_js(parts)$edge_running, "rgb(16, 17, 18)")
})
