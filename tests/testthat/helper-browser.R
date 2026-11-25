# Helpers for the browser tests (test-browser-*.R).

# The width and height in a PNG file's header.
png_size <- function(path) {
  header <- readBin(path, "raw", 24)
  expect_equal(header[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  int <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  c(width = int(header[17:20]), height = int(header[21:24]))
}

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
# The view is fitted to the graph: the Fit View button leaves it as it is.
expect_fitted <- function(app) {
  Sys.sleep(0.5) # the view settled
  view <- viewport_numbers(app)
  app$click(selector = ".react-flow__controls-fitview")
  Sys.sleep(0.5)
  expect_equal(viewport_numbers(app), view, tolerance = 1e-3)
}

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
  # React Flow sees the key through React state and an effect: only after a
  # render does it select more than one. A click straight away can beat it.
  Sys.sleep(0.2)
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

# WCAG's contrast ratio of an element's text on the background it shows on,
# in the page: window.tbContrast(selector, prop = 'color', inner = null),
# `inner` a selector in the element's shadow root. Colours as the browser
# computes them, color-mix() ones included (color(srgb 0-1)), see-through
# ones blended on what is behind them. Installed by contrast_in().
contrast_lib_js <- paste(
  "window.tbContrast = (() => { const rgba = c => { const v = c.match(/[0-9.e-]+/g).map(Number);",
  "  if (c.startsWith('color(srgb')) { v.splice(0, 3, ...v.slice(0, 3).map(x => x * 255)); }",
  "  return v.length < 4 ? [...v, 1] : v; };",
  "const lum = c => { const [r, g, b] = c.slice(0, 3).map(v => { v /= 255;",
  "  return v <= 0.03928 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4; });",
  "  return 0.2126 * r + 0.7152 * g + 0.0722 * b; };",
  "const bg = el => { for (; el; el = el.parentElement) { const c = rgba(getComputedStyle(el).backgroundColor);",
  "  if (c[3] > 0.5) return c; } return [255, 255, 255, 1]; };",
  "return (sel, prop = 'color', inner = null) => { const el = document.querySelector(sel); const b = bg(el);",
  "  const f = rgba(getComputedStyle(inner ? el.shadowRoot.querySelector(inner) : el)[prop]); const t = f.slice(0, 3).map((v, i) => v * f[3] + b[i] * (1 - f[3]));",
  "  const [x, y] = [lum(t), lum(b)]; return (Math.max(x, y) + 0.05) / (Math.min(x, y) + 0.05); }; })();"
)

# Installs tbContrast in the app's page, and returns a function of a
# selector giving its element's contrast ratio.
contrast_in <- function(app) {
  app$run_js(contrast_lib_js)
  function(selector) app$get_js(sprintf("tbContrast(\"%s\")", selector))
}
