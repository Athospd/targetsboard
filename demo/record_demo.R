# Records the board on the demo pipeline: frames to <OUT>/frames, see recorder.R.
# Uses the installed targetsboard: R CMD INSTALL . first.
source(file.path(Sys.getenv("DEMO_DIR", "demo"), "recorder.R"))
source(file.path(DEMO, "pipeline.R"))
dir <- file.path(OUT, "project")
dir.create(dir, showWarnings = FALSE)
unlink(file.path(dir, c("_targets", "report.html", "_targetsboard")), recursive = TRUE)
write_demo(dir)
writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))
app <- AppDriver$new(dir, name = "demo", width = 1280, height = 720, load_timeout = 60000)
app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 8", timeout = 30000)
s <- app$get_chromote_session()
# 1280 x 720 css pixels at 1.5x: full-HD frames, with everything 50% bigger.
s$Emulation$setDeviceMetricsOverride(width = 1280, height = 720, deviceScaleFactor = 1.5, mobile = FALSE)
Sys.sleep(1.5)
app$run_js("document.querySelector('.react-flow__controls-fitview').click()")
Sys.sleep(1.5)

# ---- Overlay: a cursor, click ripples, captions, a code card -----------------
app$run_js(readr_overlay <- paste(readLines(file.path(DEMO, "overlay.js")), collapse = "\n"))

pos <- c(1150, 660); buttons <- 0
dismiss <- function() js("document.querySelectorAll('.shiny-notification-close').forEach(b => b.click())")
fit <- function() { click('.react-flow__controls-fitview', dur = 0.6); hold(s, 1) }
js <- function(code) s$Runtime$evaluate(code, returnByValue = TRUE)$result$value
center <- function(sel) {
  r <- js(sprintf("(() => { const r = document.querySelector(\"%s\").getBoundingClientRect(); return [r.x + r.width/2, r.y + r.height/2]; })()", sel))
  unlist(r)
}
node <- function(id) sprintf(".react-flow__node[data-id='%s']", id)
mouse <- function(type, x, y, button = "none", clickCount = 0, modifiers = 0) {
  s$Input$dispatchMouseEvent(type = type, x = x, y = y, button = button, buttons = buttons,
                             clickCount = clickCount, modifiers = modifiers)
}
move <- function(to, dur = 0.8) {
  from <- pos; steps <- max(1, round(dur / 0.035))
  for (i in seq_len(steps)) {
    f <- i / steps; f <- f * f * (3 - 2 * f) # smoothstep
    p <- from + (to - from) * f
    js(sprintf("document.getElementById('demo-cursor').style.transform = 'translate(%.1fpx, %.1fpx)'", p[1], p[2]))
    mouse("mouseMoved", p[1], p[2])
    Sys.sleep(0.012)
  }
  pos <<- to
}
ripple <- function() js(sprintf("(() => { const r = document.createElement('div'); r.className = 'demo-ripple'; r.style.left = '%.1fpx'; r.style.top = '%.1fpx'; document.body.appendChild(r); setTimeout(() => r.remove(), 700); })()", pos[1], pos[2]))
click <- function(sel = NULL, at = NULL, button = "left", double = FALSE, modifiers = 0, dur = 0.8) {
  if (!is.null(sel)) at <- center(sel)
  if (!is.null(at)) move(at, dur)
  hold(s, 0.15); ripple()
  b <- if (button == "left") 1 else 2
  buttons <<- b; mouse("mousePressed", pos[1], pos[2], button, 1, modifiers)
  buttons <<- 0; mouse("mouseReleased", pos[1], pos[2], button, 1, modifiers)
  if (double) {
    buttons <<- b; mouse("mousePressed", pos[1], pos[2], button, 2, modifiers)
    buttons <<- 0; mouse("mouseReleased", pos[1], pos[2], button, 2, modifiers)
  }
}
drag <- function(sel, by, dur = 1.2) {
  move(center(sel)); hold(s, 0.2)
  buttons <<- 1; mouse("mousePressed", pos[1], pos[2], "left", 1)
  move(pos + by, dur)
  buttons <<- 0; mouse("mouseReleased", pos[1], pos[2], "left", 1)
}
key <- function(type, key, code, vk, modifiers = 0) {
  s$Input$dispatchKeyEvent(type = type, key = key, code = code, windowsVirtualKeyCode = vk, modifiers = modifiers)
}
caption <- function(kicker, text) {
  delay <- if (isTRUE(js("document.getElementById('demo-caption').classList.contains('on')"))) 420 else 0
  js(sprintf("(() => { const c = document.getElementById('demo-caption'); c.classList.remove('on');
    setTimeout(() => { c.querySelector('.k').textContent = %s; c.querySelector('.t').innerHTML = %s; c.classList.add('on'); }, %d); })()",
    jsonlite::toJSON(kicker, auto_unbox = TRUE), jsonlite::toJSON(text, auto_unbox = TRUE), delay))
}
no_caption <- function() js("document.getElementById('demo-caption').classList.remove('on')")
until <- function(cond, timeout = 60) {
  end <- Sys.time() + timeout
  while (!isTRUE(js(cond)) && Sys.time() < end) hold(s, 0.1)
  if (!isTRUE(js(cond))) message("timed out: ", cond)
}
has <- function(sel) sprintf("document.querySelector(\"%s\") !== null", sel)
preview_toggle <- "Array.from(document.querySelectorAll('button.collapse-toggle')).find(b => b.getBoundingClientRect().x > 900)"

# ---- Record ------------------------------------------------------------------
rec <- start_recording(app)
hold(s, 0.8)
caption("targetsboard", "Your <code>{targets}</code> pipeline, live in the browser")
move(c(700, 380), 1.6)
hold(s, 3)

# Watching a run
caption("Watch it run", "Run the pipeline from the board, or <code>tar_make()</code> in your console")
click("#run_all")
hold(s, 4.5)
caption("Watch it run", "Running targets spin, and the edges feeding them flow")
move(center(node("clean")) + c(40, 90), 1.2)
until(has(".branch-progress"), 20)
caption("Watch it run", "Dynamic branches count up as they finish")
move(center(node("ozone_model")) + c(30, 70), 1)
until(has(".react-flow__node-errored[data-id='forecast']"), 40)
caption("Watch it run", "Warnings and errors flag themselves")
until("!document.querySelector('.react-flow__node-running')", 30)
hold(s, 2.5)
dismiss()
# More room for the graph and the previews: fold the control panel.
p <- unlist(js("(() => { const r = Array.from(document.querySelectorAll('button.collapse-toggle')).find(b => b.getBoundingClientRect().x < 400).getBoundingClientRect(); return [r.x + r.width/2, r.y + r.height/2]; })()"))
click(at = p)
hold(s, 0.8)
fit()

# Values
caption("Values", "Double-click a target to preview its value: data frames get a skimr summary")
click(node("clean"), double = TRUE)
hold(s, 2.5)
fit()
hold(s, 1.5)
js("document.getElementById('value_preview_content').scrollIntoView({behavior: 'smooth', block: 'start'})")
hold(s, 4)
caption("Values", "ggplots render as plots")
click(node("ozone_plot"))
hold(s, 4)
click(node("effects_plot"))
hold(s, 3.5)
caption("Values", "HTML reports open right in the board")
click(node("report"))
hold(s, 5)
p <- unlist(js(paste0("(() => { const r = ", preview_toggle, ".getBoundingClientRect(); return [r.x + r.width/2, r.y + r.height/2]; })()")))
click(at = p)
hold(s, 1.2)
fit()

# The DAG
caption("Your DAG, your way", "Drag targets where you want them")
drag(node("model"), c(0, 150))
hold(s, 1.5)
caption("Your DAG, your way", "Tidy it in one click")
click(".tb-layout-buttons button:nth-child(1)")
hold(s, 2.5)
click(".tb-layout-buttons button:nth-child(2)")
hold(s, 2)
fit()
caption("Your DAG, your way", "Right-click for actions: select a lineage, run, invalidate")
click(node("ozone_model"), button = "right")
until(has(".tb-node-menu"), 5)
hold(s, 1.2)
click(".tb-node-menu [data-action='select_lineage']")
hold(s, 2.8)
key("keyDown", "Escape", "Escape", 27); key("keyUp", "Escape", "Escape", 27)
caption("Your DAG, your way", "Ctrl-click to act on several targets at once")
move(center(node("ozone_plot")))
key("keyDown", "Control", "ControlLeft", 17, 2); hold(s, 0.25)
click(node("ozone_plot"), modifiers = 2)
hold(s, 0.6)
click(node("effects_plot"), modifiers = 2)
key("keyUp", "Control", "ControlLeft", 17)
hold(s, 3)
key("keyDown", "Escape", "Escape", 27); key("keyUp", "Escape", "Escape", 27)
hold(s, 0.8)

# Errors
caption("Debugging", "Every error and warning, in one place")
click("a[data-value='problems']")
hold(s, 4)
click("a[data-value='board']")
hold(s, 1.2)
caption("Debugging", "Each error comes with its traceback, and code to reproduce it")
click(node("forecast"), double = TRUE)
hold(s, 2)
fit()
hold(s, 4.5)
caption("Debugging", "Fix the script…")
js("(() => { const c = document.getElementById('demo-code'); c.innerHTML = '<div class=\"f\">_targets.R</div><span class=\"m\">- predict(model, newdata = data.frame(Temp = 90, wind = 8))</span><span class=\"p\">+ predict(model, newdata = data.frame(Temp = 90, Wind = 8))</span>'; c.classList.add('on'); })()")
hold(s, 2.5)
write_demo(dir, fixed = TRUE)
hold(s, 1)
js("document.getElementById('demo-code').classList.remove('on')")
caption("Debugging", "…the board sees the change, and rebuilds just that target")
hold(s, 3)
click(node("forecast"))
until(has(".tb-node-toolbar [data-action='run']"), 5)
hold(s, 0.6)
click(".tb-node-toolbar [data-action='run']")
until(has(".react-flow__node-uptodate[data-id='forecast']"), 40)
hold(s, 1)
dismiss()
hold(s, 3)

# Extras
caption("And more", "The run's log, a click away")
click("a[data-value='log']")
hold(s, 3.5)
click("a[data-value='board']")
hold(s, 0.8)
caption("And more", "Light or dark, as you like it")
click("#dark_mode")
hold(s, 3.5)
no_caption()
move(c(1150, 660), 1.2)
hold(s, 1)
n <- stop_recording(rec)
cat("frames", n, "\n")
app$stop()
