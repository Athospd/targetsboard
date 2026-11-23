# Take two of the demo: faster, with the emphasis on DAG control, the value
# panel and debugging. The page is recorded bare; every overlay (cursor,
# clicks, captions, chapter cards, the code card) and the camera moves are
# logged here as events, with their times, and drawn by assemble.py.
#
# Uses the installed targetsboard: R CMD INSTALL . first. make-demo-v2.py
# runs it from the package folder, with DEMO_DIR and DEMO_OUT set.
library(shinytest2)
DEMO <- Sys.getenv("DEMO_DIR", "demo")
OUT <- Sys.getenv("DEMO_OUT", file.path(DEMO, "v2", "build"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
source(file.path(DEMO, "pipeline.R"))
SCALE <- 2 # device pixels per css pixel: frames are 2560 x 1440, sharp under the camera's zooms
# The screencast ignores emulated pixel ratios: Chrome has to start at 2x.
chromote::set_chrome_args(c(chromote::default_chrome_args(), sprintf("--force-device-scale-factor=%d", SCALE)))

dir <- file.path(OUT, "project")
unlink(dir, recursive = TRUE); dir.create(dir)
write_demo(dir)
writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))
app <- AppDriver$new(dir, name = "demo-v2", width = 1280, height = 720, load_timeout = 60000)
app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 8", timeout = 30000)
s <- app$get_chromote_session()
Sys.sleep(1.5)
app$run_js("document.querySelector('.react-flow__controls-fitview').click()")
Sys.sleep(1.5)

# ---- Events ------------------------------------------------------------------
events <- list()
ev <- function(type, ...) events[[length(events) + 1]] <<- list(type = type, t = as.numeric(Sys.time()), ...)
cursor <- list() # the cursor track: t, x, y
track <- function(p) cursor[[length(cursor) + 1]] <<- c(as.numeric(Sys.time()), p)

# ---- Recording -----------------------------------------------------------------
frames_dir <- file.path(OUT, "frames")
unlink(frames_dir, recursive = TRUE); dir.create(frames_dir)
rec <- new.env(); rec$n <- 0; rec$times <- numeric()
stop_frames <- s$Page$screencastFrame(callback_ = function(e) {
  rec$n <- rec$n + 1
  writeBin(jsonlite::base64_dec(e$data), file.path(frames_dir, sprintf("%06d.jpg", rec$n)))
  rec$times[rec$n] <- e$metadata$timestamp
  s$Page$screencastFrameAck(sessionId = e$sessionId, wait_ = FALSE)
})
hold <- function(sec) {
  end <- Sys.time() + sec
  while (Sys.time() < end) { s$Runtime$evaluate("1"); Sys.sleep(0.015) }
}

# ---- Driving ---------------------------------------------------------------------
pos <- c(1100, 640); buttons <- 0
js <- function(code) s$Runtime$evaluate(code, returnByValue = TRUE)$result$value
center <- function(sel) unlist(js(sprintf(
  "(() => { const r = document.querySelector(\"%s\").getBoundingClientRect(); return [r.x + r.width/2, r.y + r.height/2]; })()", sel)))
rect <- function(sel) unlist(js(sprintf(
  "(() => { const r = document.querySelector(\"%s\").getBoundingClientRect(); return [r.x, r.y, r.width, r.height]; })()", sel)))
node <- function(id) sprintf(".react-flow__node[data-id='%s']", id)
has <- function(sel) sprintf("document.querySelector(\"%s\") !== null", sel)
# The preview has rendered: `what` is in it, and nothing is recalculating.
preview_shows <- function(what, timeout = 15) {
  until(sprintf("(() => { const p = document.getElementById('value_preview_sidebar'); return !!p && !p.querySelector('.recalculating') && %s; })()", what), timeout)
}
mouse <- function(type, x, y, button = "none", clickCount = 0, modifiers = 0) {
  s$Input$dispatchMouseEvent(type = type, x = x, y = y, button = button, buttons = buttons,
                             clickCount = clickCount, modifiers = modifiers)
}
move <- function(to, dur = 0.45) {
  from <- pos; steps <- max(1, round(dur / 0.03))
  for (i in seq_len(steps)) {
    f <- i / steps; f <- f * f * (3 - 2 * f)
    p <- from + (to - from) * f
    mouse("mouseMoved", p[1], p[2]); track(p)
    Sys.sleep(0.01)
  }
  pos <<- to
}
click <- function(sel = NULL, at = NULL, button = "left", double = FALSE, modifiers = 0, dur = 0.45) {
  if (!is.null(sel)) at <- center(sel)
  if (!is.null(at)) move(at, dur)
  hold(0.08); ev("click", x = pos[1], y = pos[2])
  b <- if (button == "left") 1 else 2
  buttons <<- b; mouse("mousePressed", pos[1], pos[2], button, 1, modifiers)
  buttons <<- 0; mouse("mouseReleased", pos[1], pos[2], button, 1, modifiers)
  if (double) {
    buttons <<- b; mouse("mousePressed", pos[1], pos[2], button, 2, modifiers)
    buttons <<- 0; mouse("mouseReleased", pos[1], pos[2], button, 2, modifiers)
  }
}
key <- function(type, key, code, vk, modifiers = 0) {
  s$Input$dispatchKeyEvent(type = type, key = key, code = code, windowsVirtualKeyCode = vk, modifiers = modifiers)
}
escape <- function() { key("keyDown", "Escape", "Escape", 27); key("keyUp", "Escape", "Escape", 27) }
until <- function(cond, timeout = 60) {
  end <- Sys.time() + timeout
  while (!isTRUE(js(cond)) && Sys.time() < end) hold(0.08)
  if (!isTRUE(js(cond))) message("timed out: ", cond)
}
caption <- function(text, kicker = "") ev("caption", text = text, kicker = kicker)
chapter <- function(title, color) ev("chapter", title = title, color = color)
# The camera: centre (css px) and zoom, eased over `dur` seconds.
cam <- function(x = 640, y = 360, zoom = 1, dur = 0.7) ev("camera", x = x, y = y, zoom = zoom, dur = dur)
cam_on <- function(sel, zoom, dur = 0.7, dx = 0, dy = 0) { p <- center(sel); cam(p[1] + dx, p[2] + dy, zoom, dur) }
cam_rect <- function(r, zoom, dur = 0.7) cam(r[1] + r[3] / 2, r[2] + r[4] / 2, zoom, dur)
speed <- function(f) ev("speed", factor = f)
fit <- function() { click(".react-flow__controls-fitview", dur = 0.35); hold(0.6) }
toggle <- function(left) {
  p <- unlist(js(sprintf("(() => { const b = Array.from(document.querySelectorAll('button.collapse-toggle')).find(b => b.getBoundingClientRect().x %s 400); const r = b.getBoundingClientRect(); return [r.x + r.width/2, r.y + r.height/2]; })()",
                         if (left) "<" else ">")))
  click(at = p)
}
dismiss <- function() js("document.querySelectorAll('.shiny-notification-close').forEach(b => b.click())")

# ---- Scenes ------------------------------------------------------------------------
s$Page$startScreencast(format = "jpeg", quality = 90, maxWidth = 1280 * SCALE, maxHeight = 720 * SCALE, everyNthFrame = 1)
track(pos)
hold(0.4)

# Cold open: straight into a run.
caption("Your <targets> pipeline. Live.")
hold(1.1)
click("#run_all", dur = 0.6)
hold(0.6)
cam(760, 330, 1.35, 0.9)
caption("Every target, as it builds")
until(has(".react-flow__node-running[data-id='clean']"), 15)
speed(1.8)
until(has(".branch-progress"), 20)
speed(1)
cam_on(node("ozone_model"), 2.1, 0.8, dy = -6)
caption("Branches count up, one by one")
hold(2.6)
speed(2.2)
until(has(".react-flow__node-errored[data-id='forecast']"), 40)
speed(1)
cam(760, 330, 1.35, 0.7)
caption("Warnings and errors flag themselves")
until("!document.querySelector('.react-flow__node-running')", 30)
hold(1.4)
dismiss()
cam(640, 360, 1, 0.6)

# Control the DAG
chapter("Control the DAG", "#78B7C5")
hold(1.3)
caption("Tidy it. Any direction.")
click(".tb-layout-buttons button:nth-child(1)")
hold(1.3)
click(".tb-layout-buttons button:nth-child(2)")
hold(1.1)
fit()
caption("Right-click: select what a target depends on")
click(node("ozone_model"), button = "right")
until(has(".tb-node-menu"), 5)
cam_rect(rect(".tb-node-menu"), 1.9, 0.6)
hold(0.9)
click(".tb-node-menu [data-action='select_upstream']", dur = 0.4)
cam(700, 330, 1.3, 0.6)
hold(1.6)
escape()
caption("\u2026or everything downstream")
click(node("clean"), button = "right")
until(has(".tb-node-menu"), 5)
hold(0.5)
click(".tb-node-menu [data-action='select_downstream']", dur = 0.4)
hold(1.8)
escape()
caption("Ctrl-click a group, act on all of it")
move(center(node("effects_plot")))
key("keyDown", "Control", "ControlLeft", 17, 2); hold(0.2)
click(node("effects_plot"), modifiers = 2)
hold(0.3)
click(node("report"), modifiers = 2)
key("keyUp", "Control", "ControlLeft", 17)
until(has(".tb-group-toolbar"), 5)
cam_rect(rect(".tb-group-toolbar"), 2, 0.6)
hold(0.8)
click(".tb-group-toolbar [data-action='invalidate']", dur = 0.4)
until(has("#invalidate_confirm"), 5)
cam(640, 360, 1, 0.5)
hold(0.7)
click("#invalidate_confirm", dur = 0.45)
until("!document.querySelector('.modal.show')", 5)
cam(780, 330, 1.35, 0.6)
hold(0.7)
caption("Invalidate two, run two")
click(".tb-group-toolbar [data-action='run']", dur = 0.4)
cam(780, 330, 1.35, 0.7)
until(has(".react-flow__node-running"), 15)
until("!document.querySelector('.react-flow__node-running')", 30)
hold(0.8)
escape(); dismiss()
cam(640, 360, 1, 0.6)

# Values
chapter("Peek at every value", "#354823")
hold(1.3)
toggle(left = TRUE) # fold the control panel: room for the preview
hold(0.5)
caption("Data frames: a skimr summary")
click(node("clean"), double = TRUE)
preview_shows("p.innerText.includes('Data Summary')")
fit()
js("document.getElementById('value_preview_content').scrollIntoView({behavior: 'smooth', block: 'start'})")
cam(1010, 360, 1.55, 0.8)
hold(2.4)
caption("ggplots: plots")
cam(640, 360, 1, 0.5)
click(node("ozone_plot"))
preview_shows("!!p.querySelector('#value_preview_content img')")
hold(0.3)
cam(1010, 330, 1.5, 0.7)
hold(1.9)
click(node("effects_plot"))
hold(0.4)
preview_shows("!!p.querySelector('#value_preview_content img')")
hold(1.8)
caption("Reports: right in the board")
click(node("report"))
preview_shows("!!p.querySelector('iframe')")
hold(0.8)
cam(1010, 330, 1.55, 0.7)
hold(2.2)
cam(640, 360, 1, 0.6)

# Debugging
chapter("Debug, fast", "#C93312")
hold(1.3)
caption("Every problem, in one list")
click("a[data-value='problems']")
cam(420, 150, 1.7, 0.7)
hold(2.2)
cam(640, 360, 1, 0.5)
click("a[data-value='board']", dur = 0.4)
hold(0.6)
caption("Double-click the error")
click(node("forecast"), double = TRUE)
hold(0.3)
speed(3)
preview_shows("p.innerText.includes('Traceback')")
speed(1)
caption("The error, its traceback through your code\u2026")
fit()
p <- rect("#value_preview_sidebar")
cam(p[1] + p[3] / 2, 150, 2, 0.8)
hold(2.4)
caption("\u2026and code to reproduce it. One click to copy.")
cam(p[1] + p[3] / 2, 250, 2, 0.6)
copy <- "#value_preview_sidebar .tb-copy"
click(copy, dur = 0.5)
hold(1.6)
cam(640, 360, 1, 0.6)
hold(0.7)
caption("Fix it\u2026")
ev("code", on = TRUE,
   old = "predict(model, newdata = data.frame(Temp = 90, wind = 8))",
   new = "predict(model, newdata = data.frame(Temp = 90, Wind = 8))")
hold(1.8)
write_demo(dir, fixed = TRUE)
hold(0.6)
ev("code", on = FALSE)
caption("\u2026rebuild just that target")
hold(1.6)
click(node("forecast"))
until(has(".tb-node-toolbar [data-action='run']"), 5)
cam_on(node("forecast"), 1.9, 0.6, dy = -14)
click(".tb-node-toolbar [data-action='run']", dur = 0.4)
hold(0.8)
speed(2.5)
until(has(".react-flow__node-uptodate[data-id='forecast']"), 40)
speed(1)
hold(0.6)
dismiss()
cam(1010, 200, 1.6, 0.7)
caption("Fixed.")
hold(1.8)
cam(640, 360, 1, 0.6)

# Run modes, then out
toggle(left = TRUE)
hold(0.5)
caption("Debug runs: keep going after errors, stop at warnings")
cam(160, 400, 1.8, 0.7)
click("#keep_going", dur = 0.45)
hold(0.6)
click("#stop_at_warnings", dur = 0.45)
hold(1.6)
click("#keep_going", dur = 0.35)
click("#stop_at_warnings", dur = 0.35)
cam(640, 360, 1, 0.6)
caption("Light or dark")
click("#dark_mode", dur = 0.6)
hold(1.8)
caption("")
hold(0.8)

s$Page$stopScreencast()
hold(0.3)
stop_frames()
write.csv(data.frame(n = seq_len(rec$n), t = rec$times), file.path(OUT, "frames.csv"), row.names = FALSE)
jsonlite::write_json(list(scale = SCALE, events = events,
                          cursor = do.call(rbind, cursor)), file.path(OUT, "events.json"),
                     auto_unbox = TRUE, digits = NA)
cat("frames", rec$n, "events", length(events), "\n")
app$stop()
