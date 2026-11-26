# Driving the board for a demo take, and logging what assemble.py draws in
# post: the cursor, clicks, captions, chapter cards, code and console cards,
# camera moves, speed ramps and cuts. Taken from v3/record.R, for the takes
# after it.
#
# Source it once the take's AppDriver `app` is up, with `s` its chromote
# session and OUT the take's build folder. Then startScreencast(), the
# scenes, and finish_take().
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

# ---- Cuts and framing ------------------------------------------------------------
# A cut: assemble.py drops the time between skip(TRUE) and skip(FALSE), with a
# short dissolve. For waits on the app, which say nothing.
skip <- function(on) ev("skip", on = on)
wait_cut <- function(cond, timeout = 30) { skip(TRUE); until(cond, timeout); hold(0.15); skip(FALSE) }
preview_cut <- function(what, timeout = 30) {
  skip(TRUE); preview_shows(what, timeout); hold(0.25); skip(FALSE)
}
# The box around one or more elements (a CSS selector each), clipped to the
# viewport and to `within`, if given.
box <- function(sels, within = NULL) {
  r <- unlist(js(sprintf(
    "(() => { const sels = %s; let x0 = 1e9, y0 = 1e9, x1 = -1e9, y1 = -1e9;
       for (const s of sels) { const e = document.querySelector(s); if (!e) continue;
         const r = e.getBoundingClientRect(); x0 = Math.min(x0, r.left); y0 = Math.min(y0, r.top);
         x1 = Math.max(x1, r.right); y1 = Math.max(y1, r.bottom); }
       const w = %s; if (w) { const r = document.querySelector(w).getBoundingClientRect();
         x0 = Math.max(x0, r.left); y0 = Math.max(y0, r.top); x1 = Math.min(x1, r.right); y1 = Math.min(y1, r.bottom); }
       x0 = Math.max(x0, 0); y0 = Math.max(y0, 0); x1 = Math.min(x1, 1280); y1 = Math.min(y1, 720);
       return [x0, y0, x1 - x0, y1 - y0]; })()",
    jsonlite::toJSON(sels), if (is.null(within)) "null" else sprintf("\"%s\"", within))))
  r
}
# Frame a box: the largest zoom (up to max_zoom) that shows all of it, with a
# margin, above the caption band at the bottom. max_h keeps only its top.
frame_box <- function(r, pad = 18, max_zoom = 2.2, max_h = Inf, dur = 0.7, caption_band = TRUE) {
  r[4] <- min(r[4], max_h)
  usable <- if (caption_band) 0.80 else 0.96
  z <- min(1280 / (r[3] + 2 * pad), 720 * usable / (r[4] + 2 * pad), max_zoom)
  z <- max(z, 1)
  cx <- r[1] + r[3] / 2
  cy <- r[2] + r[4] / 2 + (360 - 720 * usable / 2) / z
  cam(cx, cy, z, dur)
}
frame_on <- function(sels, ..., within = NULL) frame_box(box(sels, within), ...)

# A card of R console lines or a file's lines (assemble.py's console_card()):
# `lines` as typed; "> " lines are code, "#" lines comments.
console <- function(title, lines) ev("console", on = TRUE, title = title, lines = as.list(lines))
console_off <- function() ev("console", on = FALSE)

# Ends the take: a still page sends no frames, so "end" marks where it ends.
# Writes frames.csv and events.json for assemble.py.
finish_take <- function() {
  ev("end")
  s$Page$stopScreencast()
  hold(0.3)
  stop_frames()
  write.csv(data.frame(n = seq_len(rec$n), t = rec$times), file.path(OUT, "frames.csv"), row.names = FALSE)
  jsonlite::write_json(list(scale = SCALE, events = events,
                            cursor = do.call(rbind, cursor)), file.path(OUT, "events.json"),
                       auto_unbox = TRUE, digits = NA)
  cat("frames", rec$n, "events", length(events), "\n")
}
