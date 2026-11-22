# Drives the board in Chrome and records it with the DevTools screencast.
# Frames go to <OUT>/frames/<n>.jpg, with their timestamps in <OUT>/frames.csv.
library(shinytest2)
# Run from the package folder; make-demo-v1.py sets both.
DEMO <- Sys.getenv("DEMO_DIR", "demo")
OUT <- Sys.getenv("DEMO_OUT", file.path(DEMO, "v1", "build"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

start_recording <- function(app) {
  s <- app$get_chromote_session()
  unlink(file.path(OUT, "frames"), recursive = TRUE); dir.create(file.path(OUT, "frames"))
  env <- new.env(); env$n <- 0; env$times <- numeric()
  env$stop_cb <- s$Page$screencastFrame(callback_ = function(e) {
    env$n <- env$n + 1
    writeBin(jsonlite::base64_dec(e$data), file.path(OUT, "frames", sprintf("%06d.jpg", env$n)))
    env$times[env$n] <- e$metadata$timestamp
    s$Page$screencastFrameAck(sessionId = e$sessionId, wait_ = FALSE)
  })
  s$Page$startScreencast(format = "jpeg", quality = 92, maxWidth = 1920, maxHeight = 1080, everyNthFrame = 1)
  env$s <- s
  env
}
stop_recording <- function(rec) {
  rec$s$Page$stopScreencast()
  hold(rec$s, 0.3)
  rec$stop_cb()
  write.csv(data.frame(n = seq_len(rec$n), t = rec$times), file.path(OUT, "frames.csv"), row.names = FALSE)
  rec$n
}
# Wait while the screencast keeps flowing: each call runs chromote's loop.
hold <- function(s, sec) {
  end <- Sys.time() + sec
  while (Sys.time() < end) { s$Runtime$evaluate("1"); Sys.sleep(0.015) }
}
