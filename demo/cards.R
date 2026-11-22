# Renders the title and end cards (cards/*.html) to <OUT>/<card>.png, 1920 x 1080.
DEMO <- normalizePath(Sys.getenv("DEMO_DIR", "demo"), winslash = "/")
OUT <- Sys.getenv("DEMO_OUT", file.path(DEMO, "build"))
b <- chromote::ChromoteSession$new(width = 1280, height = 720)
b$Emulation$setDeviceMetricsOverride(width = 1280, height = 720, deviceScaleFactor = 1.5, mobile = FALSE)
for (card in c("title", "end")) {
  b$Page$navigate(paste0("file:///", DEMO, "/cards/", card, ".html")); Sys.sleep(1.5)
  shot <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(shot$data), file.path(OUT, paste0(card, ".png")))
}
b$close()
