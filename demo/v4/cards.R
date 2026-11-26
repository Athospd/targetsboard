# Renders take four's cards (cards/*.html, in the Pulse brand) to
# <OUT>/<card>.png: the wordmark is inlined (it draws in currentColor), the
# logo and the board's still are linked. 16:9 cards at 1920 x 1080, the
# vertical ones at 1080 x 1920, the thumbnail at 1280 x 720.
DEMO <- normalizePath(Sys.getenv("DEMO_DIR", "demo"), winslash = "/")
OUT <- normalizePath(Sys.getenv("DEMO_OUT", file.path(DEMO, "v4", "build")), winslash = "/")
ROOT <- dirname(DEMO)
cards <- file.path(DEMO, "v4", "cards")
wordmark <- paste(readLines(file.path(ROOT, "inst", "app", "www", "wordmark.svg"), warn = FALSE), collapse = "\n")
logo <- paste0("file:///", file.path(ROOT, "man", "figures", "logo.svg"))
board <- paste0("file:///", file.path(OUT, "thumb-board.jpg"))

sizes <- list(
  tagline = c(1280, 720, 1.5), end = c(1280, 720, 1.5), thumbnail = c(1280, 720, 1),
  `tagline-v` = c(720, 1280, 1.5), `end-v` = c(720, 1280, 1.5), `frame-v` = c(720, 1280, 1.5)
)
b <- chromote::ChromoteSession$new()
for (card in names(sizes)) {
  if (card == "thumbnail" && !file.exists(file.path(OUT, "thumb-board.jpg"))) next
  size <- sizes[[card]]
  html <- paste(readLines(file.path(cards, paste0(card, ".html")), warn = FALSE), collapse = "\n")
  html <- gsub("{{wordmark}}", wordmark, html, fixed = TRUE)
  html <- gsub("{{logo}}", logo, html, fixed = TRUE)
  html <- gsub("{{board}}", board, html, fixed = TRUE)
  page <- file.path(cards, paste0(".render-", card, ".html")) # beside brand.css
  writeLines(html, page)
  b$Emulation$setDeviceMetricsOverride(width = size[1], height = size[2], deviceScaleFactor = size[3], mobile = FALSE)
  b$Page$navigate(paste0("file:///", page)); Sys.sleep(1.5)
  shot <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(shot$data), file.path(OUT, paste0(card, ".png")))
  unlink(page)
}
b$close()
