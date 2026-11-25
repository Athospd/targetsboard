# Renders the brand's PNGs from the SVGs make_logo.py writes, with headless
# Chrome, on a transparent background; the sticker at twice its size, as
# targetfy's. From the package folder:
#   Rscript dev/logo/render_logo.R
render <- function(svg, png, width, height, scale = 1) {
  b <- chromote::ChromoteSession$new(width = width, height = height)
  on.exit(b$close())
  b$Emulation$setDefaultBackgroundColorOverride(color = list(r = 0, g = 0, b = 0, a = 0))
  # The SVG at the page's size, at its top left.
  page <- tempfile(fileext = ".html")
  writeLines(sprintf(
    '<html><body style="margin:0"><img src="file:///%s" style="display:block;width:%dpx;height:%dpx"></body></html>',
    normalizePath(svg, winslash = "/"), width, height
  ), page)
  b$Page$navigate(paste0("file:///", normalizePath(page, winslash = "/")))
  Sys.sleep(1.5)
  shot <- b$Page$captureScreenshot(
    format = "png",
    clip = list(x = 0, y = 0, width = width, height = height, scale = scale)
  )
  writeBin(jsonlite::base64_dec(shot$data), png)
  message("wrote ", png)
}

render("man/figures/logo.svg", "man/figures/logo.png", 518, 600, scale = 2) # as targetfy's
render("inst/app/www/favicon.svg", "inst/app/www/favicon.png", 64, 64)
render("dev/logo/mark-ink.svg", "dev/logo/mark-512.png", 256, 256, scale = 2)
render("dev/logo/wordmark-ink.svg", "dev/logo/wordmark-ink.png", 330, 64, scale = 3)
