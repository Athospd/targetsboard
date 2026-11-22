# Renders the sticker's SVG to a PNG with a transparent background, with
# headless Chrome: Rscript render_logo.R <svg> <png> <scale>
args <- commandArgs(TRUE)
svg <- normalizePath(args[1], winslash = "/")
b <- chromote::ChromoteSession$new(width = 518, height = 600)
b$Emulation$setDefaultBackgroundColorOverride(color = list(r = 0, g = 0, b = 0, a = 0))
b$Page$navigate(paste0("file:///", svg)); Sys.sleep(1.5)
shot <- b$Page$captureScreenshot(format = "png", clip = list(x = 0, y = 0, width = 518, height = 600, scale = as.numeric(args[3])))
writeBin(jsonlite::base64_dec(shot$data), args[2])
b$close()
