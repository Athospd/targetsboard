# A rendered website in the value preview, in headless Chrome: its front
# page shows, and a page in a subfolder still loads the site's stylesheet
# from ../site_libs/. The site is laid out as Quarto renders one, so the test
# needs no Quarto. Uses the installed targetsboard.

test_that("a rendered site shows its pages with their styles", {
  dir <- withr::local_tempdir()
  site <- file.path(dir, "_site")
  dir.create(file.path(site, "posts"), recursive = TRUE)
  dir.create(file.path(site, "site_libs"))
  writeLines("body { color: rgb(1, 2, 3); }", file.path(site, "site_libs", "site.css"))
  page <- function(title, css) c(
    "<!DOCTYPE html><html><head>", sprintf("<title>%s</title>", title),
    sprintf('<link rel="stylesheet" href="%s">', css), "</head><body><p>", title, "</p></body></html>"
  )
  writeLines(page("Home", "site_libs/site.css"), file.path(site, "index.html"))
  writeLines(page("First post", "../site_libs/site.css"), file.path(site, "posts", "first.html"))
  writeLines(
    c("library(targets)", "list(tar_target(site, '_site', format = 'file'))"),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir)
  targets::tar_make(reporter = "silent")

  app <- local_board_app(dir, "quarto-site")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length >= 1", timeout = 60000)
  app$click(selector = ".react-flow__node[data-id='site']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")

  frame <- "document.querySelector('iframe.tb-preview-document')"
  color <- paste0("getComputedStyle(", frame, ".contentDocument.body).color")
  loaded <- function(title) {
    sprintf("%s?.contentDocument?.title === '%s' && %s === 'rgb(1, 2, 3)'", frame, title, color)
  }
  app$wait_for_js(loaded("Home"), timeout = preview_timeout)
  expect_equal(app$get_js(paste0(frame, ".getAttribute('allow')")), "fullscreen")

  # The page in a subfolder, as its link would open it.
  app$run_js(paste0(frame, ".contentWindow.location.href = ", frame, ".src.replace(/[^/]*$/, 'posts/first.html')"))
  app$wait_for_js(loaded("First post"), timeout = preview_timeout)
  expect_equal(app$get_js(color), "rgb(1, 2, 3)")
})
