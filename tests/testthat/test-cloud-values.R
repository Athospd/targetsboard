# Values stored in the cloud (#131): the Value pane says so and downloads
# only when asked, never over a project file.

# A local pipeline whose metadata then says its targets are in a gcp bucket:
# what the board sees of a cloud pipeline, without one.
local_cloud_store <- function(.env = parent.frame()) {
  dir <- local_pipeline(.env = .env)
  writeLines(c(
    "library(targets)",
    "list(tar_target(a, 1:3), tar_target(f, {writeLines('x', 'f.txt'); 'f.txt'}, format = 'file'))"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, {
    targets::tar_make(reporter = "silent")
    meta <- readLines("_targets/meta/meta")
    writeLines(sub("|local|", "|gcp|", meta, fixed = TRUE), "_targets/meta/meta")
  })
  dir
}

test_that("preview_target() knows where a value is stored, and the gate says so", {
  dir <- local_cloud_store()
  withr::local_dir(dir)
  a <- preview_target("a", "_targets")
  expect_equal(a$repository, "gcp")
  expect_true(preview_in_cloud(a))
  html <- as.character(preview_gate(a, ns = shiny::NS("p")))
  expect_match(html, "stored in the cloud (gcp)", fixed = TRUE)
  expect_match(html, "Download and preview", fixed = TRUE)
  expect_match(html, "temporary file", fixed = TRUE)
  # A cloud file: tar_read() would write it over its path, so no button.
  f <- preview_target("f", "_targets")
  file_gate <- as.character(preview_gate(f, ns = shiny::NS("p")))
  expect_no_match(file_gate, "Download and preview", fixed = TRUE)
  expect_match(file_gate, "f.txt", fixed = TRUE)
  expect_match(file_gate, "targets::tar_read(f)", fixed = TRUE)
  expect_false(preview_in_cloud(list(name = "x", repository = "local")))
})

test_that("on the board, a cloud value waits for the button, and a failed download says why", {
  dir <- local_cloud_store()
  app <- local_board_app(dir, "cloud-values")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='a']\") !== null", timeout = 30000)
  mouse_event(app, "a", "dblclick")
  app$wait_for_js("document.querySelector('.tb-preview-gate') !== null", timeout = preview_timeout)
  expect_match(app$get_text(".tb-preview-gate"), "stored in the cloud (gcp)", fixed = TRUE)
  app$click(selector = ".tb-preview-gate button")
  app$wait_for_js("document.querySelector('.tb-preview-download-error') !== null", timeout = preview_timeout)
  expect_match(app$get_text(".tb-preview-download-error"), "Couldn't download the value", fixed = TRUE)
})
