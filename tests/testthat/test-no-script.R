# A folder with no targets script yet (#190): the board says so, links the
# manual, writes nothing, and draws the pipeline once a script exists.

test_that("no_script_ui() names the missing script and links the targets manual", {
  html <- as.character(no_script_ui("C:/project/_targets.R"))
  expect_match(html, "No targets pipeline file detected", fixed = TRUE)
  expect_match(html, "C:/project/_targets.R", fixed = TRUE)
  expect_match(html, "</a>,", fixed = TRUE) # no space before the comma
  expect_match(html, "</code> in the", fixed = TRUE)
  expect_match(html, "href=\"https://books.ropensci.org/targets/walkthrough.html\"", fixed = TRUE)
  expect_match(html, "targets::use_targets()", fixed = TRUE)
  expect_match(html, "target=\"_blank\"", fixed = TRUE)
})

test_that("the board tells of a missing script, and draws the pipeline once it's there", {
  dir <- withr::local_tempdir()
  app <- local_board_app(dir, "no-script")
  app$wait_for_js("document.querySelector('.tb-no-script') !== null", timeout = 30000)
  expect_match(app$get_text(".tb-no-script"), "No targets pipeline file detected")
  expect_false(file.exists(file.path(dir, "_targets.R"))) # wrote nothing
  Sys.sleep(3) # the poller's first try, which fails on the missing script
  # The notice says it: no "doesn't load" notification on top.
  expect_false(app$get_js(
    "[...document.querySelectorAll('.shiny-notification')].some(n => n.textContent.includes(\"doesn't load\"))"
  ))

  writeLines(c("library(targets)", "list(tar_target(a, 1))"), file.path(dir, "_targets.R"))
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='a']\") !== null", timeout = 60000)
  app$wait_for_js("document.querySelector('.tb-no-script') === null", timeout = 10000)
  expect_true(TRUE)
})
