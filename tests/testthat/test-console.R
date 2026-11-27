# The Console tab (#185): every board action's R code, in order, to copy one
# by one or as a script.

test_that("run_code() is the tar_make() call a run makes", {
  expect_equal(run_code(), "targets::tar_make()")
  expect_equal(run_code("a"), "targets::tar_make(names = a)")
  expect_equal(run_code(c("a", "my target")), "targets::tar_make(names = c(a, `my target`))")
  expect_equal(
    run_code("a", maker = list(fun = "tar_make_future", workers = 2L), script = "other.R"),
    "targets::tar_make_future(names = a, workers = 2, script = \"other.R\")"
  )
  with_modes <- run_code(modes = list(keep_going = TRUE))
  expect_match(with_modes, "^# .*keep going after errors.*\ntargets::tar_make[(][)]$")
})

test_that("invalidate_code() is the tar_invalidate() call", {
  expect_equal(invalidate_code("a"), "targets::tar_invalidate(names = a)")
  expect_equal(invalidate_code(c("a", "b")), "targets::tar_invalidate(names = c(a, b))")
})

test_that("the console log keeps the actions in order, once each in a row", {
  log <- console_log()
  shiny::isolate({
    log$add("targets::tar_make()")
    log$add("targets::tar_read(a)")
    log$add("targets::tar_read(a)") # the same again: once
    log$add("targets::tar_make()")
    expect_equal(vapply(log$entries(), `[[`, "", "code"), c("targets::tar_make()", "targets::tar_read(a)", "targets::tar_make()"))
  })
})

test_that("console_script() is the session as a script, and the tab copies each and all", {
  entries <- list(
    list(time = as.POSIXct("2026-11-27 10:00:01"), code = "targets::tar_make()"),
    list(time = as.POSIXct("2026-11-27 10:00:09"), code = "targets::tar_read(a)")
  )
  script <- console_script(entries)
  expect_equal(script, "# 10:00:01\ntargets::tar_make()\n\n# 10:00:09\ntargets::tar_read(a)\n")
  html <- as.character(console_entries_ui(entries))
  expect_match(html, "Copy all", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("tb-code-console", html, fixed = TRUE))), 2)
  expect_match(as.character(console_entries_ui(list())), "Nothing yet", fixed = TRUE)
})

test_that("the Console tab lists the board's run and value reads, as R code", {
  dir <- local_pipeline()
  writeLines(c("library(targets)", "list(tar_target(a, 1), tar_target(b, a + 1))"), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "console")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='b']\") !== null", timeout = 30000)
  app$click(input = "board-run-all")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='b'] .base-node.tb-status-uptodate\") !== null", timeout = 90000)
  mouse_event(app, "b", "dblclick")
  app$wait_for_js("document.querySelector('.tb-preview-pane') !== null", timeout = preview_timeout)
  app$run_js("document.querySelector('.nav-link[data-value=\"console\"]').click()")
  app$wait_for_js("document.querySelectorAll('.tb-code-console').length === 2", timeout = 20000)
  codes <- app$get_js("[...document.querySelectorAll('.tb-code-console code, .tb-code-console pre')].map(e => e.textContent.trim()).filter(t => t)")
  expect_true(any(grepl("^targets::tar_make[(][)]$", unlist(codes))))
  expect_true(any(grepl("^targets::tar_read[(]b[)]$", unlist(codes))))
  copy_all <- app$get_js("document.querySelector('.tb-console-copy-all').dataset.copy")
  expect_match(copy_all, "targets::tar_make()\n\n# ", fixed = TRUE)
})
