# In the browser: a store an R session's lock blocks (a debug run left with
# Q, from an older copied snippet). Run says so and offers to unblock (#279).

test_that("Run on a store an R session blocks offers Unblock and run, which runs", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  dir <- local_pipeline()
  # An interactive R session ran the pipeline in itself, and lives on; the
  # snippet of #261 marked its end for the board, but left targets' lock.
  windows <- .Platform$OS.type == "windows"
  input <- tempfile(fileext = ".R")
  writeLines(c(
    "targets::tar_make(callr_function = NULL, reporter = 'silent')",
    "info <- targets::tar_process()",
    "dir.create('_targets/user', showWarnings = FALSE)",
    "writeLines(info$value[match(c('pid', 'created'), info$name)], '_targets/user/targetsboard_ended')",
    "Sys.sleep(600)"
  ), input)
  system2(file.path(R.home("bin"), if (windows) "Rterm.exe" else "R"),
          c(if (windows) "--ess" else "--interactive", "--vanilla", "--quiet"),
          stdin = input, stdout = FALSE, stderr = FALSE, wait = FALSE)
  pid <- function() suppressWarnings(as.integer(tryCatch(targets::tar_process()$value[1], error = function(e) NA)))
  wait_for(function() !is.na(pid()) && pid() != Sys.getpid() && file.exists("_targets/user/targetsboard_ended"), timeout = 60)
  session <- ps::ps_handle(pid())
  withr::defer(tryCatch(ps::ps_kill(session), error = function(e) NULL))
  expect_true(interactive_r(session))
  targets::tar_invalidate(everything())

  app <- local_board_app(dir, "process-lock")
  app$wait_for_js("!document.querySelector('#board-run-all').disabled", timeout = 120000)
  app$click(selector = "#board-run-all")
  app$wait_for_js("document.querySelector('.shiny-notification #board-run-unblock') !== null", timeout = 120000)
  note <- app$get_js("document.querySelector('.shiny-notification #board-run-unblock').closest('.shiny-notification').innerText")
  expect_match(note, "your R session", fixed = TRUE)
  expect_no_match(note, "\033", fixed = TRUE)
  expect_true(file.exists(file.path(dir, "_targets", "meta", "process"))) # not without asking

  app$click(selector = "#board-run-unblock")
  app$wait_for_js("Array.from(document.querySelectorAll('.shiny-notification')).some(n => n.innerText.includes('Run finished.'))",
                  timeout = 120000)
  expect_null(app$get_js("document.querySelector('#board-run-unblock')"))
  expect_equal(withr::with_dir(dir, targets::tar_outdated()), character())
})
