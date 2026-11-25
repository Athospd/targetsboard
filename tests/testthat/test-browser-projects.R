# The Project field in the browser: shown left of the script bar with
# projects, hidden without, and switching the board's graph.

test_that("a plain pipeline has no Project field", {
  dir <- local_pipeline()
  app <- local_board_app(dir, "no_projects")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 0")
  app$wait_for_js("Shiny.shinyapp.$values['board-script-show_projects'] !== undefined")
  expect_null(app$get_js("document.querySelector('#board-script-project').offsetParent")) # hidden
})

test_that("the Project field switches the board to another project's pipeline", {
  dir <- local_projects()
  app <- local_board_app(dir, "projects")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length > 0")
  app$wait_for_js("document.querySelector('#board-script-project').offsetParent !== null") # shown
  # Left of the script bar, on the same line, on main.
  expect_true(app$get_js(paste(
    "(() => { const p = document.querySelector('#board-script-project').getBoundingClientRect();",
    "const s = document.querySelector('.tb-address').getBoundingClientRect();",
    "return p.right <= s.left && Math.abs((p.top + p.bottom) - (s.top + s.bottom)) < 4; })()"
  )))
  expect_equal(app$get_js("document.querySelector('#board-script-project').value"), "main")

  app$set_inputs(`board-script-project` = "b")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='b2']\") !== null", timeout = 30000)
  expect_equal(app$get_js("document.querySelector('#board-script-path').value"), "b.R")
  expect_equal(app$get_js("document.querySelector('#board-script-project').value"), "b")
})

test_that("the Project field waits for a run to end", {
  dir <- local_projects()
  writeLines(c("library(targets)", "list(tar_target(slow, Sys.sleep(4)))"), "_targets.R")
  app <- local_board_app(dir, "projects_run")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='slow']\") !== null")
  app$click(selector = "#board-run-all")
  app$wait_for_js("document.querySelector('#board-script-project').disabled", timeout = 30000)
  expect_equal(app$get_js("document.querySelector('.tb-project').title"), "Switch project after the run")
  app$wait_for_js("!document.querySelector('#board-script-project').disabled", timeout = 60000)
  expect_equal(app$get_js("document.querySelector('.tb-project').title"), "")
})
