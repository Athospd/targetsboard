# The theme picker on the real board: a pick restyles the page and its
# canvas, a dark-only theme takes the dark mode switch, and the browser
# remembers the pick.

canvas_js <- "getComputedStyle(document.querySelector('.react-flow')).backgroundColor"
# Rules of ours that win by coming after Bootstrap's: a pick must keep them.
layout_js <- paste(
  "[getComputedStyle(document.body).fontFamily,",
  "getComputedStyle(document.querySelector('.bslib-sidebar-layout > .main')).padding,",
  "getComputedStyle(document.documentElement).getPropertyValue('--bslib-spacer').trim()]"
)
beige <- "rgb(240, 229, 215)"

# As a user picks: the select's value, then its change event.
pick_theme <- function(app, name) {
  app$run_js(sprintf(
    "const s = document.querySelector('#chrome-theme'); s.value = '%s'; s.dispatchEvent(new Event('change', {bubbles: true}));",
    name
  ))
}

test_that("a pick restyles the board, and dark-only themes take the switch", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "picker")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$run_js("localStorage.removeItem('targetsboard.theme')")
  expect_equal(app$get_js(canvas_js), beige)
  layout <- app$get_js(layout_js)

  pick_theme(app, "Pulse")
  app$wait_for_js(sprintf("%s !== '%s'", canvas_js, beige), timeout = 20000)
  expect_equal(app$get_js("localStorage.getItem('targetsboard.theme')"), "Pulse")
  expect_false(app$get_js("document.querySelector('bslib-input-dark-mode').hidden"))

  pick_theme(app, "Darkly")
  app$wait_for_js("document.querySelector('bslib-input-dark-mode').hidden", timeout = 20000)
  app$wait_for_js("document.documentElement.dataset.bsTheme === 'dark'")
  app$wait_for_js("document.querySelector('.reactflow-wrapper').classList.contains('tb-dark')")

  pick_theme(app, "Targets board")
  app$wait_for_js("!document.querySelector('bslib-input-dark-mode').hidden", timeout = 20000)
  app$wait_for_js("document.documentElement.dataset.bsTheme === 'light'") # the mode before Darkly
  app$wait_for_js(sprintf("%s === '%s'", canvas_js, beige), timeout = 20000)
  expect_equal(app$get_js(layout_js), layout) # as on load
})

test_that("the browser remembers the pick, unless the option chose the theme", {
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "picker-remember")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app$run_js("localStorage.setItem('targetsboard.theme', 'Vapor')")
  app$run_js("location.reload()")
  app$wait_for_js("document.querySelector('#chrome-theme')?.value === 'Vapor'", timeout = 20000)
  app$wait_for_js("document.querySelector('bslib-input-dark-mode').hidden", timeout = 20000)
  app$run_js("localStorage.removeItem('targetsboard.theme')")

  dir2 <- local_pipeline(make = TRUE)
  app2 <- local_board_app(dir2, "picker-option", c(
    "options(targetsboard.theme = 'Cosmo')", "targetsboard::tar_board_app()"
  ))
  app2$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3")
  app2$run_js("localStorage.setItem('targetsboard.theme', 'Vapor')")
  app2$run_js("location.reload()")
  Sys.sleep(3)
  app2$wait_for_js("document.querySelector('#chrome-theme')?.value === 'Cosmo'", timeout = 20000)
  expect_false(app2$get_js("document.querySelector('bslib-input-dark-mode').hidden"))
  app2$run_js("localStorage.removeItem('targetsboard.theme')")
})
