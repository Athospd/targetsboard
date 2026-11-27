# The Value pane's card is nested in the board's card, and both expand.
# bslib expects one full-screen card at a time (#276).

test_that("the Value pane expands inside the expanded board, and Escape leaves one level at a time", {
  skip_on_cran()
  dir <- local_pipeline(make = TRUE)
  app <- local_board_app(dir, "full-screen")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='c']\") !== null", timeout = 30000)
  app$click(selector = ".react-flow__node[data-id='c']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js("document.querySelector('.tb-preview') !== null", timeout = 30000)

  board <- "document.querySelector('.tb-board-pane').closest('.bslib-card')"
  pane <- "document.querySelector('.tb-preview')"
  enter <- function(card) sprintf("%s.querySelector(':scope > * > .bslib-full-screen-enter')", card)
  full <- function(card) app$get_js(sprintf("%s.dataset.fullScreen === 'true'", card))
  # Where the mouse is: bslib shows a card's Expand on hover.
  hover <- function(card) {
    box <- app$get_js(sprintf("(() => { const r = %s.getBoundingClientRect(); return [r.x + r.width / 2, r.y + r.height / 2]; })()", card))
    app$get_chromote_session()$Input$dispatchMouseEvent(type = "mouseMoved", x = box[[1]], y = box[[2]])
    Sys.sleep(0.3)
  }
  shown <- function(card) app$get_js(sprintf("getComputedStyle(%s).display !== 'none'", enter(card)))

  app$run_js(sprintf("%s.click()", enter(board)))
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'true'", board))
  hover(pane)
  expect_true(shown(pane)) # it was hidden: the bug

  app$run_js(sprintf("%s.click()", enter(pane)))
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'true'", pane))
  expect_true(full(board))

  # One Escape: the pane leaves, the board stays expanded, and so marked.
  app$run_js("document.activeElement.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}))")
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'false'", pane))
  expect_true(full(board))
  expect_true(app$get_js("document.body.classList.contains('bslib-has-full-screen')"))
  hover(pane)
  expect_true(shown(pane)) # and can expand again

  # The next Escape: the board leaves too.
  app$run_js("document.activeElement.dispatchEvent(new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}))")
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'false'", board))
  expect_false(app$get_js("document.body.classList.contains('bslib-has-full-screen')"))
  # Leaving full screen is all it does: the Value pane stays open.
  Sys.sleep(0.5)
  expect_true(app$get_js(sprintf("%s !== null", pane)))

  # The pane's own close, with the board expanded: the board stays.
  app$run_js(sprintf("%s.click()", enter(board)))
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'true'", board))
  app$run_js(sprintf("%s.click()", enter(pane)))
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'true'", pane))
  app$run_js(sprintf("%s.previousElementSibling.querySelector('.bslib-full-screen-exit').click()", pane))
  app$wait_for_js(sprintf("%s.dataset.fullScreen === 'false'", pane))
  expect_true(full(board))
  expect_true(app$get_js("document.body.classList.contains('bslib-has-full-screen')"))
})
