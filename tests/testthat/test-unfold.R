# Unfolding a pattern into its branches on the canvas (#147).

fake_meta <- function() {
  data.frame(
    name = c("p", "p_a", "p_b", "p_c", "x"),
    type = c("pattern", "branch", "branch", "branch", "stem"),
    parent = c(NA, "p", "p", "p", NA),
    error = c(NA, NA, "boom", NA, NA)
  ) |> transform(children = I(list(c("p_a", "p_b", "p_c"), NA, NA, NA, NA)))
}

test_that("pattern_branches() lists a pattern's branches, failing first, with their statuses", {
  out <- pattern_branches("p", fake_meta())
  expect_equal(out$pattern, "p")
  expect_equal(vapply(out$branches, `[[`, "", "name"), c("p_b", "p_a", "p_c"))
  expect_equal(vapply(out$branches, `[[`, 1L, "position"), c(2L, 1L, 3L))
  expect_equal(vapply(out$branches, `[[`, "", "status"), c("errored", "uptodate", "uptodate"))
  expect_equal(out$more, 0L)
})

test_that("pattern_branches() takes the live progress over the metadata, and caps the list", {
  progress <- data.frame(
    name = c("p_a", "p_c"), type = "branch", parent = "p", progress = c("dispatched", "errored")
  )
  out <- pattern_branches("p", fake_meta(), progress, cap = 2)
  expect_equal(vapply(out$branches, `[[`, "", "name"), c("p_b", "p_c")) # failing first, then cut
  expect_equal(out$more, 1L)
  running <- pattern_branches("p", fake_meta(), progress)$branches
  expect_equal(running[[3]]$status, "running")
})

test_that("pattern_branches() has none for a pattern not built yet, or not a pattern", {
  expect_length(pattern_branches("q", fake_meta())$branches, 0)
  expect_length(pattern_branches("x", fake_meta())$branches, 0)
  expect_length(pattern_branches("p", NULL)$branches, 0)
})

test_that("a pattern unfolds into its branches, a branch opens its preview, and it folds again", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)", "tar_option_set(error = 'continue')",
    "list(tar_target(x, c(1, 2, 3)), tar_target(p, if (x == 2) stop('two') else x * 10, pattern = map(x)))"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, try(targets::tar_make(reporter = "silent"), silent = TRUE))
  app <- local_board_app(dir, "unfold")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='p']\") !== null", timeout = 30000)
  mouse_event(app, "p", "click")
  app$wait_for_js("document.querySelector(\"button[data-action='unfold']\") !== null", timeout = 10000)
  expect_equal(app$get_text("button[data-action='unfold']"), "Branches")
  app$run_js("document.querySelector(\"button[data-action='unfold']\").click()")
  app$wait_for_js("document.querySelectorAll('.tb-branches .tb-branch').length === 3", timeout = 20000)
  chips <- app$get_js("[...document.querySelectorAll('.tb-branches .tb-branch')].map(b => b.textContent + ':' + b.className)")
  expect_equal(unlist(chips), c(
    "2:tb-branch tb-status-errored", "1:tb-branch tb-status-uptodate", "3:tb-branch tb-status-uptodate"
  ))
  expect_equal(app$get_text("button[data-action='unfold']"), "Fold")
  # A plain target has no Branches button.
  mouse_event(app, "x", "click")
  app$wait_for_js("document.querySelector(\"button[data-action='unfold']\") === null", timeout = 10000)

  # A branch: its pattern's preview, on that branch.
  app$run_js("[...document.querySelectorAll('.tb-branches .tb-branch')].find(b => b.textContent === '3').click()")
  app$wait_for_js("document.querySelector('.tb-branch-number')?.value === '3'", timeout = preview_timeout)
  expect_match(app$get_text(".tb-preview-pane"), "30")

  app$run_js("document.querySelector(\"button[data-action='unfold']\").click()")
  app$wait_for_js("document.querySelector('.tb-branches') === null", timeout = 10000)
})
