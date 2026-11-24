# The value preview's buttons and links in headless Chrome: what they send
# the server, and what they do in the page. Uses the installed targetsboard.

# A pipeline with a pattern, a file target of 60 files, and a target that
# errors, all built.
local_preview_pipeline <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(
    c(
      "library(targets)",
      "tar_option_set(error = 'continue')",
      "list(",
      "  tar_target(a, 1),",
      "  tar_target(x, 1:4),",
      "  tar_target(squares, x^2, pattern = map(x)),",
      "  tar_target(many_files, {",
      "    dir.create('out', showWarnings = FALSE)",
      "    paths <- file.path('out', sprintf('f%02d.txt', 1:60))",
      "    for (i in seq_along(paths)) writeLines(paste('line', i), paths[[i]])",
      "    paths",
      "  }, format = 'file'),",
      "  tar_target(bad, { a; stop('boom') })",
      ")"
    ),
    file.path(dir, "_targets.R")
  )
  withr::local_dir(dir, .local_envir = .env)
  targets::tar_make(reporter = "silent")
  dir
}

# Opens target `id`'s value preview, with its toolbar's Value.
open_preview <- function(app, id) {
  app$click(selector = sprintf(".react-flow__node[data-id='%s']", id))
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
}

# Replaces the page's clipboard with one that keeps what's copied.
stub_clipboard <- function(app) {
  app$run_js(
    "window.__copied = null;
     Object.defineProperty(navigator, 'clipboard', {configurable: true,
       value: {writeText: t => { window.__copied = t; return Promise.resolve(); }}});"
  )
}

copy_code_js <- "document.querySelector('.tb-preview-header .tb-copy-code').dataset.copy"

test_that("the preview's buttons and links work in the browser", {
  dir <- local_preview_pipeline()
  app <- local_board_app(dir, "preview")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length >= 5", timeout = 60000)
  stub_clipboard(app)

  # The branch picker: next, a number typed in, and all branches.
  open_preview(app, "squares")
  app$wait_for_js("document.querySelector('.tb-branch-number') !== null", timeout = preview_timeout)
  expect_equal(app$get_js(copy_code_js), "targets::tar_read(squares, branches = 1)")
  app$click(selector = "button[aria-label='Next branch']")
  app$wait_for_js(paste0(copy_code_js, " === 'targets::tar_read(squares, branches = 2)'"), timeout = preview_timeout)
  app$run_js(
    "const n = document.querySelector('.tb-branch-number'); n.value = '4';
     n.dispatchEvent(new Event('change', {bubbles: true}));"
  )
  app$wait_for_js(paste0(copy_code_js, " === 'targets::tar_read(squares, branches = 4)'"), timeout = preview_timeout)
  # Out of range: nothing changes.
  app$run_js(
    "const n = document.querySelector('.tb-branch-number'); n.value = '9';
     n.dispatchEvent(new Event('change', {bubbles: true}));"
  )
  Sys.sleep(1)
  expect_equal(app$get_js(copy_code_js), "targets::tar_read(squares, branches = 4)")
  app$click(selector = ".tb-branch-all")
  app$wait_for_js(paste0(copy_code_js, " === 'targets::tar_read(squares)'"), timeout = preview_timeout)

  # The value's tar_read(), copied, with a check for a moment.
  app$click(selector = ".tb-preview-header .tb-copy-code")
  app$wait_for_js("window.__copied === 'targets::tar_read(squares)'")
  app$wait_for_js("document.querySelector('.tb-preview-header .tb-copy-code').classList.contains('tb-copied')")
  app$wait_for_js("!document.querySelector('.tb-preview-header .tb-copy-code').classList.contains('tb-copied')")

  # File sections: the second one read when opened, and 50 more shown.
  open_preview(app, "many_files")
  sections <- "document.querySelectorAll('.tb-preview-file')"
  app$wait_for_js(paste0(sections, ".length === 60"), timeout = preview_timeout)
  visible <- paste0("Array.from(", sections, ").filter(d => !d.hidden).length")
  expect_equal(app$get_js(visible), 50)
  app$run_js(paste0(sections, "[1].open = true;"))
  app$wait_for_js(paste0(sections, "[1].querySelector('.tb-preview-file-body pre')?.textContent.includes('line 2')"), timeout = preview_timeout)
  app$click(selector = ".tb-preview-files-more")
  app$wait_for_js(paste0(visible, " === 60"))
  expect_true(app$get_js("document.querySelector('.tb-preview-files-more') === null"))

  # A problem: its code copied, and its input's link shows that target.
  open_preview(app, "bad")
  app$wait_for_js("document.querySelector('.tb-problem .tb-input-link') !== null", timeout = preview_timeout)
  app$click(selector = ".tb-problem .tb-copy")
  app$wait_for_js("window.__copied !== null && window.__copied.includes('targets::tar_')")
  app$wait_for_js("document.querySelector('.tb-problem .tb-copy').textContent === 'Copied'")
  app$wait_for_js("document.querySelector('.tb-problem .tb-copy').textContent === 'Copy'")
  app$click(selector = ".tb-problem .tb-input-link[data-target='a']")
  app$wait_for_js(paste0(node_js("a"), ".classList.contains('tb-focused')"))

  # A run that errors: its notification's link shows the problem.
  app$click(input = "board-run-all")
  app$wait_for_js("document.querySelector('.shiny-notification .tb-show-problem') !== null", timeout = 120000)
  app$click(selector = ".shiny-notification .tb-show-problem")
  app$wait_for_js(paste0(node_js("bad"), ".classList.contains('tb-focused')"))
  app$wait_for_js("document.querySelector('.tb-problem[data-target=\"bad\"]') !== null", timeout = preview_timeout)
})
