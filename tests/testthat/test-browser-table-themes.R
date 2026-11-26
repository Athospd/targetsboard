# The Problems and Metadata tables follow a theme picked live (#165): a live
# switch (setCurrentTheme()) loaded reactable's stylesheet again, after the
# board's, and its white .Reactable won.

test_that("the Problems and Metadata tables follow a theme switched live, in both modes", {
  dir <- local_pipeline()
  writeLines(c("library(targets)", "list(tar_target(a, 1), tar_target(b, stop('boom')))"), file.path(dir, "_targets.R"))
  withr::with_dir(dir, try(targets::tar_make(reporter = "silent"), silent = TRUE))
  app <- local_board_app(dir, "table-themes")
  app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 2", timeout = 30000)
  # Every opaque background in a table, and the card's around it.
  painted <- function(id) app$get_js(sprintf(
    "(() => { const root = document.getElementById('%s'); const out = [];
      [root, ...root.querySelectorAll('.Reactable, .rt-table, .rt-tbody, .rt-tr-group, .rt-tr, .rt-td')].forEach((e) => {
        const c = getComputedStyle(e).backgroundColor; if (c !== 'rgba(0, 0, 0, 0)' && c !== 'transparent') out.push(c); });
      return out.join(); })()", id))
  card_bg <- function(id) app$get_js(sprintf("getComputedStyle(document.getElementById('%s').closest('.card')).backgroundColor", id))
  for (theme in c("Darkly", "Gruvbox", "Tokyo Night")) {
    app$set_inputs(`theme-pick` = theme, wait_ = FALSE)
    app$wait_for_js(sprintf("document.getElementById('theme-pick').value === '%s'", theme))
    Sys.sleep(1.5) # the new stylesheets load
    for (mode in if (theme == "Darkly") "dark" else c("light", "dark")) {
      app$run_js(sprintf("document.documentElement.setAttribute('data-bs-theme', '%s')", mode))
      for (tab in c("tar_meta", "problems")) {
        app$click(selector = sprintf("#board-view .nav-link[data-value='%s']", tab))
        id <- if (tab == "tar_meta") "board-meta-table" else "board-problems-table"
        app$wait_for_js(sprintf("document.querySelector('#%s .rt-tbody') !== null", id), timeout = 20000)
        info <- paste(theme, mode, tab)
        expect_equal(painted(id), "", info = info) # the card shows through
        if (mode == "dark") expect_false(card_bg(id) == "rgb(255, 255, 255)", info = info)
      }
    }
  }
})
