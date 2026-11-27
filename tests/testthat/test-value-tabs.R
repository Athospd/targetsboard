# Command and Settings as tabs of the value's card; the pane's spacing (#217).

test_that("value_tabs() adds Command and Settings after the value's views, and selects the tab asked", {
  html <- as.character(value_tabs(list(bslib::nav_panel("Preview", "p"), bslib::nav_panel("Printed", "x"))))
  titles <- regmatches(html, gregexpr("data-value=\"[A-Za-z]+\"", html))[[1]]
  expect_equal(unique(titles), sprintf("data-value=\"%s\"", c("Preview", "Printed", "Command", "Settings")))
  expect_match(html, "id=\"command\"", fixed = TRUE)
  expect_match(html, "id=\"settings\"", fixed = TRUE)

  seen <- NULL
  html <- as.character(value_tabs(list(bslib::nav_panel("Preview", "p")), tabs_for = function(titles) {
    seen <<- titles
    "Settings"
  }))
  expect_equal(seen, c("Preview", "Command", "Settings"))
  expect_match(html, "<div class=\"tab-pane active\" data-value=\"Settings\"", fixed = TRUE)
  # A tab the row doesn't have: its first.
  html <- as.character(value_tabs(list(bslib::nav_panel("Preview", "p")), tabs_for = function(titles) "Printed"))
  expect_match(html, "<div class=\"tab-pane active\" data-value=\"Preview\"", fixed = TRUE)

  card <- as.character(target_tabs_card())
  expect_match(card, "tb-target-tabs", fixed = TRUE)
  expect_no_match(card, "data-value=\"Preview\"", fixed = TRUE)
})

test_that("Command and Settings are tabs for every target, and the picked tab is kept across targets", {
  dir <- local_pipeline(make = TRUE) # a, b, c built
  writeLines(c(
    "library(targets)",
    "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c + 1))"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "value-tabs")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='d']\") !== null", timeout = 30000)
  mouse_event(app, "b", "dblclick")
  app$wait_for_js("document.querySelector(\".tb-preview a[data-value='Command']\") !== null", timeout = preview_timeout)
  app$click(selector = ".tb-preview a[data-value='Command']")
  app$wait_for_js("document.querySelector('.tb-code-command .tb-copy')?.dataset.copy === 'a + 1'", timeout = 20000)

  # Another built target opens on Command too.
  mouse_event(app, "c", "dblclick")
  app$wait_for_js("document.querySelector('.tb-code-command .tb-copy')?.dataset.copy === 'b * 2'", timeout = 20000)
  expect_equal(app$get_js("document.querySelector('.tb-preview .nav-link.active').dataset.value"), "Command")

  # Never built: no value, the same tabs, in a card of their own.
  mouse_event(app, "d", "dblclick")
  app$wait_for_js("document.querySelector('.tb-target-tabs') !== null", timeout = 20000)
  app$wait_for_js("document.querySelector('.tb-code-command .tb-copy')?.dataset.copy === 'c + 1'", timeout = 20000)

  # The pane's blocks: no empty output takes a gap.
  empty_shown <- app$get_js(paste(
    "[...document.querySelectorAll('.tb-preview-pane .sidebar-content > .shiny-html-output')]",
    ".filter(el => !el.innerHTML.trim() && getComputedStyle(el).display !== 'none').length"
  ))
  if (empty_shown) message(app$get_js(paste(
    "[...document.querySelectorAll('.tb-preview-pane .sidebar-content > .shiny-html-output')]",
    ".map(el => el.outerHTML.slice(0, 200)).join('\n')"
  )))
  expect_equal(empty_shown, 0)
})

test_that("a fallback tab isn't a pick, and every pick after it is kept (#241)", {
  memory <- tab_memory()
  expect_null(memory$tabs_for(c("Preview", "Printed", "Command", "Settings")))
  memory$picked("Printed")
  expect_equal(memory$tabs_for(c("Preview", "Printed", "Command", "Settings")), "Printed")
  # No Printed here: the card falls back to its first tab, which isn't a pick.
  expect_equal(memory$tabs_for(c("Command", "Settings")), "Printed")
  memory$picked("Command")
  expect_equal(memory$tabs_for(c("Preview", "Printed", "Command", "Settings")), "Printed")
  # The user's pick on that card is.
  memory$tabs_for(c("Command", "Settings"))
  memory$picked("Command")
  memory$picked("Settings")
  expect_equal(memory$tabs_for(c("Preview", "Printed", "Command", "Settings")), "Settings")
})

test_that("on the board, a tab picked on a card that fell back is kept (#241), and Debug stays open on a redraw (#242)", {
  dir <- local_pipeline(make = TRUE) # a, b, c built
  writeLines(c(
    "library(targets)",
    "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c + 1))"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "value-tabs-fallback")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='d']\") !== null", timeout = 30000)
  tab <- function(title) sprintf(".tb-preview a[data-value='%s']", title)
  active <- "document.querySelector('.tb-preview .nav-link.active')?.dataset.value"
  mouse_event(app, "b", "dblclick")
  app$wait_for_js(sprintf("document.querySelector(\"%s\") !== null", tab("Printed")), timeout = preview_timeout)
  app$click(selector = tab("Printed"))
  # d has no value: its card falls back to Command; the user picks Settings.
  mouse_event(app, "d", "dblclick")
  app$wait_for_js("document.querySelector('.tb-target-tabs') !== null", timeout = 20000)
  app$wait_for_js(sprintf("%s === 'Command'", active), timeout = 20000)
  app$click(selector = tab("Settings"))
  app$wait_for_js(sprintf("%s === 'Settings'", active), timeout = 20000)
  mouse_event(app, "c", "dblclick")
  app$wait_for_js("document.querySelector('.tb-preview-pane')?.innerText.includes('tar_read(c)')", timeout = 20000)
  app$wait_for_js(sprintf("%s === 'Settings'", active), timeout = 20000)
  expect_equal(app$get_js(active), "Settings")

  # Debug, opened, stays open while the script changes and the board redraws.
  app$wait_for_js("document.querySelector('details.tb-debug')?.textContent.includes('b * 2')", timeout = 20000)
  app$run_js("document.querySelector('details.tb-debug').open = true")
  writeLines(c(
    "library(targets)",
    "list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2), tar_target(d, c + 1), tar_target(e, 5))"
  ), file.path(dir, "_targets.R"))
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='e']\") !== null", timeout = 30000)
  Sys.sleep(2) # the pane's outputs follow the redraw
  expect_true(app$get_js("document.querySelector('details.tb-debug')?.open === true"))
})
