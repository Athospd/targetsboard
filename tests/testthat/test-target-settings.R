# A target's settings in the Value pane (#130).

test_that("setting_defaults are targets' own defaults", {
  defaults <- callr::r(function(fields) {
    vapply(fields, function(f) format(targets::tar_option_get(f)), character(1))
  }, args = list(fields = c("iteration", "memory", "storage", "retrieval", "deployment", "error",
                            "priority", "repository")))
  expect_equal(defaults, setting_defaults[names(defaults)])
})

test_that("target_settings() keeps the manifest's settings as text, or none", {
  manifest <- data.frame(name = c("a", "b"), command = "1", memory = c("auto", "transient"), priority = c(0, 0.5))
  manifest$resources <- list(list(), list())
  settings <- target_settings(manifest)
  expect_named(settings, c("name", "memory", "priority", "resources"))
  expect_equal(settings$priority, c("0", "0.5"))
  expect_equal(settings$resources, c(NA_character_, NA_character_))
  expect_null(target_settings(NULL))
  expect_null(target_settings(data.frame(name = "a", command = "1")))
})

test_that("settings_ui() bolds what differs from the defaults, and notes main storage on a worker", {
  row <- data.frame(name = "a", pattern = NA, iteration = "list", memory = "auto", storage = "main",
                    retrieval = "main", deployment = "worker", resources = "crew (controller big)")
  html <- as.character(settings_ui(row))
  expect_match(html, "<div class=\"tb-settings\">", fixed = TRUE)
  expect_match(html, "<th>memory</th>\n?\\s*<td>\n?\\s*<code>auto</code>")
  expect_match(html, "<strong>\n?\\s*<code>list</code>")
  expect_match(html, "<strong>\n?\\s*<code>crew \\(controller big\\)</code>")
  expect_no_match(html, "<th>pattern</th>", fixed = TRUE)
  expect_match(html, "the main process writes and reads its value (storage and retrieval main)", fixed = TRUE)

  row$deployment <- "main"
  expect_no_match(as.character(settings_ui(row)), "runs on a worker", fixed = TRUE)
  expect_match(as.character(settings_ui(row[0, ])), "aren't known yet", fixed = TRUE)
  expect_match(as.character(settings_ui(NULL)), "aren't known yet", fixed = TRUE)
})

test_that("the Value pane shows a target's settings, from tar_manifest()", {
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(x, 1:3, memory = 'transient', deployment = 'main'),",
    "  tar_target(y, x, pattern = map(x), iteration = 'list')",
    ")"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "settings")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='y']\") !== null", timeout = 30000)
  app$click(selector = ".react-flow__node[data-id='y']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js("document.querySelector(\".tb-preview a[data-value='Settings']\") !== null", timeout = 20000)
  app$click(selector = ".tb-preview a[data-value='Settings']")
  app$wait_for_js("document.querySelector('.tb-settings strong') !== null", timeout = 20000)
  bold <- app$get_js("Array.from(document.querySelectorAll('.tb-settings strong')).map(e => e.textContent).join(',')")
  expect_equal(bold, "map(x),list")
})
