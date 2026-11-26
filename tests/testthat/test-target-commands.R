# A target's command, as it runs, in the Value pane (#144).

test_that("target_commands() reads the manifest's commands, or none", {
  manifest <- data.frame(name = c("a", "b", "c"), format = "rds", command = c("1", "f(a)", NA))
  expect_equal(target_commands(manifest), c(a = "1", b = "f(a)"))
  expect_length(target_commands(NULL), 0)
  expect_length(target_commands(data.frame(name = "a")), 0)
})

test_that("command_ui() is a closed Command section with the code to copy, or nothing", {
  html <- as.character(command_ui(c(b = "f(a)")))
  expect_match(html, "<details class=\"tb-command\">", fixed = TRUE)
  expect_match(html, "<summary>Command</summary>", fixed = TRUE)
  expect_match(html, "data-copy=\"f(a)\"", fixed = TRUE)
  expect_match(html, "<pre class=\"language-r\">f(a)</pre>", fixed = TRUE)
  expect_null(command_ui(character()))
  expect_null(command_ui(NA_character_))
  expect_error(preview_server("p", list(), shiny::reactive(NULL), shiny::reactive(0), shiny::reactive(NULL),
                              shiny::reactive(FALSE), shiny::reactive(NULL), shiny::reactive(0), commands = "x"),
               "is.reactive")
})

test_that("the Value pane shows a tar_map() copy's command, as it runs", {
  skip_if_not_installed("tarchetypes")
  dir <- local_pipeline()
  writeLines(c(
    "library(targets)",
    "library(tarchetypes)",
    "method <- function(x, n) x * n",
    "list(tar_map(values = list(n = c(2, 3)), tar_target(fit, method(10, n))))"
  ), file.path(dir, "_targets.R"))
  app <- local_board_app(dir, "command")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='fit_2']\") !== null", timeout = 30000)
  app$click(selector = ".react-flow__node[data-id='fit_3']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js("document.querySelector('.tb-command') !== null", timeout = 20000)
  expect_false(app$get_js("document.querySelector('.tb-command').open")) # closed at first
  expect_equal(app$get_js("document.querySelector('.tb-command .tb-copy').dataset.copy"), "method(10, 3)")
})
