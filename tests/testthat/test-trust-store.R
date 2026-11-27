# A store with custom R code (tar_format(), tar_repository_cas()): targets
# asks before reading it, at a console prompt the browser doesn't see, which
# blocked the board (#237). The board asks instead, and never sets TAR_ASK.

local_custom_store <- function(.env = parent.frame()) {
  local_pipeline(.env = .env)
  writeLines(c(
    "library(targets)",
    "fmt <- tar_format(read = function(path) readRDS(path), write = function(object, path) saveRDS(object, path))",
    "list(tar_target(a, 41, format = fmt), tar_target(b, a + 1))"
  ), "_targets.R")
  targets::tar_make(reporter = "silent")
}

test_that("preview_target() says when reading runs the store's custom code", {
  skip_on_cran()
  local_custom_store()
  expect_true(preview_target("a", "_targets")$custom_code)
  expect_false(preview_target("b", "_targets")$custom_code)
})

test_that("trust is asked unless TAR_ASK is \"false\"", {
  withr::local_envvar(TAR_ASK = NA)
  expect_true(store_needs_trust(TRUE))
  expect_false(store_needs_trust(FALSE))
  withr::local_envvar(TAR_ASK = " FALSE ")
  expect_false(store_needs_trust(TRUE))
  withr::local_envvar(TAR_ASK = "true")
  expect_true(store_needs_trust(TRUE))
})

test_that("the preview asks before running a store's custom code, and reads it once trusted", {
  skip_on_cran()
  local_custom_store()
  withr::local_envvar(TAR_ASK = NA)
  server <- local_app_server()
  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(`board-graph-flow_focused_node` = "a", `board-graph-flow_double_clicked_node` = "a")
    html <- output[["board-preview-content"]]$html
    expect_match(html, "Trust this store and read", fixed = TRUE)
    expect_match(html, "tar_format()", fixed = TRUE)
    expect_match(html, "custom (tar_format()) format", fixed = TRUE)
    expect_no_match(html, "format_custom&", fixed = TRUE)
    expect_no_match(html, "No value stored", fixed = TRUE)

    # A target without custom code reads as ever.
    session$setInputs(`board-graph-flow_focused_node` = "b")
    expect_no_match(output[["board-preview-content"]]$html, "Trust this store", fixed = TRUE)

    session$setInputs(`board-graph-flow_focused_node` = "a")
    session$setInputs(`board-preview-trust` = 1)
    html <- output[["board-preview-content"]]$html
    expect_no_match(html, "Trust this store", fixed = TRUE)
    expect_match(html, "41", fixed = TRUE)
    # The trust is the board's, for its reads: the author's TAR_ASK is untouched.
    expect_identical(Sys.getenv("TAR_ASK", unset = NA), NA_character_)
  })
})
