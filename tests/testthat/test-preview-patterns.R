# A pipeline with a pattern of 4 branches, built.
local_pattern_pipeline <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(i, 1:4),",
    "  tar_target(squares, data.frame(i = i, square = i^2), pattern = map(i)),",
    "  tar_target(plain, 1)",
    ")"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir, .local_envir = .env)
  targets::tar_make(reporter = "silent")
  dir
}

test_that("a pattern previews one branch at a time, or all of them", {
  skip_on_cran()
  local_pattern_pipeline()
  first <- preview_target("squares", "_targets")
  expect_equal(first$pattern[c("branches", "branch")], list(branches = 4L, branch = 1L))
  expect_match(first$pattern$branch_name, "^squares_")
  expect_lt(first$bytes, first$pattern$total_bytes) # the branch's own size
  expect_equal(read_preview_value(first, "_targets"), data.frame(i = 1L, square = 1))

  third <- preview_target("squares", "_targets", branch = 3)
  expect_equal(read_preview_value(third, "_targets")$i, 3L)
  expect_false(identical(preview_key(first), preview_key(third)))
  expect_equal(read_code("squares", NULL, branch_arg(third)), "targets::tar_read(squares, branches = 3)")

  all <- preview_target("squares", "_targets", branch = 0)
  expect_equal(nrow(read_preview_value(all, "_targets")), 4)
  expect_null(branch_arg(all))
  expect_equal(preview_target("squares", "_targets", branch = 9)$pattern$branch, 1L) # out of range: the first
  expect_null(preview_target("plain", "_targets")$pattern) # not a pattern
})

test_that("the branch picker steps through branches, and offers all while small", {
  pattern <- list(branches = 20L, branch = 1L, branch_name = "boot_abc", total_bytes = 1000)
  html <- as.character(branch_picker(pattern, limit = 1e6))
  expect_match(html, 'aria-label="Previous branch" disabled', fixed = TRUE)
  expect_match(html, "Shiny.setInputValue(&#39;value_preview_branch&#39;, 2,", fixed = TRUE)
  # Previous, at the first branch, stays on it: 0 would ask for all.
  expect_match(html, 'aria-label="Previous branch" disabled onclick="[^"]*branch&#39;, 1,')
  # The steps are Font Awesome's chevrons, from shiny::icon().
  expect_match(html, 'aria-label="Previous branch"[^>]*>[[:space:]]*<i class="[^"]*fa-chevron-left"')
  expect_match(html, 'aria-label="Next branch"[^>]*>[[:space:]]*<i class="[^"]*fa-chevron-right"')
  expect_match(html, 'max="20" value="1"', fixed = TRUE)
  expect_match(html, "of 20", fixed = TRUE)
  expect_match(html, "All branches</button>", fixed = TRUE)
  expect_match(html, '<code class="tb-branch-name">boot_abc</code>', fixed = TRUE)

  last <- as.character(branch_picker(modifyList(pattern, list(branch = 20L)), limit = 1e6))
  expect_match(last, 'aria-label="Next branch" disabled', fixed = TRUE)
  expect_match(as.character(branch_picker(pattern, limit = 10)), "All branches: too large", fixed = TRUE)
  all <- as.character(branch_picker(modifyList(pattern, list(branch = 0L)), limit = 1e6))
  expect_match(all, "All 20 branches", fixed = TRUE)
  expect_match(all, "One branch", fixed = TRUE)
})

test_that("the board's preview of a pattern follows its picker, and opens on branch 1", {
  skip_on_cran()
  local_pattern_pipeline()
  server <- local_app_server()
  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(flow_focused_node = "squares", flow_double_clicked_node = "squares")
    html <- output$value_preview$html
    expect_match(html, 'max="4" value="1"', fixed = TRUE)
    expect_match(html, 'data-copy="targets::tar_read(squares, branches = 1)"', fixed = TRUE)

    session$setInputs(value_preview_branch = 3)
    expect_match(output$value_preview$html, 'data-copy="targets::tar_read(squares, branches = 3)"', fixed = TRUE)
    session$setInputs(value_preview_branch = 0)
    html <- output$value_preview$html
    expect_match(html, "All 4 branches", fixed = TRUE)
    expect_match(html, 'data-copy="targets::tar_read(squares)"', fixed = TRUE)

    # Another target, then back: branch 1 again.
    session$setInputs(flow_focused_node = "plain")
    session$setInputs(flow_focused_node = "squares")
    expect_match(output$value_preview$html, 'max="4" value="1"', fixed = TRUE)
  })
})
