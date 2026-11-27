# Each target's kind for its icon (#103): target_kinds() on the manifest.

test_that("target_kinds() names reports and files, and nothing else", {
  manifest <- data.frame(
    name = c("plain", "path", "fast_path", "html", "qmd", "qmd_rep", "rmd_rep"),
    format = c("rds", "file", "file_fast", "file", "file", "file", "file"),
    command = c(
      "f(x)", "\"data.csv\"", "\"a.csv\"",
      "tarchetypes::tar_render_run(path = \"report.Rmd\", args = list())",
      "tarchetypes::tar_quarto_run(args = list(input = \"r.qmd\"))",
      "tarchetypes::tar_quarto_rep_run(args = list())",
      "tarchetypes::tar_render_rep_run(path = \"r.Rmd\")"
    )
  )
  expect_equal(
    target_kinds(manifest),
    c(path = "file", fast_path = "file", html = "rmarkdown", qmd = "quarto", qmd_rep = "quarto", rmd_rep = "rmarkdown")
  )
})

test_that("target_kinds() gives none without a manifest", {
  expect_equal(target_kinds(NULL), stats::setNames(character(), character()))
  expect_length(target_kinds(data.frame(name = character())), 0)
  # A manifest without format or command: no kinds, no error.
  expect_length(target_kinds(data.frame(name = "a")), 0)
})

test_that("target_kinds() reads a real pipeline's manifest", {
  skip_on_cran()
  skip_if_not_installed("tarchetypes")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c("---", "title: r", "---", "Hi"), "report.Rmd")
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(path, 'data.csv', format = 'file'),",
    "  tar_target(x, 1:2),",
    "  tar_target(y, x, pattern = map(x)),",
    "  tarchetypes::tar_render(report, 'report.Rmd')",
    ")"
  ), "_targets.R")
  manifest <- targets::tar_manifest(fields = c("name", "format", "command"))
  expect_equal(target_kinds(manifest)[c("path", "report")], c(path = "file", report = "rmarkdown"))
  expect_false(any(c("x", "y") %in% names(target_kinds(manifest))))
})

test_that("target_kinds() names batched reps, URLs, grouped data and storage formats (#157)", {
  manifest <- data.frame(
    name = c("reps", "reps2", "site", "groups", "fast", "plain", "report_grouped", "path", "rep_url"),
    format = c("rds", "rds", "url", "rds", "qs", "rds", "file", "file", "url"),
    iteration = c("vector", "vector", "vector", "group", "vector", "vector", "group", "vector", "vector"),
    command = c(
      "tarchetypes::tar_rep_run(command = f(), batches = 2)", "tarchetypes::tar_rep2_run(command = g(r))",
      "\"https://example.com\"", "tarchetypes::tar_group_by_run(data = d)", "1", "1",
      "tarchetypes::tar_quarto_run(args = list())", "\"a.csv\"", "tarchetypes::tar_rep_run(command = h())"
    )
  )
  expect_equal(target_kinds(manifest), c(
    reps = "rep", reps2 = "rep", site = "url", groups = "group", fast = "format:qs",
    report_grouped = "quarto", path = "file", rep_url = "rep" # first kind wins: report, rep, file, url, group, format
  ))
})

test_that("kind_hint() says what a kind means, for the Value pane", {
  expect_match(kind_hint("rep"), "batch", fixed = TRUE)
  expect_match(kind_hint("url"), "ETag", fixed = TRUE)
  expect_match(kind_hint("group"), "one branch per group", fixed = TRUE)
  expect_match(kind_hint("format:parquet"), "Stored as parquet", fixed = TRUE)
  expect_null(kind_hint(NULL))
  expect_null(kind_hint("pattern"))
  expect_null(kind_hint(NA_character_))
})

test_that("target_kinds() reads tarchetypes' factories from a real manifest (#157)", {
  skip_on_cran()
  skip_if_not_installed("tarchetypes")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c(
    "library(targets); library(tarchetypes)",
    "list(",
    "  tar_rep(r1, data.frame(x = 1), batches = 2, reps = 2),",
    "  tar_url(u, 'https://example.com'),",
    "  tar_group_by(g, data.frame(k = c(1, 2), v = 3:4), k),",
    "  tar_target(pq, data.frame(a = 1), format = 'parquet'),",
    "  tar_target(plain, 1)",
    ")"
  ), "_targets.R")
  kinds <- target_kinds(targets::tar_manifest(fields = c("name", "format", "iteration", "command")))
  expect_equal(kinds[c("r1", "u", "g", "pq")], c(r1 = "rep", u = "url", g = "group", pq = "format:parquet"))
  expect_false("plain" %in% names(kinds))
})

test_that("a custom storage format (tar_format()) is named as one, not by its encoded code", {
  format <- "format_custom&read=cmVhZFJEUyhwYXRoKQ&write=c2F2ZVJEUyhvYmplY3QsIHBhdGgp"
  kinds <- target_kinds(data.frame(name = "a", format = format, iteration = "vector", command = "1"))
  expect_equal(kinds, c(a = "format:custom"))
  expect_match(kind_hint("format:custom"), "tar_format()", fixed = TRUE)
  expect_no_match(kind_hint("format:custom"), "format = ", fixed = TRUE)
})
