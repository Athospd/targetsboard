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
