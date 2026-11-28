# Pipelines written as Quarto or R Markdown documents (#136):
# tarchetypes::tar_tangle() makes a target of each `name <- command` in the
# document's R chunks.

local_document <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  path <- file.path(dir, "pipeline.qmd")
  writeLines(c(
    "---", "title: A pipeline", "---", "",          # 1-4
    "Some prose.", "",                               # 5-6
    "```{r}", "#| format: qs", "data <- 1:10", "```", # 7-10
    "",                                              # 11
    "```{python}", "x = 1", "```",                   # 12-14
    "```{r}",                                        # 15
    "model <- lm(y ~ x,",                            # 16
    "  data = data.frame(x = data, y = data))",      # 17
    "summary(model)",                                # 18 (not a target)
    "fit = coef(model)",                             # 19
    "```"                                            # 20
  ), path)
  path
}

test_that("tangle_definitions() finds each target's line in the document", {
  found <- tangle_definitions(local_document())
  expect_equal(found$name, c("data", "model", "fit"))
  expect_equal(found$line, c(9L, 16L, 19L))
  expect_equal(unique(found$kind), "target")
  expect_null(tangle_definitions(file.path(tempdir(), "nope.qmd")))
})

test_that("the poller watches the documents a script tangles", {
  dir <- withr::local_tempdir()
  script <- file.path(dir, "_targets.R")
  writeLines(c("library(targets)", "c(tarchetypes::tar_tangle('a.qmd'), tar_tangle(path = 'b.Rmd'))"), script)
  expect_true(all(c("a.qmd", "b.Rmd") %in% sourced_paths(script)))
})

test_that("a target of a tangled document opens at its chunk's line", {
  doc <- local_document()
  script <- file.path(dirname(doc), "_targets.R")
  writeLines(c("library(targets)", "tarchetypes::tar_tangle('pipeline.qmd')"), script)
  withr::local_dir(dirname(doc))
  where <- definition_location("model", script)
  expect_equal(basename(where$file), "pipeline.qmd")
  expect_equal(where$line, 16L)
})

test_that("a document in the script bar becomes a script that tangles it", {
  doc <- local_document()
  script <- tangle_script(doc)
  expect_match(basename(script), "^targetsboard_tangle_pipeline.*[.]R$")
  code <- readLines(script)
  expect_match(code, deparse(normalizePath(doc, winslash = "/")), fixed = TRUE, all = FALSE)
  expect_match(code, "tarchetypes::tar_tangle(", fixed = TRUE, all = FALSE)
  expect_identical(tangle_script(doc), script) # the same, loaded again
  expect_equal(tangle_document(script), normalizePath(doc, winslash = "/")) # shown as the document
  expect_null(tangle_document("_targets.R"))
  withr::with_dir(dirname(doc), expect_equal(tangle_document(script), "pipeline.qmd"))
  expect_true(is_tangle_document(doc))
  expect_true(is_tangle_document("x.Rmd"))
  expect_false(is_tangle_document("_targets.R"))
})

test_that("the tangled pipeline builds, from the board's script", {
  skip_on_cran()
  skip_if_not_installed("parsermd")
  doc <- local_document()
  withr::local_dir(dirname(doc))
  script <- tangle_script(doc)
  expect_setequal(targets::tar_manifest(script = script)$name, c("data", "model", "fit"))
})

test_that("the Script tab shows the document, not the script that tangles it", {
  doc <- local_document()
  state <- script_view_state(tangle_script(doc))
  expect_match(state$text, "model <- lm(y ~ x,", fixed = TRUE)
  expect_equal(basename(state$script), "pipeline.qmd")
})
