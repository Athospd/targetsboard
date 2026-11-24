# Writes files of every type the board shows into a temporary folder.
local_files <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  path <- function(name) file.path(dir, name)
  utils::write.csv(data.frame(x = 1:150, y = "a"), path("table.csv"), row.names = FALSE)
  writeLines("# Notes\n\nSome **bold** text.", path("notes.md"))
  writeLines(c("# fit a model", "fit <- lm(y ~ x, data = d)"), path("code.R"))
  writeLines(sprintf("line %d", 1:250), path("long.log"))
  grDevices::png(path("chart.png"), 200, 150); graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new(); grDevices::dev.off()
  writeLines("<html><body>report</body></html>", path("report.html"))
  writeBin(as.raw(0:9), path("data.bin"))
  dir.create(path("folder")); writeLines("a", file.path(path("folder"), "a.txt"))
  path
}

test_that("a file target dispatches on tar_board_files, and each path on its extension", {
  target <- list(name = "f", format = "file")
  expect_s3_class(dispatch_value(c("a.csv", "b.png"), target), "tar_board_files")
  expect_identical(dispatch_value(c("a.csv"), list(format = "rds")), "a.csv") # not a file target
  expect_equal(class(board_file("dir/Plot.PNG")), c("tar_board_file_png", "tar_board_file", "character"))
  expect_equal(class(board_file("README")), c("tar_board_file", "character"))

  built <- build_preview(c("a.csv", "b.png"), target)
  expect_equal(names(built$views), c("Preview", "Printed"))
  expect_s3_class(built$views$Preview, "tar_board_preview_files")
  expect_match(as.character(built$views$Printed), "a.csv", fixed = TRUE) # the paths, unclassed
  expect_no_match(as.character(built$views$Printed), "tar_board", fixed = TRUE)
})

test_that("a file target is never too large to preview: its value is its paths", {
  expect_false(preview_too_large(list(format = "file", bytes = 5e9), limit = 1))
  expect_true(preview_too_large(list(format = "rds", bytes = 5e9), limit = 1))
})

test_that("each type of file has its view", {
  path <- local_files()
  target <- list(name = "f", format = "file")
  view <- function(name) as.character(tar_board_preview(board_file(path(name)), target))

  expect_match(view("table.csv"), "The first 100 rows.", fixed = TRUE)
  expect_match(view("notes.md"), "<strong>bold</strong>", fixed = TRUE)
  expect_match(view("code.R"), '<pre class="tb-preview-text language-r"># fit a model', fixed = TRUE)
  log <- view("long.log")
  expect_match(log, "The first 200 lines.", fixed = TRUE)
  expect_match(log, "line 200", fixed = TRUE)
  expect_no_match(log, "line 201", fixed = TRUE)
  expect_match(view("chart.png"), '<img class="tb-preview-image" src="', fixed = TRUE)
  expect_match(view("report.html"), '<iframe class="tb-preview-document" src="report_', fixed = TRUE)
  expect_match(view("folder"), "A folder of 1 files.", fixed = TRUE)
  expect_null(tar_board_preview(board_file(path("data.bin")), target)) # facts only

  # The section adds the file's path, size and time; a lost file says so.
  section <- as.character(file_section_view(path("code.R"), target))
  expect_match(section, '<div class="tb-file-facts">', fixed = TRUE)
  expect_match(section, "modified ", fixed = TRUE)
  expect_match(as.character(file_section_view(path("gone.csv"), target)), "any more.", fixed = TRUE)
})

test_that("a file's view that fails says so in its section", {
  path <- local_files()
  local_mocked_bindings(rows_view = function(...) stop("bad table"))
  html <- as.character(file_section_view(path("table.csv"), list(name = "f")))
  expect_match(html, "The file's preview failed: bad table", fixed = TRUE)
})

test_that("files show as sections, 50 at a time, only the first built", {
  one <- as.character(files_sections(files_view("a.csv")))
  expect_match(one, 'id="value_preview_file_1"', fixed = TRUE)
  expect_no_match(one, "<details", fixed = TRUE)

  many <- as.character(files_sections(files_view(sprintf("f%03d.csv", 1:120))))
  expect_match(many, "120 files.", fixed = TRUE)
  expect_equal(lengths(regmatches(many, gregexpr("<details", many))), 120)
  expect_equal(lengths(regmatches(many, gregexpr("<details[^>]* hidden[ >]", many))), 70)
  expect_equal(lengths(regmatches(many, gregexpr('class="shiny-html-output', many))), 1) # the first only
  expect_match(many, '<details class="tb-preview-file" data-index="1" open>', fixed = TRUE)
  expect_match(many, "Show 50 more", fixed = TRUE)
})

test_that("the board builds a file's view when its section opens", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c(
    "library(targets)",
    "write <- function(i) { path <- sprintf('out_%d.R', i); writeLines(sprintf('x <- %d', i), path); path }",
    "list(tar_target(files, c(write(1), write(2), write(3)), format = 'file'))"
  ), "_targets.R")
  targets::tar_make(reporter = "silent")
  server <- local_app_server()

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(flow_focused_node = "files", flow_double_clicked_node = "files")
    html <- output$value_preview$html
    expect_match(html, "3 files.", fixed = TRUE)
    expect_match(output$value_preview_file_1$html, "x &lt;- 1", fixed = TRUE)
    expect_error(output$value_preview_file_3) # not opened yet
    session$setInputs(value_preview_file_open = 3)
    expect_match(output$value_preview_file_3$html, "x &lt;- 3", fixed = TRUE)
  })
})

test_that("Quarto and R Markdown files show rendered, with their title and their R chunks", {
  dir <- withr::local_tempdir()
  qmd <- file.path(dir, "report.qmd")
  writeLines(c(
    "---", "title: \"Fuel economy\"", "format: html", "---", "",
    "Some **bold** text.", "",
    "```{r fit, echo=FALSE}", "#| label: fit", "fit <- lm(mpg ~ wt, data = mtcars)", "```"
  ), qmd)
  rmd <- file.path(dir, "analysis.Rmd")
  writeLines(c("Untitled, with a `code` span.", "", "```{r}", "x <- 1", "```"), rmd)
  target <- list(name = "f", format = "file")

  html <- as.character(tar_board_preview(board_file(qmd), target))
  expect_match(html, "tb-preview-markdown", fixed = TRUE)
  expect_match(html, "<h1>Fuel economy</h1>", fixed = TRUE) # the front matter's title
  expect_no_match(html, "format: html", fixed = TRUE) # the rest of it left out
  expect_match(html, "<strong>bold</strong>", fixed = TRUE)
  # The chunk as R code, coloured like the board's other R code.
  expect_match(html, '<pre class="language-r"><code>#| label: fit', fixed = TRUE)
  expect_no_match(html, "{r", fixed = TRUE)

  html <- as.character(tar_board_preview(board_file(rmd), target))
  expect_no_match(html, "<h1>", fixed = TRUE)
  expect_match(html, "<code>code</code>", fixed = TRUE)
  expect_match(html, '<pre class="language-r"><code>x &lt;- 1', fixed = TRUE)
})

test_that("a Markdown file's R blocks are coloured too", {
  path <- withr::local_tempfile(fileext = ".md")
  writeLines(c("```r", "x <- 1", "```", "", "```", "plain", "```"), path)
  html <- as.character(tar_board_preview(board_file(path), list(name = "f", format = "file")))
  expect_match(html, '<pre class="language-r"><code>x &lt;- 1', fixed = TRUE)
  expect_match(html, "<pre><code>plain", fixed = TRUE)
})

test_that("a Quarto or R Markdown file shows the document it was rendered to, when there is one", {
  dir <- withr::local_tempdir()
  qmd <- file.path(dir, "report.qmd")
  writeLines(c("---", "title: \"Fuel economy\"", "---", "", "```{r}", "1 + 1", "```"), qmd)
  html <- file.path(dir, "report.html")
  view <- function(files = qmd) {
    as.character(tar_board_preview(board_file(qmd), list(name = "f", format = "file", files = files)))
  }

  # Not rendered: its source, saying so.
  out <- view()
  expect_match(out, "Not rendered", fixed = TRUE)
  expect_match(out, "<h1>Fuel economy</h1>", fixed = TRUE)

  # Rendered next to it: the rendered document, as the target made it.
  writeLines("<html><body><p>[1] 2</p></body></html>", html)
  Sys.setFileTime(html, Sys.time() + 60)
  out <- view()
  expect_match(out, "Rendered as report.html", fixed = TRUE)
  expect_match(out, '<iframe class="tb-preview-document" src="report_', fixed = TRUE)
  expect_no_match(out, "<h1>Fuel economy</h1>", fixed = TRUE)
  expect_no_match(out, "older than", fixed = TRUE)

  # Rendered before the source last changed: said so.
  Sys.setFileTime(html, Sys.time() - 3600)
  expect_match(view(), "older than its source", fixed = TRUE)

  # tar_quarto() and tar_render() return the rendered file too, which has
  # its own section: the source's section shows the source.
  # Its paths are relative to the project, the working directory.
  withr::local_dir(dir)
  out <- view(files = c("report.html", "report.qmd"))
  expect_match(out, "<h1>Fuel economy</h1>", fixed = TRUE)
  expect_match(out, "report.html has its own section", fixed = TRUE)
})
