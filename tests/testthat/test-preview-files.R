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

test_that("a CSV or TSV file's line gives its rows and columns, not its size and time again", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "table.csv")
  utils::write.csv(data.frame(x = 1:1500, y = "a", z = TRUE), csv, row.names = FALSE)
  tsv <- file.path(dir, "small.tsv")
  writeLines(c("a\tb", "1\t2"), tsv) # one row
  target <- list(name = "f", format = "file")

  html <- as.character(file_section_view(csv, target))
  expect_match(html, "<span>1,500 rows x 3 columns</span>", fixed = TRUE)
  expect_no_match(html, "modified", fixed = TRUE)
  expect_no_match(html, "KB", fixed = TRUE)
  expect_match(as.character(file_section_view(tsv, target)), "<span>1 row x 2 columns</span>", fixed = TRUE)

  # Other files keep their size and time.
  log <- file.path(dir, "run.log")
  writeLines("done", log)
  expect_match(as.character(file_section_view(log, target)), "modified ", fixed = TRUE)
})
