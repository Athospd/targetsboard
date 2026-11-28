# tar_board_export() (#284): the board as one HTML page, no R and no
# Shiny: the graph, and each local target's Value pane, its files and its
# branches too (step 2).

# The export's state, as write_export() sets it, for the test.
local_export_state <- function(max_total, .env = parent.frame()) {
  exporting$on <- TRUE
  exporting$bytes <- 0
  exporting$max_total <- max_total
  withr::defer(exporting$on <- FALSE, envir = .env)
}

local_export_pipeline <- function(.env = parent.frame()) {
  dir <- local_pipeline(.env = .env)
  writeLines(c(
    "library(targets)",
    "tar_option_set(error = 'continue')",
    "list(",
    "  tar_target(data, data.frame(x = 1:3, y = c(2, 4, 6))),",
    "  tar_target(plot, structure(list(f = function() graphics::plot(data$x, data$y)), class = 'export_plot')),",
    "  tar_target(big, rep(1, 1e5)),",
    "  tar_target(bad, stop('boom')),",
    "  tar_target(later, data, cue = tar_cue('never')),",
    "  tar_target(squares, data$x^2, pattern = map(data)),",
    "  tar_target(outputs, {",
    "    dir.create('out', showWarnings = FALSE)",
    "    grDevices::png('out/chart.png', width = 100, height = 100); graphics::plot(1:3); grDevices::dev.off()",
    "    utils::write.csv(data, 'out/table.csv', row.names = FALSE)",
    "    writeLines('<html><body><h1>My report</h1></body></html>', 'out/report.html')",
    "    writeLines(c('line one', 'line two'), 'out/notes.txt')",
    "    writeBin(as.raw(rep(1, 5000)), 'out/big.bin')",
    "    c('out/chart.png', 'out/table.csv', 'out/report.html', 'out/notes.txt', 'out/big.bin')",
    "  }, format = 'file')",
    ")"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, try(targets::tar_make(reporter = "silent", names = c(data, plot, big, bad, squares, outputs)), silent = TRUE))
  dir
}

test_that("a target's panel: its facts and views, or why its value isn't included", {
  dir <- local_export_pipeline()
  withr::local_dir(dir)
  html <- as.character(export_panel("data", "_targets", max_bytes = 1e7))
  expect_match(html, 'data-tb-export-target="data"', fixed = TRUE)
  expect_match(html, "data.frame", fixed = TRUE)
  expect_match(html, "Printed", fixed = TRUE)
  expect_match(html, 'class="nav-link active"', fixed = TRUE) # tabs as Bootstrap 5 marks them
  expect_match(html, 'id="export-data-value_tab"', fixed = TRUE) # its own ids
  html <- as.character(export_panel("data", "_targets", max_bytes = 1e7, command = c(data = "make_data()")))
  expect_match(html, "make_data()", fixed = TRUE)
  expect_match(as.character(export_panel("big", "_targets", max_bytes = 100)), "Not included: .* is over the export.s 100 B")
  expect_match(as.character(export_panel("bad", "_targets", max_bytes = 1e7)), "It errored: boom", fixed = TRUE)
  expect_match(as.character(export_panel("later", "_targets", max_bytes = 1e7)), "Not built", fixed = TRUE)
  # A pattern: each branch its own pane, picked from a menu, the first shown.
  html <- as.character(export_panel("squares", "_targets", max_bytes = 1e7))
  expect_match(html, 'class="form-select form-select-sm tb-export-branches"', fixed = TRUE)
  expect_match(html, "Branch 3 of 3: squares_", fixed = TRUE)
  expect_match(html, '<div class="tb-export-branch" data-tb-branch="1">', fixed = TRUE)
  expect_match(html, '<div class="tb-export-branch" data-tb-branch="2" hidden>', fixed = TRUE)
  expect_match(html, 'id="export-squares-3-value_tab"', fixed = TRUE)
  local_mocked_bindings(export_branches = 2)
  expect_match(as.character(export_panel("squares", "_targets", max_bytes = 1e7)), "The first 2 of 3 branches", fixed = TRUE)
  # A cloud value is left out (its download costs, and needs credentials).
  local_mocked_bindings(preview_in_cloud = function(target) TRUE)
  expect_match(as.character(export_panel("data", "_targets", max_bytes = 1e7)), "Stored in the cloud: not included", fixed = TRUE)
})

test_that("a plot becomes an image in the page", {
  built <- static_views(list(Plot = tar_board_preview_plot(function() graphics::plot(1:3))))
  expect_match(as.character(built$Plot), '<img src="data:image/png;base64,', fixed = TRUE)
})

test_that("a file target's files are in the page, each as the board shows it, within the caps", {
  dir <- local_export_pipeline()
  withr::local_dir(dir)
  local_export_state(max_total = 1e7)
  html <- as.character(export_panel("outputs", "_targets", max_bytes = 4000))
  expect_match(html, '<img class="tb-preview-image" src="data:image/png;base64,', fixed = TRUE)
  expect_match(html, 'srcdoc="&lt;html&gt;&lt;body&gt;&lt;h1&gt;My report', fixed = TRUE) # the report itself
  expect_match(html, "line two", fixed = TRUE)
  expect_match(html, "out/table.csv", fixed = TRUE)
  expect_match(html, "Not included: 5 kB is over the export.s 4 kB") # big.bin
  expect_no_match(html, "report_[0-9a-f]+/", perl = TRUE) # no URL a board would serve
  # The page's total, reached: the files after it are listed only.
  local_export_state(max_total = 100)
  html <- as.character(export_panel("outputs", "_targets", max_bytes = 1e7))
  expect_match(html, "the page reached its 100 B", fixed = TRUE)
  # Past export_files, the rest are listed, not built.
  local_mocked_bindings(export_files = 2)
  local_export_state(max_total = 1e7)
  html <- as.character(export_panel("outputs", "_targets", max_bytes = 1e7))
  expect_match(html, "3 more files, not included", fixed = TRUE)
  expect_no_match(html, "line two", fixed = TRUE)
})

test_that("tar_board_export() writes one page: the graph, the panels, and says what went in", {
  dir <- local_export_pipeline()
  withr::local_dir(dir)
  expect_message(
    out <- tar_board_export("board.html", callr_function = NULL),
    "board.html holds the values of 7 targets: .*squares"
  )
  expect_equal(out, "board.html")
  html <- paste(readLines("board.html", warn = FALSE), collapse = "\n")
  expect_match(html, "_configuration", fixed = TRUE) # the widget's graph
  expect_match(html, 'data-tb-export-target="data"', fixed = TRUE)
  expect_match(html, "Snapshot of _targets", fixed = TRUE)
  expect_match(html, "targetsboard:focus", fixed = TRUE)
  expect_no_match(html, '<script src="', fixed = TRUE) # self-contained
  expect_no_match(html, 'webfonts/[^)]*[.]woff2') # its icons' fonts too
  # Whole: a line break in a data URI breaks the font.
  expect_no_match(html, "base64,[A-Za-z0-9+/=]+[[:space:]]+[A-Za-z0-9+/]")
  expect_lt(file.size("board.html"), 4e6)
  expect_false(dir.exists("board_files"))

  # names: only those, tidyselect as tar_make().
  tar_board_export("some.html", names = c(data, starts_with("sq")), callr_function = NULL)
  html <- paste(readLines("some.html", warn = FALSE), collapse = "\n")
  expect_match(html, 'data-tb-export-target="squares"', fixed = TRUE)
  expect_no_match(html, 'data-tb-export-target="big"', fixed = TRUE)

  # Not self-contained: the page and a folder of its files.
  tar_board_export("split.html", self_contained = FALSE, callr_function = NULL)
  expect_true(dir.exists("split_files"))
})
