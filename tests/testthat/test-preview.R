test_that("HTML report paths are detected only when the file exists", {
  dir <- withr::local_tempdir()
  report <- file.path(dir, "report.html")
  writeLines("<html></html>", report)

  expect_true(is_html_file_path(c("data.csv", report)))
  expect_false(is_html_file_path("missing.html"))
  expect_false(is_html_file_path("notes.txt"))
  expect_equal(extract_html_file_path(c("data.csv", report)), report)
})

test_that("serve_html_report() serves only the report's folder", {
  dir <- withr::local_tempdir()
  report <- file.path(dir, "my report.html")
  writeLines("<html></html>", report)

  url <- serve_html_report(report)
  prefix <- sub("/.*", "", url)

  expect_match(url, "^report_[0-9a-f]{12}/my%20report[.]html$")
  expect_equal(
    normalizePath(shiny::resourcePaths()[[prefix]]),
    normalizePath(dir)
  )
  withr::defer(shiny::removeResourcePath(prefix))
})

test_that("tar_board_preview() gives the built-in views by class", {
  target <- list(name = "x")
  expect_null(tar_board_preview(quote(a + b), target)) # no view of its own: Printed only
  expect_match(as.character(tar_board_preview("not a file", target)), "tb-scalar", fixed = TRUE)
  expect_identical(tar_board_preview(shiny::p("hi"), target), shiny::p("hi"))

  dir <- withr::local_tempdir()
  report <- file.path(dir, "r.html")
  writeLines("<html></html>", report)
  expect_match(as.character(tar_board_preview(report, target)), "<iframe", fixed = TRUE)

  skip_if_not_installed("ggplot2")
  plot <- ggplot2::ggplot()
  expect_s3_class(tar_board_preview(plot, target), "tar_board_preview_plot")
})

test_that("a preview is its views as tabs, with Printed always last", {
  target <- list(name = "x")
  built <- build_preview(quote(a + b), target)
  expect_equal(names(built$views), "Printed")
  expect_match(as.character(built$views$Printed), "a + b", fixed = TRUE)
  expect_equal(built$facts$class, "call")
  expect_null(built$note)

  built <- build_preview(shiny::p("hi"), target)
  expect_equal(names(built$views), c("Preview", "Printed"))

  local_mocked_bindings(tar_board_preview = function(x, target, ...) {
    list(Rows = shiny::p("rows"), Columns = shiny::p("columns"))
  })
  built <- build_preview(data.frame(x = 1:2), target)
  expect_equal(names(built$views), c("Rows", "Columns", "Printed"))
  expect_equal(built$facts$shape, "2 \u00d7 1")
})

test_that("a preview that fails falls back to Printed, with a note", {
  target <- list(name = "x")
  local_mocked_bindings(tar_board_preview = function(x, target, ...) stop("no luck"))
  built <- build_preview(1:3, target)
  expect_equal(names(built$views), "Printed")
  expect_equal(built$note, "The integer preview failed: no luck")

  # A method that returns something that is no view.
  local_mocked_bindings(tar_board_preview = function(x, target, ...) list(1, 2))
  expect_match(build_preview(1:3, target)$note, "neither a view nor a named list of views")

  # A plot that errors when drawn: caught before it reaches the page.
  local_mocked_bindings(tar_board_preview = function(x, target, ...) {
    tar_board_preview_plot(function() stop("bad axis"))
  })
  built <- build_preview(1:3, target)
  expect_equal(built$note, "The integer preview failed: bad axis")
  expect_equal(names(built$views), "Printed")
})

test_that("Printed is capped for long values", {
  text <- printed_text(1:100000, max_lines = 10)
  lines <- strsplit(text, "\n")[[1]]
  expect_length(lines, 11)
  expect_match(lines[[11]], "^[.][.][.] [0-9,]+ more lines$")
})

test_that("the preview cache keeps the last few builds, by target and build", {
  cache <- preview_cache(size = 2)
  cache$set("a", 1)
  cache$set("b", 2)
  cache$set("a", 3) # a is the latest again
  cache$set("c", 4)
  expect_null(cache$get("b"))
  expect_equal(cache$get("a"), 3)
  expect_equal(cache$get("c"), 4)
  cache$clear()
  expect_null(cache$get("a"))

  # A rebuilt target is a new key: never shown stale.
  t1 <- list(name = "x", time = as.POSIXct("2026-01-01 10:00", tz = "UTC"), bytes = 10)
  t2 <- modifyList(t1, list(time = as.POSIXct("2026-01-01 11:00", tz = "UTC")))
  expect_false(identical(preview_key(t1), preview_key(t2)))
})

test_that("the frame shows the target's facts, a Copy for tar_read(), and full-screen", {
  target <- list(name = "fit", format = "rds", bytes = 2500000, time = as.POSIXct("2026-11-23 14:05", tz = ""))
  built <- build_preview(1:3, target)
  html <- as.character(preview_frame(target, built, store = "my_store"))
  expect_match(html, '<code class="tb-preview-name">fit</code>', fixed = TRUE)
  for (fact in c("integer", "length 3", "2.5 MB", "rds format", "built 2026-11-23 14:05")) {
    expect_match(html, paste0("<span>", fact, "</span>"), fixed = TRUE)
  }
  expect_match(html, 'data-copy="targets::tar_read(fit, store = &quot;my_store&quot;)"', fixed = TRUE)
  expect_match(html, "bslib-full-screen-enter", fixed = TRUE)
  expect_match(html, ">Printed<", fixed = TRUE)
})

test_that("a large value is read only when asked", {
  withr::local_options(targetsboard.preview_bytes = 1000)
  big <- list(name = "big", bytes = 5e6)
  expect_true(preview_too_large(big))
  expect_false(preview_too_large(list(name = "small", bytes = 10)))
  expect_false(preview_too_large(list(name = "unknown")))
  html <- as.character(preview_gate(big))
  expect_match(html, "This value is 5 MB in the store, more than the 1 kB", fixed = TRUE)
  expect_match(html, 'id="value_preview_read"', fixed = TRUE)
})

test_that("_targetsboard.R adds preview methods for a project's own classes", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "_targetsboard.R")
  expect_equal(load_preview_methods(path), character()) # no file: nothing
  writeLines(c(
    "tar_board_preview.tb_test_thing <- function(x, target, ...) {",
    "  list(Thing = htmltools::tags$b(x$label), Plot = tar_board_preview_plot(function() plot(1)))",
    "}",
    "not_a_method <- 1"
  ), path)
  expect_equal(load_preview_methods(path), "tb_test_thing")
  thing <- structure(list(label = "hello"), class = "tb_test_thing")
  built <- build_preview(thing, list(name = "x"))
  expect_equal(names(built$views), c("Thing", "Plot", "Printed"))
  expect_null(built$note)

  # Edited and loaded again: the new method replaces the old one.
  writeLines("tar_board_preview.tb_test_thing <- function(x, target, ...) htmltools::tags$i('v2')", path)
  load_preview_methods(path)
  expect_match(as.character(tar_board_preview(thing, list(name = "x"))), "v2", fixed = TRUE)

  writeLines("stop('typo')", path)
  expect_error(load_preview_methods(path), "typo")
  # Removed from the file: dropped. With no file, there are none.
  writeLines("# nothing yet", path)
  load_preview_methods(path)
  expect_null(project_preview_method(thing))
  writeLines("tar_board_preview.numeric <- function(x, target, ...) htmltools::tags$i('mine')", path)
  load_preview_methods(path)
  # Also for a class the board has a method for.
  expect_match(as.character(tar_board_preview(2.5, list(name = "x"))), "<i>mine</i>", fixed = TRUE)
  unlink(path)
  load_preview_methods(path)
  expect_match(as.character(tar_board_preview(2.5, list(name = "x"))), "tb-scalar", fixed = TRUE)
})

test_that("the value preview shows a frame, asks before a large value, and caches what it built", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  writeLines(c(
    "tar_board_preview.numeric <- function(x, target, ...) {",
    "  htmltools::tags$b(class = 'tb-test-view', paste('number', x))",
    "}"
  ), "_targetsboard.R")
  # Kept for the session: take it away again for the other tests.
  withr::defer(clear_project_methods())
  server <- local_app_server()
  reads <- 0
  read <- targets::tar_read_raw
  local_mocked_bindings(
    tar_read_raw = function(...) { reads <<- reads + 1; read(...) },
    .package = "targets"
  )
  withr::local_options(targetsboard.preview_bytes = 1e9)

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(flow_focused_node = "c", flow_double_clicked_node = "c")
    html <- output$value_preview$html
    expect_match(html, '<code class="tb-preview-name">c</code>', fixed = TRUE)
    # _targetsboard.R's method, loaded at start.
    expect_match(html, '<b class="tb-test-view">number 4</b>', fixed = TRUE)
    expect_match(html, 'data-copy="targets::tar_read(c)"', fixed = TRUE)
    expect_equal(reads, 1)

    # Back and forth: c comes from the cache.
    session$setInputs(flow_focused_node = "b")
    session$setInputs(flow_focused_node = "c")
    output$value_preview$html
    expect_equal(reads, 2)

    # A method edited, then Refresh: it takes the new one, built again.
    writeLines("tar_board_preview.numeric <- function(x, target, ...) htmltools::tags$i('edited')", "_targetsboard.R")
    session$setInputs(refresh = 1)
    expect_match(output$value_preview$html, "<i>edited</i>", fixed = TRUE)
    expect_equal(reads, 3)

    # Over the size limit: not read until asked.
    options(targetsboard.preview_bytes = 1)
    session$setInputs(flow_focused_node = "a")
    html <- output$value_preview$html
    expect_match(html, "Load the value", fixed = TRUE)
    expect_equal(reads, 3)
    session$setInputs(value_preview_read = 1)
    html <- output$value_preview$html
    expect_no_match(html, "Load the value", fixed = TRUE)
    expect_match(html, '<i>edited</i>', fixed = TRUE)
    expect_equal(reads, 4)
  })
})

test_that("a _targetsboard.R that fails to load is notified, and the board carries on", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  writeLines("stop('typo in my methods')", "_targetsboard.R")
  notes <- character()
  local_mocked_bindings(
    showNotification = function(ui, ...) notes[[length(notes) + 1]] <<- as.character(ui),
    .package = "shiny"
  )
  server <- local_app_server()
  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    expect_equal(notes, "_targetsboard.R didn't load: typo in my methods")
    session$setInputs(flow_focused_node = "c", flow_double_clicked_node = "c")
    expect_match(output$value_preview$html, "tb-preview-printed", fixed = TRUE)
  })
})

test_that("the methods test left no numeric method behind", {
  html <- as.character(tar_board_preview(1.5, list(name = "x")))
  expect_match(html, '<div class="tb-scalar">1.5</div>', fixed = TRUE) # the board's own
})

test_that("Printed doesn't draw a plot on R's default device", {
  skip_if_not_installed("ggplot2")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  devices <- grDevices::dev.list()
  build_preview(ggplot2::ggplot(), list(name = "x"))
  expect_false(file.exists("Rplots.pdf"))
  expect_equal(grDevices::dev.list(), devices) # and closes what it opened
})

test_that("a data frame previews as Rows and Columns tables", {
  frame <- data.frame(
    n = c(1.5, 2.5, NA), word = c("a", "b", "a"), flag = c(TRUE, FALSE, TRUE),
    day = as.Date("2026-01-01") + 0:2, kind = factor(c("x", "y", "x")),
    stringsAsFactors = FALSE
  )
  built <- build_preview(frame, list(name = "x"))
  expect_equal(names(built$views), c("Rows", "Columns", "Printed"))
  expect_null(built$note)
  expect_s3_class(built$views$Columns, "reactable")

  expect_match(as.character(built$views$Columns$x$tag$attribs$columns[[5]]$cell[[1]]), "min 1.5", fixed = TRUE)
  summary <- column_summary(c(frame$n, 3:10))
  expect_equal(summary[c("type", "missing", "unique")], list(type = "dbl", missing = 1L, unique = 10L))
  expect_equal(summary$stats, "min 1.5 \u00b7 mean 5.6 \u00b7 max 10")
  expect_match(as.character(summary$spark), "<svg class=\"tb-spark\"", fixed = TRUE)
  expect_equal(column_summary(frame$word)$stats, "a (2), b (1)")
  expect_equal(column_summary(frame$flag)$stats, "2 TRUE, 1 FALSE")
  expect_equal(column_summary(frame$day)$stats, "2026-01-01 to 2026-01-03")
  expect_equal(
    vapply(frame, type_abbr, character(1)),
    c(n = "dbl", word = "chr", flag = "lgl", day = "date", kind = "fct")
  )
})

test_that("a long data frame sends only its first rows, and says so", {
  long <- data.frame(i = seq_len(2500))
  html <- as.character(rows_view(long))
  expect_match(html, "The first 1,000 of 2,500 rows.", fixed = TRUE)
  expect_no_match(as.character(rows_view(long[1:10, , drop = FALSE])), "The first", fixed = TRUE)
  # A list column shows as text instead of failing.
  listy <- data.frame(id = 1:2)
  listy$items <- list(1:3, letters[1:2])
  expect_s3_class(rows_view(listy), "shiny.tag.list")
})

test_that("vectors preview as a summary, a distribution and their values", {
  target <- list(name = "x")
  expect_match(as.character(tar_board_preview(42.195, target)), '<div class="tb-scalar">42.195</div>', fixed = TRUE)

  html <- as.character(tar_board_preview(c(1:10, NA), target))
  for (part in c("<span>int</span>", "<span>length 11</span>", "<span>1 missing</span>", "min 1 \u00b7 mean 5.5 \u00b7 max 10", "tb-spark")) {
    expect_match(html, part, fixed = TRUE)
  }
  expect_no_match(as.character(tar_board_preview(c(1, 2, 3), target)), "tb-spark", fixed = TRUE) # too few for one
  expect_match(as.character(tar_board_preview(factor(c("a", "b", "a")), target)), "<span>fct</span>", fixed = TRUE)
  expect_s3_class(tar_board_preview(c(alpha = 1, beta = 2), target), "shiny.tag.list") # a name and value table
  expect_match(as.character(tar_board_preview(as.Date("2026-01-01") + 0:9, target)), "2026-01-01 to 2026-01-10", fixed = TRUE)
  # Matrices aren't vectors: no view of their own yet.
  expect_null(tar_board_preview(matrix(1:4, 2), target))
  expect_null(tar_board_preview(numeric(), target))
})

test_that("a plot is drawn at twice the pixels of its size on the page", {
  file <- withr::local_tempfile(fileext = ".png")
  draw_png(tar_board_preview_plot(function() plot(1:10)), file, width = 300, height = 195)
  header <- readBin(file, "raw", 24)
  dims <- vapply(c(17, 21), function(at) sum(as.integer(header[at:(at + 3)]) * 256^(3:0)), numeric(1))
  expect_equal(dims, c(600, 390))
  skip_if_not_installed("ggplot2")
  draw_png(tar_board_preview_plot(ggplot2::ggplot()), file, width = 200, height = 130)
  expect_gt(file.size(file), 0)
})

test_that("the frame gives a plot an image sized by the page, and a widget the width", {
  plot_view <- tar_board_preview_plot(function() plot(1))
  built <- list(facts = list(class = "x"), views = list(Preview = plot_view, Printed = shiny::tags$pre("x")))
  html <- as.character(preview_frame(list(name = "p"), built, plot_ids = "value_preview_plot_1"))
  expect_match(html, '<div class="tb-preview-plot">[[:space:]]*<div id="value_preview_plot_1" class="shiny-image-output" style="width:100%;height:100%;">')

  widget <- size_widget(reactable::reactable(data.frame(a = 1)))
  expect_equal(widget[c("width", "height")], list(width = "100%", height = 420))
  tall <- reactable::reactable(data.frame(a = 1), height = 600)
  expect_equal(size_widget(tall)$height, "600px") # its own height stays
})

test_that("date-times get a histogram without overflowing hist()", {
  stamps <- as.POSIXct("2026-01-01", tz = "UTC") + seq(0, 86400 * 30, length.out = 40)
  expect_no_warning(spark <- spark_histogram(stamps))
  expect_match(as.character(spark), "<rect", fixed = TRUE)
})

test_that("the Rows table heads each number and date column with its distribution", {
  frame <- data.frame(
    n = c(seq(1, 50, length.out = 1999), 1000), # the last row past the first 1,000
    word = rep(c("a", "b"), 1000),
    day = as.Date("2026-01-01") + 0:1999
  )
  table <- rows_view(frame)
  html <- as.character(table)
  # Two histograms, one each for n and day: none for the text column.
  expect_equal(lengths(regmatches(html, gregexpr("tb-col-spark", html, fixed = TRUE))), 2)
  # As an element, which reactable draws, not an HTML string it would escape.
  expect_match(html, '"name":"svg","attribs":[{][^}]*"className":"tb-spark"')
  # Of every row, not only the first 1,000 the table shows.
  expect_match(html, "distribution of all 2,000 rows", fixed = TRUE)

  # Filters per column, a search, and 10 rows a page to begin with.
  widget <- Find(function(x) inherits(x, "htmlwidget"), table)
  expect_true(widget$x$tag$attribs$filterable)
  expect_true(widget$x$tag$attribs$searchable)
  expect_equal(widget$x$tag$attribs$defaultPageSize, 10)

  # Too few values for a distribution: no histogram.
  expect_no_match(as.character(rows_view(data.frame(n = 1:3))), "tb-col-spark", fixed = TRUE)
})
