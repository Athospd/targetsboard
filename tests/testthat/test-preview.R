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
  expect_null(tar_board_preview(1:3, target)) # no view of its own: Printed only
  expect_null(tar_board_preview("not a file", target))
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
  built <- build_preview(1:3, target)
  expect_equal(names(built$views), "Printed")
  expect_match(as.character(built$views$Printed), "[1] 1 2 3", fixed = TRUE)
  expect_equal(built$facts, list(class = "integer", shape = "length 3"))
  expect_null(built$note)

  built <- build_preview(shiny::p("hi"), target)
  expect_equal(names(built$views), c("Preview", "Printed"))

  local_mocked_bindings(tar_board_preview = function(x, target, ...) {
    list(Rows = shiny::p("rows"), Columns = shiny::p("columns"))
  })
  built <- build_preview(data.frame(x = 1:2), target)
  expect_equal(names(built$views), c("Rows", "Columns", "Printed"))
  expect_equal(built$facts$shape, "2 × 1")
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
})

test_that("the value preview shows a frame, asks before a large value, and caches what it built", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  writeLines(c(
    "tar_board_preview.numeric <- function(x, target, ...) {",
    "  htmltools::tags$b(class = 'tb-test-view', paste('number', x))",
    "}"
  ), "_targetsboard.R")
  # Registered for the session: take it away again for the other tests.
  withr::defer(rm(
    list = "tar_board_preview.numeric",
    envir = get(".__S3MethodsTable__.", envir = asNamespace("targetsboard"))
  ))
  server <- local_app_server()
  reads <- 0
  read <- targets::tar_read_raw
  local_mocked_bindings(
    tar_read_raw = function(...) { reads <<- reads + 1; read(...) },
    .package = "targets"
  )
  withr::local_options(targetsboard.preview_bytes = 1e9)

  shiny::testServer(server, {
    environment(server)$stop_poller()
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
    environment(server)$stop_poller()
    expect_equal(notes, "_targetsboard.R didn't load: typo in my methods")
    session$setInputs(flow_focused_node = "c", flow_double_clicked_node = "c")
    expect_match(output$value_preview$html, "tb-preview-printed", fixed = TRUE)
  })
})

test_that("the methods test left no numeric method behind", {
  expect_null(tar_board_preview(1.5, list(name = "x")))
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
