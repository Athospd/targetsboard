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

test_that("previews dispatch on the target's class", {
  expect_identical(
    render_and_output_bundle(1:3)$output_ui_fun,
    shiny::verbatimTextOutput
  )
  expect_identical(
    render_and_output_bundle(shiny::p("hi"))$output_ui_fun,
    shiny::uiOutput
  )
  expect_identical(
    render_and_output_bundle("not a file")$output_ui_fun,
    shiny::verbatimTextOutput
  )

  dir <- withr::local_tempdir()
  report <- file.path(dir, "r.html")
  writeLines("<html></html>", report)
  expect_identical(
    render_and_output_bundle(report)$output_ui_fun,
    shiny::htmlOutput
  )
})

test_that("ggplot previews use plotOutput when ggplot2 is installed", {
  skip_if_not_installed("ggplot2")
  plot <- ggplot2::ggplot()
  expect_identical(render_and_output_bundle(plot)$output_ui_fun, shiny::plotOutput)
})

test_that("data frame previews fall back to print without skimr", {
  bundle <- render_and_output_bundle(data.frame(x = 1))
  expect_identical(bundle$output_ui_fun, shiny::verbatimTextOutput)
})
