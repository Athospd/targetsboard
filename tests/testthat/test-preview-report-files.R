# A report target's files (#275): the rendered report first, its sources
# in one closed list; every section labelled by its path in the shared
# folder.

test_that("sections are labelled by their path in the shared folder, not basename()", {
  expect_equal(file_labels(c("blog/posts/a/index.qmd", "blog/posts/b/index.qmd", "blog/index.qmd")),
               c("posts/a/index.qmd", "posts/b/index.qmd", "index.qmd"))
  expect_equal(file_labels(c("_site", "index.qmd", "posts/a/index.qmd")), c("_site", "index.qmd", "posts/a/index.qmd"))
  expect_equal(file_labels(c("C:/x/out/a.csv", "C:/x/out/b.csv")), c("a.csv", "b.csv"))
  expect_equal(file_labels("one/file.txt"), "file.txt")
  html <- as.character(files_sections(files_view(c("p/a/index.qmd", "p/b/index.qmd"))))
  expect_match(html, "<code>a/index.qmd</code>", fixed = TRUE)
  expect_match(html, "<code>b/index.qmd</code>", fixed = TRUE)
})

test_that("a rendered report is the first path, as tar_quarto() and tar_render() give it", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("_site/posts", recursive = TRUE)
  writeLines("<html></html>", "_site/index.html")
  file.create(c("index.qmd", "report.qmd", "report.html", "notes.Rmd", "notes.pdf", "data.csv"))
  expect_true(rendered_report(c("_site", "index.qmd", "_quarto.yml"))) # a site
  expect_true(rendered_report(c("report.html", "report.qmd")))
  expect_true(rendered_report(c("notes.pdf", "notes.Rmd", "data.csv")))
  expect_false(rendered_report(c("data.csv", "report.qmd"))) # not a document
  expect_false(rendered_report(c("report.html", "data.csv"))) # no source among them
  expect_false(rendered_report("report.html"))
  expect_false(rendered_report(c("_site/posts", "index.qmd"))) # a folder without index.html
})

test_that("a report's files open on the report, its sources in one closed list", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  file.create(c("report.html", "report.qmd", "data.csv"))
  html <- as.character(files_sections(files_view(c("report.html", "report.qmd", "data.csv"))))
  # The report's view straight away, not in a section.
  expect_match(html, '<div id="file_1" class="shiny-html-output"></div>', fixed = TRUE)
  expect_match(html, "Sources and inputs (2)", fixed = TRUE)
  expect_no_match(html, "<code>report.html</code>", fixed = TRUE)
  # The sources: closed, built when opened.
  expect_no_match(html, "data-index=\"2\" open", fixed = TRUE)
  expect_match(html, 'data-tb-output="file_2"', fixed = TRUE)
  expect_match(html, 'data-tb-output="file_3"', fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('class="shiny-html-output', html))), 1)
  # More than 50 sources: Show more, still.
  paths <- c("report.html", "report.qmd", sprintf("d%03d.csv", 1:60))
  html <- as.character(files_sections(files_view(paths)))
  expect_match(html, "Sources and inputs (61)", fixed = TRUE)
  expect_match(html, "Show 50 more", fixed = TRUE)
})
