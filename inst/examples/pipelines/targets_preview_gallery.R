# One target per kind of view the value preview has: double-click each to
# see its preview. For visual checks, and for tests/testthat/test-gallery.R,
# which builds this pipeline and checks each target's tabs.
#
#   Sys.setenv(TAR_CONFIG = "inst/examples/pipelines/_targets.yaml", TAR_PROJECT = "preview_gallery")
#   targets::tar_make()
#   targetsboard::tar_board()
#
# The tabs each target should show, before Printed, which is always last:
#
#   Data frames   Rows, Columns
#   Vectors       Preview: a summary, a histogram, the names or counts;
#                 a single value in large type
#   Plots, HTML   Preview: a plot, a widget, or the HTML
#   Lists         Preview: a tree
#   Models        Estimates, Fit (broom); Summary (summary()); a tree
#   Files         Preview: one file, or a section per file
#   Printed only  a matrix, a formula
#
# Files are written inside the store, so they go with tar_destroy(). Targets
# that need ggplot2, plotly or broom are left out when those aren't
# installed; without broom, the models show Summary.

library(targets)

gallery_file <- function(name) {
  dir <- file.path(tar_path_store(), "gallery")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.path(dir, name)
}

write_lines <- function(name, lines) {
  path <- gallery_file(name)
  writeLines(lines, path)
  path
}

optional <- function(package, target) if (requireNamespace(package, quietly = TRUE)) target

Filter(Negate(is.null), list(
  # Data frames: Rows and Columns.
  tar_target(cars_frame, transform(mtcars, model = rownames(mtcars), cyl = factor(cyl))),
  tar_target(wide_frame, as.data.frame(matrix(round(rnorm(200 * 30), 2), ncol = 30))),
  tar_target(long_frame, data.frame(id = seq_len(5000), value = round(runif(5000), 3))),
  tar_target(list_column_frame, {
    frame <- data.frame(id = 1:3, when = as.POSIXct("2026-01-01 09:00", tz = "UTC") + 3600 * 0:2)
    frame$items <- list(1:3, letters[1:2], NULL)
    frame
  }),

  # Vectors: a summary, a distribution, the names or counts; a single value alone.
  tar_target(one_number, 42.195),
  tar_target(one_word, "hello"),
  tar_target(draws, rnorm(10000, mean = 5, sd = 2)),
  tar_target(counts, c(3L, 7L, NA, 2L, 9L, 4L, 4L, 8L, 1L, 6L, NA, 5L)),
  tar_target(flags, c(TRUE, FALSE, TRUE, TRUE, NA, FALSE)),
  tar_target(species, iris$Species),
  tar_target(words, sample(c("red", "green", "blue", "yellow"), 200, replace = TRUE)),
  tar_target(named_scores, c(alpha = 0.91, beta = 0.78, gamma = 0.66)),
  tar_target(days, seq(as.Date("2026-01-01"), by = "week", length.out = 52)),
  tar_target(stamps, as.POSIXct("2026-01-01", tz = "UTC") + sort(runif(40, 0, 86400 * 30))),

  # Plots, widgets and HTML.
  optional("ggplot2", tar_target(
    scatter_plot,
    ggplot2::ggplot(cars_frame, ggplot2::aes(wt, mpg, colour = cyl)) + ggplot2::geom_point(size = 2)
  )),
  optional("plotly", tar_target(
    interactive_plot,
    plotly::plot_ly(cars_frame, x = ~wt, y = ~mpg, color = ~cyl, type = "scatter", mode = "markers")
  )),
  tar_target(table_widget, reactable::reactable(cars_frame[, 1:5])),
  tar_target(html_card, htmltools::tags$div(
    htmltools::tags$h4("An HTML value"),
    htmltools::tags$p("A tag made with htmltools, shown as it is.")
  )),

  # Lists: a tree, two levels deep, 50 items a level.
  tar_target(settings, list(
    model = list(family = "gaussian", formula = "mpg ~ wt + hp", weights = NULL),
    folds = 1:10,
    tags = c("demo", "gallery"),
    nested = list(level_2 = list(level_3 = list(level_4 = "deep"))),
    data = cars_frame[1:3, 1:3]
  )),
  tar_target(long_list, as.list(seq_len(120))),

  # Models: Estimates and Fit with broom; Summary; a tree.
  tar_target(fit, lm(mpg ~ wt + hp, data = cars_frame)),
  tar_target(logistic, glm(am ~ wt, data = cars_frame, family = binomial)),
  tar_target(t_test, t.test(mpg ~ am, data = cars_frame)),
  tar_target(clusters, kmeans(cars_frame[, c("mpg", "wt")], centers = 3)),
  tar_target(step_function, ecdf(draws)),
  tar_target(experiment, structure(
    list(id = "run-7", started = Sys.Date(), params = list(alpha = 0.1, depth = 4L), scores = runif(5)),
    class = "experiment_record"
  )),

  # Printed only: no view of their own.
  tar_target(matrix_value, matrix(1:12, nrow = 3)),
  tar_target(model_formula, mpg ~ wt + hp),

  # Files: one section per file, each by its type.
  tar_target(chart_png, {
    path <- gallery_file("chart.png")
    grDevices::png(path, width = 800, height = 500)
    plot(draws[1:500], type = "l", main = "A png file")
    grDevices::dev.off()
    path
  }, format = "file"),
  tar_target(chart_jpg, {
    path <- gallery_file("chart.jpg")
    grDevices::jpeg(path, width = 800, height = 500, quality = 90)
    boxplot(mpg ~ cyl, data = cars_frame, main = "A jpg file")
    grDevices::dev.off()
    path
  }, format = "file"),
  tar_target(chart_svg, write_lines("chart.svg", c(
    '<svg xmlns="http://www.w3.org/2000/svg" width="320" height="120" viewBox="0 0 320 120">',
    '<rect x="10" y="10" width="300" height="100" rx="12" fill="#3b82f6"/>',
    '<text x="160" y="68" font-size="24" text-anchor="middle" fill="white">An svg file</text>',
    "</svg>"
  )), format = "file"),
  tar_target(chart_pdf, {
    path <- gallery_file("chart.pdf")
    grDevices::pdf(path)
    hist(draws, main = "A pdf file")
    grDevices::dev.off()
    path
  }, format = "file"),
  tar_target(table_csv, {
    path <- gallery_file("cars.csv")
    utils::write.csv(cars_frame, path, row.names = FALSE)
    path
  }, format = "file"),
  tar_target(table_tsv, {
    path <- gallery_file("long.tsv")
    utils::write.table(long_frame, path, sep = "\t", row.names = FALSE, quote = FALSE)
    path
  }, format = "file"),
  tar_target(report_html, write_lines(
    "report.html",
    "<html><body><h1>A report</h1><p>An HTML file, shown as a page.</p></body></html>"
  ), format = "file"),
  tar_target(notes_md, write_lines(
    "notes.md",
    c("# Notes", "", "A **markdown** file, with a list and code:", "", "- one", "- two", "", "```r", "x <- 1", "```")
  ), format = "file"),
  tar_target(code_r, write_lines(
    "model.R",
    c("# An R file", "fit_model <- function(data, formula = mpg ~ wt) {", "  lm(formula, data = data) # a comment", "}")
  ), format = "file"),
  tar_target(report_qmd, write_lines(
    "report.qmd",
    c("---", "title: \"A Quarto file\"", "---", "", "```{r}", "summary(mtcars$mpg)", "```")
  ), format = "file"),
  tar_target(config_json, write_lines("config.json", c("{", '  "alpha": 0.1,', '  "tags": ["demo", "gallery"]', "}")), format = "file"),
  tar_target(config_yaml, write_lines("config.yaml", c("alpha: 0.1", "tags:", "  - demo", "  - gallery")), format = "file"),
  tar_target(run_log, write_lines("run.log", sprintf("[%03d] step done", 1:250)), format = "file"),
  tar_target(output_folder, {
    path <- gallery_file("folder")
    dir.create(path, showWarnings = FALSE)
    for (i in 1:3) writeLines("part", file.path(path, sprintf("part_%d.txt", i)))
    path
  }, format = "file"),
  tar_target(binary_file, {
    path <- gallery_file("data.bin")
    writeBin(as.raw(0:255), path)
    path
  }, format = "file"),
  tar_target(many_files, vapply(seq_len(120), function(i) {
    write_lines(sprintf("many_%03d.txt", i), sprintf("File %d of 120", i))
  }, character(1)), format = "file"),

  # Patterns: many branches, of values and of files.
  tar_target(rep, 1:20),
  tar_target(boot, cars_frame[sample(nrow(cars_frame), replace = TRUE), c("mpg", "wt")], pattern = map(rep)),
  tar_target(boot_files, {
    path <- gallery_file(sprintf("boot_%02d.txt", rep))
    writeLines(sprintf("Bootstrap %d: mean mpg %.2f", rep, mean(boot$mpg)), path)
    path
  }, pattern = map(rep, boot), format = "file")
))
