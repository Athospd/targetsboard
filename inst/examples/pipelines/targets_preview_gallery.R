# One target per kind of value the value preview shows: double-click each
# to see its preview. Used for the preview overhaul's checks and screenshots.
#
#   Sys.setenv(TAR_CONFIG = "inst/examples/pipelines/_targets.yaml", TAR_PROJECT = "preview_gallery")
#   targets::tar_make()
#   targetsboard::tar_board()
#
# Files are written inside the store, so they go with tar_destroy(). Targets
# that need ggplot2 or plotly are left out when those aren't installed.

library(targets)

gallery_file <- function(name) {
  dir <- file.path(tar_path_store(), "gallery")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.path(dir, name)
}

optional <- function(package, target) if (requireNamespace(package, quietly = TRUE)) target

Filter(Negate(is.null), list(
  # Data frames and vectors
  tar_target(cars_frame, transform(mtcars, model = rownames(mtcars), cyl = factor(cyl))),
  tar_target(wide_frame, as.data.frame(matrix(round(rnorm(200 * 30), 2), ncol = 30))),
  tar_target(one_number, 42.195),
  tar_target(draws, rnorm(10000, mean = 5, sd = 2)),
  tar_target(species, iris$Species),
  tar_target(named_scores, c(alpha = 0.91, beta = 0.78, gamma = 0.66)),
  tar_target(days, seq(as.Date("2026-01-01"), by = "week", length.out = 52)),

  # Plots and widgets
  optional("ggplot2", tar_target(
    scatter_plot,
    ggplot2::ggplot(cars_frame, ggplot2::aes(wt, mpg, colour = cyl)) + ggplot2::geom_point(size = 2)
  )),
  optional("plotly", tar_target(
    interactive_plot,
    plotly::plot_ly(cars_frame, x = ~wt, y = ~mpg, color = ~cyl, type = "scatter", mode = "markers")
  )),

  # Lists and models
  tar_target(settings, list(
    model = list(family = "gaussian", formula = "mpg ~ wt + hp", weights = NULL),
    folds = 1:10,
    tags = c("demo", "gallery"),
    nested = list(level_2 = list(level_3 = list(level_4 = "deep")))
  )),
  tar_target(fit, lm(mpg ~ wt + hp, data = cars_frame)),

  # Files
  tar_target(chart_png, {
    path <- gallery_file("chart.png")
    grDevices::png(path, width = 800, height = 500)
    plot(draws[1:500], type = "l", main = "A png file")
    grDevices::dev.off()
    path
  }, format = "file"),
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
  tar_target(report_html, {
    path <- gallery_file("report.html")
    writeLines("<html><body><h1>A report</h1><p>An HTML file, shown as a page.</p></body></html>", path)
    path
  }, format = "file"),
  tar_target(notes_md, {
    path <- gallery_file("notes.md")
    writeLines(c("# Notes", "", "A **markdown** file, with a list:", "", "- one", "- two"), path)
    path
  }, format = "file"),
  tar_target(code_r, {
    path <- gallery_file("model.R")
    writeLines(c("# An R file", "fit_model <- function(data) {", "  lm(mpg ~ wt, data = data) # a comment", "}"), path)
    path
  }, format = "file"),

  # Patterns: many branches, of values and of files
  tar_target(rep, 1:20),
  tar_target(boot, cars_frame[sample(nrow(cars_frame), replace = TRUE), c("mpg", "wt")], pattern = map(rep)),
  tar_target(boot_files, {
    path <- gallery_file(sprintf("boot_%02d.txt", rep))
    writeLines(sprintf("Bootstrap %d: mean mpg %.2f", rep, mean(boot$mpg)), path)
    path
  }, pattern = map(rep, boot), format = "file")
))
