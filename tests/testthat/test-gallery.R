# The preview gallery (inst/examples/pipelines/targets_preview_gallery.R)
# has one target per kind of view: each must show its tabs, with no preview
# failing, and each file target's files their views.

gallery_tabs <- list(
  rows_columns = c("Rows", "Columns"),
  preview = "Preview",
  estimates_fit = c("Estimates", "Fit"),
  summary = "Summary",
  printed_only = character()
)

gallery_expected <- c(
  cars_frame = "rows_columns", wide_frame = "rows_columns", long_frame = "rows_columns",
  list_column_frame = "rows_columns", boot = "rows_columns",
  one_number = "preview", one_word = "preview", draws = "preview", counts = "preview",
  flags = "preview", species = "preview", words = "preview", named_scores = "preview",
  days = "preview", stamps = "preview", rep = "preview",
  scatter_plot = "preview", interactive_plot = "preview", table_widget = "preview", html_card = "preview",
  settings = "preview", long_list = "preview", experiment = "preview",
  fit = "estimates_fit", logistic = "estimates_fit", t_test = "estimates_fit", clusters = "estimates_fit",
  step_function = "summary",
  matrix_value = "printed_only", model_formula = "printed_only",
  chart_png = "preview", chart_jpg = "preview", chart_svg = "preview", chart_pdf = "preview",
  table_csv = "preview", table_tsv = "preview", report_html = "preview", notes_md = "preview",
  code_r = "preview", report_qmd = "preview", config_json = "preview", config_yaml = "preview",
  run_log = "preview", output_folder = "preview", binary_file = "preview", many_files = "preview",
  boot_files = "preview",
  sims = "rows_columns", sims_batch = "preview", by_cyl = "rows_columns", cyl_means = "rows_columns",
  moment_2 = "preview", moment_3 = "preview", fresh_draws = "preview", cran_page = "preview"
)

test_that("every target of the preview gallery shows its views", {
  skip_on_cran()
  skip_if_not_installed("broom")
  script <- system.file("examples", "pipelines", "targets_preview_gallery.R", package = "targetsboard")
  dir <- withr::local_tempdir()
  file.copy(script, file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  targets::tar_make(reporter = "silent")

  names <- targets::tar_manifest(fields = "name")$name
  expect_equal(setdiff(names, names(gallery_expected)), character()) # a new target needs its tabs here
  for (name in names) {
    target <- preview_target(name, "_targets")
    built <- build_preview(targets::tar_read_raw(name), target)
    expect_equal(names(built$views), c(gallery_tabs[[gallery_expected[[name]]]], "Printed"), label = name)
    expect_null(built$note, label = name)
    if (is_file_target(target)) {
      for (path in utils::head(targets::tar_read_raw(name), 3)) {
        html <- as.character(file_section_view(path, target))
        expect_no_match(html, "tb-preview-note", fixed = TRUE, label = path)
      }
    }
  }
})
