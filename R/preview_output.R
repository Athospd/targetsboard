is_html_file_path <- function(x) {
  any(grepl("[.]html?$", x, ignore.case = TRUE) & file.exists(x))
}

extract_html_file_path <- function(x) {
  x[grepl("[.]html?$", x, ignore.case = TRUE) & file.exists(x)]
}

# Serve only the folder of this one report (so a non-self-contained Quarto
# report still finds its _files/ assets) under its own URL prefix, and
# return the URL of the report.
serve_html_report <- function(path) {
  path <- normalizePath(path, winslash = "/")
  report_dir <- dirname(path)
  prefix <- paste0("report_", substr(digest::digest(report_dir), 1, 12))
  shiny::addResourcePath(prefix, report_dir)
  paste0(prefix, "/", utils::URLencode(basename(path)))
}

#' How the board previews a target's value
#'
#' The value preview, opened by a double-click on a target, shows its value
#' in a frame: a header with the target's class, size, format and build time,
#' and one tab per *view* of the value. `tar_board_preview()` gives the views:
#' an S3 generic, so a class gets its own preview with a method.
#'
#' A method returns one view, or a named list of views shown as tabs in that
#' order. A view is HTML (a tag, a tag list or an htmlwidget) or a plot,
#' wrapped in [tar_board_preview_plot()]. `NULL` means no view of its own.
#' The frame always adds a last tab, "Printed", with what `print()` shows,
#' and falls back to it alone when a method errors.
#'
#' @section Files:
#' The value of a file target (`format = "file"`) is its paths. The board
#' shows each file in its own section, 50 at a time, and builds a file's view
#' only when its section opens: `tar_board_preview()` is called with the
#' path, of class `tar_board_file_<extension>` (lower case) and
#' `tar_board_file`. The board's method shows images, PDF and HTML files,
#' CSV and TSV files as tables, Markdown, and the first lines of text files,
#' with R code in colour. A method for, say, `tar_board_file_parquet` adds a
#' type.
#'
#' @section Where methods come from:
#' A package registers its methods as usual, with `S3method()` in its
#' NAMESPACE (roxygen2's `@exportS3Method targetsboard::tar_board_preview`).
#' For a project's own classes, put the methods in a file `_targetsboard.R`
#' in the board's folder, beside `_targets.R`. The board sources that file,
#' and only that file, when it starts and on Refresh, and registers the
#' `tar_board_preview.<class>()` functions it defines. A method there
#' replaces the board's own, or a package's, for the same class, and is
#' dropped when it is removed from the file. The pipeline itself never runs
#' inside the board.
#'
#' @param x The target's value, as [targets::tar_read()] returns it.
#' @param target What the board knows about the target, from the store: a
#'   list with `name`, `format`, `bytes` (its size in the store) and `time`
#'   (when it was built). For a pattern, also `pattern`: `branches` (how
#'   many), `branch` (the one shown, or 0 for all of them combined),
#'   `branch_name` and `total_bytes`; `bytes`, `format` and `time` are then
#'   the branch's.
#' @param ... For future arguments.
#' @return A view, a named list of views, or `NULL`.
#' @examples
#' # In _targetsboard.R, for values of class "bootstrap_summary":
#' tar_board_preview.bootstrap_summary <- function(x, target, ...) {
#'   list(
#'     Estimates = htmltools::tags$pre(format(x$estimates)),
#'     Draws = tar_board_preview_plot(function() hist(x$draws))
#'   )
#' }
#' @export
tar_board_preview <- function(x, target, ...) {
  # A method from the project's _targetsboard.R comes first, also for a
  # class the board has its own method for: R would find the board's by name
  # in this namespace before any registered one.
  method <- project_preview_method(x)
  if (!is.null(method)) return(method(x, target, ...))
  UseMethod("tar_board_preview")
}

# The methods of the project's _targetsboard.R, by class: replaced in full
# each time the file is read.
project_methods <- new.env(parent = emptyenv())

project_preview_method <- function(x) {
  if (!length(ls(project_methods))) return(NULL)
  for (class in .class2(x)) {
    if (exists(class, envir = project_methods, inherits = FALSE)) return(get(class, envir = project_methods))
  }
  NULL
}

clear_project_methods <- function() rm(list = ls(project_methods), envir = project_methods)

#' A plot, as a view of a value
#'
#' For [tar_board_preview()] methods: the board draws `plot` at the size of
#' the preview, again when it is resized or made full-screen.
#'
#' @param plot A plot to draw: a ggplot or another object that draws when
#'   printed, or a function that draws (base or grid graphics).
#' @return A view for [tar_board_preview()].
#' @examples
#' # A function that draws, with base graphics: the board calls it to draw.
#' view <- tar_board_preview_plot(function() hist(rnorm(100)))
#' class(view)
#'
#' # A ggplot: the board prints it to draw.
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'   view <- tar_board_preview_plot(plot)
#' }
#' @export
tar_board_preview_plot <- function(plot) {
  structure(list(plot = plot), class = "tar_board_preview_plot")
}

draw_preview_plot <- function(view) {
  if (is.function(view$plot)) view$plot() else print(view$plot)
  invisible()
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.ggplot <- function(x, target, ...) {
  if (!rlang::is_installed("ggplot2")) return(NULL)
  tar_board_preview_plot(x)
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.gg <- tar_board_preview.ggplot

#' @rdname tar_board_preview
#' @export
tar_board_preview.htmlwidget <- function(x, target, ...) x

#' @rdname tar_board_preview
#' @export
tar_board_preview.character <- function(x, target, ...) {
  if (!is_html_file_path(x)) {
    if (!is_plain_vector(x) || !length(x)) return(NULL)
    return(vector_view(x))
  }
  # A path to an HTML report, from a target that isn't a file target.
  path <- extract_html_file_path(x)[1]
  shiny::tagList(
    file_facts(path),
    shiny::tags$iframe(class = "tb-preview-document", src = serve_html_report(path), title = basename(path))
  )
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.shiny.tag <- function(x, target, ...) x

#' @rdname tar_board_preview
#' @export
tar_board_preview.shiny.tag.list <- function(x, target, ...) x
