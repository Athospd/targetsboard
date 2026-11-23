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
#' @section Where methods come from:
#' A package registers its methods as usual, with `S3method()` in its
#' NAMESPACE (roxygen2's `@exportS3Method targetsboard::tar_board_preview`).
#' For a project's own classes, put the methods in a file `_targetsboard.R`
#' in the board's folder, beside `_targets.R`. The board sources that file,
#' and only that file, when it starts and on Refresh, and registers the
#' `tar_board_preview.<class>()` functions it defines. A method there
#' replaces the board's own for the same class. The pipeline itself never
#' runs inside the board.
#'
#' @param x The target's value, as [targets::tar_read()] returns it.
#' @param target What the board knows about the target, from the store: a
#'   list with `name`, `format`, `bytes` (its size in the store) and `time`
#'   (when it was built).
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
tar_board_preview <- function(x, target, ...) UseMethod("tar_board_preview")

#' A plot, as a view of a value
#'
#' For [tar_board_preview()] methods: the board draws `plot` at the size of
#' the preview, again when it is resized or made full-screen.
#'
#' @param plot A plot to draw: a ggplot or another object that draws when
#'   printed, or a function that draws (base or grid graphics).
#' @return A view for [tar_board_preview()].
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
tar_board_preview.default <- function(x, target, ...) NULL

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
  if (!is_html_file_path(x)) return(NULL)
  path <- extract_html_file_path(x)[1]
  shiny::tagList(
    shiny::tags$p(path),
    shiny::tags$iframe(
      src = serve_html_report(path), width = "100%", height = "800px",
      frameborder = 0, scrolling = "auto"
    )
  )
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.shiny.tag <- function(x, target, ...) x

#' @rdname tar_board_preview
#' @export
tar_board_preview.shiny.tag.list <- function(x, target, ...) x

#' @rdname tar_board_preview
#' @export
tar_board_preview.data.frame <- function(x, target, ...) {
  if (!rlang::is_installed("skimr")) return(NULL)
  list(Summary = shiny::tags$pre(printed_text(skimr::skim(x))))
}
