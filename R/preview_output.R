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

#' @export
render_and_output_bundle <- function(x, ...) UseMethod("render_and_output_bundle")

#' @export
render_and_output_bundle.default <- function(x, ...) {
  render_fun <- \(x) shiny::renderPrint({
    x
  })
  output_ui_fun <- shiny::verbatimTextOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}

#' @export
render_and_output_bundle.ggplot <- function(x, ...) {
  if (!rlang::is_installed("ggplot2")) return(render_and_output_bundle.default(x, ...))
  render_fun <- shiny::renderPlot
  output_ui_fun <- shiny::plotOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}
 
#' @export
render_and_output_bundle.gg <- render_and_output_bundle.ggplot

#' @export
render_and_output_bundle.plotly <- function(x, ...) {
  if (!rlang::is_installed("plotly")) return(render_and_output_bundle.default(x, ...))
  render_fun <- plotly::renderPlotly
  output_ui_fun <- plotly::plotlyOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}
 
#' @export
render_and_output_bundle.character <- function(x, ...) {
  
  if(is_html_file_path(x)) {
    render_fun <- \(x) shiny::renderUI({
      x <- extract_html_file_path(x)[1]
      tagList(
        shiny::p(x),
        tags$iframe(src = serve_html_report(x),
                    width = '100%', height = '800px', 
                    frameborder = 0, scrolling = 'auto')
      )
    })
    output_ui_fun <- shiny::htmlOutput
  } else {
    render_fun <- shiny::renderPrint
    output_ui_fun <- shiny::verbatimTextOutput
  }
  
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}


#' @export
render_and_output_bundle.shiny.tag <- function(x, ...) {
  render_fun <- shiny::renderUI
  output_ui_fun <- shiny::uiOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}

#' @export
render_and_output_bundle.data.frame <- function(x, ...) {
  if (!rlang::is_installed("skimr")) return(render_and_output_bundle.default(x, ...))
  render_fun <- \(x) shiny::renderPrint({
    skimr::skim(x)
  })
  output_ui_fun <- shiny::verbatimTextOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}