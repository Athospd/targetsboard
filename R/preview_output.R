is_html_file_path <- function(x) {
  any(unlist(Map(\(x) grepl(".html$", x), x)))
}
 
extract_html_file_path <- function(x) {
  x[unlist(Map(\(x) grepl(".html$", x), x))]
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
  render_fun <- shiny::renderPlot
  output_ui_fun <- shiny::plotOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}
 
#' @export
render_and_output_bundle.gg <- render_and_output_bundle.ggplot

#' @export
render_and_output_bundle.plotly <- function(x, ...) {
  render_fun <- plotly::renderPlotly
  output_ui_fun <- plotly::plotlyOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}
 
#' @export
render_and_output_bundle.character <- function(x, ...) {
  
  if(is_html_file_path(x)) {
    render_fun <- \(x) shiny::renderUI({
      x <- extract_html_file_path(x)[1]
      # shiny::addResourcePath("temp", fs::path_dir(fs::path_abs("inst/test_quarto/train_test_report.html")))
      tagList(
        shiny::p(x),
        tags$iframe(src= f("root/{x}"),
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
  render_fun <- \(x) shiny::renderPrint({
    skimr::skim(x)
  })
  output_ui_fun <- shiny::verbatimTextOutput
  return(list(render_fun = render_fun, output_ui_fun = output_ui_fun))
}