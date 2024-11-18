# #' targetsboard
# #'
# #' targetsboard widget
# #'
# #' @import htmlwidgets
# #'
# #' @export
# targetsboard <- function(nodes, edges, ..., width = NULL, height = NULL) {
#   # describe a React component to send to the browser for rendering.
#   component <- reactR::component(
#     name = "ReactFlow",
#     varArgs = list(
#       nodes_ = nodes,
#       edges_ = edges,
#       onNodesChange=list(),
#       onEdgesChange=list(),
#       onConnect=list(),
#       ...
#     )
#   )

#   # create widget
#   htmlwidgets::createWidget(
#     name = 'targetsboard',
#     reactR::reactMarkup(component),
#     width = width,
#     height = height,
#     package = 'targetsboard'
#   )
# }

#' Called by HTMLWidgets to produce the widget's root element.
#' @noRd
widget_html.targetsboard <- function(id, style, class, ...) {
  htmltools::tagList(
    # Necessary for RStudio viewer version < 1.2
    reactR::html_dependency_corejs(),
    reactR::html_dependency_react(),
    reactR::html_dependency_reacttools(),
    htmltools::tags$div(id = id, class = class, style = style)
  )
}

#' Shiny bindings for targetsboard
#'
#' Output and render functions for using targetsboard within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a targetsboard
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name targetsboard-shiny
#'
#' @export
targetsboardOutput <- function(outputId, width = '100%', height = '100%'){
  htmlwidgets::shinyWidgetOutput(outputId, 'targetsboard', width, height, package = 'targetsboard')
}

#' @rdname targetsboard-shiny
#' @export
rendertargetsboard <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, targetsboardOutput, env, quoted = TRUE)
}

#' targetsboard
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#'
#' @export
targetsboard <- function(inputId, nodes, edges, height = "100%", width = "100%") {
  reactR::createReactShinyInput(
    inputId = inputId,
    class = "targetsboard",
    dependencies = htmltools::htmlDependency(
      name = "targetsboard",
      version = "1.0.0",
      src = "htmlwidgets/",
      package = "targetsboard",
      script = "targetsboard.js"
    ),
    default = 0,
    configuration = c(
      list(
        nodes = nodes,
        edges = edges
      ),
      list(
        height = height,
        width = width,
        background_color = "#f0e5d7",
        background_marks_color = "#2f223d"
      )
    ),
    container = function(...) htmltools::tags$div(style = 'height: 100%', ...)
  )
}

#' updatetargetsboardInput
#'
#' @export
update_targetsboard <- function(session, inputId, value, configuration = NULL) {
  message <- list(value = value)
  if (!is.null(configuration)) message$configuration <- configuration
  session$sendInputMessage(inputId, message);
}


#' dataframe_to_reactflow_node_data
#'
#' @export
dataframe_to_reactflow_node_data <- function(dataframe) {

  nodes_global_params <- list(
    `deletable?` = TRUE
  )

  dataframe |>
    dplyr::select(
      c("id", "description", "type", "status", "status", "level", "label", "color")
    ) |>
    dplyr::rowwise() |>
    dplyr::summarise(
      id = id,
      position = list(list(x = 0, y = 0)),
      type = status,
      # style = list(list(background = color, color = "white")),
      data = list(list(
        label = label,
        level = level,
        description = description,
        target_type = type,
        target_status = status
      ))
    ) |>
    dplyr::mutate(
      !!!nodes_global_params
    ) |>
    purrr::transpose()
}

#' dataframe_to_reactflow_edge_data
#'
#' @export
dataframe_to_reactflow_edge_data <- function(dataframe) {
  dataframe |>
    dplyr::mutate(
      id = f("{from}_{arrows}_{to}")
    ) |>
    dplyr::select(id, source = from, target = to) |>
    purrr::transpose()
}

#' @export
tar_board <- function(metadata, script = "_targets.R", port = 9999) {
  bg_process <- callr::r_bg(
    \(metadata, script, port) {
      print(shiny::shinyApp(
        targetsboard::app_ui(metadata, script), 
        targetsboard::app_server(metadata), 
        options = list(port = port)
      ))
    },
    args = list(
      metadata = metadata, 
      script = script, 
      port = port
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = "|",
    stderr = "|"
  )
  invisible(bg_process)
}

#' @export
tar_visnetwork_bg <- function(script, ...) {
  tar_vis_tempdir <- tempdir()
  tar_vis_path <- fs::path(tar_vis_tempdir, "tar_visnetwork.rds")

  tar_visnetwork_instance <- \(script, tar_vis_tempdir, tar_vis_path, ...) {
    while(TRUE) {
      tar_vis_obj <- targets::tar_visnetwork(script = script, ...)
      saveRDS(tar_vis_obj, file = tar_vis_path)
      
      saveRDS(1, file = fs::path(tar_vis_tempdir, sample(c("a", "b", "c"), 1)))
    }
  }

  bg_process <- callr::r_bg(
    tar_visnetwork_instance,
    args = list(script = script, tar_vis_tempdir = tar_vis_tempdir, tar_vis_path = tar_vis_path, ...),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = "|",
    stderr = "|"
  )
  invisible(list(process = bg_process, tar_vis_path = tar_vis_path))
}
