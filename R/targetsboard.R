#' DAG widget for Shiny
#'
#' The interactive targets DAG used by the board, as a Shiny input you can
#' place in your own app. Nodes are coloured by status, running targets show a
#' spinner and their incoming edges are animated.
#'
#' The widget reports the ids of the selected targets to the server as
#' `input$<inputId>_selected_nodes` (and the running ones as
#' `input$<inputId>_dispatched_nodes`).
#'
#' @param inputId The input slot of the widget.
#' @param nodes,edges The `nodes` and `edges` data frames of
#'   `targets::tar_visnetwork()$x`.
#' @param height,width CSS size of the widget.
#'
#' @return `targetsboard()` returns a Shiny UI element.
#'   `update_targetsboard()` is called for its side effect.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' net <- targets::tar_visnetwork(targets_only = TRUE)$x
#'
#' ui <- fluidPage(targetsboard("dag", net$nodes, net$edges, height = "600px"))
#'
#' server <- function(input, output, session) {
#'   observe({
#'     invalidateLater(2000)
#'     net <- targets::tar_visnetwork(targets_only = TRUE)$x
#'     update_targetsboard(session, "dag", net$nodes, net$edges, height = "600px")
#'   })
#'   observe(print(input$dag_selected_nodes))
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
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
    configuration = board_configuration(inputId, nodes, edges, height, width),
    container = function(...) htmltools::tags$div(style = 'height: 100%', ...)
  )
}

#' @rdname targetsboard
#' @param session The Shiny session.
#' @export
update_targetsboard <- function(session, inputId, nodes, edges, height = "100%", width = "100%") {
  session$sendInputMessage(inputId, list(
    value = 0,
    configuration = board_configuration(inputId, nodes, edges, height, width)
  ))
}

# The widget's whole configuration: an update replaces it, so both the
# constructor and the update send all of it.
board_configuration <- function(inputId, nodes, edges, height, width) {
  list(
    id = inputId, # the widget names its Shiny inputs after it
    nodes = dataframe_to_reactflow_node_data(nodes),
    edges = dataframe_to_reactflow_edge_data(edges),
    height = height,
    width = width,
    background_color = "#f0e5d7",
    background_marks_color = "#2f223d"
  )
}

#' Convert tar_visnetwork() nodes to React Flow nodes
#' @noRd
dataframe_to_reactflow_node_data <- function(dataframe) {

  nodes_global_params <- list(
    `deletable?` = TRUE
  )

  dataframe |>
    dplyr::select(
      c("id", "description", "type", "status", "level", "label", "color")
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

#' Convert tar_visnetwork() edges to React Flow edges
#' @noRd
dataframe_to_reactflow_edge_data <- function(dataframe) {
  dataframe |>
    dplyr::mutate(
      id = f("{from}_{arrows}_{to}")
    ) |>
    dplyr::select(id, source = from, target = to) |>
    purrr::transpose()
}

#' Launch the targets board
#'
#' `tar_board()` starts the board in a background R process and returns at
#' once, so your session stays free to run `tar_make()` and friends while the
#' board follows along. `tar_board_app()` returns the board as a Shiny app
#' object instead, to run in the foreground or deploy.
#'
#' @param script Path to the targets script.
#' @param port Port to serve the board on, at `http://127.0.0.1:<port>`.
#' @param targets_only Show only targets, without functions and globals.
#' @param metadata Optional: `targets::tar_visnetwork()$x` for the first
#'   render. Computed from `script` when `NULL`.
#' @param options Passed on to [shiny::shinyApp()], e.g. `list(port = 8080)`.
#' @param ... Passed on to [targets::tar_visnetwork()] for the first render.
#'
#' @return `tar_board()`: the background process, a [callr::r_bg()] object.
#'   Use `$is_alive()` and `$kill()` on it, and read the board's log with
#'   `readLines(app$get_output_file())`.
#'   `tar_board_app()`: a [shiny::shinyApp()] object.
#'
#' @examples
#' \dontrun{
#' app <- tar_board()
#' browseURL("http://127.0.0.1:9999")
#' targets::tar_make()
#' app$kill()
#'
#' # or in the foreground:
#' shiny::runApp(tar_board_app())
#' }
#' @export
tar_board <- function(script = "_targets.R", port = 9999, targets_only = TRUE, metadata = NULL, ...) {
  # Computed here, so that a broken script errors in your session at once.
  if (is.null(metadata))
    metadata <- targets::tar_visnetwork(script = script, targets_only = targets_only, ...)$x

  log_file <- tempfile("targetsboard_", fileext = ".log")
  bg_process <- callr::r_bg(
    \(metadata, script, port, targets_only) {
      print(targetsboard::tar_board_app(
        script = script,
        targets_only = targets_only,
        metadata = metadata,
        options = list(port = port)
      ))
    },
    args = list(
      metadata = metadata,
      script = script,
      port = port,
      targets_only = targets_only
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = log_file,
    stderr = "2>&1"
  )
  message("targetsboard log: ", log_file)
  invisible(bg_process)
}

#' @rdname tar_board
#' @export
tar_board_app <- function(script = "_targets.R", targets_only = TRUE, metadata = NULL, options = list(), ...) {
  if (is.null(metadata))
    metadata <- targets::tar_visnetwork(script = script, targets_only = targets_only, ...)$x

  shiny::shinyApp(
    app_ui(metadata, script),
    app_server(metadata, script, targets_only),
    options = options
  )
}

#' Poll tar_visnetwork() in a background process
#'
#' Writes the result to an RDS file whenever the graph changes.
#'
#' @param script Path to the targets script.
#' @param interval Seconds to wait between two `tar_visnetwork()` calls.
#' @param ... Passed on to [targets::tar_visnetwork()].
#' @return A list: the `process` ([callr::r_bg()]) and `tar_vis_path`.
#' @noRd
tar_visnetwork_bg <- function(script, interval = 1, ...) {
  tar_vis_path <- tempfile("tar_visnetwork_", fileext = ".rds")

  tar_visnetwork_instance <- \(script, tar_vis_path, interval, ...) {
    previous <- NULL
    while (TRUE) {
      tar_vis_obj <- targets::tar_visnetwork(script = script, ...)
      # Only write when the graph changed, so readers aren't woken for nothing.
      if (!identical(tar_vis_obj$x, previous)) {
        # Write to a temp file and rename it into place, so the app never
        # reads a half-written file. On Windows the rename fails while the
        # app holds the file open: then retry on the next iteration.
        tmp <- paste0(tar_vis_path, ".tmp")
        saveRDS(tar_vis_obj, file = tmp)
        if (file.rename(tmp, tar_vis_path)) {
          previous <- tar_vis_obj$x
        } else {
          unlink(tmp)
        }
      }
      Sys.sleep(interval)
    }
  }

  bg_process <- callr::r_bg(
    tar_visnetwork_instance,
    args = list(script = script, tar_vis_path = tar_vis_path, interval = interval, ...),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = sub("[.]rds$", ".log", tar_vis_path),
    stderr = "2>&1"
  )
  invisible(list(process = bg_process, tar_vis_path = tar_vis_path))
}
