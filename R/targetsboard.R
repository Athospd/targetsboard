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
        id = inputId,
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
  if (!is.null(configuration)) {
    configuration$id <- inputId # the widget names its Shiny inputs after it
    message$configuration <- configuration
  }
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

#' tar_board
#'
#' Launches the board in a background R process. Its output goes to a log
#' file: read it with `readLines(app$get_output_file())`.
#'
#' @export
tar_board <- function(script = "_targets.R", metadata = NULL, port = 9999, targets_only = TRUE, ...) {
  if(is.null(metadata))
    metadata <- targets::tar_visnetwork(script = script, targets_only = targets_only, ...)$x

  log_file <- tempfile("targetsboard_", fileext = ".log")
  bg_process <- callr::r_bg(
    \(metadata, script, port, targets_only) {
      print(shiny::shinyApp(
        targetsboard::app_ui(metadata, script),
        targetsboard::app_server(metadata, script, targets_only),
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

#' tar_visnetwork_bg
#'
#' @param script Path to the targets script.
#' @param interval Seconds to wait between two `tar_visnetwork()` calls.
#' @param ... Passed on to [targets::tar_visnetwork()].
#'
#' @export
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
