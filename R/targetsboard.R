#' DAG widget for Shiny
#'
#' The interactive targets DAG used by the board, as a Shiny input you can
#' place in your own app. Targets are coloured by status, running targets show
#' a spinner and their incoming edges are animated, and targets whose last
#' build raised warnings carry a warning mark.
#'
#' Clicking a target focuses it: what it depends on and what depends on it are
#' highlighted, and the rest of the graph dims. Clicking the background clears
#' the focus.
#'
#' The widget reports to the server, as Shiny inputs:
#' - `input$<inputId>_focused_node`: the id of the focused target, or `NULL`;
#' - `input$<inputId>_selected_nodes`: the ids of the selected targets
#'   (several with Shift or Ctrl);
#' - `input$<inputId>_running_nodes`: the ids of the running targets.
#'
#' @param inputId The input slot of the widget.
#' @param nodes,edges The `nodes` and `edges` data frames of
#'   `targets::tar_visnetwork()$x`.
#' @param height,width CSS size of the widget.
#' @param warnings Ids of the targets whose last build raised warnings, as in
#'   the `warnings` field of `targets::tar_meta()`.
#'
#' `update_targetsboard()` replaces the whole graph. Targets already drawn keep
#' their place; new ones are placed next to their neighbours, and the
#' "Tidy" buttons lay the whole graph out again. `update_targetsboard_status()` only changes
#' the status of some targets, which is much lighter: use it to follow a
#' running pipeline.
#'
#' @return `targetsboard()` returns a Shiny UI element.
#'   `update_targetsboard()` and `update_targetsboard_status()` are called for
#'   their side effect.
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
#'   observe(print(input$dag_focused_node))
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#' @export
targetsboard <- function(inputId, nodes, edges, height = "100%", width = "100%", warnings = character()) {
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
    configuration = board_configuration(inputId, nodes, edges, height, width, warnings),
    container = function(...) htmltools::tags$div(style = 'height: 100%', ...)
  )
}

#' @rdname targetsboard
#' @param session The Shiny session.
#' @export
update_targetsboard <- function(session, inputId, nodes, edges, height = "100%", width = "100%",
                                warnings = character()) {
  session$sendInputMessage(inputId, list(
    value = 0,
    configuration = board_configuration(inputId, nodes, edges, height, width, warnings)
  ))
}

#' @rdname targetsboard
#' @param statuses A named character vector: target statuses, as in
#'   `targets::tar_progress()`, named by target. Targets not in the widget are
#'   ignored.
#' @param warnings For `update_targetsboard_status()`: `NULL` leaves the warning
#'   marks as they are; otherwise the ids of all the targets that carry one.
#' @export
update_targetsboard_status <- function(session, inputId, statuses = character(), warnings = NULL) {
  stopifnot(is.character(statuses), !is.null(names(statuses)) || length(statuses) == 0)
  message <- list(
    id = inputId,
    nodes = unname(Map(
      function(id, status) list(id = id, type = status_to_node_type(status), target_status = status),
      names(statuses), unname(statuses)
    ))
  )
  if (!is.null(warnings)) message$warnings <- I(as.character(warnings)) # always an array
  session$sendCustomMessage("targetsboard:status", message)
}

# The widget's whole configuration: an update replaces it, so both the
# constructor and the update send all of it.
board_configuration <- function(inputId, nodes, edges, height, width, warnings = character()) {
  list(
    id = inputId, # the widget names its Shiny inputs after it
    nodes = dataframe_to_reactflow_node_data(nodes, warnings),
    edges = dataframe_to_reactflow_edge_data(edges),
    height = height,
    width = width,
    background_color = "#f0e5d7",
    background_marks_color = "#2f223d"
  )
}

#' Convert tar_visnetwork() nodes to React Flow nodes
#' @importFrom rlang .data
#' @noRd
dataframe_to_reactflow_node_data <- function(dataframe, warnings = character()) {

  nodes_global_params <- list(
    `deletable?` = TRUE
  )

  dataframe |>
    dplyr::select(
      c("id", "description", "type", "status", "level", "label", "color")
    ) |>
    dplyr::rowwise() |>
    dplyr::summarise(
      id = .data$id,
      position = list(list(x = 0, y = 0)),
      # `data` must come before `type` is overwritten: later expressions in
      # summarise() see the new columns, also through .data.
      data = list(list(
        label = .data$label,
        level = .data$level,
        description = .data$description,
        target_type = .data$type,
        target_status = .data$status,
        warning = .data$id %in% warnings
      )),
      type = status_to_node_type(.data$status) # picks the node component
    ) |>
    dplyr::mutate(
      !!!nodes_global_params
    ) |>
    purrr::transpose()
}

# The widget has one node component per status the board shows: uptodate,
# outdated, running and errored. Map every status targets reports
# (tar_visnetwork() and tar_progress()) onto one of them; anything unknown
# shows as outdated. Warnings are not a status: see the warning mark.
status_to_node_type <- function(status) {
  types <- c(
    uptodate = "uptodate", completed = "uptodate", skipped = "uptodate",
    warned = "uptodate", # older targets reported warnings as a status
    outdated = "outdated", canceled = "outdated", queued = "outdated",
    none = "outdated",
    errored = "errored",
    dispatched = "running"
  )
  type <- unname(types[status])
  type[is.na(type)] <- "outdated"
  type
}

#' Convert tar_visnetwork() edges to React Flow edges
#' @noRd
dataframe_to_reactflow_edge_data <- function(dataframe) {
  dataframe |>
    dplyr::mutate(
      id = paste(.data$from, .data$arrows, .data$to, sep = "_")
    ) |>
    dplyr::select(dplyr::all_of(c(id = "id", source = "from", target = "to"))) |>
    purrr::transpose()
}

# targets::tar_visnetwork(), which the board relies on, needs visNetwork,
# but targets only suggests it: so we import it ourselves.
#' @importFrom visNetwork visNetwork
NULL

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
  # Else the background board dies at once on "address already in use" and
  # the page at the port is whatever was there, often an older board.
  if (port_in_use(port)) {
    stop(
      "Port ", port, " is already in use, maybe by a board you started before: ",
      "`$kill()` it, or pick another `port`.",
      call. = FALSE
    )
  }
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

#' Is something already listening on this port?
#'
#' Tries to connect rather than to bind: on Windows, binding succeeds even
#' while another process listens on 127.0.0.1 at that port.
#' @noRd
port_in_use <- function(port) {
  con <- tryCatch(
    suppressWarnings(socketConnection("127.0.0.1", port, open = "r+b", timeout = 1)),
    error = function(e) NULL
  )
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
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

#' Recompute tar_visnetwork() in a background process, only when needed
#'
#' `tar_visnetwork()` hashes the whole pipeline to tell outdated targets from
#' up-to-date ones, which is slow. So the process only recomputes it when:
#' - the script, or an R file under `R/`, changed (new targets or functions),
#' - a run ended: the progress file changed since the last recompute, has not
#'   changed for `settle` seconds, and the pipeline process is gone,
#' - the app asked for it, by writing to `refresh_path`.
#' Between recomputes, the app follows a run through the cheap progress file.
#'
#' Each recompute is written to `tar_vis_path` as an RDS file holding
#' `list(x = tar_visnetwork()$x, progress_mtime = )`: the modification time of
#' the progress file it reflects, so the app knows which progress is newer.
#'
#' @param script Path to the targets script.
#' @param interval Seconds between two checks; checks are cheap file stats.
#' @param settle Seconds without progress before a run counts as ended.
#' @param ... Passed on to [targets::tar_visnetwork()].
#' @return A list: the `process` ([callr::r_bg()]), `tar_vis_path` and
#'   `refresh_path`.
#' @noRd
tar_visnetwork_bg <- function(script, interval = 0.5, settle = 1, ...) {
  tar_vis_path <- tempfile("tar_visnetwork_", fileext = ".rds")
  refresh_path <- sub("[.]rds$", ".refresh", tar_vis_path)

  # Runs in the background process: self-contained, it can't call this
  # package's internals.
  tar_visnetwork_instance <- function(script, tar_vis_path, refresh_path, interval, settle, ...) {
    progress_file <- file.path(targets::tar_config_get("store"), "meta", "progress")

    code_signature <- function() {
      files <- c(script, sort(list.files("R", pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)))
      list(
        files = files,
        mtimes = file.mtime(files),
        refresh = if (file.exists(refresh_path)) readLines(refresh_path, warn = FALSE)
      )
    }
    # Is the pipeline process targets recorded still running? (A copy of
    # active_run(): a killed run leaves targets marked dispatched for good.)
    running <- function() {
      info <- tryCatch(targets::tar_process(), error = function(e) NULL)
      if (is.null(info)) return(FALSE)
      pid <- suppressWarnings(as.integer(info$value[info$name == "pid"]))
      created <- as.POSIXct(info$value[info$name == "created"], tz = "UTC")
      if (length(pid) != 1 || is.na(pid) || length(created) != 1 || is.na(created)) return(FALSE)
      handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
      if (is.null(handle)) return(FALSE)
      started <- tryCatch(ps::ps_create_time(handle), error = function(e) NA)
      isTRUE(tryCatch(ps::ps_is_running(handle), error = function(e) FALSE)) &&
        !is.na(started) &&
        abs(as.numeric(difftime(started, created, units = "secs"))) <= 2
    }

    computed_code <- NULL
    computed_progress <- NULL # progress mtime of the last recompute
    seen_progress <- NULL     # and the one last seen, since when
    seen_since <- Sys.time()

    while (TRUE) {
      code <- code_signature()
      progress_mtime <- file.mtime(progress_file)
      if (!identical(progress_mtime, seen_progress)) {
        seen_progress <- progress_mtime
        seen_since <- Sys.time()
      }
      run_ended <- !is.null(computed_progress) &&
        !identical(progress_mtime, computed_progress) &&
        difftime(Sys.time(), seen_since, units = "secs") >= settle &&
        !running()

      if (!identical(code, computed_code) || run_ended) {
        x <- tryCatch(
          targets::tar_visnetwork(script = script, ...)$x,
          error = function(e) {
            message("tar_visnetwork() failed: ", conditionMessage(e))
            NULL
          }
        )
        # Write to a temp file and rename it into place, so the app never
        # reads a half-written file. On Windows the rename fails while the
        # app holds the file open: then retry on the next check.
        written <- is.null(x) || {
          tmp <- paste0(tar_vis_path, ".tmp")
          saveRDS(list(x = x, progress_mtime = progress_mtime), file = tmp)
          file.rename(tmp, tar_vis_path) || { unlink(tmp); FALSE }
        }
        # A broken script is retried once it changes again, not every check.
        if (written) {
          computed_code <- code
          computed_progress <- progress_mtime
        }
      }
      Sys.sleep(interval)
    }
  }

  bg_process <- callr::r_bg(
    tar_visnetwork_instance,
    args = list(
      script = script, tar_vis_path = tar_vis_path, refresh_path = refresh_path,
      interval = interval, settle = settle, ...
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = sub("[.]rds$", ".log", tar_vis_path),
    stderr = "2>&1"
  )
  invisible(list(process = bg_process, tar_vis_path = tar_vis_path, refresh_path = refresh_path))
}
