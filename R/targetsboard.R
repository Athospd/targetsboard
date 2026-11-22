#' DAG widget for Shiny
#'
#' The interactive targets DAG used by the board, as a Shiny input you can
#' place in your own app. Targets are coloured by status, running targets show
#' a spinner and their incoming edges are animated, and targets whose last
#' build raised warnings carry a warning mark.
#'
#' Clicking a target focuses it: what it depends on and what depends on it are
#' highlighted, and the rest of the graph dims. Clicking the background, or
#' Escape, clears the focus and the selection.
#'
#' Several targets can be selected: Ctrl-click (Cmd-click on a Mac) adds one,
#' and Shift-drag selects those in a box. A group of two or more stays
#' highlighted with all it depends on and all that depends on it. A
#' right-click on one of the group opens the group's menu
#' (`node_group_actions`), and a toolbar over the group holds
#' `toolbar_group_actions`; a right-click on any other target selects that one
#' alone.
#'
#' The widget reports to the server, as Shiny inputs:
#' - `input$<inputId>_focused_node`: the id of the focused target, or `NULL`;
#' - `input$<inputId>_selected_nodes`: the ids of the selected targets;
#' - `input$<inputId>_running_nodes`: the ids of the running targets;
#' - `input$<inputId>_double_clicked_node`: the id of a target double-clicked;
#' - `input$<inputId>_context_node`: the id of a target right-clicked, which
#'   also focuses it;
#' - `input$<inputId>_node_action`: `list(action, node, nodes)`, when an
#'   action of a right-click menu or of a toolbar is chosen (see
#'   `node_actions`, `toolbar_actions` and their group versions in
#'   [targetsboard_options()]): `node` is the target it was chosen on, `nodes`
#'   every target it covers, `node` alone or the selected group;
#' - `input$<inputId>_viewport`: `list(x, y, zoom)`, after each pan or zoom;
#' - `input$<inputId>_image`: `list(format, data_url)`, the image asked for by
#'   [targetsboard_export_image()], or `list(format, error)`;
#' - `input$<inputId>_layout`: `list(direction, viewport, positions)`, where
#'   the targets are (`positions`, named by target, each `list(x, y)`), the
#'   view (`list(x, y, zoom)`) and the direction of the last layout (`"LR"` or
#'   `"TB"`): once the targets are laid out, and after each drag, pan or zoom.
#'   Give it back as `layout` to draw the graph as it was.
#'
#' The event inputs (double-click, right-click, action) fire again for the
#' same target, so `observeEvent()` sees every one.
#'
#' @param inputId The input slot of the widget.
#' @param nodes,edges The `nodes` and `edges` data frames of
#'   `targets::tar_visnetwork()$x`.
#' @param height,width CSS size of the widget.
#' @param warnings Ids of the targets whose last build raised warnings, as in
#'   the `warnings` field of `targets::tar_meta()`.
#' @param layout Optional: a layout the widget reported as
#'   `input$<inputId>_layout`. The targets in it are drawn where they were, in
#'   its direction, and the others next to their neighbours; when it has them
#'   all, its view is restored too, else the graph is fitted into view. The
#'   "Tidy" buttons lay the graph out afresh.
#' @param options Display options, from [targetsboard_options()]. For
#'   `update_targetsboard()`, `NULL` keeps the current ones.
#'
#' `update_targetsboard()` replaces the whole graph. Targets already drawn keep
#' their place; new ones are placed next to their neighbours, and the
#' "Tidy" buttons lay the whole graph out again. `update_targetsboard_status()` only changes
#' the status of some targets, which is much lighter: use it to follow a
#' running pipeline. `update_targetsboard_options()` changes only the display
#' options it names, e.g. `color_mode` to follow `bslib::input_dark_mode()`.
#' `update_targetsboard_focus()` focuses a target from the server, and
#' `targetsboard_fit_view()` brings targets into view: together, they can show
#' a target picked elsewhere in the app. `update_targetsboard_selection()`
#' selects targets from the server.
#'
#' @return `targetsboard()` returns a Shiny UI element.
#'   The update functions and `targetsboard_fit_view()` are called for their
#'   side effect.
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
targetsboard <- function(inputId, nodes, edges, height = "100%", width = "100%", warnings = character(),
                         options = targetsboard_options(), layout = NULL) {
  if (!is.null(layout) && !is_board_layout(layout)) {
    rlang::abort("`layout` must be a layout the widget reported as `input$<inputId>_layout`.")
  }
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
    configuration = board_configuration(inputId, nodes, edges, height, width, warnings, options, layout),
    container = function(...) htmltools::tags$div(style = 'height: 100%', ...)
  )
}

#' @rdname targetsboard
#' @param session The Shiny session.
#' @export
update_targetsboard <- function(session, inputId, nodes, edges, height = "100%", width = "100%",
                                warnings = character(), options = NULL) {
  session$sendInputMessage(inputId, list(
    value = 0,
    configuration = board_configuration(inputId, nodes, edges, height, width, warnings, options)
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

#' @rdname targetsboard
#' @param node For `update_targetsboard_focus()`: the id of the target to
#'   focus and select alone, as a click does, or `NULL` to clear the focus
#'   and the selection.
#' @export
update_targetsboard_focus <- function(session, inputId, node = NULL) {
  if (!is.null(node) && !rlang::is_string(node)) {
    rlang::abort("`node` must be one target id, or NULL.")
  }
  send_command(session, inputId, "focus", node = node)
}

#' @rdname targetsboard
#' @param nodes For `targetsboard_fit_view()`: ids of the targets to fit in
#'   view; `NULL`, all of them. For `update_targetsboard_selection()`: ids of
#'   the targets to select, instead of those selected; `NULL` clears the
#'   selection. Ids not in the widget are ignored.
#' @param padding For `targetsboard_fit_view()`: room around them, as a
#'   fraction of the view.
#' @param duration For `targetsboard_fit_view()`: length of the animation, in
#'   milliseconds.
#' @param max_zoom For `targetsboard_fit_view()`: the most it zooms in, so a
#'   single target doesn't fill the view.
#' @export
targetsboard_fit_view <- function(session, inputId, nodes = NULL, padding = 0.2,
                                  duration = 400, max_zoom = 1.5) {
  if (!is.null(nodes) && !is.character(nodes)) {
    rlang::abort("`nodes` must be a character vector of target ids, or NULL.")
  }
  stopifnot(rlang::is_scalar_double(padding) || rlang::is_scalar_integer(padding))
  stopifnot(rlang::is_scalar_double(duration) || rlang::is_scalar_integer(duration))
  stopifnot(rlang::is_scalar_double(max_zoom) || rlang::is_scalar_integer(max_zoom))
  send_command(
    session, inputId, "fit_view",
    nodes = if (!is.null(nodes)) I(nodes), # always an array
    padding = padding, duration = duration, max_zoom = max_zoom
  )
}

#' @rdname targetsboard
#' @export
update_targetsboard_selection <- function(session, inputId, nodes = NULL) {
  if (!is.null(nodes) && !is.character(nodes)) {
    rlang::abort("`nodes` must be a character vector of target ids, or NULL.")
  }
  send_command(session, inputId, "select", nodes = I(as.character(nodes))) # always an array
}

#' Images of the DAG widget's graph
#'
#' `targetsboard_export_image()` asks the widget for an image of its whole
#' graph, whatever is in view. The widget sends it back as
#' `input$<inputId>_image`: `list(format, data_url)`, or `list(format, error)`
#' if it could not draw it. `save_targetsboard_image()` writes that image to a
#' file.
#'
#' @param session The Shiny session.
#' @param inputId The input slot of the widget.
#' @param format `"png"` or `"svg"`.
#' @param image `input$<inputId>_image`.
#' @param path File to write.
#' @return `targetsboard_export_image()` is called for its side effect.
#'   `save_targetsboard_image()` returns `path`, invisibly.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   observeEvent(input$snapshot, targetsboard_export_image(session, "dag"))
#'   observeEvent(input$dag_image, save_targetsboard_image(input$dag_image, "dag.png"))
#' }
#' }
#' @export
targetsboard_export_image <- function(session, inputId, format = c("png", "svg")) {
  send_command(session, inputId, "export_image", format = rlang::arg_match(format))
}

#' @rdname targetsboard_export_image
#' @export
save_targetsboard_image <- function(image, path) {
  if (!is.null(image$error)) rlang::abort(paste("The widget could not draw the image:", image$error))
  url <- image$data_url
  if (!rlang::is_string(url) || !startsWith(url, "data:")) {
    rlang::abort("`image` must be the widget's `<inputId>_image` input.")
  }
  comma <- regexpr(",", url, fixed = TRUE)
  header <- substr(url, 1, comma - 1)
  payload <- substr(url, comma + 1, nchar(url))
  if (endsWith(header, ";base64")) {
    writeBin(jsonlite::base64_dec(payload), path)
  } else {
    writeBin(url_decode(payload), path)
  }
  invisible(path)
}

# Percent-decoding into bytes, vectorised: utils::URLdecode() steps through
# the string in R, and never finished on a graph's SVG, megabytes long.
url_decode <- function(x) {
  bytes <- charToRaw(x)
  pct <- which(bytes == charToRaw("%"))
  pct <- pct[pct + 2 <= length(bytes)]
  if (length(pct) == 0) return(bytes)
  # The value of a hex digit: 0-9 from its code, a-f and A-F from code %% 32.
  digit <- function(b) {
    code <- as.integer(b)
    ifelse(code <= 57L, code - 48L, code %% 32L + 9L)
  }
  bytes[pct] <- as.raw(16L * digit(bytes[pct + 1]) + digit(bytes[pct + 2]))
  bytes[-c(pct + 1, pct + 2)]
}

# A command to the widget: one custom message type, picked by `command`.
# NULL arguments are left out (as JSON, NULL would become an empty object):
# the widget reads a missing one as null.
send_command <- function(session, inputId, command, ...) {
  args <- Filter(Negate(is.null), list(...))
  session$sendCustomMessage(
    "targetsboard:command",
    c(list(id = inputId, command = command), args)
  )
}

#' @rdname targetsboard
#' @param ... For `update_targetsboard_options()`: the options to change, as
#'   arguments of [targetsboard_options()].
#' @export
update_targetsboard_options <- function(session, inputId, ...) {
  changed <- list(...)
  if (length(changed) == 0) return(invisible())
  stopifnot(!is.null(names(changed)), all(nzchar(names(changed))))
  options <- do.call(targetsboard_options, changed) # validates them
  session$sendCustomMessage(
    "targetsboard:options",
    list(id = inputId, options = unclass(options)[names(changed)])
  )
}

#' Display options of the DAG widget
#'
#' @param color_mode `"light"`, `"dark"`, or `"system"` to follow the
#'   browser's preference. The board's own beige background is for light mode.
#' @param minimap Show the minimap, with targets coloured by status.
#' @param controls Show the zoom controls.
#' @param background The background pattern: `"dots"`, `"lines"`, `"cross"`
#'   or `"none"`.
#' @param edge_type How edges are drawn: `"default"` (curves),
#'   `"smoothstep"`, `"step"`, `"straight"` or `"simplebezier"`.
#' @param only_render_visible Render only the targets in view, which keeps big
#'   pipelines responsive.
#' @param node_actions The right-click menu of a target: a named character
#'   vector of labels, named by action. Choosing one sets
#'   `input$<inputId>_node_action` to `list(action = , node = , nodes = )`.
#'   The widget does some actions itself, without telling the server:
#'   `copy_name` copies the target's name to the clipboard, quoted as an R
#'   string (`"model"`), and `select_upstream`, `select_downstream` and
#'   `select_lineage` select it with all it depends on, all that depends on
#'   it, or both. Empty, the default: no menu.
#' @param toolbar_actions Buttons over the focused target, as `node_actions`:
#'   a named character vector of labels, named by action, reported the same
#'   way. Empty, the default: no toolbar.
#' @param node_group_actions,toolbar_group_actions The menu and the toolbar of
#'   a selected group of two or more targets, as `node_actions` and
#'   `toolbar_actions`; `{n}` in a label is the number of targets. `copy_name`
#'   copies their names as an R vector, `c("a", "b")`, and the selection
#'   actions start from all of them. Empty, the default: no menu or toolbar for
#'   a group.
#' @param download_image Add a button to the zoom controls (shown with
#'   `controls`) that downloads an image of the whole graph, as PNG.
#' @return A list of options for [targetsboard()].
#' @export
targetsboard_options <- function(color_mode = c("light", "dark", "system"),
                                 minimap = TRUE,
                                 controls = TRUE,
                                 background = c("dots", "lines", "cross", "none"),
                                 edge_type = c("default", "smoothstep", "step", "straight", "simplebezier"),
                                 only_render_visible = FALSE,
                                 node_actions = character(),
                                 toolbar_actions = character(),
                                 node_group_actions = character(),
                                 toolbar_group_actions = character(),
                                 download_image = FALSE) {
  flag <- function(x) {
    if (!rlang::is_bool(x)) {
      rlang::abort(sprintf("`%s` must be TRUE or FALSE.", deparse(substitute(x))))
    }
    x
  }
  structure(
    list(
      color_mode = rlang::arg_match(color_mode),
      minimap = flag(minimap),
      controls = flag(controls),
      background = rlang::arg_match(background),
      edge_type = rlang::arg_match(edge_type),
      only_render_visible = flag(only_render_visible),
      node_actions = action_items(node_actions),
      toolbar_actions = action_items(toolbar_actions, "toolbar_actions"),
      node_group_actions = action_items(node_group_actions, "node_group_actions"),
      toolbar_group_actions = action_items(toolbar_group_actions, "toolbar_group_actions"),
      download_image = flag(download_image)
    ),
    class = "targetsboard_options"
  )
}

# A named character vector of labels, named by action, as the widget's list
# of {id, label} items.
action_items <- function(actions, arg = "node_actions") {
  if (length(actions) == 0) return(list())
  ids <- names(actions)
  if (!is.character(actions) || is.null(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    rlang::abort(sprintf("`%s` must be a character vector of labels, named by unique actions.", arg))
  }
  unname(Map(function(id, label) list(id = id, label = label), ids, unname(actions)))
}

# The widget's whole configuration: an update replaces it, so both the
# constructor and the update send all of it, except the options, which an
# update may leave out to keep the current ones.
board_configuration <- function(inputId, nodes, edges, height, width, warnings = character(),
                                options = NULL, layout = NULL) {
  config <- list(
    id = inputId, # the widget names its Shiny inputs after it
    nodes = dataframe_to_reactflow_node_data(nodes, warnings),
    edges = dataframe_to_reactflow_edge_data(edges),
    height = height,
    width = width,
    background_color = "#f0e5d7",
    background_marks_color = "#2f223d"
  )
  if (!is.null(options)) {
    stopifnot(inherits(options, "targetsboard_options"))
    config$options <- unclass(options)
  }
  # Read once, when the widget is first drawn: updates leave it out.
  config$layout <- layout
  config
}

#' Convert tar_visnetwork() nodes to React Flow nodes
#' @importFrom rlang .data
#' @noRd
dataframe_to_reactflow_node_data <- function(dataframe, warnings = character()) {

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
    # The board is read-only: no target can be deleted from it.
    dplyr::mutate(deletable = FALSE) |>
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
    dplyr::mutate(deletable = FALSE) |>
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
#' Like `targets::tar_watch()`, the board needs no arguments: it shows the
#' pipeline of the current project, with the script and store set in its
#' configuration (`_targets.R` and `_targets/` unless `_targets.yaml`, or the
#' `TAR_CONFIG` and `TAR_PROJECT` environment variables, say otherwise). The
#' graph comes from the script: the store only records the targets' statuses.
#' Without a script yet, the board starts empty and draws the pipeline once the
#' script appears.
#'
#' Also like `targets::tar_watch()`, `tar_board()` serves the board on a random
#' free port, waits until it is up, opens it in your browser and prints its
#' url.
#'
#' @param script Path to the targets script. Defaults to the project's, as
#'   `targets::tar_config_get("script")` gives it.
#' @param port Port to serve the board on. Defaults to the `shiny.port` option
#'   or, without one, a random free port.
#' @param host Host to serve the board on. Defaults to the `shiny.host` option
#'   or, without one, `"127.0.0.1"`: only your own machine can reach it.
#' @param browse Open the board in your browser once it is up.
#' @param verbose Show a spinner while the board starts, and print its url.
#' @param targets_only Show only targets, without functions and globals.
#' @param metadata Optional: `targets::tar_visnetwork()$x` for the first
#'   render. Computed from `script` when `NULL`, and empty when there is no
#'   script yet.
#' @param options Passed on to [shiny::shinyApp()], e.g. `list(port = 8080)`.
#' @param ... Passed on to [targets::tar_visnetwork()] for the first render.
#'
#' @return `tar_board()`: the background process, a [callr::r_bg()] object,
#'   invisibly. Use `$is_alive()` and `$kill()` on it, read the board's log
#'   with `readLines(app$get_output_file())`, and reopen the board with
#'   `browseURL(attr(app, "url"))`.
#'   `tar_board_app()`: a [shiny::shinyApp()] object.
#'
#' @examples
#' \dontrun{
#' app <- tar_board()                 # opens the board in your browser
#' targets::tar_make()                # the board follows along
#' browseURL(attr(app, "url"))        # reopen it after closing the tab
#' app$kill()
#'
#' # or in the foreground:
#' shiny::runApp(tar_board_app())
#' }
#' @importFrom parallelly freePort
#' @export
tar_board <- function(script = targets::tar_config_get("script"),
                      port = getOption("shiny.port", targets::tar_random_port()),
                      targets_only = TRUE, metadata = NULL,
                      host = getOption("shiny.host", "127.0.0.1"),
                      browse = TRUE, verbose = TRUE, ...) {
  # Else the background board dies at once on "address already in use" and
  # the page at the port is whatever was there, often an older board.
  if (port_in_use(port, host)) {
    stop(
      "Port ", port, " is already in use, maybe by a board you started before: ",
      "`$kill()` it, or pick another `port`.",
      call. = FALSE
    )
  }
  # Computed here, so that a broken script errors in your session at once.
  if (is.null(metadata)) metadata <- initial_metadata(script, targets_only, ...)

  log_file <- tempfile("targetsboard_", fileext = ".log")
  bg_process <- callr::r_bg(
    \(metadata, script, host, port, targets_only) {
      print(targetsboard::tar_board_app(
        script = script,
        targets_only = targets_only,
        metadata = metadata,
        options = list(host = host, port = port)
      ))
    },
    args = list(
      metadata = metadata,
      script = script,
      host = host,
      port = port,
      targets_only = targets_only
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = log_file,
    stderr = "2>&1"
  )
  url <- board_url(host, port)
  attr(bg_process, "url") <- url

  if (browse) {
    wait_for_board(bg_process, host, port, log_file, verbose)
    open_url(url)
  }
  if (verbose) {
    cli::cli_ul()
    cli::cli_li("url: {.url {url}}")
    cli::cli_li("log: {.path {log_file}}")
    cli::cli_end()
  }
  invisible(bg_process)
}

# The address to browse: a board listening on every interface is reached
# through this machine's own address.
board_url <- function(host, port) {
  if (host %in% c("0.0.0.0", "::")) host <- "127.0.0.1"
  paste0("http://", host, ":", port)
}

#' Wait until the board answers on its port
#'
#' As `targets::tar_watch()` does: spin until the port takes connections, and
#' fail with the board's log if its process dies first.
#' @noRd
wait_for_board <- function(process, host, port, log_file, verbose = TRUE) {
  spinner <- cli::make_spinner(template = "{spin} Starting the board")
  while (!port_in_use(port, host)) {
    if (!process$is_alive()) {
      if (verbose) spinner$finish()
      log <- if (file.exists(log_file)) readLines(log_file, warn = FALSE)
      stop(
        "The board stopped before it was up. Its log:
",
        paste(utils::tail(log, 20), collapse = "
"),
        call. = FALSE
      )
    }
    if (verbose) spinner$spin()
    Sys.sleep(0.05)
  }
  if (verbose) spinner$finish()
  invisible(TRUE)
}

# A seam for the tests: they can't mock utils::browseURL() itself.
open_url <- function(url) utils::browseURL(url)

#' The graph for the board's first render
#'
#' `targets::tar_visnetwork()$x` of the script, or an empty graph when there is
#' no script yet: the board then waits for one, as `targets::tar_watch()` does.
#' A script that exists but is broken still errors.
#' @noRd
initial_metadata <- function(script, targets_only = TRUE, ...) {
  if (!file.exists(script)) {
    message("No targets script at ", script, " yet: the board will draw it once it exists.")
    return(empty_metadata())
  }
  targets::tar_visnetwork(script = script, targets_only = targets_only, ...)$x
}

# The shape of tar_visnetwork()$x, without targets.
empty_metadata <- function() {
  list(
    nodes = data.frame(
      id = character(), label = character(), description = character(),
      type = character(), status = character(), level = integer(),
      color = character()
    ),
    edges = data.frame(from = character(), to = character(), arrows = character())
  )
}

#' Is something already listening on this port?
#'
#' Tries to connect rather than to bind: on Windows, binding succeeds even
#' while another process listens on 127.0.0.1 at that port.
#' @noRd
port_in_use <- function(port, host = "127.0.0.1") {
  if (host %in% c("0.0.0.0", "::")) host <- "127.0.0.1"
  con <- tryCatch(
    suppressWarnings(socketConnection(host, port, open = "r+b", timeout = 1)),
    error = function(e) NULL
  )
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

#' @rdname tar_board
#' @export
tar_board_app <- function(script = targets::tar_config_get("script"), targets_only = TRUE,
                          metadata = NULL, options = list(), ...) {
  if (is.null(metadata)) metadata <- initial_metadata(script, targets_only, ...)

  shiny::shinyApp(
    # A function of the request: each page load opens the board with the
    # layout saved last, not the one there was when the app started.
    function(request) app_ui(metadata, script, layout = read_board_layout()),
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
#' `list(x = tar_visnetwork()$x, widget = tar_visnetwork(), progress_mtime = )`:
#' `widget` for the `tar_visnetwork` tab, and the modification time of the
#' progress file it reflects, so the app knows which progress is newer.
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
        widget <- tryCatch(
          targets::tar_visnetwork(script = script, ...),
          error = function(e) {
            message("tar_visnetwork() failed: ", conditionMessage(e))
            NULL
          }
        )
        # Write to a temp file and rename it into place, so the app never
        # reads a half-written file. On Windows the rename fails while the
        # app holds the file open: then retry on the next check.
        written <- is.null(widget) || {
          tmp <- paste0(tar_vis_path, ".tmp")
          saveRDS(list(x = widget$x, widget = widget, progress_mtime = progress_mtime), file = tmp)
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
