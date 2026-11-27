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
#' @section Projects:
#' A `_targets.yaml` can define several projects, each with its own script
#' and store, and the `TAR_PROJECT` environment variable chooses one
#' (`"main"` unless set): see the targets manual's chapter on projects. The
#' board starts on that project, or on `project`, and a Project field left of
#' the Script field switches it, when `_targets.yaml` has more than one
#' project or the board isn't on `"main"`. Loading another project's script
#' switches to that project too. The board's project is its own: your R
#' session's `TAR_PROJECT` is never changed, and the Script tab gives the
#' line to make it follow.
#'
#' @section Large pipelines:
#' The page keeps every target on the graph in memory, so very large graphs
#' can make the browser slow or crash the tab. When a script loaded from the
#' Script field has more than 500 targets on its graph, the board asks
#' before drawing it, and Cancel keeps the current graph. Set the limit with
#' `options(targetsboard.confirm_nodes = )` before starting the board, e.g.
#' `Inf` never to ask.
#'
#' @section Runs:
#' Run uses [targets::tar_make()], unless the script sets up future
#' (`future::plan()`) or clustermq (`options(clustermq.scheduler = )`): then
#' [targets::tar_make_future()] or [targets::tar_make_clustermq()], and the
#' run summary says which. Their workers are a literal `workers =` in the
#' `plan()` call, or 1; set them with `options(targetsboard.workers = )`.
#' The script is read for these calls, never run.
#'
#' @section Your editor:
#' Started from RStudio, Positron (with the rstudioapi package) or VS Code's
#' terminal (with its `code` command), a target's right-click menu has "Open
#' in editor": its `tar_*()` call, or a function's definition, in the script
#' or the `R/` folder, opened at its line. The board runs in the background,
#' so it asks this session to open it: this works while the session is idle
#' at its prompt (it needs the later package). Without an editor, the menu
#' has no such item.
#'
#' @section Value previews:
#' A double-click on a target opens its value in a frame, with a tab per view
#' of it and a last tab with what `print()` shows. A class gets its own views
#' with a [tar_board_preview()] method, from a package or from the project's
#' `_targetsboard.R`. A value larger than 100 MB in the store is read only
#' when asked; set the limit with `options(targetsboard.preview_bytes = )`.
#'
#' @section Themes:
#' The board's look is the "Targets board" theme. A few Bootstrap themes
#' from Bootswatch, checked for readable text in light and dark mode, can
#' replace it: pick one in the top bar, and the browser remembers it. A dark
#' theme, such as Darkly, keeps the page in dark mode and hides the switch.
#' To start the board with a theme, whatever the browser remembers, set
#' `options(targetsboard.theme = )` to its name, e.g. `"Cosmo"`.
#'
#' @param script Path to the targets script. Defaults to the project's, as
#'   `targets::tar_config_get("script", project = project)` gives it.
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
#' @param scripts Optional: other targets scripts to switch to, offered in the
#'   board's Script field. The field lists them under `script` and filters
#'   them as you type; only these can be chosen. Pick one and press Load.
#' @param project The project of `_targets.yaml` to start on. Defaults to
#'   the `TAR_PROJECT` environment variable, or `"main"`.
#' @param ... Passed on to [targets::tar_visnetwork()] on every redraw of
#'   `script`'s graph, e.g. `names = starts_with("model")`, `allow`,
#'   `exclude`, `shortcut = TRUE`, `degree_from`. tidyselect expressions are
#'   kept as written. Another script loaded from the board is drawn whole.
#'
#' @return `tar_board()`: the background process, a [callr::r_bg()] object,
#'   invisibly. Use `$is_alive()` and `$kill()` on it, read the board's log
#'   with `readLines(app$get_output_file())`, and reopen the board with
#'   `browseURL(attr(app, "url"))`.
#'   `tar_board_app()`: a [shiny::shinyApp()] object.
#'
#' @examples
#' if (interactive()) {
#' app <- tar_board()                 # opens the board in your browser
#' targets::tar_make()                # the board follows along
#' browseURL(attr(app, "url"))        # reopen it after closing the tab
#' app$kill()
#'
#' # switch between scripts from the board's Script field:
#' tar_board(scripts = c("_targets.R", "pipelines/other.R"))
#'
#' # or in the foreground:
#' shiny::runApp(tar_board_app())
#' }
#' @export
tar_board <- function(script = targets::tar_config_get("script", project = project),
                      port = getOption("shiny.port", targets::tar_random_port()),
                      targets_only = TRUE, metadata = NULL,
                      host = getOption("shiny.host", "127.0.0.1"),
                      browse = TRUE, verbose = TRUE, scripts = NULL,
                      project = Sys.getenv("TAR_PROJECT", "main"), ...) {
  if (!nzchar(project)) project <- "main"
  check_project(project) # in your session, before anything starts
  script_choices(script, scripts) # check them in your session
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
  filters <- visnetwork_filters(...)
  if (is.null(metadata)) metadata <- with_project(project, initial_metadata(script, targets_only, filters))
  board_theme_option() # an unknown theme errors here, not in the board

  log_file <- tempfile("targetsboard_", fileext = ".log")
  # Your IDE, if any: the board asks this session to open definitions in it.
  opener <- editor_opener()
  editor <- if (!is.null(opener) && requireNamespace("later", quietly = TRUE)) {
    tempfile("targetsboard_editor_", fileext = ".rds")
  }
  bg_process <- callr::r_bg(
    \(metadata, script, scripts, project, host, port, targets_only, dev, confirm_nodes, preview_bytes, theme, filters,
      editor) {
      options(
        targetsboard.dev = dev, targetsboard.confirm_nodes = confirm_nodes,
        targetsboard.preview_bytes = preview_bytes, targetsboard.theme = theme,
        targetsboard.editor = editor
      )
      # The filters as arguments again: values, or expressions for tidyselect.
      print(do.call(targetsboard::tar_board_app, c(list(
        script = script,
        scripts = scripts,
        project = project,
        targets_only = targets_only,
        metadata = metadata,
        options = list(host = host, port = port)
      ), filters)))
    },
    args = list(
      metadata = metadata,
      script = script,
      scripts = scripts,
      project = project,
      host = host,
      port = port,
      targets_only = targets_only,
      dev = board_dev_mode(),
      confirm_nodes = confirm_nodes_limit(),
      preview_bytes = preview_bytes_limit(),
      theme = getOption("targetsboard.theme"),
      filters = filters,
      editor = editor
    ),
    supervise = TRUE,
    poll_connection = TRUE,
    stdout = log_file,
    stderr = "2>&1"
  )
  if (!is.null(editor)) editor_bridge(editor, opener, alive = bg_process$is_alive)
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
initial_metadata <- function(script, targets_only = TRUE, filters = list()) {
  if (!file.exists(script)) {
    message("No targets script at ", script, " yet: the board will draw it once it exists.")
    return(empty_metadata())
  }
  x <- do.call(targets::tar_visnetwork, c(list(script = script, targets_only = targets_only), filters))$x
  x$nodes <- clear_invalidated_errors(x$nodes, recorded_errors())
  x
}

# tar_visnetwork()'s filters, as given to tar_board() or tar_board_app():
# each as its value where it evaluates in the caller (`shortcut = TRUE`,
# `names = my_names`), else as its expression (`names = starts_with("model")`,
# `names = c(a, b)`), for tidyselect in each tar_visnetwork() of the poller,
# another process. A named list, for do.call().
visnetwork_filters <- function(...) {
  lapply(rlang::enquos(...), function(quo) {
    tryCatch(rlang::eval_tidy(quo), error = function(e) rlang::quo_get_expr(quo))
  })
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
tar_board_app <- function(script = targets::tar_config_get("script", project = project), targets_only = TRUE,
                          metadata = NULL, options = list(), scripts = NULL,
                          project = Sys.getenv("TAR_PROJECT", "main"), ...) {
  if (!nzchar(project)) project <- "main"
  check_project(project)
  script_choices(script, scripts)
  filters <- visnetwork_filters(...)
  if (is.null(metadata)) metadata <- with_project(project, initial_metadata(script, targets_only, filters))

  shiny::shinyApp(
    # A function of the request: each page load opens the board with the
    # layout saved last, not the one there was when the app started.
    function(request) app_ui(metadata, script, layout = read_board_layout(), scripts = scripts),
    app_server(metadata, script, targets_only, scripts, project, filters),
    # The board's project, in the process that serves it, for as long as it
    # runs: then TAR_PROJECT is as it was, for a board run in your session.
    onStart = function() {
      old <- Sys.getenv("TAR_PROJECT", NA)
      set_active_project(project)
      shiny::onStop(function() {
        if (is.na(old)) Sys.unsetenv("TAR_PROJECT") else Sys.setenv(TAR_PROJECT = old)
      })
    },
    options = options
  )
}
