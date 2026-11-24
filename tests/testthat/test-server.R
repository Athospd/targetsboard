test_that("one background poller is shared by all sessions", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  shiny::testServer(server, {
    rds_path <- backend_of(server)$rds_path()
    wait_for(function() file.exists(rds_path))

    session$elapse(600)
    expect_setequal(board()$x$nodes$id, c("a", "b", "c"))
  })

  pid <- poller_of(server)$process$get_pid()
  shiny::testServer(server, {
    expect_equal(poller_of(server)$process$get_pid(), pid)
  })

  process <- poller_of(server)$process
  backend_of(server)$stop_poller()
  expect_false(process$is_alive())
})

test_that("Load restarts the poller only for an existing, different script", {
  skip_on_cran()
  local_pipeline()
  file.copy("_targets.R", "other.R")
  server <- local_app_server()

  shiny::testServer(server, {
    pid <- poller_of(server)$process$get_pid()

    session$setInputs(script = "missing.R", load_script = 1)
    expect_equal(poller_of(server)$process$get_pid(), pid)

    session$setInputs(script = "_targets.R", load_script = 2)
    expect_equal(poller_of(server)$process$get_pid(), pid)

    session$setInputs(script = "other.R", load_script = 3)
    expect_false(poller_of(server)$process$get_pid() == pid)
    expect_equal(poller_of(server)$script, "other.R")
  })
})

test_that("Load with no script chosen asks for one, and keeps the board as it is", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()
  notes <- character()
  local_mocked_bindings(
    showNotification = function(ui, ...) notes[[length(notes) + 1]] <<- as.character(ui),
    .package = "shiny"
  )
  states <- character()
  local_mocked_bindings(
    update_task_button = function(id, ..., state = NULL) states[[length(states) + 1]] <<- paste(id, state),
    .package = "bslib"
  )

  shiny::testServer(server, {
    pid <- poller_of(server)$process$get_pid()
    # The field is empty once a typed text that matches no script is left.
    session$setInputs(script = "", load_script = 1)
    expect_equal(notes, "Choose a script to load.")
    expect_equal(states, "load_script ready")
    expect_equal(poller_of(server)$process$get_pid(), pid)
    expect_equal(poller_of(server)$script, "_targets.R")
  })
})

test_that("Load stays busy, and covers the board, until the new script's graph arrives or the script fails to load", {
  skip_on_cran()
  local_pipeline()
  writeLines(c("Sys.sleep(2)", "library(targets)", "list(tar_target(x, 1))"), "slow.R")
  writeLines("stop('broken pipeline')", "broken.R")
  server <- local_app_server()
  states <- character()
  local_mocked_bindings(
    update_task_button = function(id, ..., state = NULL) states[[length(states) + 1]] <<- paste(id, state),
    .package = "bslib"
  )
  spinner <- character()
  local_mocked_bindings(
    showSpinner = function(id, expr) spinner[[length(spinner) + 1]] <<- paste("show", id),
    hideSpinner = function(id, expr) spinner[[length(spinner) + 1]] <<- paste("hide", id),
    .package = "shinycssloaders"
  )

  shiny::testServer(server, {
    # Nothing to load: the button is ready again at once.
    session$setInputs(script = "missing.R", load_script = 1)
    expect_equal(states, "load_script ready")
    session$setInputs(script = "_targets.R", load_script = 2)
    expect_equal(states, rep("load_script ready", 2))

    # A script that takes a while: busy until its graph is drawn.
    session$setInputs(script = "slow.R", load_script = 3)
    session$elapse(600)
    expect_length(states, 2)
    # A spinner covers the board meanwhile, so the old graph can't be used.
    expect_equal(spinner, "show board_loading")
    expect_equal(output$board_loading_caption, "Loading pipeline\u2026") # no script name
    rds_path <- backend_of(server)$rds_path()
    wait_for(function() file.exists(rds_path))
    session$elapse(600)
    expect_equal(states[[3]], "load_script ready")
    expect_true("x" %in% board()$x$nodes$id)
    expect_equal(spinner, c("show board_loading", "hide board_loading"))

    # A script that doesn't load: ready again, and the error says why.
    session$setInputs(script = "broken.R", load_script = 4)
    session$elapse(600)
    expect_length(states, 3)
    expect_equal(output$board_loading_caption, "Loading pipeline\u2026")
    wait_for(function() file.exists(poller_error_path(backend_of(server)$rds_path())))
    session$elapse(600)
    expect_equal(states[[4]], "load_script ready")
    expect_equal(spinner, rep(c("show board_loading", "hide board_loading"), 2))
  })
})

test_that("Load asks before drawing a graph over the size limit, and Cancel keeps the current one", {
  skip_on_cran()
  local_pipeline() # a, b and c: under the limit
  writeLines(c("library(targets)", "list(tar_target(w, 1), tar_target(x, w), tar_target(y, x), tar_target(z, y))"), "big.R")
  writeLines(c("library(targets)", "list(tar_target(v1, 1), tar_target(v2, v1), tar_target(v3, v2), tar_target(v4, v3))"), "bigger.R")
  withr::local_options(targetsboard.confirm_nodes = 3)
  server <- local_app_server()
  modals <- list()
  local_mocked_bindings(
    showModal = function(ui, ...) modals[[length(modals) + 1]] <<- ui,
    removeModal = function(...) modals[[length(modals) + 1]] <<- "removed",
    .package = "shiny"
  )
  local_mocked_bindings(update_task_button = function(...) NULL, .package = "bslib")
  spinner <- character()
  local_mocked_bindings(
    showSpinner = function(id, expr) spinner[[length(spinner) + 1]] <<- "show",
    hideSpinner = function(id, expr) spinner[[length(spinner) + 1]] <<- "hide",
    .package = "shinycssloaders"
  )

  shiny::testServer(server, {
    wait_new_graph <- function() {
      rds_path <- backend_of(server)$rds_path()
      wait_for(function() file.exists(rds_path))
      session$elapse(600)
    }
    wait_new_graph() # the launch script's graph, as the poller recomputes it

    # Over the limit: held back, and the author is asked, with the count.
    session$setInputs(script = "big.R", load_script = 1)
    wait_new_graph()
    expect_length(modals, 1)
    expect_match(as.character(modals[[1]]), "4 targets")
    expect_false("w" %in% board()$x$nodes$id)
    expect_equal(spinner, "show")

    # Draw it anyway: drawn, and the spinner goes.
    session$setInputs(draw_large_graph = 1)
    expect_true(all(c("w", "z") %in% board()$x$nodes$id))
    expect_equal(spinner, c("show", "hide"))
    expect_identical(modals[[2]], "removed")

    # Another one over the limit, cancelled: the current graph stays, and
    # the board goes back to the script it showed.
    session$setInputs(script = "bigger.R", load_script = 2)
    wait_new_graph()
    expect_length(modals, 3)
    session$setInputs(cancel_large_graph = 1)
    expect_true("w" %in% board()$x$nodes$id)
    expect_false("v1" %in% board()$x$nodes$id)
    expect_equal(poller_of(server)$script, "big.R")
    expect_equal(spinner, c("show", "hide", "show", "hide"))

    # The cancelled script's graph never arrives later either.
    wait_new_graph()
    expect_false("v1" %in% board()$x$nodes$id)
  })
})

test_that("a double-click previews a target's value, then the preview follows the focus", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  shiny::testServer(server, {
    session$setInputs(flow_focused_node = "c")
    expect_error(output[["preview-content"]]) # focusing alone reads no value

    session$setInputs(flow_double_clicked_node = "c")
    expect_match(output[["preview-content"]]$html, '<pre class="tb-preview-printed">[1] 4</pre>', fixed = TRUE)

    # It follows the focus while open.
    session$setInputs(flow_focused_node = "b")
    expect_match(output[["preview-content"]]$html, '<pre class="tb-preview-printed">[1] 2</pre>', fixed = TRUE)

    # Clearing the focus closes it, and focusing again doesn't reopen it.
    session$setInputs(flow_focused_node = NULL)
    session$setInputs(flow_focused_node = "c")
    expect_error(output[["preview-content"]])

    # Nor does its own toggle, once closed by it.
    session$setInputs(flow_double_clicked_node = "c")
    expect_match(output[["preview-content"]]$html, "[1] 4", fixed = TRUE)
    session$setInputs(`preview-sidebar` = TRUE)
    session$setInputs(`preview-sidebar` = FALSE)
    session$setInputs(flow_focused_node = "b")
    expect_error(output[["preview-content"]])
  })
})

test_that("a target's menu runs it, invalidates it after confirmation, or previews it", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  ran <- list()
  modal <- NULL
  local_mocked_bindings(start_run = function(script, names = NULL, ...) {
    ran[[length(ran) + 1]] <<- names
    NULL
  })
  local_mocked_bindings(
    showModal = function(ui, ...) modal <<- as.character(ui),
    removeModal = function(...) NULL,
    showNotification = function(...) NULL,
    .package = "shiny"
  )

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(flow_node_action = list(action = "run", node = "b"))
    expect_equal(ran, list("b"))

    # Invalidates the target the menu was for, whatever is selected.
    session$setInputs(flow_selected_nodes = c("a", "c"))
    session$setInputs(flow_node_action = list(action = "invalidate", node = "b"))
    expect_match(modal, "<code>b</code>", fixed = TRUE)
    expect_no_match(modal, "<code>a</code>", fixed = TRUE)
    session$setInputs(invalidate_confirm = 1)
    meta <- targets::tar_meta(targets_only = TRUE)$name
    expect_false("b" %in% meta)
    expect_true(all(c("a", "c") %in% meta))

    session$setInputs(flow_focused_node = "a", flow_node_action = list(action = "value", node = "a"))
    expect_match(output[["preview-content"]]$html, '<pre class="tb-preview-printed">[1] 1</pre>', fixed = TRUE)
  })
})

test_that("the actions of a selected group cover every target of it", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  ran <- list()
  modal <- NULL
  local_mocked_bindings(start_run = function(script, names = NULL, ...) {
    ran[[length(ran) + 1]] <<- names
    NULL
  })
  local_mocked_bindings(
    showModal = function(ui, ...) modal <<- as.character(ui),
    removeModal = function(...) NULL,
    showNotification = function(...) NULL,
    .package = "shiny"
  )

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    # As the widget sends it: `nodes`, a JSON array, can arrive as a list.
    session$setInputs(flow_node_action = list(action = "run", node = "a", nodes = list("a", "c")))
    expect_equal(ran, list(c("a", "c")))

    session$setInputs(flow_node_action = list(action = "invalidate", node = "a", nodes = c("a", "c")))
    expect_match(modal, "<code>a</code>", fixed = TRUE)
    expect_match(modal, "<code>c</code>", fixed = TRUE)
    expect_no_match(modal, "<code>b</code>", fixed = TRUE)
    session$setInputs(invalidate_confirm = 1)
    expect_equal(targets::tar_meta(targets_only = TRUE)$name, "b")

    # A long list is cut short in the confirmation, but all are invalidated.
    many <- paste0("t", 1:12)
    session$setInputs(flow_node_action = list(action = "invalidate", node = "t1", nodes = many))
    expect_match(modal, "<code>t10</code>", fixed = TRUE)
    expect_no_match(modal, "<code>t11</code>", fixed = TRUE)
    expect_match(modal, "and 2 more", fixed = TRUE)
    expect_equal(to_invalidate(), many)
  })
})

test_that("Invalidate all asks for every target of the graph, then invalidates them", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  modal <- NULL
  local_mocked_bindings(
    showModal = function(ui, ...) modal <<- as.character(ui),
    removeModal = function(...) NULL,
    showNotification = function(...) NULL,
    .package = "shiny"
  )

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(invalidate_all = 1)
    for (name in c("a", "b", "c")) expect_match(modal, sprintf("<code>%s</code>", name), fixed = TRUE)
    expect_equal(nrow(targets::tar_meta(targets_only = TRUE)), 3) # not yet
    session$setInputs(invalidate_confirm = 1)
    expect_equal(nrow(targets::tar_meta(targets_only = TRUE)), 0)
  })
})

test_that("board_targets() leaves out the functions and globals of the graph", {
  nodes <- fake_nodes(c("raw", "clean_fun", "model"))
  nodes$type <- c("stem", "function", "pattern")
  expect_equal(board_targets(nodes), c("raw", "model"))
  expect_equal(board_targets(nodes[0, ]), character())
  expect_equal(board_targets(NULL), character())
})

test_that("the board's sidebar has Invalidate all, not the selection's buttons", {
  html <- as.character(app_ui(list(nodes = fake_nodes(), edges = fake_edges()), "_targets.R"))
  expect_match(html, 'id="invalidate_all"', fixed = TRUE)
  expect_match(html, "Invalidate all", fixed = TRUE)
  expect_no_match(html, 'id="run_selected"', fixed = TRUE)
  expect_no_match(html, "Invalidate selected", fixed = TRUE)
})

test_that("tar_meta() is re-read only when the metadata file changes", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  reads <- 0
  real_tar_meta <- targets::tar_meta
  local_mocked_bindings(
    tar_meta = function(...) {
      # Count the table's reads, all fields: a new graph also reads the
      # recorded errors (clear_invalidated_errors()), once per recompute.
      if (!"fields" %in% names(list(...))) reads <<- reads + 1
      real_tar_meta(...)
    },
    .package = "targets"
  )

  shiny::testServer(server, {
    invisible(output[["meta-table"]])
    before <- reads
    for (i in 1:3) {
      session$elapse(1000)
      invisible(output[["meta-table"]])
    }
    expect_equal(reads, before)

    Sys.sleep(1.1) # file times can have 1s resolution
    targets::tar_invalidate(a)
    targets::tar_make(reporter = "silent")
    session$elapse(1000)
    invisible(output[["meta-table"]])
    expect_equal(reads, before + 1)
  })
})

test_that("a run's progress reaches the widget as status-only updates", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  sent <- list()
  local_mocked_bindings(update_targetsboard_status = function(session, inputId, statuses, ...) {
    if (length(statuses)) sent[[length(sent) + 1]] <<- statuses # not only branch counts
  })

  shiny::testServer(server, {
    # Keep the graph as it is: only the progress file changes.
    backend_of(server)$stop_poller()
    session$flushReact() # the first flush, which a real app does at start
    targets::tar_make(reporter = "silent")
    session$elapse(600)

    expect_length(sent, 1)
    expect_equal(sent[[1]][c("a", "b", "c")], c(a = "completed", b = "completed", c = "completed"))
  })
})

test_that("a script that stops loading is notified until it loads again", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()
  shown <- list()
  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL) shown[[length(shown) + 1]] <<- list(id = id, ui = ui),
    removeNotification = function(id, ...) shown[[length(shown) + 1]] <<- list(id = id, ui = NULL),
    .package = "shiny"
  )

  shiny::testServer(server, {
    error_path <- poller_error_path(backend_of(server)$rds_path())
    writeLines("stop('broken pipeline')", "_targets.R")
    wait_for(function() file.exists(error_path))
    session$elapse(600)
    last <- shown[[length(shown)]]
    expect_equal(last$id, "script_error")
    expect_match(last$ui, "_targets.R doesn't load.*broken pipeline")

    writeLines(c("library(targets)", "list(tar_target(z, 1))"), "_targets.R")
    wait_for(function() !file.exists(error_path))
    session$elapse(600)
    last <- shown[[length(shown)]]
    expect_equal(last$id, "script_error")
    expect_null(last$ui)
  })
})

test_that("Refresh asks the poller to recompute", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  shiny::testServer(server, {
    path <- poller_of(server)$refresh_path
    expect_false(file.exists(path))
    session$setInputs(refresh = 1)
    expect_true(file.exists(path))
  })
})

test_that("targets whose last build raised warnings get a warning mark", {
  skip_on_cran()
  local_pipeline()
  writeLines(
    c(
      "library(targets)",
      "list(tar_target(a, {warning('careful'); 1}), tar_target(b, a + 1))"
    ),
    "_targets.R"
  )
  # In this process, so the warning is ours to silence rather than printed by
  # the background process tar_make() would start.
  suppressWarnings(targets::tar_make(reporter = "silent", callr_function = NULL))
  server <- local_app_server()

  shiny::testServer(server, {
    expect_equal(warned_targets(), "a")
  })
})

test_that("the tar_visnetwork tab shows tar_visnetwork()'s own widget", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    widget <- targets::tar_visnetwork(targets_only = TRUE)
    board(list(x = widget$x, widget = widget, progress_mtime = progress_mtime()))
    expect_match(output[["visnetwork-graph"]], "hierarchical", fixed = TRUE) # tar_visnetwork()'s layout
  })
})

test_that("the power button turns the board off", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  stopped <- NULL
  local_mocked_bindings(stopApp = function(returnValue = NULL) stopped <<- returnValue, .package = "shiny")

  shiny::testServer(server, {
    session$setInputs(turn_off = 1)
    expect_equal(stopped, "The board is turned off.")
  })
})

test_that("the graph follows the page's dark mode switch", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  sent <- list()
  local_mocked_bindings(update_targetsboard_options = function(session, inputId, ...) {
    sent[[length(sent) + 1]] <<- c(inputId = inputId, list(...))
  })

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(dark_mode = "dark")
    expect_equal(sent[[length(sent)]], list(inputId = "flow", color_mode = "dark"))
    session$setInputs(dark_mode = "light")
    expect_equal(sent[[length(sent)]]$color_mode, "light")
  })
})

test_that("a row clicked in the tar_meta() table shows its target on the board", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  calls <- list()
  record <- function(what) function(...) calls[[length(calls) + 1]] <<- list(what, list(...))
  local_mocked_bindings(
    update_targetsboard_focus = function(session, inputId, node) record("focus")(inputId, node),
    targetsboard_fit_view = function(session, inputId, nodes, ...) record("fit")(inputId, nodes)
  )
  local_mocked_bindings(
    nav_select = function(id, selected, ...) record("tab")(id, selected),
    .package = "bslib"
  )

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(`meta-row_clicked` = "b")
    expect_equal(calls, list(
      list("tab", list("view", "board")),
      list("focus", list("flow", "b")),
      list("fit", list("flow", c("b", "a", "c"))) # b, what it depends on, what depends on it
    ))
  })
})

test_that("the board writes the widget's layout into the store's user folder", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()
  layout <- list(
    direction = "TB", viewport = list(x = 5, y = 6, zoom = 0.75),
    positions = list(a = list(x = 0, y = 0), b = list(x = 0, y = 150), c = list(x = 0, y = 300))
  )
  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$setInputs(flow_layout = layout)
    expect_equal(read_board_layout(file.path("_targets", "user", "targetsboard_layout.json")), layout)
  })
})

test_that("a pattern carries the warning mark of its branches", {
  meta <- data.frame(
    name = c("raw", "slow", "slow_1", "slow_2", "model"),
    type = c("stem", "pattern", "branch", "branch", "stem"),
    parent = c(NA, NA, "slow", "slow", NA),
    warnings = c(NA, NA, "odd one", "odd two", "careful")
  )
  expect_equal(warned_targets_of(meta), c("model", "slow"))
  expect_equal(warned_targets_of(meta[c(1, 5), c("name", "warnings")]), "model") # without type
  expect_equal(warned_targets_of(NULL), character())
})

test_that("during a run, patterns show their branch counts; they go when it stops", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  writeLines(c(
    "library(targets)",
    "list(tar_target(nums, 1:4), tar_target(slow, {Sys.sleep(1); nums}, pattern = map(nums)))"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  server <- local_app_server()
  sent <- list()
  local_mocked_bindings(update_targetsboard_status = function(session, inputId, statuses = character(),
                                                              warnings = NULL, branches = NULL) {
    if (!is.null(branches)) sent[[length(sent) + 1]] <<- branches
  })

  shiny::testServer(server, {
    backend_of(server)$stop_poller()
    session$flushReact()
    session$setInputs(run_all = 1)
    process <- backend_of(server)$runner$process
    seen <- FALSE
    while (process$is_alive()) {
      Sys.sleep(0.3)
      session$elapse(600)
      last <- sent[[length(sent)]]
      if (nrow(last) && "slow" %in% last$name) {
        seen <- TRUE
        expect_equal(last$branches[last$name == "slow"], 4L)
      }
    }
    expect_true(seen)
    session$elapse(1200)
    expect_equal(nrow(sent[[length(sent)]]), 0) # cleared once the run stopped
  })
})
