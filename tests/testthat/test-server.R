test_that("one background poller is shared by all sessions", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  shiny::testServer(server, {
    rds_path <- environment(server)$poller_rds_path()
    wait_for(function() file.exists(rds_path))

    session$elapse(600)
    expect_setequal(board()$x$nodes$id, c("a", "b", "c"))
  })

  pid <- poller_of(server)$process$get_pid()
  shiny::testServer(server, {
    expect_equal(poller_of(server)$process$get_pid(), pid)
  })

  process <- poller_of(server)$process
  environment(server)$stop_poller()
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

test_that("focusing a target previews its value in one reused output", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  shiny::testServer(server, {
    session$setInputs(flow_focused_node = "c")
    expect_match(output$value_preview$html, "value_preview_content")
    expect_match(output$value_preview_content, "4")

    session$setInputs(flow_focused_node = "b")
    invisible(output$value_preview)
    expect_match(output$value_preview_content, "2")
  })
})

test_that("tar_meta() is re-read only when the metadata file changes", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  server <- local_app_server()

  reads <- 0
  real_tar_meta <- targets::tar_meta
  local_mocked_bindings(
    tar_meta = function(...) {
      reads <<- reads + 1
      real_tar_meta(...)
    },
    .package = "targets"
  )

  shiny::testServer(server, {
    invisible(output$meta)
    before <- reads
    for (i in 1:3) {
      session$elapse(1000)
      invisible(output$meta)
    }
    expect_equal(reads, before)

    Sys.sleep(1.1) # file times can have 1s resolution
    targets::tar_invalidate(a)
    targets::tar_make(reporter = "silent")
    session$elapse(1000)
    invisible(output$meta)
    expect_equal(reads, before + 1)
  })
})

test_that("a run's progress reaches the widget as status-only updates", {
  skip_on_cran()
  local_pipeline()
  server <- local_app_server()

  sent <- list()
  local_mocked_bindings(update_targetsboard_status = function(session, inputId, statuses) {
    sent[[length(sent) + 1]] <<- statuses
  })

  shiny::testServer(server, {
    # Keep the graph as it is: only the progress file changes.
    environment(server)$stop_poller()
    session$flushReact() # the first flush, which a real app does at start
    targets::tar_make(reporter = "silent")
    session$elapse(600)

    expect_length(sent, 1)
    expect_equal(sent[[1]][c("a", "b", "c")], c(a = "completed", b = "completed", c = "completed"))
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
    environment(server)$stop_poller()
    widget <- targets::tar_visnetwork(targets_only = TRUE)
    board(list(x = widget$x, widget = widget, progress_mtime = progress_mtime()))
    expect_match(output$vis_network, "hierarchical", fixed = TRUE) # tar_visnetwork()'s layout
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
    environment(server)$stop_poller()
    session$setInputs(dark_mode = "dark")
    expect_equal(sent[[length(sent)]], list(inputId = "flow", color_mode = "dark"))
    session$setInputs(dark_mode = "light")
    expect_equal(sent[[length(sent)]]$color_mode, "light")
  })
})
