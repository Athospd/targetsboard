# The tabs beside the board, each tested alone with testServer() and fake
# reactives: problems_server(), meta_server(), log_server(),
# visnetwork_server(), internals_server() and script_view_server().

fake_meta <- function() {
  data.frame(
    name = c("raw", "model", "report", "gone"),
    type = "stem",
    parent = NA_character_,
    error = c(NA, "object 'y' not found", NA, "an old error"),
    warnings = c(NA, NA, "NAs introduced", NA)
  )
}

fake_board <- function() list(x = list(nodes = fake_nodes(), edges = fake_edges()))

test_that("problems_server() lists the board's problems, counts them, and returns clicks", {
  board <- shiny::reactiveVal(fake_board())
  meta <- shiny::reactiveVal(fake_meta())
  shiny::testServer(problems_server, args = list(board = board, tar_meta = meta), {
    # Only the board's targets: "gone" isn't on it.
    expect_match(output$table, "object 'y' not found", fixed = TRUE)
    expect_no_match(output$table, "an old error", fixed = TRUE)
    expect_match(output$table, session$ns("row_clicked"), fixed = TRUE) # namespaced
    expect_match(output$count$html, ">2<", fixed = TRUE)
    expect_match(output$count$html, "text-bg-danger", fixed = TRUE)

    # Only warnings: yellow.
    meta(fake_meta()[3, ])
    session$flushReact()
    expect_match(output$count$html, ">1<", fixed = TRUE)
    expect_match(output$count$html, "text-bg-warning", fixed = TRUE)
    # None: no count.
    meta(fake_meta()[1, ])
    session$flushReact()
    expect_null(output$count$html)

    expect_null(session$returned())
    session$setInputs(row_clicked = "model")
    expect_equal(session$returned(), "model")
  })
})

test_that("meta_server() shows tar_meta() and returns clicks", {
  meta <- shiny::reactiveVal(NULL)
  shiny::testServer(meta_server, args = list(tar_meta = meta), {
    expect_error(output$table) # no metadata yet: nothing to show
    meta(fake_meta())
    session$flushReact()
    expect_match(output$table, "an old error", fixed = TRUE) # every target
    expect_match(output$table, session$ns("row_clicked"), fixed = TRUE)
    session$setInputs(row_clicked = "raw")
    expect_equal(session$returned(), "raw")
  })
})

test_that("meta_server() adds functions' and globals' records when the graph draws them (#305)", {
  skip_on_cran()
  dir <- local_pipeline()
  writeLines(c("library(targets)", "half <- function(x) x / 2", "list(tar_target(a, half(1)))"), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  targets::tar_make(reporter = "silent")
  meta <- shiny::reactiveVal(targets::tar_meta(targets_only = TRUE))
  board <- shiny::reactiveVal(list(x = list(nodes = data.frame(id = "a", type = "stem"))))
  shiny::testServer(meta_server, args = list(tar_meta = meta, board = board), {
    session$flushReact()
    expect_equal(shown()$name, "a")
    expect_no_match(output$table, '"filterable":true', fixed = TRUE)
    board(list(x = list(nodes = data.frame(id = c("a", "half"), type = c("stem", "function")))))
    session$flushReact()
    expect_setequal(shown()$name, c("a", "half"))
    expect_match(output$table, '"filterable":true', fixed = TRUE) # the type column's filter
  })
})

test_that("log_server() shows the board's latest run, headed by what it was", {
  runner <- new.env()
  backend <- list(runner = runner)
  shiny::testServer(log_server, args = list(backend = backend), {
    expect_match(output$text, "No run started from the board yet", fixed = TRUE)

    path <- withr::local_tempfile(fileext = ".log")
    writeLines("built model", path)
    runner$process <- structure(list(), log = path, header = "Run started: all targets")
    session$elapse(1100)
    expect_equal(output$text, "Run started: all targets\n\nbuilt model")
  })
})

test_that("visnetwork_server() draws tar_visnetwork()'s widget, once there is one", {
  board <- shiny::reactiveVal(fake_board())
  mode <- shiny::reactiveVal("light")
  shiny::testServer(visnetwork_server, args = list(board = board, color_mode = mode), {
    expect_error(output$graph) # no widget yet
    nodes <- fake_nodes()
    board(c(fake_board(), list(widget = visNetwork::visNetwork(nodes[c("id", "label")], fake_edges()))))
    session$flushReact()
    expect_match(output$graph, "\"label\":[\"raw\",\"model\",\"report\"]", fixed = TRUE)
    expect_no_match(output$graph, "#dee2e6", fixed = TRUE) # light: visNetwork's own colours

    mode("dark")
    session$flushReact()
    expect_match(output$graph, "\"font\":{\"color\":\"#dee2e6\"}", fixed = TRUE)
  })
})

test_that("in dark mode, tar_visnetwork()'s labels and legend get dark mode's text colour", {
  nodes <- fake_nodes()
  widget <- visNetwork::visLegend(visNetwork::visNetwork(nodes[c("id", "label")], fake_edges()),
    addNodes = data.frame(label = "Outdated", color = "#78B7C5", shape = "dot"), useGroups = FALSE
  )
  expect_identical(visnetwork_in_mode(widget, "light"), widget)
  dark <- visnetwork_in_mode(widget, "dark")
  expect_equal(dark$x$options$nodes$font$color, "#dee2e6")
  expect_equal(dark$x$legend$nodes$font.color, "#dee2e6")
})

test_that("internals_server() prints the graph and the app's inputs", {
  board <- shiny::reactiveVal(c(fake_board(), list(progress_mtime = NA, widget = "left out")))
  inputs <- shiny::reactiveVal(list(flow_focused_node = "model"))
  shiny::testServer(internals_server, args = list(board = board, inputs = inputs), {
    expect_match(output$text, "flow_focused_node", fixed = TRUE)
    expect_match(output$text, "progress_mtime", fixed = TRUE)
    expect_no_match(output$text, "left out", fixed = TRUE)
  })
})

test_that("the tabs' modules take reactives where they change, and constants where they don't", {
  expect_error(problems_server("problems", fake_board(), shiny::reactive(NULL)), "is.reactive")
  expect_error(meta_server("meta", fake_meta()), "is.reactive")
  expect_error(log_server("log", shiny::reactive(list())), "is.reactive")
  expect_error(visnetwork_server("visnetwork", fake_board(), shiny::reactive("light")), "is.reactive")
  expect_error(visnetwork_server("visnetwork", shiny::reactive(NULL), "light"), "is.reactive")
  expect_error(internals_server("internals", shiny::reactive(NULL), list()), "is.reactive")
})

test_that("the tables' row clicks send their input with the full namespace", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  meta <- shiny::reactiveVal(targets::tar_meta())
  shiny::testServer(meta_server, args = list(tar_meta = meta), {
    expect_match(output$table, session$ns("row_clicked"), fixed = TRUE)
    expect_match(session$ns("row_clicked"), "-row_clicked$")
  })
})

test_that("where_things_are() finds the script, the store and what is in it, and the configuration", {
  local_pipeline(make = TRUE)
  # From the working directory as R reports it: on macOS the temporary
  # folder is a link (/var to /private/var).
  here <- normalizePath(getwd(), winslash = "/")
  full <- function(...) paste(c(here, ...), collapse = "/")
  paths <- where_things_are("_targets.R")
  expect_equal(
    paths$label,
    c("Script", "Folder", "Data store", "Metadata", "Progress", "Values", "Workspaces", "Configuration")
  ) # no Project row: a plain pipeline
  expect_equal(paths$path[paths$label == "Script"], full("_targets.R")) # absolute
  expect_equal(paths$path[paths$label == "Metadata"], full("_targets", "meta", "meta"))
  expect_true(all(paths$exists[1:6]))
  expect_false(paths$exists[paths$label == "Workspaces"]) # nothing errored yet
  expect_false(paths$exists[paths$label == "Configuration"]) # no _targets.yaml

  # A store and a project from _targets.yaml.
  writeLines(c("main:", "  store: elsewhere", "other:", "  store: other_store"), "_targets.yaml")
  withr::local_envvar(TAR_PROJECT = "other")
  paths <- where_things_are("_targets.R")
  expect_equal(paths$path[paths$label == "Data store"], full("other_store"))
  expect_true(paths$exists[paths$label == "Configuration"])
  # The board's project, as the line for the console.
  expect_equal(paths$path[paths$label == "Project"], 'Sys.setenv(TAR_PROJECT = "other")')

  html <- as.character(paths_table(paths))
  expect_match(html, "Where things are", fixed = TRUE)
  expect_match(html, paste0('data-copy="', full("_targets.R"), '"'), fixed = TRUE) # copies with a click
  expect_match(html, "not created yet", fixed = TRUE)
})

test_that("script_view_server() shows the loaded script as it is on disk, and follows it", {
  local_pipeline()
  writeLines("list()", "other.R")
  script <- shiny::reactiveVal("_targets.R")
  shiny::testServer(script_view_server, args = list(script = script), {
    session$flushReact()
    expect_match(output$code$html, "tar_target(a, 1)", fixed = TRUE)
    expect_match(output$code$html, 'class="language-r"', fixed = TRUE) # coloured by highlight.js
    expect_match(output$paths$html, "Where things are", fixed = TRUE)

    # Edited on disk: shown again within a second.
    writeLines(c("library(targets)", "list(tar_target(edited, 1))"), "_targets.R")
    session$elapse(1000)
    expect_match(output$code$html, "tar_target(edited, 1)", fixed = TRUE)

    # Another script loaded.
    script("other.R")
    session$flushReact()
    expect_match(output$code$html, "other.R", fixed = TRUE)
    expect_no_match(output$code$html, "edited", fixed = TRUE)

    # Gone: says so.
    script("missing.R")
    session$flushReact()
    expect_match(output$code$html, "can't be read", fixed = TRUE)
  })
})

test_that("absolute_path() gives a path not created yet as the system names its folders", {
  local_pipeline()
  here <- normalizePath(getwd(), winslash = "/")
  expect_equal(absolute_path("not/made/yet"), paste0(here, "/not/made/yet"))
  expect_equal(absolute_path(file.path(getwd(), "store")), paste0(here, "/store"))
  expect_equal(absolute_path("_targets.R"), paste0(here, "/_targets.R"))
  expect_false(startsWith(absolute_path("~/x"), here)) # a home path is already absolute
})

test_that("the leftovers notice doesn't redraw on a metadata write that leaves the leftovers as they were (#243)", {
  meta <- shiny::reactiveVal(fake_meta())
  drawn <- 0
  leftovers_ui <- get("leftovers_ui", asNamespace("targetsboard"))
  local_mocked_bindings(leftovers_ui = function(...) { drawn <<- drawn + 1; leftovers_ui(...) })
  shiny::testServer(meta_server, args = list(tar_meta = meta, script_targets = shiny::reactiveVal(fake_meta()$name[-1])), {
    output$leftovers
    before <- drawn
    changed <- fake_meta()
    changed$warnings[2] <- "a new warning" # a write that changes no leftover
    meta(changed)
    session$flushReact()
    expect_equal(drawn, before)
  })
})
