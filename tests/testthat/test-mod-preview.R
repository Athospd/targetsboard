# The value preview module, on its own: preview_server() with its inputs
# as reactiveVals.

# preview_server()'s arguments, for the pipeline in the working directory.
preview_args <- function(.env = parent.frame()) {
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller(), envir = .env)
  list(
    backend = backend,
    focused = shiny::reactiveVal(NULL),
    open_request = shiny::reactiveVal(NULL),
    board = shiny::reactiveVal(list(x = targets::tar_visnetwork(targets_only = TRUE)$x)),
    running = shiny::reactiveVal(FALSE),
    tar_meta = shiny::reactiveVal(targets::tar_meta()),
    methods = shiny::reactiveVal(0)
  )
}

test_that("preview_server() checks its arguments", {
  args <- list(
    backend = list(), focused = shiny::reactive(NULL), open_request = shiny::reactive(0),
    board = shiny::reactive(NULL), running = shiny::reactive(FALSE), tar_meta = shiny::reactive(NULL),
    methods = shiny::reactive(0)
  )
  expect_error(do.call(preview_server, c("p", utils::modifyList(args, list(focused = "a")))), "is.reactive")
  expect_error(do.call(preview_server, c("p", utils::modifyList(args, list(backend = shiny::reactive(1))))), "is.reactive")
})

test_that("the preview opens on request, follows the focus, and closes with it or its toggle", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  args <- preview_args()
  shiny::testServer(preview_server, args = args, {
    args$focused("c")
    session$flushReact()
    expect_false(is_open())
    expect_error(output$content) # focused, not asked for

    args$open_request(1)
    session$flushReact()
    expect_true(is_open())
    expect_match(output$content$html, "[1] 4", fixed = TRUE)
    # The ids it writes are in its namespace.
    expect_equal(session$ns("content"), "proxy1-content")

    args$focused("b")
    session$flushReact()
    expect_match(output$content$html, "[1] 2", fixed = TRUE)

    args$focused(NULL)
    session$flushReact()
    expect_false(is_open())

    args$focused("c")
    args$open_request(2)
    session$flushReact()
    expect_true(is_open())
    session$setInputs(sidebar = FALSE)
    expect_false(is_open())
  })
})

test_that("a pattern's preview starts on its first branch, on every new focus", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(
    c("library(targets)", "list(tar_target(x, 1:3), tar_target(y, x * 10, pattern = map(x)), tar_target(z, 5))"),
    "_targets.R"
  )
  targets::tar_make(reporter = "silent")
  args <- preview_args()
  shiny::testServer(preview_server, args = args, {
    args$focused("y")
    args$open_request(1)
    session$flushReact()
    session$setInputs(branch = 3)
    expect_equal(branch(), 3L)
    expect_match(output$content$html, "branches = 3", fixed = TRUE)
    args$focused("z")
    session$flushReact()
    expect_equal(branch(), 1L)
  })
})

test_that("the problem pane's input links come back as target_link", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  args <- preview_args()
  shiny::testServer(preview_server, args = args, {
    expect_null(session$returned$target_link())
    session$setInputs(target_link = "a")
    expect_equal(session$returned$target_link(), "a")
  })
})

test_that("the rule and skip lines don't redraw on a metadata write that leaves the target as it was (#243)", {
  skip_on_cran()
  local_pipeline(make = TRUE)
  args <- preview_args()
  # An age rule: its line says how long ago c built.
  args$rules <- shiny::reactiveVal(data.frame(name = "c", rule = "age", arg = "as.difftime(1, units = \"days\")",
                                              `for` = NA, check.names = FALSE))
  args$skipped <- shiny::reactiveVal(list(c = "Skipped: its rule says so."))
  drawn <- c(rule = 0, why = 0)
  rule_ui <- get("rule_ui", asNamespace("targetsboard"))
  skipped_ui <- get("skipped_ui", asNamespace("targetsboard"))
  local_mocked_bindings(
    rule_ui = function(...) { drawn[["rule"]] <<- drawn[["rule"]] + 1; rule_ui(...) },
    skipped_ui = function(...) { drawn[["why"]] <<- drawn[["why"]] + 1; skipped_ui(...) }
  )
  shiny::testServer(preview_server, args = args, {
    args$focused("c")
    args$open_request(1)
    session$flushReact()
    output$rule
    output$why
    before <- drawn
    # Another target's record changes: c's build time doesn't.
    meta <- args$tar_meta()
    meta$time[meta$name == "a"] <- meta$time[meta$name == "a"] + 60
    args$tar_meta(meta)
    session$flushReact()
    expect_equal(drawn, before)
    # c built again: they say its new time.
    meta$time[meta$name == "c"] <- meta$time[meta$name == "c"] + 60
    args$tar_meta(meta)
    session$flushReact()
    output$rule
    expect_gt(drawn[["rule"]], before[["rule"]])
  })
})
