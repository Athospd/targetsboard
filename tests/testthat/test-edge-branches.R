# Branches no other test reached (#283, part 6): one test each.

test_that("ages and spans read in their own units: seconds, minutes, and one of each", {
  expect_equal(format_age(as.difftime(90, units = "secs")), "90 seconds")
  expect_equal(format_age(as.difftime(1, units = "mins")), "1 minute")
  expect_equal(format_age(as.difftime(5, units = "mins")), "5 minutes")
  expect_equal(format_age(as.difftime(1, units = "days")), "1 day")
  expect_equal(format_span(40), "40 seconds")
  expect_equal(format_span(1), "1 second")
  expect_equal(format_span(-120), "2 minutes")
  expect_equal(format_span(60), "1 minute")
  expect_equal(format_span(7200), "2 hours")
  expect_equal(format_span(3 * 86400), "3 days")
})

test_that("each of targets' cues has its reason", {
  sitrep <- data.frame(
    name = c("f", "r", "i", "u", "l", "s"),
    format = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    repository = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    iteration = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    file = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    seed = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  reasons <- list(
    sitrep = sitrep,
    vertices = data.frame(name = c(sitrep$name, "up", "down"), type = "stem", status = "outdated"),
    edges = data.frame(from = c("up", "f", "r"), to = c("down", "down", "down")),
    parents = NULL,
    manifest = data.frame(name = c("u", "l"), format = c("url", "file"))
  )
  why <- outdated_reasons(reasons)
  expect_equal(why$f, c(format = "Its storage format changed."))
  expect_equal(why$r, c(repository = "Its repository changed."))
  expect_equal(why$i, c(iteration = "Its iteration changed."))
  expect_equal(why$u, c(file = "Its URL's remote file changed, or can't be reached."))
  expect_equal(why$l, c(file = "Its file changed, or is missing."))
  expect_equal(why$s, c(seed = "Its seed changed."))
  expect_equal(why$down, c(upstream = "Upstream, `up`, `f` and `r` are outdated."))
  reasons$edges <- data.frame(from = "up", to = "down")
  expect_equal(outdated_reasons(reasons)$down, c(upstream = "Upstream, `up` is outdated."))
})

test_that("the editor opener goes through rstudioapi in RStudio or Positron, and the code command in VS Code", {
  withr::local_options(rlang_interactive = TRUE)
  skip_if_not_installed("rstudioapi")
  opened <- NULL
  local_mocked_bindings(
    isAvailable = function(...) TRUE,
    navigateToFile = function(file, line = -1L, ...) opened <<- list(file = file, line = line),
    .package = "rstudioapi"
  )
  editor_opener()("R/a.R", 3L)
  expect_equal(opened, list(file = "R/a.R", line = 3L))

  local_mocked_bindings(isAvailable = function(...) FALSE, .package = "rstudioapi")
  withr::local_envvar(TERM_PROGRAM = "")
  expect_null(editor_opener()) # a terminal, no IDE
  # VS Code: its code command on the PATH, as a script that records its arguments.
  skip_on_os("windows") # a shell script isn't an executable there
  bin <- withr::local_tempdir()
  log <- file.path(bin, "args")
  writeLines(c("#!/bin/sh", sprintf("echo \"$@\" > '%s'", log)), file.path(bin, "code"))
  Sys.chmod(file.path(bin, "code"), "755")
  withr::local_envvar(TERM_PROGRAM = "vscode", PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep))
  editor_opener()("R/a.R", 3L)
  wait_for(function() file.exists(log) && length(readLines(log)) > 0, timeout = 10)
  expect_equal(readLines(log), "-g R/a.R:3")
})

test_that("a table file's shape: no line break at the end, a header alone, one column", {
  dir <- withr::local_tempdir()
  csv <- function(text, name = "t.csv") {
    path <- file.path(dir, name)
    writeBin(charToRaw(text), path)
    table_file_shape(path)
  }
  expect_equal(csv("a,b\n1,2\n3,4"), "2 rows x 2 columns") # no line break at the end
  expect_equal(csv("a,b\n"), "0 rows x 2 columns")
  expect_equal(csv("a\n1\n"), "1 row x 1 column")
  expect_equal(csv("a\tb\tc\n1\t2\t3\n", "t.tsv"), "1 row x 3 columns")
})

test_that("a port is checked on this machine when the board listens on every address", {
  port <- targets::tar_random_port()
  server <- serverSocket(port)
  on.exit(close(server))
  expect_true(port_in_use(port, "0.0.0.0"))
})

test_that("an environment and an object of an unusual class have their labels in a list tree", {
  expect_equal(type_label(new.env()), "environment")
  expect_equal(type_label(structure(list(1), class = "my_class")), "my_class")
  expect_equal(type_label(quote(x + 1)), "call")
})

test_that("Stop kills a run the board didn't start, outside any R session", {
  skip_on_cran()
  local_pipeline()
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  killed <- 0
  local_mocked_bindings(
    active_run = function() structure(list(), class = "fake_handle"),
    interactive_r = function(handle) FALSE,
    stop_run = function(handle) killed <<- killed + 1
  )
  shiny::testServer(run_server, args = list(
    backend = backend, board = shiny::reactiveVal(NULL), running = shiny::reactiveVal(TRUE),
    action = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(stop = 1)
    expect_equal(killed, 1)
  })
})
