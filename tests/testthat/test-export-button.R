# The Export button (#309): its estimate, its R code, and a build in the
# background that the browser then saves.

meta_fixture <- function() {
  data.frame(
    name = c("data", "model", "sq", "sq_a", "sq_b", "remote", "big"),
    type = c("stem", "stem", "pattern", "branch", "branch", "stem", "stem"),
    parent = c(NA, NA, NA, "sq", "sq", NA, NA),
    bytes = c(1000, 2000, 0, 300, 400, 50, 5e7),
    repository = c("local", "local", "local", "local", "local", "aws", "local"),
    stringsAsFactors = FALSE
  )
}

test_that("the estimate sums the values that go in, branches by their pattern, and counts the left out", {
  est <- export_estimate(meta_fixture(), c("data", "sq", "remote", "big"), max_bytes = 1e7)
  expect_equal(est, list(n = 4L, bytes = 1700, cloud = 1L, over = 1L))
  expect_equal(export_estimate(NULL, "data", 1e7)$bytes, 0)
  expect_equal(export_estimate(meta_fixture(), character(), 1e7)$n, 0L)
  expect_equal(export_estimate(meta_fixture(), "model", 100)$over, 1L)
})

test_that("the R code names the targets only when they aren't all, and the cap only when not the default", {
  expect_equal(export_code("x.html", c("a", "b"), 10e6, all = TRUE), 'targetsboard::tar_board_export("x.html")')
  expect_equal(
    export_code("x.html", c("a", "my target"), 2e6),
    'targetsboard::tar_board_export("x.html", names = c(a, `my target`), max_bytes = 2e+06)'
  )
})

test_that("Export builds in the background, offers the page to save, and Cancel stops it", {
  board <- shiny::reactiveVal(list(x = list(nodes = data.frame(id = c("data", "model"), type = "stem"))))
  started <- NULL
  alive <- TRUE
  fake <- list(
    is_alive = function() alive,
    get_result = function() NULL,
    kill = function() alive <<- FALSE
  )
  start <- function(args) {
    started <<- args
    writeLines("<html></html>", args$file)
    fake
  }
  shiny::testServer(export_server, args = list(
    board = board, selected = shiny::reactive("model"), running = shiny::reactive(FALSE),
    tar_meta = shiny::reactive(meta_fixture()), script = shiny::reactive("_targets.R"), start = start
  ), {
    session$setInputs(open = 1, which = "selected", max_mb = 5)
    expect_match(paste(output$summary$html), "1 target, about 2 kB of values")
    expect_match(paste(output$summary$html), "names = c(model), max_bytes = 5e+06", fixed = TRUE)
    session$setInputs(build = 1)
    expect_equal(started$names, "model")
    expect_equal(started$max_bytes, 5e6)
    expect_match(paste(output$footer$html), "Building the page")
    alive <<- FALSE
    session$elapse(600)
    expect_match(paste(output$footer$html), "-board-")
    expect_equal(readLines(output$save), "<html></html>")

    # Cancel: the process killed, back to the options.
    alive <<- TRUE
    built(NULL)
    session$setInputs(build = 2)
    session$setInputs(cancel = 1)
    expect_false(alive)
    expect_match(paste(output$footer$html), ">Export<")
  })
})
