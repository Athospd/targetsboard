# graph_data_server(): the session's polls, tested alone with a fake backend.

local_fake_backend <- function(path, busy = FALSE) {
  list(rds_path = shiny::reactiveVal(path), busy = function() busy)
}

test_that("graph_data_server() re-reads the poller's graph when its file changes", {
  withr::local_dir(withr::local_tempdir()) # no store here
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(x = "first"), path)
  shiny::testServer(graph_data_server, args = list(backend = local_fake_backend(path)), {
    data <- session$returned
    expect_equal(data$poller_board()$x, "first")
    expect_null(data$script_error())
    expect_false(data$running())
    expect_equal(NROW(data$tar_meta()), 0) # no store: no metadata
    expect_equal(data$warned_targets(), character())

    saveRDS(list(x = "second"), path)
    touch_later(path)
    session$elapse(600)
    expect_equal(data$poller_board()$x, "second")

    writeLines("object 'y' not found", poller_error_path(path))
    session$elapse(600)
    expect_equal(data$script_error(), "object 'y' not found")
  })
})

test_that("graph_data_server() reports a run going", {
  withr::local_dir(withr::local_tempdir())
  path <- withr::local_tempfile(fileext = ".rds")
  shiny::testServer(graph_data_server, args = list(backend = local_fake_backend(path, busy = TRUE)), {
    expect_null(session$returned$poller_board()) # no graph written yet
    expect_true(session$returned$running())
  })
})

test_that("graph_data_server() takes the backend as it is, not as a reactive", {
  expect_error(graph_data_server("graph_data", shiny::reactive(list())), "is.reactive")
})
