# Invalidate asks only about targets with a record left in the metadata:
# one already invalidated, or never built, has nothing to invalidate (#252).

fake_records <- function() {
  data.frame(
    name = c("a", "b", "p", "p_1", "p_2"),
    type = c("stem", "stem", "pattern", "branch", "branch"),
    parent = c(NA, NA, NA, "p", "p")
  )
}

test_that("recorded_targets() keeps the targets with a record of their own or of a branch", {
  meta <- fake_records()
  expect_equal(recorded_targets(c("a", "b", "c", "p"), meta), c("a", "b", "p"))
  # A pattern invalidated itself, some branches still recorded: still recorded.
  expect_equal(recorded_targets("p", meta[meta$name != "p", ]), "p")
  # A pattern with no record left, neither its own nor its branches'.
  expect_equal(recorded_targets(c("p", "a"), meta[meta$name %in% c("a", "b"), ]), "a")
  expect_equal(recorded_targets(c("a", "b"), meta[0, ]), character())
  expect_equal(recorded_targets(c("a", "b"), NULL), character())
  # The board's own attribute for the Console comes along.
  expect_true(attr(recorded_targets(structure(c("a", "c"), all = TRUE), meta), "all"))
})

test_that("the modal counts the targets it invalidates, and says how many have no record", {
  html <- as.character(invalidate_modal(c("a", "b"), skipped = 3))
  expect_match(html, "Invalidate 2 targets?", fixed = TRUE)
  expect_match(html, "3 others are already invalidated or were never built.", fixed = TRUE)
  html <- as.character(invalidate_modal("a", skipped = 1))
  expect_match(html, "1 other is already invalidated or was never built.", fixed = TRUE)
  expect_no_match(as.character(invalidate_modal("a")), "already invalidated", fixed = TRUE)
})

test_that("Invalidate all and the graph's Invalidate skip targets without a record (#252)", {
  skip_on_cran()
  local_pipeline()
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  nodes <- data.frame(id = c("a", "b", "c", "p"), type = "stem")
  meta <- shiny::reactiveVal(fake_records())
  action <- shiny::reactiveVal(NULL)
  notes <- character()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") notes <<- c(notes, paste(as.character(ui), collapse = " ")),
    .package = "shiny"
  )
  shiny::testServer(run_server, args = list(
    backend = backend, board = shiny::reactiveVal(list(x = list(nodes = nodes))),
    running = shiny::reactiveVal(FALSE), action = action, tar_meta = meta
  ), {
    session$setInputs(invalidate_all = 1)
    expect_equal(as.character(to_invalidate()), c("a", "b", "p"))

    # The graph's Invalidate on a selection: c has no record.
    action(list(action = "invalidate", nodes = list("c", "a")))
    session$flushReact()
    expect_equal(as.character(to_invalidate()), "a")

    # Nothing has a record: no modal, a note says so.
    to_invalidate(NULL)
    meta(fake_records()[0, ])
    session$setInputs(invalidate_all = 2)
    expect_null(to_invalidate())
    expect_match(notes, "Nothing to invalidate", fixed = TRUE, all = FALSE)
    action(list(action = "invalidate", nodes = list("a")))
    session$flushReact()
    expect_null(to_invalidate())
  })
})
