test_that("targetsboard() hands a saved layout to the widget, updates don't", {
  layout <- list(
    direction = "TB", viewport = list(x = 1, y = 2, zoom = 0.5),
    positions = list(raw = list(x = 0, y = 0))
  )
  config <- board_configuration("dag", fake_nodes(), fake_edges(), "500px", "100%", layout = layout)
  expect_equal(config$layout, layout)
  expect_false("layout" %in% names(board_configuration("dag", fake_nodes(), fake_edges(), "500px", "100%")))
  expect_s3_class(targetsboard("dag", fake_nodes(), fake_edges(), layout = layout), "shiny.tag.list")
  expect_error(targetsboard("dag", fake_nodes(), fake_edges(), layout = list(positions = 1)), "_layout")

  sent <- NULL
  session <- list(sendInputMessage = function(inputId, message) sent <<- message)
  update_targetsboard(session, "dag", fake_nodes(), fake_edges())
  expect_null(sent$configuration$layout)
})

test_that("the board's layout goes to and from the store's user folder", {
  store <- withr::local_tempdir()
  path <- board_layout_path(store)
  expect_equal(path, file.path(store, "user", "targetsboard_layout.json"))
  expect_null(read_board_layout(path))

  layout <- list(
    direction = "LR", viewport = list(x = 12.5, y = -40, zoom = 0.8),
    positions = list(raw_data = list(x = 0, y = 0), model = list(x = 250.25, y = -10))
  )
  expect_true(write_board_layout(layout, path))
  expect_equal(read_board_layout(path), layout)

  # No targets drawn: an empty layout still reads back as one.
  write_board_layout(list(direction = "LR", positions = setNames(list(), character())), path)
  expect_length(read_board_layout(path)$positions, 0)

  expect_error(write_board_layout(list(positions = list(a = list(x = "1"))), path), "_layout")
})

test_that("a layout is not written where there is no store", {
  dir <- withr::local_tempdir()
  path <- board_layout_path(file.path(dir, "_targets"))
  expect_false(write_board_layout(list(positions = list()), path))
  expect_false(dir.exists(file.path(dir, "_targets")))
})

test_that("an unreadable layout is ignored with a warning", {
  store <- withr::local_tempdir()
  path <- board_layout_path(store)
  dir.create(dirname(path))
  writeLines("{not json", path)
  expect_warning(expect_null(read_board_layout(path)), "Could not read")
  writeLines('{"positions": {"a": {"x": "left"}}}', path)
  expect_warning(expect_null(read_board_layout(path)), "not one")
})

test_that("a layout is replaced whole, leaving no temporary file", {
  store <- withr::local_tempdir()
  path <- board_layout_path(store)
  write_board_layout(list(direction = "LR", positions = list(a = list(x = 1, y = 2))), path)
  write_board_layout(list(direction = "TB", positions = list(a = list(x = 3, y = 4))), path)
  expect_equal(read_board_layout(path)$direction, "TB")
  expect_equal(list.files(dirname(path)), "targetsboard_layout.json")
})
