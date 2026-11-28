# The board's settings per script, beside its layout (#305): whether it
# drew each script with its functions and globals when last left.

test_that("the switch's choice is read back per script, and only from a store that exists", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  path <- board_settings_path("_targets")
  expect_null(read_functions_shown("_targets.R", path))
  expect_false(write_functions_shown("_targets.R", TRUE, path)) # no store: nothing written
  expect_false(file.exists(path))

  dir.create("_targets")
  write_functions_shown("_targets.R", TRUE, path)
  write_functions_shown("other.R", FALSE, path)
  expect_true(read_functions_shown("_targets.R", path))
  expect_false(read_functions_shown("other.R", path))
  # The same script by another path: one key, from the working directory.
  expect_true(read_functions_shown(file.path(getwd(), "_targets.R"), path))
  expect_null(read_functions_shown("third.R", path))
  write_functions_shown("_targets.R", FALSE, path)
  expect_false(read_functions_shown("_targets.R", path))
  expect_false(read_functions_shown("other.R", path))

  writeLines("{not json", path) # a broken file: as if none
  expect_null(read_functions_shown("_targets.R", path))
  write_functions_shown("_targets.R", TRUE, path)
  expect_true(read_functions_shown("_targets.R", path))
})

test_that("the board starts as the switch was left, unless told otherwise", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("_targets")
  expect_true(saved_targets_only("_targets.R", "main"))
  write_functions_shown("_targets.R", TRUE)
  expect_false(saved_targets_only("_targets.R", "main"))
  expect_true(saved_targets_only("other.R", "main"))
})

test_that("the backend remembers the switch for its script, and draws another script as it was left", {
  skip_on_cran()
  local_pipeline()
  dir.create("_targets") # a store, for the choice to be written to
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  backend$set_targets_only(FALSE)
  expect_true(read_functions_shown("_targets.R"))

  file.copy("_targets.R", "other.R")
  write_functions_shown("other.R", FALSE)
  backend$start_once()
  backend$start_poller("other.R")
  expect_true(shiny::isolate(backend$targets_only()))
  # One with no choice saved keeps the switch as it is.
  file.copy("_targets.R", "third.R")
  backend$set_targets_only(FALSE, restart = FALSE) # remembered for other.R
  backend$start_poller("third.R")
  expect_false(shiny::isolate(backend$targets_only()))
  expect_true(read_functions_shown("other.R"))
})
