# Two pipelines on one store (#245): loading a script that no project names
# keeps the current store, as tar_make(script = ) would. The board says so.

test_that("the notice names the store, the scripts before, and gives a store of its own", {
  html <- as.character(shared_store_ui("other.R", c("_targets.R"), "_targets"))
  expect_match(html, "other.R uses the same store as _targets.R (_targets)", fixed = TRUE)
  expect_match(html, "Targets with the same names are shared.", fixed = TRUE)
  expect_match(html, "other:\n  script: other.R\n  store: _targets_other", fixed = TRUE)
  expect_match(html, "tb-copy", fixed = TRUE)
  html <- as.character(shared_store_ui("my pipeline.R", c("a.R", "b.R"), "_targets"))
  expect_match(html, "same store as a.R and b.R", fixed = TRUE)
  expect_match(html, "my_pipeline:\n  script: my pipeline.R", fixed = TRUE)
})

test_that("the leftovers may be another pipeline's, when the store had another script", {
  leftovers <- data.frame(name = c("x", "y"), bytes = c(10, 20))
  expect_no_match(as.character(leftovers_ui(leftovers)), "another pipeline", fixed = TRUE)
  html <- as.character(leftovers_ui(leftovers, others = "a.R"))
  expect_match(html, "They may be another pipeline's: this store also held a.R's targets this session.", fixed = TRUE)
})

test_that("Load warns when a script shares the store, not for another project or the same script", {
  skip_on_cran()
  local_projects()
  writeLines(c("library(targets)", "list(tar_target(o1, 1))"), "other.R")
  notes <- character()
  local_mocked_bindings(showNotification = function(ui, ...) {
    notes <<- c(notes, paste(as.character(ui), collapse = ""))
    NULL
  }, .package = "shiny")
  args <- script_args()
  shiny::testServer(script_server, args = args, {
    session$setInputs(path = "a.R", load = 1) # project a: its own store
    expect_false(any(grepl("same store", notes)))
    expect_equal(session$returned$store_others(), character())
    session$setInputs(path = "_targets.R", load = 2) # back to main
    session$setInputs(path = "other.R", load = 3) # no project: main's store
    expect_true(any(grepl("other.R uses the same store as _targets.R", notes, fixed = TRUE)))
    expect_equal(session$returned$store_others(), "_targets.R")
    n <- length(notes)
    session$setInputs(path = "other.R", load = 4) # the same again
    expect_length(notes, n)
  })
})
