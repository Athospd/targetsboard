# The R code the board writes for the author to paste (#283): it must parse
# for any target name targets accepts and any path, and a few snippets must
# run as pasted.

# Valid symbols only: targets refuses other names. Non-ASCII from code
# points, to keep this file ASCII.
odd_names <- c("x.y", "TRUE_", intToUtf8(c(233, 116, 233)), strrep("a", 200), "a1")
odd_paths <- c(
  "_targets.R", "my dir/_targets.R", "it's/_targets.R", "say \"hi\"/_targets.R",
  "C:\\Users\\me\\pipeline\\_targets.R", file.path(intToUtf8(c(233, 116, 233)), "_targets.R")
)

parses <- function(code) !inherits(try(parse(text = code, keep.source = FALSE), silent = TRUE), "try-error")

test_that("every snippet parses, for odd target names and odd paths", {
  for (name in odd_names) for (path in odd_paths) {
    store <- file.path(dirname(path), "_targets")
    dir <- dirname(path)
    snippets <- c(
      read = read_code(name, store),
      read_branches = read_code(name, store, branch_arg(list(name = name, pattern = list(branch = 2L)))),
      slice_vector = slice_code(list(input = name, index = 2L, type = "stem"), "vector", store),
      slice_list = slice_code(list(input = name, index = 2L, type = "stem"), "list", store),
      slice_group = slice_code(list(input = name, index = 2L, type = "stem"), "group", store),
      slice_branch = slice_code(list(input = name, index = 2L, type = "pattern"), "vector", store),
      reproduce = reproduce_code(name, "x + 1", workspace = FALSE, inputs = c(name, "b"), seed = 1L, script = path, store = store, dir = dir),
      workspace = reproduce_code(name, "x + 1", workspace = TRUE, script = path, store = store, dir = dir),
      profile = profile_code(name, "x + 1", workspace = FALSE, inputs = name, script = path, store = store, dir = dir),
      debug = debug_run_code(name, script = path, store = store, dir = dir),
      run = run_code(c(name, "b"), maker = list(fun = "tar_make_future", workers = 2L), script = path),
      invalidate = invalidate_code(c(name, "b")),
      export = export_code(path, c(name, "b"), 2e6),
      project = project_env_code(basename(dirname(path)))
    )
    for (kind in names(snippets)) {
      expect(parses(snippets[[kind]]), sprintf("%s code doesn't parse, for %s in %s:\n%s", kind, name, path, snippets[[kind]]))
    }
  }
})

test_that("read, run and invalidate code run as pasted, in a folder with an awkward path", {
  skip_on_cran()
  dir <- file.path(withr::local_tempdir(), paste("it's", intToUtf8(c(233, 116, 233))))
  dir.create(dir)
  name <- odd_names[[3]]
  writeLines(c(
    "library(targets)",
    sprintf("list(tar_target(x.y, 1:3), tar_target(%s, x.y * 2))", name)
  ), file.path(dir, "pipeline.R"), useBytes = TRUE)
  code <- c(
    run_code(NULL, script = "pipeline.R"),
    paste0("stopifnot(identical(", read_code(name, "_targets"), ", c(2, 4, 6)))"),
    paste0("stopifnot(identical(", slice_code(list(input = "x.y", index = 2L, type = "stem"), "vector"), ", 2L))"),
    invalidate_code(name),
    paste0("stopifnot(identical(targets::tar_outdated(script = 'pipeline.R'), ", deparse(name), "))")
  )
  out <- callr::r(function(dir, code) {
    setwd(dir)
    for (line in code) eval(parse(text = line, encoding = "UTF-8"), envir = globalenv())
    "ok"
  }, args = list(dir = dir, code = code))
  expect_equal(out, "ok")
})
