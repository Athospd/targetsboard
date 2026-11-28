# Every function that reads a script's calls without running it, over
# awkward syntax, the bodies of base R's own functions, and the example
# pipelines: none may error (#283). An error in one of them ends the
# board's session (#253: `x[, 1]` did).

walkers <- list(
  run_maker = run_maker,
  sourced_paths = sourced_paths,
  cue_rules = cue_rules,
  tar_map_templates = tar_map_templates,
  function_definitions = function(script) function_definitions(script),
  global_definitions = function(script) function_definitions(script, functions = FALSE)
)

expect_walkers_survive <- function(script, label) {
  for (name in names(walkers)) {
    result <- tryCatch(walkers[[name]](script), error = function(e) e)
    expect(
      !inherits(result, "error"),
      sprintf("%s() failed on %s: %s", name, label, if (inherits(result, "error")) conditionMessage(result) else "")
    )
  }
}

awkward <- c(
  "x[, 1]", "x[1, ]", "x[[]]", "x[]", "f(a)(b)", "f()()()", "pkg::f()", "pkg:::f", "pkg::f", "obj$f()", "obj@slot",
  "(\\(x) x)(1)", "\\(x) x", "function(x, ...) NULL", "function() {}", "~ y + x", "y ~ .", "!!x", "!!!xs",
  "`odd name`()", "`odd name` <- 1", "a -> b", "a <<- b", "b ->> a", "if (x) y", "if (x) y else z",
  "for (i in 1:3) NULL", "while (FALSE) NULL", "repeat break", "NULL", "1", "1L", "1i", "TRUE", "NA", "Inf",
  "'a string'", "r\"(raw)\"", "-1", "x %in% y", "x |> f()", "x |> f(y = _)", "{}", "(((1)))",
  "f(,)", "f(a = )", "c(,1)", "list(a = , b = 2)", "switch(x, a = , b = 1)",
  "tar_target()", "tar_target(name)", "targets::tar_target(, 1)", "tar_map(values = )",
  "tarchetypes::tar_map(list(a = 1:2), tar_target(x, a))", "tar_cue(mode = )",
  "tar_option_set()", "tar_option_set(cue = )", "future::plan()", "plan(workers = )", "options()",
  "options(clustermq.scheduler = )", "source()", "source(file = )", "tar_source(files = c())",
  "tar_source(c('a', NA))", "source(1)", "tar_tangle()", "tar_age(name, 1, age = )",
  paste0(strrep("f(", 50), "1", strrep(")", 50)), # deep nesting
  "x <- y <- z <- 1", "x$a$b$c <- 1", "names(x) <- 'a'", "x[[1]][[2]] <- f"
)

test_that("the script readers survive awkward syntax, one line at a time and all together", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  for (code in awkward) {
    expect_true(!inherits(try(parse(text = code), silent = TRUE), "try-error"), label = code)
    writeLines(c("library(targets)", code, "list(tar_target(a, 1))"), "_targets.R")
    expect_walkers_survive("_targets.R", code)
  }
  writeLines(c("library(targets)", awkward, "list(tar_target(a, 1))"), "_targets.R")
  expect_walkers_survive("_targets.R", "all of them")
  writeLines("this is ( not R", "_targets.R") # a script that doesn't parse
  expect_walkers_survive("_targets.R", "a script that doesn't parse")
  expect_walkers_survive("missing.R", "a missing script")
})

test_that("the script readers survive the bodies of base R's functions", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  bodies <- unlist(lapply(c("base", "stats", "utils"), function(pkg) {
    ns <- asNamespace(pkg)
    lapply(ls(ns), function(name) {
      f <- get(name, envir = ns)
      if (is.function(f) && !is.primitive(f)) paste(deparse(body(f)), collapse = "\n")
    })
  }))
  # Only what parses again: deparse() doesn't always round-trip.
  bodies <- bodies[vapply(bodies, function(code) !inherits(try(parse(text = code), silent = TRUE), "try-error"), logical(1))]
  expect_gt(length(bodies), 1000)
  writeLines(c("library(targets)", bodies, "list(tar_target(a, 1))"), "_targets.R")
  expect_walkers_survive("_targets.R", "base R's function bodies")
})

test_that("the script readers survive every example pipeline", {
  skip_on_cran()
  examples <- system.file("examples", "pipelines", package = "targetsboard")
  skip_if(!nzchar(examples), "no examples installed")
  withr::local_dir(examples)
  scripts <- list.files(".", pattern = "[.]R$")
  expect_gt(length(scripts), 5)
  for (script in scripts) expect_walkers_survive(script, script)
})
