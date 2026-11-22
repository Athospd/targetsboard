# A pipeline whose last run left every kind of problem: an error inside the
# pipeline's own functions, a warning, and a pattern with a failed branch.
local_problem_store <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  writeLines(c(
    "library(targets)",
    "tar_option_set(error = 'continue')",
    "analyze <- function(data) fit(data)",
    "fit <- function(data) stats::lm(y ~ x, data = data)",
    "list(",
    "  tar_target(data, data.frame(x = 1:3)),",
    "  tar_target(model, analyze(data)),",
    "  tar_target(warned, {warning('2 rows dropped'); data}),",
    "  tar_target(nums, 1:3),",
    "  tar_target(branch, if (nums == 2) stop('bad branch') else nums, pattern = map(nums))",
    ")"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir, .local_envir = .env)
  targets::tar_make(reporter = "silent")
  dir
}

test_that("an errored target's problem has its error, traceback, seed and command", {
  skip_on_cran()
  local_problem_store()
  problem <- target_problem("model", "_targets.R")
  expect_true(has_problem(problem))
  expect_match(problem$error, "object 'y' not found")
  expect_true(problem$workspace)
  expect_equal(problem$command, "analyze(data)")
  expect_type(problem$seed, "integer")
  expect_setequal(problem$functions, c("analyze", "fit"))

  segments <- fold_traceback(problem$traceback, problem$functions)
  # The command and the pipeline's own functions, then the calls in packages
  # folded; targets' error handling left out.
  expect_equal(segments[[1]], list(own = TRUE, calls = c("analyze(data)", "fit(data)")))
  expect_false(segments[[2]]$own)
  expect_match(segments[[2]]$calls[1], "stats::lm", fixed = TRUE)
  expect_false(any(grepl("handleSimpleError|simpleError", unlist(lapply(segments, `[[`, "calls")))))

  expect_null(target_problem("data", "_targets.R")$error)
  expect_false(has_problem(target_problem("data", "_targets.R")))
  expect_null(target_problem("absent", "_targets.R"))
})

test_that("a warned target's problem reproduces from its inputs and seed", {
  skip_on_cran()
  local_problem_store()
  problem <- target_problem("warned", "_targets.R")
  expect_true(has_problem(problem))
  expect_null(problem$error)
  expect_equal(problem$warnings, "2 rows dropped")
  expect_false(problem$workspace)

  code <- reproduce_code("warned", problem$command, FALSE, "data", problem$seed)
  expect_match(code, "targets::tar_load_globals()", fixed = TRUE)
  expect_match(code, "targets::tar_load(c(data))", fixed = TRUE)
  expect_match(code, paste0("set.seed(", problem$seed, ")"), fixed = TRUE)
  # The code runs and warns as the target did.
  expect_warning(eval(parse(text = code)), "2 rows dropped")
})

test_that("a pattern's problem lists its failed branches, with their positions", {
  skip_on_cran()
  local_problem_store()
  problem <- target_problem("branch", "_targets.R")
  expect_true(has_problem(problem))
  expect_null(problem$error)
  expect_length(problem$branches, 1)
  branch <- problem$branches[[1]]
  expect_equal(branch$error, "bad branch")
  expect_true(branch$workspace)
  expect_equal(c(branch$position, branch$of), c(2, 3)) # nums == 2

  html <- as.character(problem_ui(problem, "nums"))
  expect_match(html, "Branches with problems: 1", fixed = TRUE)
  expect_match(html, "branch 2 of 3", fixed = TRUE)
  expect_no_match(html, ">Warnings<", fixed = TRUE)
  expect_match(html, paste0("targets::tar_workspace(", branch$name, ")"), fixed = TRUE)
  expect_match(html, paste0('debug_run_script(&quot;', branch$name), fixed = TRUE)
  # A branch's debug run builds its pattern.
  expect_match(html, "names = &quot;branch&quot;", fixed = TRUE)
})

test_that("the pane shows the snippets to copy, but no debug run while running", {
  problem <- list(
    name = "model", type = "stem", error = "boom", warnings = NULL, seed = 1L,
    workspace = TRUE, traceback = c("analyze(data)", "stats::lm(y ~ x)"),
    command = "analyze(data)", functions = "analyze", branches = list()
  )
  html <- as.character(problem_ui(problem, c("data", "params"), script = "_targets.R"))
  expect_match(html, "<pre>targets::tar_workspace(model)", fixed = TRUE)
  expect_match(html, ">Error<", fixed = TRUE)
  expect_match(html, "callr_function = NULL, use_crew = FALSE, as_job = FALSE", fixed = TRUE)
  expect_match(html, "# Restart R first", fixed = TRUE)
  expect_match(html, 'data-target="params"', fixed = TRUE)
  expect_match(html, "1 call in packages", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr(">Copy<", html, fixed = TRUE))), 2)

  html <- as.character(problem_ui(problem, running = TRUE))
  expect_match(html, "Debug run: not while a run is going.", fixed = TRUE)
  expect_no_match(html, "debug_run_script", fixed = TRUE)

  expect_match(debug_run_code("model", "pipelines/a.R"), 'debug_run_script("model", script = "pipelines/a.R")', fixed = TRUE)
})

test_that("a debug run stops in its target, after building what it needs", {
  skip_on_cran()
  local_problem_store()
  wrapper <- debug_run_script("model")
  code <- paste(readLines(wrapper), collapse = "\n")
  expect_match(code, 'debug = "model"', fixed = TRUE)
  expect_error(debug_run_script(c("a", "b")), "one target name")

  # The snippet, run in a fresh R (not interactive, so no debugger opens):
  # it builds the target, and not the other errored one.
  callr::r(
    function(dir, code) { setwd(dir); try(eval(parse(text = code))) },
    list(getwd(), debug_run_code("model"))
  )
  progress <- targets::tar_progress()
  expect_equal(progress$progress[progress$name == "model"], "errored")
  expect_false(any(grepl("^branch", progress$name)))
})

test_that("the value preview leads with the problem; an errored target shows nothing else", {
  skip_on_cran()
  local_problem_store()
  server <- local_app_server()
  shiny::testServer(server, {
    environment(server)$stop_poller()
    session$setInputs(flow_focused_node = "model", flow_double_clicked_node = "model")
    html <- output$value_preview$html
    expect_match(html, 'class="tb-problem" data-target="model"', fixed = TRUE)
    expect_match(html, 'data-target="data"', fixed = TRUE) # its input, linked
    expect_no_match(html, "value_preview_content", fixed = TRUE)

    session$setInputs(flow_focused_node = "warned")
    html <- output$value_preview$html
    expect_match(html, 'data-target="warned"', fixed = TRUE)
    expect_match(html, "value_preview_content", fixed = TRUE) # and its value

    session$setInputs(flow_focused_node = "data")
    expect_no_match(output$value_preview$html, "tb-problem", fixed = TRUE)
  })
})
