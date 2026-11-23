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

test_that("a pattern whose run stopped at a branch's error still shows that branch", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  # targets' default error = "stop": the run ends at the failed branch, and
  # the pattern itself never gets a row in the metadata.
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(nums, 1:3),",
    "  tar_target(branch, if (nums == 2) stop('bad branch') else nums, pattern = map(nums))",
    ")"
  ), file.path(dir, "_targets.R"))
  withr::local_dir(dir)
  try(targets::tar_make(reporter = "silent"), silent = TRUE)
  expect_false("branch" %in% targets::tar_meta(fields = type)$name)

  problem <- target_problem("branch", "_targets.R")
  expect_true(has_problem(problem))
  expect_equal(problem$type, "pattern")
  expect_equal(problem$command, "if (nums == 2) stop(\"bad branch\") else nums")
  expect_length(problem$branches, 1)
  expect_equal(problem$branches[[1]]$error, "bad branch")
  expect_true(problem$branches[[1]]$workspace)
  expect_match(as.character(problem_ui(problem)), "Branches with problems: 1", fixed = TRUE)

  # A name with no metadata and no branches is still no problem.
  expect_null(target_problem("absent", "_targets.R"))
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
  expect_match(html, "say(&#39;Copied&#39;)", fixed = TRUE) # says it copied

  html <- as.character(problem_ui(problem, running = TRUE))
  expect_match(html, "Debug run: not while a run is going.", fixed = TRUE)
  expect_no_match(html, "debug_run_script", fixed = TRUE)

  expect_match(debug_run_code("model", "pipelines/a.R"), 'debug_run_script("model", script = "pipelines/a.R")', fixed = TRUE)
})

test_that("the code to copy names the script, the store and the folder, and runs as is in a fresh R", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "pipelines"))
  writeLines(c(
    "library(targets)",
    "tar_option_set(error = 'continue')",
    "analyze <- function(data) fit(data)",
    "fit <- function(data) stats::lm(y ~ x, data = data)",
    "list(",
    "  tar_target(data, data.frame(x = 1:3)),",
    "  tar_target(model, analyze(data)),",
    "  tar_target(warned, {warning('2 rows dropped'); data})",
    ")"
  ), file.path(dir, "pipelines", "p.R"))
  withr::local_dir(dir)
  targets::tar_make(script = "pipelines/p.R", store = "my_store", reporter = "silent")
  seed <- targets::tar_meta(warned, seed, store = "my_store")$seed
  place <- list(script = "pipelines/p.R", store = "my_store", dir = dir)

  model <- reproduce_code("model", "analyze(data)", TRUE,
                          script = place$script, store = place$store, dir = place$dir)
  expect_match(model, paste0("# Run from ", dir, ", the board's folder."), fixed = TRUE)
  expect_match(model, 'targets::tar_workspace(model, script = "pipelines/p.R", store = "my_store")', fixed = TRUE)
  warned <- reproduce_code("warned", "{warning('2 rows dropped'); data}", FALSE, "data", seed,
                           script = place$script, store = place$store, dir = place$dir)
  expect_match(warned, 'targets::tar_load_globals(script = "pipelines/p.R")', fixed = TRUE)
  expect_match(warned, 'targets::tar_load(c(data), store = "my_store")', fixed = TRUE)
  debug <- debug_run_code("model", place$script, store = place$store, dir = place$dir)
  expect_match(debug, 'script = targetsboard::debug_run_script("model", script = "pipelines/p.R")', fixed = TRUE)
  expect_match(debug, 'store = "my_store"', fixed = TRUE)
  expect_match(debug, "# Run from ", fixed = TRUE)

  # Pasted into a fresh R in the board's folder, each reproduces the problem
  # itself: the target's own error, or its warning.
  run_pasted <- function(code) {
    callr::r(function(dir, code) {
      setwd(dir)
      warned <- character()
      error <- tryCatch(
        withCallingHandlers(
          { eval(parse(text = code), envir = globalenv()); NA_character_ },
          warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") }
        ),
        error = function(e) conditionMessage(e)
      )
      list(error = error, warnings = warned)
    }, list(dir, code))
  }
  expect_match(run_pasted(model)$error, "object 'y' not found")
  expect_true("2 rows dropped" %in% run_pasted(warned)$warnings)
  # The debug run builds the target again, in the board's store (its
  # progress file rewritten), and makes no store of its own.
  progress_file <- file.path("my_store", "meta", "progress")
  before <- file.mtime(progress_file)
  Sys.sleep(1.1) # file times have a second's resolution on some systems
  run_pasted(debug)
  expect_gt(file.mtime(progress_file), before)
  progress <- targets::tar_progress(store = "my_store")
  expect_equal(progress$progress[progress$name == "model"], "errored")
  expect_false(dir.exists("_targets"))
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

    # A pattern whose branch errored: its problem says why there is no value.
    session$setInputs(flow_focused_node = "branch")
    html <- output$value_preview$html
    expect_match(html, 'data-target="branch"', fixed = TRUE)
    expect_no_match(html, "No value stored", fixed = TRUE)

    session$setInputs(flow_focused_node = "data")
    expect_no_match(output$value_preview$html, "tb-problem", fixed = TRUE)
  })
})

test_that("the problems table has one row per target, a pattern counting its branches", {
  meta <- data.frame(
    name = c("ok", "model", "warned", "branch", "b_1", "b_2", "b_3", "gone"),
    type = c("stem", "stem", "stem", "pattern", "branch", "branch", "branch", "stem"),
    parent = c(NA, NA, NA, NA, "branch", "branch", "branch", NA),
    error = c(NA, "object 'y' not found", NA, NA, NA, "bad branch", NA, "old"),
    warnings = c(NA, NA, "2 rows dropped", NA, NA, NA, NA, NA)
  )
  table <- problems_table(meta, c("ok", "model", "warned", "branch"))
  expect_equal(table$name, c("branch", "model", "warned")) # errors first
  expect_equal(table$problem, c("Error", "Error", "Warnings"))
  expect_equal(table$message, c("bad branch", "object 'y' not found", "2 rows dropped"))
  expect_equal(table$branches, c("1 of 3 errored", "", ""))
  expect_true("gone" %in% problems_table(meta)$name) # without the pipeline's targets
  expect_equal(nrow(problems_table(meta[1, ])), 0)
  expect_equal(nrow(problems_table(NULL)), 0)
})

test_that("a run's errored targets come from its progress, branches as their pattern", {
  progress <- data.frame(
    name = c("data", "model", "b_2", "b_3"),
    type = c("stem", "stem", "branch", "branch"),
    parent = c("data", "model", "branch", "branch"),
    progress = c("completed", "errored", "errored", "errored")
  )
  expect_equal(run_errored_targets(progress), c("model", "branch"))
  expect_equal(run_errored_targets(progress[1, ]), character())
  expect_equal(describe_names("a"), "a")
  expect_equal(describe_names(c("a", "b")), "a and b")
  expect_equal(describe_names(c("a", "b", "c")), "a, b and c")
  expect_equal(describe_names(letters[1:5]), "a, b, c and 2 more")
})

test_that("the run log shows the end of the log file", {
  path <- withr::local_tempfile()
  writeLines(paste("line", 1:600), path)
  log <- read_run_log(path)
  expect_match(log, "^line 101\n")
  expect_match(log, "line 600$")
  writeLines("\033[31m2 targets produced errors.\033[39m", path)
  expect_equal(read_run_log(path), "2 targets produced errors.") # no colours
  expect_null(read_run_log(NULL))
  expect_null(read_run_log(file.path(tempdir(), "absent.log")))
})

test_that("the Problems tab lists the problems; a row or a notification shows one on the board", {
  skip_on_cran()
  local_problem_store()
  server <- local_app_server()
  notes <- list()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      notes[[length(notes) + 1]] <<- list(html = as.character(ui), type = type)
    },
    .package = "shiny"
  )
  focused <- NULL
  local_mocked_bindings(
    update_targetsboard_focus = function(session, id, node) focused <<- node,
    targetsboard_fit_view = function(...) NULL
  )

  shiny::testServer(server, {
    environment(server)$stop_poller()
    expect_equal(problems()$name, c("branch", "model", "warned"))
    expect_match(output$problems, "object 'y' not found", fixed = TRUE) # the table renders
    expect_match(output$problems_count$html, ">3<", fixed = TRUE)
    expect_match(output$problems_count$html, "text-bg-danger", fixed = TRUE)
    expect_match(output$run_log, "No run started from the board yet", fixed = TRUE)

    session$setInputs(problem_row_clicked = "model")
    expect_equal(focused, "model")
    expect_true(preview_open())

    # A run that keeps going: it finishes, with errors, and says where.
    preview_open(FALSE)
    session$setInputs(keep_going = TRUE, run_all = 1)
    session$elapse(600)
    environment(server)$runner$process$wait(120 * 1000)
    session$elapse(1100)
    expect_length(notes, 1)
    expect_equal(notes[[1]]$type, "warning")
    expect_match(notes[[1]]$html, "Run finished, with errors in branch and model.", fixed = TRUE)
    expect_match(notes[[1]]$html, "Show problem", fixed = TRUE)
    expect_match(notes[[1]]$html, "show_problem", fixed = TRUE)
    expect_match(output$run_log, "errored pipeline", fixed = TRUE)
    # Headed by what the run was.
    expect_match(output$run_log, "^Run started [0-9-]+ [0-9:]+: all targets, keep going after errors
")

    session$setInputs(show_problem = "branch")
    expect_equal(focused, "branch")
    expect_true(preview_open())
  })
})

test_that("the page has the Problems and Log tabs", {
  html <- as.character(app_ui(list(nodes = fake_nodes(), edges = fake_edges()), "_targets.R"))
  expect_match(html, 'data-value="problems"', fixed = TRUE)
  expect_match(html, 'id="problems_count"', fixed = TRUE)
  expect_match(html, 'data-value="log"', fixed = TRUE)
  expect_match(html, 'id="run_log"', fixed = TRUE)
})
