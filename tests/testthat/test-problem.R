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
  problem <- target_problem("model", command = "analyze(data)")
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

  expect_null(target_problem("data")$error)
  expect_false(has_problem(target_problem("data")))
  expect_null(target_problem("absent"))
})

test_that("a warned target's problem reproduces from its inputs and seed", {
  skip_on_cran()
  local_problem_store()
  problem <- target_problem("warned", command = target_commands(targets::tar_manifest())[["warned"]])
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
  problem <- target_problem("branch")
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

  problem <- target_problem("branch", command = "if (nums == 2) stop(\"bad branch\") else nums")
  expect_true(has_problem(problem))
  expect_equal(problem$type, "pattern")
  expect_equal(problem$command, "if (nums == 2) stop(\"bad branch\") else nums")
  expect_length(problem$branches, 1)
  expect_equal(problem$branches[[1]]$error, "bad branch")
  expect_true(problem$branches[[1]]$workspace)
  expect_match(as.character(problem_ui(problem)), "Branches with problems: 1", fixed = TRUE)

  # A name with no metadata and no branches is still no problem.
  expect_null(target_problem("absent"))
})

test_that("the pane shows the snippets to copy, but no debug run while running", {
  problem <- list(
    name = "model", type = "stem", error = "boom", warnings = NULL, seed = 1L,
    workspace = TRUE, traceback = c("analyze(data)", "stats::lm(y ~ x)"),
    command = "analyze(data)", functions = "analyze", branches = list()
  )
  html <- as.character(problem_ui(problem, c("data", "params"), script = "_targets.R"))
  expect_match(html, "<pre class=\"language-r\">targets::tar_workspace(model)", fixed = TRUE)
  expect_match(html, ">Error<", fixed = TRUE)
  expect_match(html, "callr_function = NULL, use_crew = FALSE, as_job = FALSE", fixed = TRUE)
  expect_match(html, "# Restart R first", fixed = TRUE)
  expect_match(html, 'data-target="params"', fixed = TRUE)
  expect_match(html, "1 call in packages", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr(">Copy<", html, fixed = TRUE))), 2)
  # Copied by actions.js, which says so; its inputs' links show them.
  expect_match(html, 'class="btn btn-sm btn-default tb-copy" data-copy="targets::tar_workspace(model)', fixed = TRUE)
  expect_match(html, 'data-target="params" data-tb-input="target_link" data-tb-value="&quot;params&quot;"', fixed = TRUE)

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
  Sys.setFileTime(progress_file, Sys.time() - 60) # any rewrite is then later
  before <- file.mtime(progress_file)
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
    backend_of(server)$stop_poller()
    session$setInputs(`board-graph-flow_focused_node` = "model", `board-graph-flow_double_clicked_node` = "model")
    html <- output[["board-preview-content"]]$html
    expect_match(html, 'class="tb-problem" data-target="model"', fixed = TRUE)
    expect_match(html, 'data-target="data"', fixed = TRUE) # its input, linked
    expect_no_match(html, "tb-preview-printed", fixed = TRUE)

    session$setInputs(`board-graph-flow_focused_node` = "warned")
    html <- output[["board-preview-content"]]$html
    expect_match(html, 'data-target="warned"', fixed = TRUE)
    expect_match(html, "tb-preview-printed", fixed = TRUE) # and its value

    # A pattern whose branch errored: its problem says why there is no value.
    session$setInputs(`board-graph-flow_focused_node` = "branch")
    html <- output[["board-preview-content"]]$html
    expect_match(html, 'data-target="branch"', fixed = TRUE)
    expect_no_match(html, "No value stored", fixed = TRUE)

    session$setInputs(`board-graph-flow_focused_node` = "data")
    expect_no_match(output[["board-preview-content"]]$html, "tb-problem", fixed = TRUE)
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
  args <- local_board_args()
  backend <- args$backend
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

  shiny::testServer(board_server, args = args, {
    backend$stop_poller()
    expect_match(output[["problems-table"]], "object 'y' not found", fixed = TRUE) # the table renders
    expect_match(output[["problems-count"]]$html, ">3<", fixed = TRUE)
    expect_match(output[["problems-count"]]$html, "text-bg-danger", fixed = TRUE)
    expect_match(output[["log-text"]], "No run started from the board yet", fixed = TRUE)

    # A problem picked: shown on the board, with the preview asked for.
    requests <- preview_request()
    session$setInputs(`problems-row_clicked` = "model")
    expect_equal(focused, "model")
    expect_equal(preview_request(), requests + 1)

    # A run that keeps going: it finishes, with errors, and says where.
    session$setInputs(`run-keep_going` = TRUE, `run-all` = 1)
    session$elapse(600)
    backend$runner$process$wait(120 * 1000)
    session$elapse(1100)
    expect_length(notes, 1)
    expect_equal(notes[[1]]$type, "warning")
    expect_match(notes[[1]]$html, "Run finished, with errors in branch and model.", fixed = TRUE)
    expect_match(notes[[1]]$html, "Show problem", fixed = TRUE)
    expect_match(notes[[1]]$html, "show_problem", fixed = TRUE)
    expect_match(output[["log-text"]], "errored pipeline", fixed = TRUE)
    # Headed by what the run was.
    expect_match(output[["log-text"]], "^Run started [0-9-]+ [0-9:]+: all targets, keep going after errors
")

    requests <- preview_request()
    session$setInputs(`run-show_problem` = "branch")
    expect_equal(focused, "branch")
    expect_equal(preview_request(), requests + 1)
  })
})

test_that("the page has the Problems and Log tabs", {
  html <- as.character(app_ui(list(nodes = fake_nodes(), edges = fake_edges()), "_targets.R"))
  expect_match(html, 'data-value="problems"', fixed = TRUE)
  expect_match(html, 'id="board-problems-count"', fixed = TRUE)
  expect_match(html, 'data-value="log"', fixed = TRUE)
  expect_match(html, 'id="board-log-text"', fixed = TRUE)
})

test_that("a problem opens without running the script (#176)", {
  skip_on_cran()
  local_problem_store()
  # Sourcing the script would now take 5 s: target_problem() mustn't.
  lines <- readLines("_targets.R")
  writeLines(c("if (nzchar(Sys.getenv('TB_SLOW_SCRIPT'))) Sys.sleep(5)", lines), "_targets.R")
  withr::local_envvar(TB_SLOW_SCRIPT = "true")
  for (name in c("model", "branch")) {
    took <- system.time(problem <- target_problem(name))[["elapsed"]]
    expect_true(has_problem(problem))
    expect_lt(took, 1)
  }
  # The branch's position still comes, from the pattern's children.
  expect_equal(c(problem$branches[[1]]$position, problem$branches[[1]]$of), c(2, 3))
})

test_that("the preview's problem follows its target, not the graph's redraws (#176)", {
  skip_on_cran()
  local_problem_store()
  built <- 0
  real <- target_problem
  local_mocked_bindings(target_problem = function(...) {
    built <<- built + 1
    real(...)
  })
  graph <- targets::tar_visnetwork(targets_only = TRUE)$x
  backend <- board_backend("_targets.R", targets_only = TRUE)
  withr::defer(backend$stop_poller())
  args <- list(
    backend = backend, focused = shiny::reactiveVal("model"), open_request = shiny::reactiveVal(1),
    board = shiny::reactiveVal(list(x = graph)), running = shiny::reactiveVal(FALSE),
    tar_meta = shiny::reactiveVal(targets::tar_meta()), methods = shiny::reactiveVal(0),
    commands = shiny::reactiveVal(c(model = "analyze(data)"))
  )
  shiny::testServer(preview_server, args = args, {
    session$flushReact()
    expect_match(output$content$html, "analyze(data)", fixed = TRUE)
    before <- built
    args$board(list(x = graph, redrawn = TRUE)) # the poller redraws
    args$running(TRUE) # a run starts
    args$commands(c(model = "analyze(data)", other = "1")) # another command changes
    session$flushReact()
    output$content
    expect_equal(built, before)
    args$commands(c(model = "analyze(data) + 0")) # its own command changes
    session$flushReact()
    expect_match(output$content$html, "analyze(data) + 0", fixed = TRUE)
    expect_equal(built, before + 1)
  })
})

test_that("a target's warnings show as targets stored them, not split on \". \" (#203)", {
  # targets joins a target's warnings with ". ": one warning with a ". " in
  # it can't be told from two, so the text is shown whole.
  html <- as.character(problem_details(list(warnings = "Removed 3 rows. Check the input"), list()))
  expect_match(html, "<p class=\"tb-problem-warnings\">Removed 3 rows. Check the input</p>", fixed = TRUE)
  expect_no_match(html, "<li>", fixed = TRUE)
})
