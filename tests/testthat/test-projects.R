# targets projects (R/projects.R): the board on one of _targets.yaml's
# projects, chosen by TAR_PROJECT or `project`, and switched in the board.

test_that("a plain pipeline has one project, main, and the board doesn't show it", {
  local_pipeline()
  withr::local_envvar(TAR_PROJECT = NA)
  expect_null(listed_projects())
  expect_equal(active_project(), "main")
  expect_equal(board_projects(), "main")
  expect_false(shows_projects())
  withr::local_envvar(TAR_PROJECT = "")
  expect_equal(active_project(), "main") # empty: as unset
})

test_that("the projects of _targets.yaml, their scripts and stores", {
  local_projects()
  expect_equal(listed_projects(), c("a", "b"))
  expect_equal(board_projects(), c("a", "b", "main")) # the active one too
  expect_true(shows_projects())
  expect_equal(project_script("b"), "b.R")
  expect_equal(project_store("b"), "store_b")
  expect_equal(project_scripts(), c("a.R", "b.R", "_targets.R"))

  # A script belongs to the current project if it names it, else to the
  # first that does; one no project names stays on the current project.
  expect_equal(project_of_script("b.R", "main"), "b")
  expect_equal(project_of_script("a.R", "main"), "a")
  expect_equal(project_of_script("_targets.R", "a"), "main")
  writeLines("list()", "other.R")
  expect_equal(project_of_script("other.R", "b"), "b")

  # Only main in the file: hidden. On another project: shown, so you know.
  writeLines(c("main:", "  store: s"), "_targets.yaml")
  expect_false(shows_projects(board_projects("main"), "main"))
  expect_true(shows_projects(board_projects("a"), "a"))
})

test_that("with_project() runs code on a project and puts TAR_PROJECT back", {
  local_projects()
  expect_equal(with_project("b", targets::tar_config_get("store")), "store_b")
  expect_equal(Sys.getenv("TAR_PROJECT", NA), NA_character_) # unset again
  withr::local_envvar(TAR_PROJECT = "a")
  expect_equal(with_project("main", active_project()), "main")
  expect_equal(Sys.getenv("TAR_PROJECT"), "a")
})

test_that("the board starts only on a project there is", {
  local_projects()
  expect_silent(check_project("b"))
  expect_silent(check_project("main")) # targets' default, always there
  expect_error(check_project("zzz"), 'No project "zzz" in _targets.yaml. Its projects: a, b, main.', fixed = TRUE)
  expect_error(check_project(c("a", "b")), "must be the name of a project")
})

test_that("the Project field lists the projects on the active one", {
  local_projects()
  html <- as.character(project_picker("script-project", "output['script-show_projects']", active = "b"))
  expect_match(html, 'class="shiny-panel-conditional tb-project"', fixed = TRUE)
  expect_match(html, "data-display-if=\"output[&#39;script-show_projects&#39;]\"", fixed = TRUE)
  expect_match(html, ">Project</label>", fixed = TRUE)
  expect_match(html, '<option value="b" selected>b</option>', fixed = TRUE)
  expect_match(html, '<option value="main">main</option>', fixed = TRUE)
  expect_match(html, "form-select form-select-sm", fixed = TRUE) # Bootstrap's select

  # The active project gone from the file: says so.
  expect_equal(
    project_choices("c", c("a", "b")),
    c(a = "a", b = "b", main = "main", "c (not in _targets.yaml)" = "c")
  )
})

test_that("tar_board() starts on TAR_PROJECT or `project`, with its script, and leaves your session alone", {
  local_projects()
  local_mocked_bindings(r_bg = function(func, args, ...) args, .package = "callr")
  args <- suppressMessages(tar_board(browse = FALSE, verbose = FALSE))
  expect_equal(args$project, "main") # easy entry: nothing to set
  expect_equal(args$script, "_targets.R")

  args <- suppressMessages(tar_board(project = "b", browse = FALSE, verbose = FALSE))
  expect_equal(args$project, "b")
  expect_equal(args$script, "b.R")
  expect_setequal(args$metadata$nodes$id, c("b1", "b2")) # drawn on b
  expect_equal(Sys.getenv("TAR_PROJECT", NA), NA_character_) # untouched

  withr::local_envvar(TAR_PROJECT = "a")
  args <- suppressMessages(tar_board(browse = FALSE, verbose = FALSE))
  expect_equal(args$project, "a")
  expect_equal(args$script, "a.R")

  expect_error(tar_board(project = "zzz", browse = FALSE), 'No project "zzz"')
})

test_that("tar_board_app() sets its project while it runs", {
  local_projects()
  metadata <- list(nodes = fake_nodes(), edges = fake_edges())
  app <- tar_board_app(project = "b", metadata = metadata)
  withr::local_envvar(TAR_PROJECT = NA)
  app$onStart()
  expect_equal(Sys.getenv("TAR_PROJECT"), "b")
  expect_error(tar_board_app(project = "zzz", metadata = metadata), 'No project "zzz"')
})

test_that("the Project field switches project, and Load follows a script's project", {
  skip_on_cran()
  local_projects()
  args <- script_args()
  shiny::testServer(script_server, args = args, {
    backend <- args$backend
    expect_equal(output$show_projects, "true")

    session$setInputs(project = "b")
    expect_equal(backend$project(), "b")
    expect_equal(backend$loaded_script(), "b.R")
    expect_equal(Sys.getenv("TAR_PROJECT"), "b") # the board's process: the poller and runs follow
    expect_equal(targets::tar_config_get("store"), "store_b")

    # Load: another project's script, on that project.
    session$setInputs(path = "a.R", load = 1)
    expect_equal(backend$project(), "a")
    expect_equal(backend$loaded_script(), "a.R")
    # A script no project names: on the current project.
    writeLines(c("library(targets)", "list(tar_target(o1, 1))"), "other.R")
    session$setInputs(path = "other.R", load = 2)
    expect_equal(backend$project(), "a")
    expect_equal(backend$loaded_script(), "other.R")

    # During a run, the project stays.
    args$running(TRUE)
    session$flushReact()
    session$setInputs(project = "b")
    expect_equal(backend$project(), "a")
    session$setInputs(path = "b.R", load = 3)
    expect_equal(backend$project(), "a")
  })
})

test_that("the Project field shows only with projects, and follows _targets.yaml as it changes", {
  skip_on_cran()
  local_projects()
  args <- script_args()
  shiny::testServer(script_server, args = args, {
    expect_equal(output$show_projects, "true")
    writeLines(c("main:", "  store: s"), "_targets.yaml") # only main: nothing to choose
    session$elapse(1000)
    expect_equal(output$show_projects, "false")
    writeLines(c("a:", "  script: a.R", "c:", "  script: c.R"), "_targets.yaml")
    session$elapse(1000)
    expect_equal(output$show_projects, "true")
    expect_equal(listed(), c("a", "c"))
  })
})
