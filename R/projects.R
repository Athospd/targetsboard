#' targets projects: the board on one of `_targets.yaml`'s projects
#'
#' A project of `_targets.yaml` names a script and a store (and any other
#' setting; `inherits` takes another's). `TAR_PROJECT` chooses the active
#' one, `"main"` unless set. The board keeps its active project in its own
#' process's `TAR_PROJECT`: every `targets::tar_config_get()` there, and the
#' poller and the runs it starts (they inherit the variable), follow it, so
#' nothing else needs to pass the project around. The author's R session is
#' never changed.
#'
#' @name projects
#' @noRd
NULL

# The active project: TAR_PROJECT, "main" when unset or empty.
active_project <- function() {
  project <- Sys.getenv("TAR_PROJECT")
  if (nzchar(project)) project else "main"
}

# The projects the board offers: those of _targets.yaml (TAR_CONFIG), in its
# order, then "main", which targets always has (its defaults, if the file
# doesn't name it), and the active one, even if the file no longer has it.
board_projects <- function(active = active_project(), listed = listed_projects()) {
  unique(c(listed, "main", active))
}

# The projects _targets.yaml (TAR_CONFIG) defines: none without the file.
listed_projects <- function() {
  tryCatch(targets::tar_config_projects(), error = function(e) NULL)
}

# Whether the board shows its projects: when there is a choice, or the
# board isn't on the default. A plain pipeline never sees them.
shows_projects <- function(projects = board_projects(active), active = active_project()) {
  length(projects) > 1 || !identical(active, "main")
}

# A project's script and store, as targets resolves them (inherits too).
project_script <- function(project = active_project()) targets::tar_config_get("script", project = project)
project_store <- function(project = active_project()) targets::tar_config_get("store", project = project)

# The scripts of the projects, for the script bar's suggestions: none when
# the board doesn't show projects.
project_scripts <- function(projects = board_projects(active), active = active_project()) {
  if (!shows_projects(projects, active)) return(character())
  unique(vapply(projects, project_script, character(1), USE.NAMES = FALSE))
}

#' The project a script belongs to
#'
#' The current project if it names the script; else the first other project
#' that does; else the current one: a script no project names runs on the
#' current project's store, as `tar_make(script = )` would.
#' @noRd
project_of_script <- function(script, current = active_project(), projects = board_projects(current)) {
  same <- function(project) {
    identical(normalizePath(project_script(project), mustWork = FALSE), normalizePath(script, mustWork = FALSE))
  }
  if (same(current)) return(current)
  for (project in setdiff(projects, current)) if (same(project)) return(project)
  current
}

# Sets the active project in this process. "main" unsets the variable, as
# for a plain pipeline.
set_active_project <- function(project) {
  if (identical(project, "main")) Sys.unsetenv("TAR_PROJECT") else Sys.setenv(TAR_PROJECT = project)
  invisible(project)
}

# Runs `code` on `project`, then puts TAR_PROJECT back as it was: for what
# tar_board() computes in the author's session.
with_project <- function(project, code) {
  old <- Sys.getenv("TAR_PROJECT", NA)
  set_active_project(project)
  on.exit(if (is.na(old)) Sys.unsetenv("TAR_PROJECT") else Sys.setenv(TAR_PROJECT = old), add = TRUE)
  force(code)
}

# A project the board can start on: "main", or one in _targets.yaml. targets
# itself gives an unknown project its defaults without a word.
check_project <- function(project) {
  if (!rlang::is_string(project) || !nzchar(project)) {
    stop("`project` must be the name of a project, a string.", call. = FALSE)
  }
  known <- board_projects("main")
  if (!project %in% known) {
    stop(
      "No project \"", project, "\" in ", Sys.getenv("TAR_CONFIG", "_targets.yaml"), ". ",
      "Its projects: ", paste(known, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(project)
}

# The Project field's choices: the projects, as written, named for their
# labels. One the file no longer lists says so ("main", targets' default,
# always exists).
project_choices <- function(active = active_project(), listed = listed_projects()) {
  projects <- board_projects(active, listed)
  gone <- !projects %in% c(listed, "main")
  stats::setNames(projects, ifelse(gone, paste(projects, "(not in _targets.yaml)"), projects))
}

#' The Project field, left of the script bar
#'
#' A native select on the active project, drawn once: the server updates
#' its choices (updateSelectInput()), shows it only when the board shows
#' projects (a conditionalPanel on `show_projects`), and disables it during
#' a run, which writes to the project's store (the `tb-disable` message,
#' srcjs/actions.js: Shiny has no update for that).
#' @noRd
project_picker <- function(id, show, active = active_project(), listed = listed_projects()) {
  input <- shiny::selectInput(
    id, "Project", project_choices(active, listed), selected = active,
    selectize = FALSE, width = "auto"
  )
  # Bootstrap's select, small as the bar's other controls.
  select <- htmltools::tagQuery(input)$find("select")$removeClass("form-control")$addClass("form-select form-select-sm")
  shiny::conditionalPanel(show, class = "tb-project", select$allTags())
}

# The line that makes the author's console follow a project.
project_env_code <- function(project) {
  sprintf("Sys.setenv(TAR_PROJECT = %s)", deparse(project))
}
