#' The Workers tab: crew's controllers (#137)
#'
#' Shown only when the pipeline sets a crew controller. One row per
#' controller, and one for "main", the targets with `deployment = "main"`:
#' how many of its targets run now, are built and errored in this run, from
#' the progress file; and the last run's totals, `tar_crew()`'s targets and
#' seconds, which targets writes when the run ends. targets records no more
#' than that per controller, and nothing per worker.
#'
#' @param id The module's id.
#' @noRd
workers_ui <- function(id) {
  shiny::tags$div(class = "tb-workers", reactable::reactableOutput(shiny::NS(id, "table")))
}

#' @rdname workers_ui
#' @param progress Reactive: the progress poll, `list(mtime, data)`.
#' @param manifest Reactive: the poller's manifest, or NULL.
#' @param controllers Reactive: crew's controller names, or NULL.
#' @param crew Reactive: `tar_crew()`, or NULL.
#' @return A reactive: whether the pipeline uses crew, for the tab.
#' @noRd
workers_server <- function(id, progress, manifest, controllers, crew) {
  stopifnot(
    shiny::is.reactive(progress), shiny::is.reactive(manifest),
    shiny::is.reactive(controllers), shiny::is.reactive(crew)
  )
  shiny::moduleServer(id, function(input, output, session) {
    table <- shiny::reactive(workers_table(progress()$data, manifest(), controllers(), crew()))
    output$table <- reactable::renderReactable({
      shiny::req(table())
      reactable::reactable(
        table(),
        theme = board_table_theme(),
        sortable = FALSE,
        columns = list(
          controller = reactable::colDef(name = "Controller"),
          running = reactable::colDef(name = "Running"),
          built = reactable::colDef(name = "Built"),
          errored = reactable::colDef(name = "Errored"),
          targets = reactable::colDef(name = "Last run: targets", na = "\u2014"),
          seconds = reactable::colDef(name = "Last run: seconds", na = "\u2014", format = reactable::colFormat(digits = 1))
        )
      )
    })
    # The tab is hidden until the poller finds crew: a table suspended then
    # didn't resume when nav_show() showed it. It is small.
    shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
    shiny::reactive(length(controllers()) > 0)
  })
}

#' Each target's controller: "main" for `deployment = "main"`, the one its
#' `tar_resources_crew(controller = )` names, or the default, the first.
#' @param manifest The poller's manifest: `name`, `deployment`, `resources`
#'   as text ("crew (controller b)").
#' @return A named character vector, by target.
#' @noRd
target_controllers <- function(manifest, controllers) {
  # No target sets resources: the manifest has no such column.
  resources <- manifest$resources %||% rep(NA_character_, nrow(manifest))
  named <- sub("^.*crew \\(controller ([^)]+)\\).*$", "\\1", resources)
  named[is.na(resources) | named == resources] <- NA
  out <- ifelse(is.na(named), controllers[1], named)
  out[manifest$deployment %in% "main"] <- "main"
  stats::setNames(out, manifest$name)
}

#' The Workers table
#' @param progress `tar_progress()` with `type` and `parent`, or NULL.
#' @param crew `tar_crew()`, or NULL.
#' @return A data frame, one row per controller and "main" when it has
#'   targets; NULL without controllers.
#' @noRd
workers_table <- function(progress, manifest, controllers, crew) {
  if (!length(controllers)) return(NULL)
  assigned <- if (!is.null(manifest)) target_controllers(manifest, controllers) else character()
  rows <- c(controllers, if ("main" %in% assigned) "main")
  # Branches go where their pattern goes; patterns themselves aren't run.
  progress <- progress[!progress$type %in% "pattern", , drop = FALSE]
  where <- unname(assigned[ifelse(progress$type %in% "branch", progress$parent, progress$name)])
  where[is.na(where)] <- controllers[1]
  count <- function(state) vapply(rows, function(r) sum(where == r & progress$progress == state), integer(1))
  # An unnamed controller gets a new name each time the script loads: a lone
  # one takes the crew file's only row.
  if (length(controllers) == 1 && !is.null(crew) && nrow(crew) == 1) crew$controller <- controllers
  last <- match(rows, crew$controller)
  data.frame(
    controller = rows,
    running = unname(count("dispatched")),
    built = unname(count("completed")),
    errored = unname(count("errored")),
    targets = as.integer(crew$targets[last]),
    seconds = as.numeric(crew$seconds[last])
  )
}
