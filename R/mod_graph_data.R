#' What changes on the board's graph, for one browser session
#'
#' A module with only a server: reactives polled from the poller's files
#' and the store. The graph as the poller last recomputed it
#' (`poller_board`), why the script doesn't load (`script_error`), the run's
#' live `progress`, whether a run is going (`running`), `tar_meta()`
#' (`tar_meta`), the targets that warned (`warned_targets`), each built
#' target's time and size (`costs`, target_costs()) and why the
#' outdated ones are outdated (`outdated`, outdated_reasons()) and the
#' targets that are files or reports (`kinds`, target_kinds()).
#'
#' @param id The module's id.
#' @param backend `board_backend()`: shared by every session, not reactive.
#' @return A list of those reactives.
#' @noRd
graph_data_server <- function(id, backend) {
  stopifnot(!shiny::is.reactive(backend))
  shiny::moduleServer(id, function(input, output, session) {
    # The graph, as last recomputed by the poller: re-read only when the
    # poller rewrites its file. The server draws it once Load accepts it.
    poller_board <- shiny::reactivePoll(
      500, session,
      checkFunc = function() {
        path <- backend$rds_path()
        if (is.null(path)) NA else file.mtime(path)
      },
      valueFunc = function() {
        path <- backend$rds_path()
        if (is.null(path) || !file.exists(path)) return(NULL)
        tryCatch(readRDS(path), error = function(e) NULL)
      }
    )

    # A script that stops loading (a typo while editing it): say so, for as
    # long as it lasts, rather than silently keep the last graph.
    script_error <- shiny::reactivePoll(
      500, session,
      checkFunc = function() {
        path <- backend$rds_path()
        if (is.null(path)) NA else file.mtime(poller_error_path(path))
      },
      valueFunc = function() {
        path <- backend$rds_path()
        error_path <- if (!is.null(path)) poller_error_path(path)
        if (is.null(error_path) || !file.exists(error_path)) return(NULL)
        tryCatch(readLines(error_path, warn = FALSE), error = function(e) NULL)
      }
    )

    # Live statuses, from the progress file targets writes during a run:
    # cheap to check, so every half second.
    progress <- shiny::reactivePoll(
      500, session,
      checkFunc = progress_mtime,
      valueFunc = function() {
        list(
          mtime = progress_mtime(),
          # type and parent: the run summary counts as tar_progress_summary().
          data = tryCatch(targets::tar_progress(fields = c("type", "parent", "progress")), error = function(e) NULL)
        )
      }
    )

    # Whether a run is going, whoever started it: checked twice a second.
    running <- shiny::reactivePoll(500, session, checkFunc = backend$busy, valueFunc = backend$busy)

    # Re-read tar_meta() only when targets writes new metadata; checking the
    # file's modification time every second is cheap.
    tar_meta_data <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() file.mtime(store_file("meta")),
      valueFunc = function() {
        tryCatch(targets::tar_meta(targets_only = TRUE), error = function(e) NULL)
      }
    )

    # Targets whose last build raised warnings, for the warning marks.
    warned_targets <- shiny::reactive(warned_targets_of(tar_meta_data()))
    # Each built target's time and size, for its badge (target_costs()).
    costs <- shiny::reactive(target_costs(tar_meta_data()))

    # What the poller writes after the graph: why the outdated targets are,
    # and each target's definition, for its icon.
    after_graph <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() {
        path <- backend$rds_path()
        if (is.null(path)) NA else file.mtime(poller_reasons_path(path))
      },
      valueFunc = function() {
        path <- backend$rds_path()
        reasons_path <- if (!is.null(path)) poller_reasons_path(path)
        if (is.null(reasons_path) || !file.exists(reasons_path)) return(NULL)
        tryCatch(readRDS(reasons_path), error = function(e) NULL)
      }
    )
    # tarchetypes' rerun rules, from the script's text (cue_rules()): read
    # again when the poller redraws, as it does when the script changes.
    rules <- shiny::reactive({
      after_graph()
      cue_rules(backend$loaded_script())
    })
    outdated <- shiny::reactive(outdated_reasons(after_graph(), rules()))
    skipped <- shiny::reactive(skipped_reasons(after_graph(), rules()))
    kinds <- shiny::reactive(target_kinds(after_graph()$manifest))
    commands <- shiny::reactive(target_commands(after_graph()$manifest))
    # The script's targets, NULL until the manifest is read.
    script_targets <- shiny::reactive(after_graph()$manifest$name)
    settings <- shiny::reactive(target_settings(after_graph()$manifest))
    cloud <- shiny::reactive(after_graph()$cloud) # metadata in a bucket (#111)
    # crew (#137): its controllers, as the script sets them, and the last
    # run's totals, which targets writes to the store when the run ends.
    manifest <- shiny::reactive(after_graph()$manifest)
    controllers <- shiny::reactive(after_graph()$controllers)
    crew <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() file.mtime(store_file("crew")),
      valueFunc = function() tryCatch(as.data.frame(targets::tar_crew()), error = function(e) NULL)
    )

    list(
      poller_board = poller_board,
      script_error = script_error,
      progress = progress,
      running = running,
      tar_meta = tar_meta_data,
      warned_targets = warned_targets,
      costs = costs,
      outdated = outdated,
      skipped = skipped,
      kinds = kinds,
      commands = commands,
      script_targets = script_targets,
      settings = settings,
      cloud = cloud,
      rules = rules,
      manifest = manifest,
      controllers = controllers,
      crew = crew
    )
  })
}

# A file in the targets store's meta folder, e.g. "progress" or "meta".
store_file <- function(name) {
  file.path(targets::tar_config_get("store"), "meta", name)
}

progress_mtime <- function() file.mtime(store_file("progress"))

#' Targets whose last build raised warnings
#'
#' targets records a branch's warnings on the branch alone: its pattern
#' carries the mark.
#' @param meta `tar_meta(targets_only = TRUE)`.
#' @noRd
warned_targets_of <- function(meta) {
  if (is.null(meta) || !"warnings" %in% names(meta)) return(character())
  warned <- meta[!is.na(meta$warnings), ]
  names <- warned$name
  if (all(c("type", "parent") %in% names(warned))) {
    branch <- warned$type %in% "branch" & !is.na(warned$parent)
    names[branch] <- warned$parent[branch]
  }
  sort(unique(names))
}
