#' The value preview: a sidebar beside the board's graph
#'
#' Shows the focused target's value, in a frame of views (preview_frame()),
#' or its problem first if it errored or warned; and, above, why it is
#' outdated if it is. Opened on request, it
#' follows the focus; it closes with the focus, or by its own toggle.
#' @param id The module's id.
#' @noRd
preview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::sidebar(
    id = ns("sidebar"), class = "tb-preview-pane",
    width = "40%",
    open = FALSE,
    position = "right",
    # Apart from the value's output: new reasons don't rebuild the value.
    shiny::uiOutput(ns("description")),
    shiny::uiOutput(ns("rule")),
    shiny::uiOutput(ns("kind")),
    shiny::uiOutput(ns("why")),
    shiny::uiOutput(ns("branch_inputs")),
    shiny::uiOutput(ns("debug")),
    # The same spinner card as Load's while a value is read and its preview
    # built, over the previous preview.
    shiny::tags$div(
      class = "tb-preview-loading tb-loading",
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("content")),
        type = 8, custom.css = TRUE, hide.ui = FALSE, id = ns("spinner"),
        caption = "Loading value\u2026"
      )
    )
  )
}

#' @param backend The board's backend (board_backend()): its preview cache
#'   and its script.
#' @param focused Reactive: the focused target's name, or `NULL`.
#' @param open_request Reactive event: opens the preview on the focused
#'   target, each time it changes; `NULL` until the first request.
#' @param board Reactive: the graph the board draws, for a target's inputs.
#' @param running Reactive: whether the pipeline is running.
#' @param tar_meta Reactive: `targets::tar_meta()`, polled.
#' @param methods Reactive: changes when the project's preview methods are
#'   loaded again.
#' @param outdated Reactive: why outdated targets are (outdated_reasons()).
#' @param skipped Reactive: why up-to-date targets are (skipped_reasons()).
#' @param settings Reactive: each target's settings (target_settings()).
#' @param branch_request Reactive event: `list(pattern, position)`, a branch
#'   clicked on the canvas, for the preview to show.
#' @param log `function(code)`: logs the value's tar_read() (console_log()).
#' @return A list of reactives: `target_link`, the target a link in the
#'   problem pane asks to show.
#' @noRd
preview_server <- function(id, backend, focused, open_request, board, running, tar_meta, methods,
                           outdated = shiny::reactive(list()), commands = shiny::reactive(character()),
                           rules = shiny::reactive(NULL), skipped = shiny::reactive(list()),
                           settings = shiny::reactive(NULL), kinds = shiny::reactive(character()),
                           branch_request = shiny::reactive(NULL),
                           log = function(code) NULL) {
  stopifnot(
    !shiny::is.reactive(backend), shiny::is.reactive(focused), shiny::is.reactive(open_request),
    shiny::is.reactive(board), shiny::is.reactive(running), shiny::is.reactive(tar_meta),
    shiny::is.reactive(methods), shiny::is.reactive(outdated), shiny::is.reactive(commands),
    shiny::is.reactive(rules), shiny::is.reactive(skipped), shiny::is.reactive(settings),
    shiny::is.reactive(kinds), shiny::is.reactive(branch_request)
  )
  shiny::moduleServer(id, function(input, output, session) {
    previews <- backend$previews
    store <- function() targets::tar_config_get("store")

    is_open <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(open_request(), is_open(TRUE))
    shiny::observeEvent(focused(), {
      if (is.null(focused())) is_open(FALSE)
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$sidebar, {
      if (isFALSE(input$sidebar)) is_open(FALSE)
    })
    shiny::observe({
      bslib::toggle_sidebar("sidebar", open = is_open() && !is.null(focused()))
    })

    # The value it shows, as the console reads it (#185).
    shiny::observe({
      shiny::req(is_open(), focused())
      target <- preview_target(focused(), store(), branch = branch())
      if (!is.null(target)) log(read_code(target$name, store(), branch_arg(target)))
    })

    # A pattern's preview opens on its first branch, or on the branch asked
    # for on the canvas (#147), which may come before its focus does; the
    # picker changes it.
    branch <- shiny::reactiveVal(1L)
    asked <- NULL # list(pattern, position)
    shiny::observeEvent(branch_request(), {
      asked <<- branch_request()
      if (identical(shiny::isolate(focused()), asked$pattern)) {
        branch(as.integer(asked$position))
        asked <<- NULL
      }
    })
    shiny::observeEvent(focused(), {
      branch(if (identical(asked$pattern, focused())) as.integer(asked$position) else 1L)
      asked <<- NULL
    }, priority = 10, ignoreNULL = FALSE)
    shiny::observeEvent(input$branch, branch(as.integer(input$branch)))

    # A store whose custom R code the author trusted, this session (#237).
    trusted <- shiny::reactiveVal(character())
    shiny::observeEvent(input$trust, trusted(union(trusted(), normalizePath(store(), winslash = "/", mustWork = FALSE))))
    is_trusted <- function() normalizePath(store(), winslash = "/", mustWork = FALSE) %in% trusted()

    # A large value is read only when asked: it can take long and fill the
    # board's memory.
    read_anyway <- shiny::reactiveVal(character())
    shiny::observeEvent(input$read, {
      target <- preview_target(focused(), store(), branch = branch())
      if (!is.null(target)) read_anyway(union(read_anyway(), preview_key(target)))
    })

    # The preview follows the focused target's records in tar_meta(), its
    # branches' too: it changes when the target is invalidated or built
    # again. Only its own: a run writes new metadata for each target it
    # builds, which shouldn't redraw a preview whose target didn't change.
    seen_records <- NULL
    shiny::observeEvent(focused(), {
      seen_records <<- focused_records(shiny::isolate(tar_meta()), focused())
    }, ignoreNULL = FALSE)
    records_changed <- shiny::reactiveVal(0)
    shiny::observeEvent(tar_meta(), {
      records <- focused_records(tar_meta(), shiny::isolate(focused()))
      if (identical(records, seen_records)) return()
      seen_records <<- records
      records_changed(shiny::isolate(records_changed()) + 1)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # The pipeline's functions as written (function_definitions()), read
    # again with each new graph: the poller redraws when their files change.
    functions <- shiny::reactive({
      board()
      function_definitions(backend$poller$script)
    })
    # A function on the canvas (targets_only = FALSE) has code, not a value.
    is_function <- function(name) {
      nodes <- shiny::isolate(board())$x$nodes
      "type" %in% names(nodes) && name %in% nodes$id[nodes$type %in% "function"]
    }

    # tar_target(description = ), or tar_map(descriptions = )'s.
    output$description <- shiny::renderUI({
      shiny::req(is_open(), focused())
      description_ui(target_description(board()$x$nodes, focused()))
    })

    # The command that runs (target_commands()): after tar_map() and friends,
    # not always what the author wrote.
    output$command <- shiny::renderUI({
      shiny::req(is_open(), focused())
      command_ui(commands()[focused()])
    })

    # Its memory, storage, deployment and the rest (target_settings()).
    output$settings <- shiny::renderUI({
      shiny::req(is_open(), focused())
      all <- settings()
      settings_ui(if (!is.null(all)) all[all$name %in% focused(), , drop = FALSE])
    })

    # The slices a pattern's branch ran on (#142), worked out when its
    # Inputs section opens: tar_branches() runs the script, a second or so.
    # shortcut: it blocks the session meanwhile; an ExtendedTask if it grows slow.
    output$branch_inputs <- shiny::renderUI({
      shiny::req(is_open(), focused())
      target <- preview_target(focused(), store(), branch = branch())
      branch_inputs_section(target$pattern$branch_name, ns = session$ns)
    })
    branches_of <- new.env(parent = emptyenv()) # tar_branches() per pattern
    output$inputs <- shiny::renderUI({
      branch <- shiny::req(input$inputs_open)
      name <- shiny::isolate(focused())
      known <- branches_of[[name]]
      if (is.null(known) || !branch %in% known[[1]]) {
        known <- tryCatch(
          as.data.frame(do.call(targets::tar_branches, list(
            name = as.symbol(name), script = backend$poller$script, store = store()
          ))),
          error = function(e) NULL
        )
        branches_of[[name]] <- known
      }
      meta <- tryCatch(targets::tar_meta(fields = c("type", "children", "bytes", "format", "repository"), store = store()),
                       error = function(e) NULL)
      # A store with custom R code: the slices are read once it is trusted
      # (#237), as the preview's value is; until then, only their code shows.
      custom <- store_needs_trust(has_custom_code(meta))
      all <- settings()
      iterations <- if (!is.null(all) && "iteration" %in% names(all)) stats::setNames(all$iteration, all$name)
      branch_inputs_ui(
        branch_inputs(known, branch, meta), iterations, store = store(),
        read = if (!custom || is_trusted()) function(code) read_trusted(eval(parse(text = code), envir = baseenv()))
      )
    })

    # The focused target, once per change: focused() fires again with each
    # redraw, its value seldom (#242).
    focused_target <- shiny::reactiveVal(NULL)
    shiny::observe(focused_target(focused()), priority = 1)

    # The focused target's command, once per change: commands() changes with
    # every recompute of the poller, the target's command seldom.
    focused_command <- shiny::reactiveVal(NULL)
    shiny::observe({
      command <- if (!is.null(focused())) unname(commands()[focused()])
      focused_command(if (length(command) == 1 && !is.na(command)) command)
    }, priority = 1) # before the outputs that read it: they render once
    # Its problem follows the target, its records and its command, not the
    # graph: a redraw or a run starting doesn't build it again.
    focused_problem <- shiny::reactive({
      shiny::req(focused_target())
      records_changed()
      target_problem(focused_target(), command = focused_command())
    })

    # The focused target's inputs, once per change: the board redraws often.
    focused_inputs <- shiny::reactiveVal(character())
    shiny::observe({
      edges <- board()$x$edges
      name <- focused()
      focused_inputs(if (!is.null(name)) intersect(edges$from[edges$to == name], board_targets(board()$x$nodes)) else character())
    }, priority = 1)

    # Reproduce and debug any target, not only one that failed: the Problems
    # part gives the same code for those, so they don't get it twice. It
    # follows its target, not the board: a redraw or a run starting would
    # build it again, closed (#242). The debug run's block follows the run.
    debug_target <- shiny::reactive({
      shiny::req(is_open(), focused_target())
      name <- focused_target()
      records_changed()
      # A global object or function (targets_only = FALSE) isn't a target (#243).
      if (!debuggable(shiny::isolate(board())$x$nodes, name) || has_problem(focused_problem())) return(NULL)
      target <- preview_target(name, store(), branch = branch())
      if (is.null(target)) return(NULL)
      # A pattern: the branch picked, if one is (not all of them, branch 0).
      own <- target$pattern$branch_name %||% if (is.null(target$pattern)) name
      if (is.null(own)) return(NULL)
      list(name = name, own = own, branch = !is.null(target$pattern))
    })
    # What the Debug section's code needs: its seed and workspace too.
    debug_details <- shiny::reactive({
      debug <- debug_target()
      if (is.null(debug)) return(NULL)
      # records_changed() in debug_target() follows this target's records:
      # another target's new metadata doesn't read the store again.
      meta <- shiny::isolate(tar_meta())
      row <- if (!is.null(meta)) meta[meta$name %in% debug$own, , drop = FALSE][1, , drop = FALSE]
      field <- function(f) if (!is.null(row) && f %in% names(row)) na_null(row[[f]])
      c(debug, list(
        command = focused_command(), inputs = focused_inputs(), seed = field("seed"), seconds = field("seconds"),
        workspace = debug$own %in% tryCatch(targets::tar_workspaces(), error = function(e) character())
      ))
    })
    output$debug <- shiny::renderUI({
      debug <- debug_details()
      if (is.null(debug)) return(NULL)
      debug_ui(
        debug$own, run = debug$name, command = debug$command, inputs = debug$inputs,
        seed = debug$seed, workspace = debug$workspace,
        branch = debug$branch, script = backend$poller$script,
        store = store(), dir = normalizePath(getwd(), winslash = "/"),
        run_block = shiny::uiOutput(session$ns("debug_run")),
        profile_block = profile_ui(session$ns("profile"))
      )
    })
    # Profile it (#141): what Reproduce gives, under profvis.
    profile_server("profile", shiny::reactive({
      debug <- debug_details()
      # Not before its command is known: there'd be nothing to profile.
      if (is.null(debug) || is.null(debug$command) || (debug$branch && !debug$workspace)) return(NULL)
      dir <- normalizePath(getwd(), winslash = "/")
      list(
        name = debug$own, seconds = debug$seconds, dir = dir,
        code = profile_code(debug$own, debug$command, debug$workspace, debug$inputs, debug$seed,
                            script = backend$poller$script, store = store(), dir = dir)
      )
    }))
    output$debug_run <- shiny::renderUI({
      debug <- debug_target()
      if (is.null(debug)) return(NULL)
      debug_run_block(debug$own, running(), backend$poller$script, run = debug$name,
                      store = store(), dir = normalizePath(getwd(), winslash = "/"))
    })

    # A tarchetypes rerun rule (cue_rules()), whatever the status: a skipped
    # target looks up to date, an aged one gives no hint of when it is due.
    # What its kind means, for kinds whose icon doesn't say enough (#157).
    output$kind <- shiny::renderUI({
      shiny::req(is_open(), focused())
      hint <- kind_hint(unname(kinds()[focused()]))
      if (!is.null(hint)) shiny::tags$p(class = "tb-rule tb-kind-hint", hint)
    })

    # The focused target's build times, once per change: tar_meta() is read
    # again on every metadata write during a run, which would redraw the
    # lines below and reset their Copy (#243). `own`, its record's; `latest`,
    # its or its branches' latest.
    focused_built <- shiny::reactiveVal(list(own = NA, latest = NA))
    shiny::observe(focused_built(build_times(tar_meta(), focused())), priority = 1)

    output$rule <- shiny::renderUI({
      shiny::req(is_open(), focused())
      rule <- target_rule(rules(), focused())
      shiny::req(rule)
      rule_ui(rule_sentence(rule, focused_built()$own))
    })

    # Why it is outdated, or why a run skips it.
    output$why <- shiny::renderUI({
      shiny::req(is_open(), focused())
      name <- focused()
      reasons <- outdated()[[name]]
      if (length(reasons)) return(reasons_ui(reasons, functions()))
      sentences <- skipped()[[name]]
      shiny::req(sentences)
      skipped_ui(sentences, focused_built()$latest)
    })

    # The pane hides an empty block (index.css), and Shiny doesn't render a
    # hidden output: these render anyway, or a block once empty stays so.
    # Each waits for the pane to be open (is_open()).
    for (id in c("description", "rule", "kind", "why", "branch_inputs", "debug", "debug_run")) {
      shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
    }

    # The tab the user last picked in the value's card, kept across targets
    # where it exists (target_tabs(), tab_memory()).
    tabs <- tab_memory()
    shiny::observeEvent(input$value_tab, tabs$picked(input$value_tab))
    tabs_for <- tabs$tabs_for

    output$content <- shiny::renderUI({
      # The deduplicated focus: focused() fires again with each redraw, which
      # rebuilt the pane and closed what was open in it (#242, #264).
      shiny::req(is_open(), focused_target())
      name <- focused_target()
      if (is_function(name)) {
        all <- functions()
        return(function_ui(all[all$name == name, , drop = FALSE], open = TRUE) %||%
          shiny::p("This function's code wasn't found in the script or the files it sources."))
      }
      # A target that errored or warned shows its problem first; an errored
      # one has no value to show.
      problem <- focused_problem()
      problem_part <- NULL
      if (has_problem(problem)) {
        graph <- shiny::isolate(board())$x
        inputs <- intersect(graph$edges$from[graph$edges$to == name], board_targets(graph$nodes))
        problem_part <- problem_ui(
          problem, inputs, running = running(), script = backend$poller$script,
          store = store(), dir = normalizePath(getwd(), winslash = "/"), ns = session$ns
        )
        if (!is.null(problem$error)) return(shiny::tagList(problem_part, target_tabs_card(tabs_for, session$ns)))
      }
      methods()
      records_changed()
      # The problem says why there is no value: a pattern whose branches
      # errored. Without one, say it plainly.
      no_value <- function() {
        shiny::tagList(
          problem_part %||% shiny::p("No value stored for this target yet."),
          target_tabs_card(tabs_for, session$ns)
        )
      }
      target <- preview_target(name, store(), branch = branch())
      if (is.null(target)) return(no_value())
      key <- preview_key(target)
      if (store_needs_trust(target$custom_code) && !is_trusted()) {
        return(shiny::tagList(problem_part, trust_gate(target, store = store(), ns = session$ns),
                              target_tabs_card(tabs_for, session$ns)))
      }
      if ((preview_too_large(target) || preview_in_cloud(target)) && !key %in% read_anyway()) {
        return(shiny::tagList(problem_part, preview_gate(target, store = store(), ns = session$ns),
                              target_tabs_card(tabs_for, session$ns)))
      }
      built <- previews$get(key)
      if (is.null(built)) {
        missing <- new.env() # a value can be NULL itself
        failed <- NULL
        value <- tryCatch(suppressWarnings(read_trusted(read_preview_value(target, store()))), error = function(e) {
          failed <<- conditionMessage(e)
          missing
        })
        # A cloud value that doesn't download: why, a package or credentials (#131).
        if (identical(value, missing) && preview_in_cloud(target)) {
          return(shiny::tagList(problem_part, shiny::tags$div(
            class = "tb-preview-download-error alert alert-warning",
            paste0("Couldn't download the value from ", target$repository, ": ", cli::ansi_strip(failed))
          ), target_tabs_card(tabs_for, session$ns)))
        }
        if (identical(value, missing)) {
          # A format whose package isn't installed here: say which (#228).
          needed <- missing_format_package(target$format)
          if (!is.null(needed)) {
            return(shiny::tagList(problem_part, missing_package_card(target, needed, store(), session$ns),
                                  target_tabs_card(tabs_for, session$ns)))
          }
          return(no_value())
        }
        built <- previews$set(key, build_preview(value, target))
      }
      # Plots are drawn by the board, at the size of the preview. Reusing
      # the ids replaces the previous renderers instead of piling them up.
      plots <- Filter(function(view) inherits(view, "tar_board_preview_plot"), built$views)
      plot_ids <- sprintf("plot_%d", seq_along(plots))
      for (i in seq_along(plots)) {
        local({
          view <- plots[[i]]
          output[[plot_ids[[i]]]] <- render_preview_plot(view, plot_ids[[i]], session)
        })
      }
      # A file's view is built here, in the session, when its section
      # opens: the first now, the others on input$file_open.
      files <- Find(function(view) inherits(view, "tar_board_preview_files"), built$views)
      file_views(list(paths = files$paths, target = target))
      if (!is.null(files)) render_file_section(1)
      shiny::tagList(problem_part, preview_frame(target, built, session$ns(plot_ids), store(), ns = session$ns,
                                                 tabs_for = tabs_for))
    })

    file_views <- shiny::reactiveVal(NULL)
    render_file_section <- function(i) {
      files <- shiny::isolate(file_views())
      if (is.null(files) || i > length(files$paths)) return()
      path <- files$paths[[i]]
      # With the target's paths: a document's rendered file may be one.
      target <- c(files$target, list(files = files$paths))
      output[[file_output_id(i)]] <- shiny::renderUI(file_section_view(path, target))
    }
    shiny::observeEvent(input$file_open, render_file_section(input$file_open))

    list(target_link = shiny::reactive(input$target_link))
  })
}

#' A target's records in tar_meta(), its branches' too
#' @noRd
focused_records <- function(meta, name) {
  if (is.null(meta) || is.null(name)) return(NULL)
  parent <- if ("parent" %in% names(meta)) meta$parent else NA_character_
  meta[meta$name %in% name | parent %in% name, ]
}

#' A target's description, from the graph's nodes (tar_visnetwork()), or NULL
#' when it has none
#' @noRd
target_description <- function(nodes, id) {
  if (is.null(nodes) || !all(c("id", "description") %in% names(nodes))) return(NULL)
  text <- nodes$description[nodes$id %in% id]
  if (length(text) != 1 || is.na(text) || !nzchar(trimws(text))) return(NULL)
  text
}

description_ui <- function(text) {
  if (is.null(text)) return(NULL)
  shiny::tags$p(class = "tb-description", text)
}

#' The Command and Settings tabs of the value's card (#217)
#'
#' Their own outputs, filled apart from the value: the poller's recomputes
#' don't build the value again.
#' @noRd
target_tabs <- function(ns = identity) {
  list(
    bslib::nav_panel("Command", shiny::tags$div(class = "tb-preview-view", shiny::uiOutput(ns("command")))),
    bslib::nav_panel("Settings", shiny::tags$div(class = "tb-preview-view", shiny::uiOutput(ns("settings"))))
  )
}

#' The value's tab row: its views, then Command and Settings
#'
#' @param panels The value's views, as `bslib::nav_panel()`s.
#' @param tabs_for A function of the row's titles, giving the tab to select
#'   (the last one picked), or `NULL` for the first.
#' @noRd
value_tabs <- function(panels = list(), tabs_for = function(titles) NULL, ns = identity, own_tabs = target_tabs(ns)) {
  panels <- c(panels, own_tabs)
  titles <- vapply(panels, nav_title, "")
  selected <- tabs_for(titles)
  do.call(bslib::navset_underline, c(
    list(id = ns("value_tab"), selected = if (isTRUE(selected %in% titles)) selected),
    panels
  ))
}

#' The tab the user last picked, across the cards of targets
#'
#' A card without it falls back to its first tab, which the browser then
#' reports as selected: that one report isn't a pick, the user's next are
#' (#241).
#' @return A list: `picked(title)`, for each tab the browser reports; and
#'   `tabs_for(titles)`, for value_tabs(), the tab to select.
#' @noRd
tab_memory <- function() {
  kept <- NULL
  fallback <- NULL
  list(
    picked = function(title) {
      ignore <- identical(title, fallback)
      fallback <<- NULL
      if (!ignore) kept <<- title
      invisible()
    },
    tabs_for = function(titles) {
      fallback <<- if (!is.null(kept) && !kept %in% titles) titles[1]
      kept
    }
  )
}

# A nav_panel()'s title.
nav_title <- function(panel) as.character(panel$attribs$`data-value` %||% "")

#' Command and Settings in a card of their own, for a target without a
#' value to show: the same place as beside a value
#' @noRd
target_tabs_card <- function(tabs_for = function(titles) NULL, ns = identity) {
  bslib::card(class = "tb-preview tb-target-tabs", value_tabs(tabs_for = tabs_for, ns = ns))
}

#' A target's command, highlighted and copyable, for its Command tab
#' @noRd
command_ui <- function(command) {
  command <- unname(command)
  if (length(command) != 1 || is.na(command) || !nzchar(command)) {
    return(shiny::p(class = "tb-preview-note", "Its command isn't known yet."))
  }
  code_block("As it runs, from tar_manifest()", "command", command)
}

#' A target's settings, for its Settings tab
#'
#' Every setting it has, those that differ from targets' defaults
#' (`setting_defaults`) in bold; a note when it runs on a worker but the
#' main process stores or retrieves its value.
#' @param row Its row of target_settings().
#' @noRd
settings_ui <- function(row) {
  unknown <- shiny::p(class = "tb-preview-note", "Its settings aren't known yet.")
  if (is.null(row) || nrow(row) != 1) return(unknown)
  fields <- setdiff(names(row), "name")
  values <- vapply(fields, function(f) as.character(row[[f]][[1]]), character(1))
  shown <- fields[!is.na(values) & nzchar(values)]
  if (!length(shown)) return(unknown)
  changed <- function(f) is.na(setting_defaults[[f]]) || !identical(values[[f]], setting_defaults[[f]])
  rows <- lapply(shown, function(f) {
    value <- shiny::tags$code(values[[f]])
    shiny::tags$tr(shiny::tags$th(f), shiny::tags$td(if (changed(f)) shiny::tags$strong(value, .noWS = "inside") else value))
  })
  main <- intersect(c("storage", "retrieval"), shown[values[shown] %in% "main"])
  note <- if (length(main) && !identical(unname(values["deployment"]), "main")) {
    sprintf(
      "It runs on a worker, but the main process %s its value (%s main): with a slow network file system, a large value makes it slow.",
      paste(c(storage = "writes", retrieval = "reads")[main], collapse = " and "),
      paste(main, collapse = " and ")
    )
  }
  shiny::tags$div(
    class = "tb-settings",
    shiny::tags$table(class = "tb-settings-table", shiny::tags$tbody(rows)),
    if (!is.null(note)) shiny::tags$p(class = "tb-preview-note", note)
  )
}

#' Reproduce and debug-run code for a target, in a section closed at first
#'
#' The Problems part's code (reproduce_code(), debug_run_block()), for a
#' target that runs but may give a wrong value. A branch's inputs are slices
#' of its upstream targets: only its workspace, when it has one, loads them,
#' so without one it gets the debug run alone.
#' @param name The target, or the branch.
#' @param run The target the debug run builds: the branch's pattern.
#' @noRd
debug_ui <- function(name, run = name, command = NULL, inputs = character(), seed = NULL,
                     workspace = FALSE, branch = FALSE, running = FALSE, script = NULL,
                     store = NULL, dir = NULL, run_block = NULL, profile_block = NULL) {
  if (length(command) != 1 || is.na(command)) command <- NULL
  shiny::tags$details(
    class = "tb-command tb-debug",
    shiny::tags$summary("Debug"),
    if (!branch || workspace) {
      code_block("Reproduce", "reproduce",
                 reproduce_code(name, command, workspace, inputs, seed, script = script, store = store, dir = dir))
    },
    run_block %||% debug_run_block(name, running, script, run = run, store = store, dir = dir),
    profile_block
  )
}

#' Whether the Value pane offers Reproduce and Debug run: for a target, not
#' a function or global object the canvas shows with `targets_only = FALSE`
#' @noRd
debuggable <- function(nodes, name) {
  !("type" %in% names(nodes) && any(nodes$type[nodes$id %in% name] %in% c("function", "object")))
}

#' A target's build times in tar_meta(): `own`, its record's, and `latest`,
#' its or its branches' latest; NA when unknown
#' @noRd
build_times <- function(meta, name) {
  out <- list(own = NA, latest = NA)
  if (is.null(meta) || is.null(name) || !"time" %in% names(meta)) return(out)
  out$own <- meta$time[meta$name %in% name][1]
  parent <- if ("parent" %in% names(meta)) meta$parent else NA_character_
  times <- meta$time[meta$name %in% name | parent %in% name]
  times <- times[!is.na(times)]
  if (length(times)) out$latest <- max(times)
  out
}
