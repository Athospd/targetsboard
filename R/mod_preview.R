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
    id = ns("sidebar"),
    width = "40%",
    open = FALSE,
    position = "right",
    # Apart from the value's output: new reasons don't rebuild the value.
    shiny::uiOutput(ns("description")),
    shiny::uiOutput(ns("why")),
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
#' @return A list of reactives: `target_link`, the target a link in the
#'   problem pane asks to show.
#' @noRd
preview_server <- function(id, backend, focused, open_request, board, running, tar_meta, methods,
                           outdated = shiny::reactive(list())) {
  stopifnot(
    !shiny::is.reactive(backend), shiny::is.reactive(focused), shiny::is.reactive(open_request),
    shiny::is.reactive(board), shiny::is.reactive(running), shiny::is.reactive(tar_meta),
    shiny::is.reactive(methods), shiny::is.reactive(outdated)
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

    # A pattern's preview opens on its first branch; the picker changes it.
    branch <- shiny::reactiveVal(1L)
    shiny::observeEvent(focused(), branch(1L), priority = 10, ignoreNULL = FALSE)
    shiny::observeEvent(input$branch, branch(as.integer(input$branch)))

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

    # tar_target(description = ), or tar_map(descriptions = )'s.
    output$description <- shiny::renderUI({
      shiny::req(is_open(), focused())
      description_ui(target_description(board()$x$nodes, focused()))
    })

    output$why <- shiny::renderUI({
      shiny::req(is_open(), focused())
      reasons_ui(outdated()[[focused()]])
    })

    output$content <- shiny::renderUI({
      shiny::req(is_open(), focused())
      name <- focused()
      # A target that errored or warned shows its problem first; an errored
      # one has no value to show.
      problem <- target_problem(name, backend$poller$script)
      problem_part <- NULL
      if (has_problem(problem)) {
        edges <- board()$x$edges
        inputs <- intersect(edges$from[edges$to == name], board_targets(board()$x$nodes))
        problem_part <- problem_ui(
          problem, inputs, running = running(), script = backend$poller$script,
          store = store(), dir = normalizePath(getwd(), winslash = "/"), ns = session$ns
        )
        if (!is.null(problem$error)) return(problem_part)
      }
      methods()
      records_changed()
      # The problem says why there is no value: a pattern whose branches
      # errored. Without one, say it plainly.
      no_value <- function() problem_part %||% shiny::p("No value stored for this target yet.")
      target <- preview_target(name, store(), branch = branch())
      if (is.null(target)) return(no_value())
      key <- preview_key(target)
      if (preview_too_large(target) && !key %in% read_anyway()) {
        return(shiny::tagList(problem_part, preview_gate(target, store = store(), ns = session$ns)))
      }
      built <- previews$get(key)
      if (is.null(built)) {
        missing <- new.env() # a value can be NULL itself
        value <- tryCatch(suppressWarnings(read_preview_value(target, store())), error = function(e) missing)
        if (identical(value, missing)) return(no_value())
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
      shiny::tagList(problem_part, preview_frame(target, built, session$ns(plot_ids), store(), ns = session$ns))
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
