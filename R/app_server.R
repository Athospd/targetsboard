#' @export
app_server <- function(metadata, script = "_targets.R", targets_only = TRUE) {
  # One background poller per app, shared by every browser session.
  # Restarted only when someone loads a different script.
  poller <- new.env()
  poller_rds_path <- shiny::reactiveVal(NULL)

  stop_poller <- function() {
    if (inherits(poller$process, "r_process") && poller$process$is_alive())
      poller$process$kill()
    poller$process <- NULL
  }

  start_poller <- function(script) {
    stop_poller()
    assets <- tar_visnetwork_bg(script = script, targets_only = targets_only)
    poller$process <- assets$process
    poller$script <- script
    poller_rds_path(assets$tar_vis_path)
  }

  function(input, output, session) {
    if (is.null(poller$process)) {
      start_poller(script)
      shiny::onStop(stop_poller, session = NULL) # runs when the app stops
    }

    autoInvalidateShort <- reactiveTimer(1000)

    ###############################################################
    visnetwork_metadata <- reactiveValues(
      metadata = metadata,
      nodes_tab = metadata$nodes,
      edges_tab = metadata$edges
    )

    observe({
      bslib::toggle_sidebar("output_preview_sidebar", open = !is.null(input$flow_selected_nodes[1]))
    })

    output$output_preview <- shiny::renderUI({
      req(input$flow_selected_nodes)
      tar_output <- tryCatch(
        targets::tar_read_raw(input$flow_selected_nodes[1]),
        error = function(e) shiny::p("No artifact available for this target.")
      )

      c(render_fun, output_ui_fun) %<-% render_and_output_bundle(tar_output)
      # Reuse one output id: assigning it again replaces the previous
      # renderer instead of piling up a new output per click.
      output$output_preview_content <- render_fun(tar_output)
      output_ui_fun("output_preview_content")
    })

    observeEvent(input$load_script, {
      new_script <- input$script
      if (!file.exists(new_script)) {
        shiny::showNotification(
          paste0("Script not found: ", new_script),
          type = "error"
        )
        return()
      }
      if (!identical(new_script, poller$script)) start_poller(new_script)
    })

    observe({
      autoInvalidateShort()
      rds_path <- poller_rds_path()
      req(rds_path)
      if(file.exists(rds_path)) {
        new_metadata <- tryCatch(
          readRDS(rds_path)$x,
          error = function(e) NULL
        )
        req(new_metadata)
        visnetwork_metadata$metadata <- new_metadata
        visnetwork_metadata$nodes_tab <- visnetwork_metadata$metadata$nodes
        visnetwork_metadata$edges_tab <- visnetwork_metadata$metadata$edges
      }
    })

    observe({
      updated_nodes = dataframe_to_reactflow_node_data(visnetwork_metadata$nodes_tab)
      updated_edges = dataframe_to_reactflow_edge_data(visnetwork_metadata$edges_tab)

      update_targetsboard(
        session,
        "flow",
        value = 0,
        configuration = c(
          list(
            nodes = updated_nodes,
            edges = updated_edges
          ),
          list(
            id = "flow",
            height = "100%",
            width = "100%",
            background_color = "#f0e5d7",
            background_marks_color = "#2f223d"
          )
        )
      )
    })

    output$nodes <- reactable::renderReactable({
      reactable::reactable(visnetwork_metadata$nodes_tab)
    })

    # Re-read tar_meta() only when targets writes new metadata; checking the
    # file's modification time every second is cheap.
    tar_meta_data <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() {
        file.mtime(file.path(targets::tar_config_get("store"), "meta", "meta"))
      },
      valueFunc = function() {
        tryCatch(targets::tar_meta(targets_only = TRUE), error = function(e) NULL)
      }
    )

    output$meta <- reactable::renderReactable({
      req(tar_meta_data())
      reactable::reactable(tar_meta_data(), wrap = FALSE)
    })

    output$debug <- shiny::renderPrint({
      c(
        reactiveValuesToList(visnetwork_metadata),
        reactiveValuesToList(input)
      )
    })
  }
}
