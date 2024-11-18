#' @export
app_server <- function(metadata) {
  nodes <- dataframe_to_reactflow_node_data(metadata$nodes)
  edges <- dataframe_to_reactflow_edge_data(metadata$edges)
  tar_visnetwork_assets <- NULL

  function(input, output, session) {
    golem::add_resource_path(
      'www', app_sys('app/www')
    )
    autoInvalidateLong <- reactiveTimer(3500)
    autoInvalidateShort <- reactiveTimer(1000)

    ###############################################################
    visnetwork_metadata <- reactiveValues(
      tar_visnetwork_app = NULL,
      tar_vis_rds_path = NULL,
      metadata = metadata,
      nodes_tab = metadata$nodes,
      edges_tab = metadata$edges
    )

    observe({   
      print(!is.null(input$selected_nodes[1]))
      bslib::toggle_sidebar("output_preview_sidebar", open = !is.null(input$selected_nodes[1]))
    })

    output$output_preview <- shiny::renderUI({
      req(input$selected_nodes)
      tar_output <- tryCatch(
        targets::tar_read_raw(input$selected_nodes[1]),
        error = function(e) shiny::p("No artifact available for this target.")
      )

      c(render_fun, output_ui_fun) %<-% render_and_output_bundle(tar_output)
      random_name <- paste0("x", digest::sha1(runif(1)))
      output[[random_name]] <- render_fun(tar_output)
      tar_output_ui <- output_ui_fun(random_name)
      shiny::tagList(tar_output_ui)
    })

    observe({
      if(inherits(isolate(visnetwork_metadata$tar_visnetwork_app), "r_process"))
        isolate(visnetwork_metadata$tar_visnetwork_app$kill())

      tar_visnetwork_assets <- tar_visnetwork_bg(script = input$script, targets_only = TRUE)
      visnetwork_metadata$tar_visnetwork_app <- tar_visnetwork_assets$process
      visnetwork_metadata$tar_vis_rds_path <- tar_visnetwork_assets$tar_vis_path
    })

    observe({
      autoInvalidateShort()
      req(visnetwork_metadata$tar_vis_rds_path)
      if(file.exists(visnetwork_metadata$tar_vis_rds_path)) {
        visnetwork_metadata$metadata <- readRDS(visnetwork_metadata$tar_vis_rds_path)$x
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

    output$meta <- reactable::renderReactable({
      autoInvalidateShort()
      reactable::reactable(targets::tar_meta(targets_only = TRUE), wrap = FALSE)
    })

    output$debug <- shiny::renderPrint({
      c(
        reactiveValuesToList(visnetwork_metadata),
        reactiveValuesToList(input)
      )
    })
  }
}
