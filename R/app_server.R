#' @export
app_server <- function(metadata) {
  nodes <- dataframe_to_reactflow_node_data(metadata$x$nodes)
  edges <- dataframe_to_reactflow_edge_data(metadata$x$edges)

  function(input, output, session) {
    golem::add_resource_path(
      'www', app_sys('app/www')
    )
    autoInvalidateLong <- reactiveTimer(3500)
    autoInvalidateShort <- reactiveTimer(500)
  
    ###############################################################
    visnetwork_metadata <- reactiveValues(
      nodes_tab = metadata$x$nodes,
      edges_tab = metadata$x$edges
    )

    output$output <- shiny::renderUI({
      req(input$selected_nodes)
      tar_output <- tryCatch(
        targets::tar_read_raw(input$selected_nodes[1]),
        error = function(e) shiny::p("No artifact available from this target.")
      )

      c(render_fun, output_ui_fun) %<-% render_and_output_bundle(tar_output)
      random_name <- paste0("x", digest::sha1(runif(1)))
      output[[random_name]] <- render_fun(tar_output)
      # Sys.sleep(0.3)
      tar_output_ui <- output_ui_fun(random_name)
      shiny::tagList(tar_output_ui)
    })
    
    observe({
      autoInvalidateLong()
      promises::future_promise({
        metadata <- targets::tar_visnetwork(targets_only = TRUE, script = input$script)
      }, seed = TRUE) %...>%
      (
        function(result){
          visnetwork_metadata$nodes_tab <- result$x$nodes
          visnetwork_metadata$edges_tab <- result$x$edges
        }
      ) 
    })
    
    observe({
      print("passou aqui no tar_visnetwork")
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
      reactiveValuesToList(input)
    })
  }
}
