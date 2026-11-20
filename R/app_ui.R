#' app_ui
#' 
#' @import shiny bslib
#' @export
app_ui <- function(metadata, script = "_targets.R") {
  nodes <- dataframe_to_reactflow_node_data(metadata$nodes)
  edges <- dataframe_to_reactflow_edge_data(metadata$edges)
  shiny::tagList(  
    add_external_resources(),
    page_navbar(
      title = "Targets board",
      nav_spacer(),
      nav_item(actionButton("turn_off", NULL, icon = icon("power-off"))),
      sidebar =  sidebar(
        title = "Control Panel",
        textInput("script", "Script", script)
      ),
      accordion(
        open = c("DAG", "debug"),
        accordion_panel(
          "DAG",
          navset_card_underline(
            layout_sidebar(
              sidebar = sidebar(
                id = "output_preview_sidebar",
                width = "40%",
                open = FALSE,
                position = "right",
                uiOutput("output_preview")
              )
            ),
            nav_panel("vis1", targetsboard('flow', nodes, edges)),
            nav_panel("vis2", visNetwork::visNetworkOutput("vis_network"))
          
          )
        ),
        accordion_panel(
          "tar_meta",
          card(
            reactable::reactableOutput("meta")
          )
        ),
        accordion_panel(
          "Table",
          card(
            reactable::reactableOutput("nodes")
          )
        ),
        accordion_panel(
          "Debug",
          card(
            verbatimTextOutput("debug")
          )
        )
      )
    )
  )
}
