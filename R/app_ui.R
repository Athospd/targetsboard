#' app_ui
#' 
#' @import shiny bslib
#' @export
app_ui <- function(metadata, script = "_targets.R") {
  nodes <- dataframe_to_reactflow_node_data(metadata$nodes)
  edges <- dataframe_to_reactflow_edge_data(metadata$edges)
  shiny::tagList(  
    add_external_resources(),
    page_sidebar(
      title = "Targets board",
      sidebar =  sidebar(
        title = "Control Panel",
        textInput("script", "Script", script)
      ),
      accordion(
        open = c("DAG", "debug"),
        accordion_panel(
          "DAG",
          card(
            class = "main-dag",
            full_screen = TRUE,
            height = "500px",
            style = "resize:vertical;",
            card_body(
              class = "p-0",
              layout_sidebar(
                sidebar = sidebar(
                  id = "output_preview_sidebar",
                  width = "40%",
                  open = FALSE,
                  position = "right",
                  uiOutput("output_preview")
                ),
                targetsboard('flow', nodes, edges)
              )
            )
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
