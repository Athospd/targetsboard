#' The board's UI
#'
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @import shiny bslib
#' @noRd
app_ui <- function(metadata, script = "_targets.R") {
  shiny::tagList(  
    add_external_resources(),
    page_sidebar(
      title = "Targets board",
      sidebar =  sidebar(
        title = "Control Panel",
        textInput("script", "Script", script),
        actionButton("load_script", "Load", icon = icon("rotate"))
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
                targetsboard('flow', metadata$nodes, metadata$edges)
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
