#' The board's UI
#'
#' The top bar holds only the title and the power button, which turns the
#' board off. The page is one card of tabs: Board, the board's own graph;
#' tar_visnetwork(), the graph as that function draws it; tar_meta(); and
#' Debug. Each tab's `value` is its snake_case id.
#'
#' @param metadata `targets::tar_visnetwork()$x`, for the first render.
#' @param script Path to the targets script.
#' @import shiny bslib
#' @noRd
app_ui <- function(metadata, script = targets::tar_config_get("script")) {
  shiny::tagList(
    add_external_resources(),
    page_sidebar(
      title = tags$div(
        class = "d-flex justify-content-between align-items-center w-100",
        "Targets board",
        actionButton(
          "turn_off", NULL,
          icon = icon("power-off"), title = "Turn the board off", class = "btn-sm"
        )
      ),
      fillable = TRUE,
      sidebar = sidebar(
        title = "Control Panel",
        textInput("script", "Script", script),
        actionButton("load_script", "Load", icon = icon("folder-open")),
        actionButton("refresh", "Refresh", icon = icon("rotate")),
        tags$hr(),
        actionButton("run_all", "Run all", icon = icon("play")),
        actionButton("run_selected", "Run selected", icon = icon("forward"), disabled = TRUE),
        actionButton("stop", "Stop", icon = icon("stop"), disabled = TRUE),
        actionButton("invalidate", "Invalidate selected", icon = icon("eraser"), disabled = TRUE)
      ),
      navset_card_underline(
        id = "view",
        full_screen = TRUE,
        nav_panel(
          "Board", value = "board",
          # The value preview belongs to the board's own graph: it follows
          # the focused target.
          layout_sidebar(
            class = "p-0",
            sidebar = sidebar(
              id = "value_preview_sidebar",
              width = "40%",
              open = FALSE,
              position = "right",
              uiOutput("value_preview")
            ),
            targetsboard("flow", metadata$nodes, metadata$edges)
          )
        ),
        nav_panel(
          "tar_visnetwork()", value = "tar_visnetwork",
          visNetwork::visNetworkOutput("vis_network", height = "100%")
        ),
        nav_panel(
          "tar_meta()", value = "tar_meta",
          reactable::reactableOutput("meta")
        ),
        nav_panel(
          "Debug", value = "debug",
          verbatimTextOutput("debug")
        )
      )
    )
  )
}
