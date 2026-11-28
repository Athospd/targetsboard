# Export from the board (#309): a button under the run controls opens a
# dialog: which targets, the size cap, an estimate and the R code. Export
# builds tar_board_export()'s page in a background process, with Cancel;
# then the browser saves it.

#' What an export would hold, from the store's metadata
#' @param meta `tar_meta(targets_only = TRUE)`, or NULL.
#' @param names The targets whose values to include.
#' @param max_bytes The export's cap on one value.
#' @return `list(n, bytes, cloud, over)`: the targets, the size of the
#'   values that go in, and how many are left out for being in the cloud or
#'   over the cap.
#' @noRd
export_estimate <- function(meta, names, max_bytes) {
  none <- list(n = length(names), bytes = 0, cloud = 0L, over = 0L)
  if (is.null(meta) || !nrow(meta) || !length(names)) return(none)
  parent <- if ("parent" %in% names(meta)) meta$parent else NA
  # A stem's own record, or a pattern's branches.
  rows <- meta[(meta$name %in% names & meta$type %in% "stem") | parent %in% names, , drop = FALSE]
  owner <- ifelse(rows$type %in% "branch", parent[match(rows$name, meta$name)], rows$name)
  bytes <- tapply(as.numeric(rows$bytes %||% 0), owner, sum, na.rm = TRUE)
  cloud <- unique(owner[rows$repository %in% c("aws", "gcp")])
  over <- setdiff(names(bytes)[bytes > max_bytes], cloud)
  kept <- setdiff(names(bytes), c(cloud, over))
  list(n = length(names), bytes = sum(bytes[kept]), cloud = length(cloud), over = length(over))
}

#' The R code of an export
#' @noRd
export_code <- function(file, names, max_bytes, all = FALSE) {
  args <- c(
    deparse1(file),
    if (!all) paste0("names = c(", paste(ifelse(make.names(names) == names, names, paste0("`", names, "`")), collapse = ", "), ")"),
    if (max_bytes != 10e6) paste0("max_bytes = ", format(max_bytes, scientific = TRUE))
  )
  paste0("targetsboard::tar_board_export(", paste(args, collapse = ", "), ")")
}

#' Builds the page in a background process
#' @return The callr process.
#' @noRd
export_start <- function(args) {
  callr::r_bg(
    function(args) do.call(targetsboard::tar_board_export, c(args, list(callr_function = NULL))),
    args = list(args = args), supervise = TRUE
  )
}

export_ui <- function(id) {
  shiny::actionButton(shiny::NS(id, "open"), "Export", icon = shiny::icon("download"),
                      title = "Save the board as one HTML page, with its values")
}

#' @param board Reactive: the graph the board draws, for its targets.
#' @param selected Reactive: the selected targets, or NULL.
#' @param running Reactive: whether a run is going.
#' @param tar_meta Reactive: the store's metadata.
#' @param script Reactive: the script the board draws.
#' @param start Starts a build: `export_start()`, replaced in tests.
#' @noRd
export_server <- function(id, board, selected, running, tar_meta, script, start = export_start) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    job <- shiny::reactiveVal(NULL)
    built <- shiny::reactiveVal(NULL) # the page, once built
    file_name <- function() paste0(basename(getwd()), "-board-", format(Sys.Date()), ".html")

    chosen <- shiny::reactive({
      all <- board_targets(board()$x$nodes)
      if (identical(input$which, "selected")) intersect(selected(), all) else all
    })
    max_bytes <- shiny::reactive(max(0, input$max_mb %||% 10) * 1e6)

    shiny::observeEvent(input$open, {
      built(NULL)
      picked <- length(selected())
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("download"), "Export the board"),
        shiny::tags$p("One HTML page: the graph, and each target's value. It opens in any browser, without R."),
        shiny::radioButtons(ns("which"), "Values of", inline = TRUE, choices = c(
          "The targets on the board" = "board",
          stats::setNames("selected", sprintf("The %d selected", picked))
        )[c(TRUE, picked > 0)]),
        shiny::numericInput(ns("max_mb"), "Leave out a value larger than (MB)", value = 10, min = 0, step = 1, width = "18rem"),
        shiny::uiOutput(ns("summary")),
        footer = shiny::uiOutput(ns("footer"), inline = TRUE),
        size = "l"
      ))
    })

    output$summary <- shiny::renderUI({
      names <- chosen()
      est <- export_estimate(tar_meta(), names, max_bytes())
      left_out <- c(
        if (est$cloud) sprintf("%d in the cloud", est$cloud),
        if (est$over) sprintf("%d over the cap", est$over)
      )
      shiny::tagList(
        shiny::tags$p(paste0(
          sprintf("%d target%s, about %s of values", est$n, if (est$n == 1) "" else "s", format_bytes(est$bytes)),
          if (length(left_out)) paste0("; not included: ", and_list(left_out)), "."
        )),
        if (isTRUE(running())) shiny::tags$p(class = "text-warning", "A run is going: the page shows the store as it is now."),
        shiny::tags$p(class = "text-muted", shiny::icon("lock"), "The page holds your data's values: share it as you would that data."),
        code_block("The same, in R", "export", export_code(file_name(), names, max_bytes(), all = !identical(input$which, "selected")))
      )
    })

    output$footer <- shiny::renderUI({
      if (!is.null(job())) {
        return(shiny::tagList(
          shiny::tags$span(class = "me-2", shiny::icon("spinner", class = "fa-spin"), "Building the page..."),
          shiny::actionButton(ns("cancel"), "Cancel")
        ))
      }
      shiny::tagList(
        shiny::modalButton("Close"),
        if (is.null(built())) {
          shiny::actionButton(ns("build"), "Export", class = "btn-primary", disabled = if (!length(chosen())) NA)
        } else {
          shiny::downloadButton(ns("save"), paste("Save", file_name()), class = "btn-primary")
        }
      )
    })

    shiny::observeEvent(input$build, {
      shiny::req(is.null(job()), length(chosen()))
      out <- tempfile("targetsboard_export_", fileext = ".html")
      args <- list(file = out, names = chosen(), script = script(), max_bytes = max_bytes())
      job(list(out = out, process = start(args)))
    })
    shiny::observeEvent(input$cancel, {
      j <- job()
      shiny::req(j)
      j$process$kill()
      job(NULL)
    })
    shiny::observe({
      j <- job()
      shiny::req(j)
      if (j$process$is_alive()) return(shiny::invalidateLater(500))
      error <- tryCatch({
        j$process$get_result()
        NULL
      }, error = function(e) cli::ansi_strip(conditionMessage(e$parent %||% e)))
      job(NULL)
      if (is.null(error) && file.exists(j$out)) {
        built(j$out)
      } else {
        shiny::showNotification(paste("The export failed:", error %||% "it wrote no page."), type = "error", duration = NULL)
      }
    })

    output$save <- shiny::downloadHandler(
      filename = function() file_name(),
      content = function(file) file.copy(built(), file, overwrite = TRUE),
      contentType = "text/html"
    )
  })
}
