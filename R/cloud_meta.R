#' A pipeline whose metadata lives in the cloud (#111)
#'
#' With `tar_option_set(repository_meta = "aws")` or `"gcp"`, the store's
#' metadata is the local copy of the bucket's: the board's statuses can be
#' stale. A notice over the board says so, with a button that runs
#' `tar_meta_download()` once confirmed: it is billed per request and needs
#' the cloud package and credentials. Nothing downloads on its own.
#'
#' @param cloud `list(repository, bucket, prefix)`, as poller_why() gives,
#'   or NULL.
#' @param button_id The download button's input id.
#' @noRd
cloud_meta_ui <- function(cloud, button_id) {
  if (is.null(cloud)) return(NULL)
  where <- paste(c(
    cloud$repository,
    if (!is.null(cloud$bucket)) paste("bucket", cloud$bucket),
    if (!is.null(cloud$prefix)) paste("prefix", cloud$prefix)
  ), collapse = ", ")
  shiny::tags$div(
    class = "tb-cloud-meta alert alert-info", role = "status",
    shiny::tags$span(paste0(
      "This pipeline's metadata lives in the cloud (", where, "): ",
      "the board shows the local copy, which may be behind."
    )),
    shiny::actionButton(button_id, "Download metadata", class = "btn-sm ms-2")
  )
}

cloud_meta_output <- function(id) shiny::uiOutput(shiny::NS(id, "notice"))

#' @param cloud Reactive: where the metadata lives, or NULL.
#' @param script Reactive: the board's script, for `tar_meta_download()`.
#' @param download `function(script)`: downloads the metadata, in a process
#'   of its own (it runs the script).
#' @param refresh Called after a download: the board reads the store again.
#' @return A list: `last_error`, a reactive of the last download's error.
#' @noRd
cloud_meta_server <- function(id, cloud, script, download = download_cloud_meta, refresh = function() NULL) {
  stopifnot(shiny::is.reactive(cloud), shiny::is.reactive(script))
  shiny::moduleServer(id, function(input, output, session) {
    output$notice <- shiny::renderUI(cloud_meta_ui(cloud(), session$ns("download")))

    shiny::observeEvent(input$download, {
      shiny::showModal(shiny::modalDialog(
        title = "Download the metadata?",
        shiny::tags$p(
          "This runs ", shiny::tags$code("targets::tar_meta_download()"), ": it reads the bucket, ",
          "which your cloud provider may bill, and needs its R package and your credentials. ",
          "It replaces the local copy of the metadata."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm"), "Download", class = "btn-primary")
        )
      ))
    })

    last_error <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$confirm, {
      shiny::removeModal()
      # shortcut: it blocks the session while it downloads; an ExtendedTask if it grows slow.
      error <- tryCatch({
        download(script())
        NULL
      }, error = function(e) conditionMessage(e))
      last_error(error)
      if (is.null(error)) {
        refresh()
        shiny::showNotification("Downloaded the metadata.", type = "message")
      } else {
        shiny::showNotification(paste0("Couldn't download the metadata: ", cli::ansi_strip(error)), type = "error", duration = NULL)
      }
    })

    list(last_error = shiny::reactive(last_error()))
  })
}

# In a process of its own: tar_meta_download() runs the script for its options.
# callr wraps the error: its cause says what went wrong (a package, credentials).
download_cloud_meta <- function(script) {
  tryCatch(
    callr::r(function(script) targets::tar_meta_download(script = script), args = list(script = script)),
    error = function(e) stop(conditionMessage(e$parent %||% e), call. = FALSE)
  )
  invisible()
}
