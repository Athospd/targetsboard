#' The run queue (#106)
#'
#' Runs asked for while one goes can't start a second `tar_make()` on the
#' store: they wait in one queue, merged, and start as one run when it ends.
#' The queue is `list(all, names)`: Run all queues every target, whatever the
#' pipeline is by then. Its line, under the run summary, names what waits,
#' with Clear queue.
#'
#' @param id The module's id.
#' @noRd
run_queue_ui <- function(id) shiny::uiOutput(shiny::NS(id, "queue"))

#' @rdname run_queue_ui
#' @param request Reactive event: `list(names, n)`, targets to queue (`names`
#'   NULL: every target); `n` makes each request an event. NULL until the
#'   first.
#' @param unqueue Reactive event: `list(names, n)`, targets to take out.
#' @param clear Reactive event: empties the queue (Stop). NULL until the
#'   first.
#' @param running Reactive: whether the pipeline is running.
#' @param busy A function: whether a run goes, its process included, until
#'   targets records the end.
#' @param targets Reactive: the pipeline's targets.
#' @return A list of reactives: `queued`, the targets waiting; `due`, an
#'   event, `list(names, n)`, the run to start now that none goes (`names`
#'   NULL: every target), NULL until the first.
#' @noRd
run_queue_server <- function(id, request, unqueue, clear, running, busy, targets) {
  stopifnot(
    shiny::is.reactive(request), shiny::is.reactive(unqueue), shiny::is.reactive(clear),
    shiny::is.reactive(running), !shiny::is.reactive(busy), is.function(busy), shiny::is.reactive(targets)
  )
  shiny::moduleServer(id, function(input, output, session) {
    queue <- shiny::reactiveVal(empty_queue())
    queued <- shiny::reactive(queued_targets(queue(), targets()))
    due <- shiny::reactiveVal(NULL)

    shiny::observeEvent(request(), {
      names <- request()$names
      queue(queue_add(queue(), names))
      shiny::showNotification(
        paste0("Queued ", if (is.null(names)) "all targets" else describe_names(names), ": it runs when this run ends."),
        type = "message"
      )
    })
    shiny::observeEvent(unqueue(), queue(queue_remove(queue(), unqueue()$names, targets())))
    shiny::observeEvent(clear(), queue(empty_queue()))
    shiny::observeEvent(input$clear_queue, queue(empty_queue()))

    # Due once no run goes: as soon as running() says so, or, while targets
    # hasn't recorded the end yet, a second later.
    shiny::observe({
      waiting <- queue()
      if (queue_empty(waiting) || running()) return()
      if (busy()) return(shiny::invalidateLater(1000))
      shiny::isolate({
        queue(empty_queue())
        due(list(names = queue_run_names(waiting), n = (due()$n %||% 0) + 1))
      })
    })

    output$queue <- shiny::renderUI(run_queue_line(queued(), session$ns("clear_queue")))

    list(queued = queued, due = shiny::reactive(due()))
  })
}

empty_queue <- function() list(all = FALSE, names = character())

queue_empty <- function(queue) !queue$all && !length(queue$names)

# `names` NULL: every target (Run all).
queue_add <- function(queue, names) {
  if (is.null(names)) return(list(all = TRUE, names = character()))
  if (queue$all) return(queue)
  list(all = FALSE, names = union(queue$names, names))
}

# `targets`: the pipeline's, to take some out of "every target".
queue_remove <- function(queue, names, targets = character()) {
  left <- setdiff(if (queue$all) targets else queue$names, names)
  list(all = FALSE, names = left)
}

queued_targets <- function(queue, targets) if (queue$all) targets else queue$names

# The run it starts: tar_make(names = ) (NULL, the whole pipeline).
queue_run_names <- function(queue, targets = NULL) if (queue$all) NULL else queue$names

#' The queue's line under the run summary, with Clear queue
#' @noRd
run_queue_line <- function(names, clear_id) {
  if (!length(names)) return(NULL)
  shiny::tags$div(
    class = "tb-run-queue d-flex align-items-start gap-2 small",
    shiny::icon("clock"),
    shiny::tags$span(paste0("Queued: ", paste(names, collapse = ", "))),
    shiny::actionButton(clear_id, "Clear queue", class = "btn-sm btn-link p-0 ms-auto text-nowrap")
  )
}
