#' The run queue (#106)
#'
#' Runs asked for while one goes can't start a second `tar_make()` on the
#' store: they wait in one queue, merged, and start as one run when it ends.
#' `list(all, names)`: Run all queues every target, whatever the pipeline is
#' by then.
#' @noRd
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
run_queue_ui <- function(names, clear_id) {
  if (!length(names)) return(NULL)
  shiny::tags$div(
    class = "tb-run-queue d-flex align-items-start gap-2 small",
    shiny::icon("clock"),
    shiny::tags$span(paste0("Queued: ", paste(names, collapse = ", "))),
    shiny::actionButton(clear_id, "Clear queue", class = "btn-sm btn-link p-0 ms-auto text-nowrap")
  )
}
