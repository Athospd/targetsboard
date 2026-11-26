#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @keywords internal
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

# targets::tar_random_port(), tar_board()'s default port, needs parallelly,
# which targets only suggests: imported so it is installed with the board.
#' @importFrom parallelly freePort
NULL

# A named list, a JSON object even when empty: `{}`, where an empty plain
# list would be `[]`.
json_object <- function(x = list()) {
  if (length(x)) x else structure(list(), names = character())
}

#' A value read again every `ms` milliseconds
#'
#' Read from disk, say: what reads the returned reactive updates only when
#' the value differs from the last, so nothing redraws while the files stay
#' the same. `read` may read reactives: a change in them reads it at once.
#' For the Script tab and the projects of `_targets.yaml`.
#' @noRd
watch_changes <- function(read, ms = 1000, session = shiny::getDefaultReactiveDomain()) {
  value <- shiny::reactiveVal(shiny::isolate(read())) # right from the start
  shiny::observe({
    shiny::invalidateLater(ms, session)
    new <- read()
    if (!identical(new, shiny::isolate(value()))) value(new)
  })
  shiny::reactive(value())
}
