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

# zeallot's multiple assignment, used internally: c(a, b) %<-% list(1, 2).
#' @importFrom zeallot %<-%
NULL
