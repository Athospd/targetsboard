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

#' F string
#'
#' Alias to glue::glue to resemble the Pythonic f-string.
#'
#' @seealso [glue::glue()]
#'
#' @keywords internal
#' @export
#' @examples
#' mean <- 3.5
#' f("the mean is {mean}")
f <- glue::glue

#' Not in operator
#'
#' the Negate(`\%in\%`) function.
#'
#' @keywords internal
#' @export
"%not_in%" <- Negate(`%in%`)

#' Multiple assignment operator
#'
#' See \code{zeallot::\link[zeallot:operator]{\%<-\%}} for details.
#'
#' @name %<-%
#' @rdname zeallot-multi-assignment
#' @keywords internal
#' @export
#' @importFrom zeallot %<-%
#' @usage c(x, y, z) \%<-\% list(a, b, c)
NULL
