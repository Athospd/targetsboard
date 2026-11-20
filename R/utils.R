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
#' @importFrom glue glue
f <- glue::glue

#' Not in operator
#'
#' The negation of `%in%`: `x %not_in% table` is `!(x %in% table)`.
#'
#' @usage `\%not_in\%`(...)
#' @param ... `x` and `table`, as for [base::%in%].
#' @return A logical vector as long as `x`.
#' @keywords internal
#' @export
#' @examples
#' c("a", "z") %not_in% letters[1:3]
"%not_in%" <- Negate(`%in%`)

#' @importFrom zeallot %<-%
#' @export
zeallot::`%<-%`
