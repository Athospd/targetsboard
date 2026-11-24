# What the board's buttons and links do in the browser, written as data
# attributes that srcjs/actions.js acts on: no JavaScript in R strings.

#' Make a button or link send a value to the server when clicked
#'
#' The value arrives as `input[[input_id]]`, as an event: a second click
#' with the same value is seen too.
#' @param tag A button or a link.
#' @param input_id The input's id.
#' @param value A number or a string.
#' @return The tag.
#' @noRd
send_on_click <- function(tag, input_id, value) {
  htmltools::tagAppendAttributes(
    tag,
    `data-tb-input` = input_id,
    `data-tb-value` = as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
  )
}
