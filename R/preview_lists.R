# Previews of lists, as a tree, and of models, as tables of their
# estimates and fit (broom) or their summary().

# How deep the tree goes, and how many items each level shows.
tree_depth <- 2
tree_items <- 50

# What an item is, in a few characters: "dbl [3]", "list [2]",
# "data.frame 32 x 12", "lm".
type_label <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.data.frame(x)) return(paste(class(x)[1], paste(format(dim(x), big.mark = ","), collapse = " \u00d7 ")))
  if (is.function(x)) return("function")
  if (is.environment(x)) return("environment")
  if (is.list(x) && !is.object(x)) return(sprintf("list [%s]", format(length(x), big.mark = ",")))
  if (is.atomic(x) && is.null(dim(x))) return(sprintf("%s [%s]", type_abbr(x), format(length(x), big.mark = ",")))
  if (!is.null(dim(x))) return(paste(class(x)[1], paste(format(dim(x), big.mark = ","), collapse = " \u00d7 ")))
  class(x)[1]
}

# The first few values of an atomic item, on one line.
short_value <- function(x, n = 5, width = 80) {
  if (!is.atomic(x) || !is.null(dim(x)) || !length(x)) return(NULL)
  shown <- utils::head(x, n)
  text <- if (is.double(shown) && !is.object(shown)) {
    vapply(shown, function(v) if (is.na(v)) "NA" else format_number(v), character(1))
  } else if (is.character(shown) || is.factor(shown)) {
    as.character(shown) # format() pads text to one width
  } else {
    format(shown, trim = TRUE)
  }
  text <- paste(text, collapse = ", ")
  if (length(x) > n) text <- paste0(text, ", \u2026")
  if (nchar(text) > width) text <- paste0(substr(text, 1, width - 1), "\u2026")
  text
}

item_name <- function(names, i) {
  name <- if (is.null(names)) "" else names[[i]]
  if (is.na(name) || !nzchar(name)) sprintf("[[%d]]", i) else name
}

#' One item of a list: a line with its name, type and first values, or, for
#' a list above the deepest level, a section with its own items
#' @noRd
tree_node <- function(name, x, depth) {
  label <- shiny::tagList(
    shiny::tags$code(class = "tb-tree-name", name),
    shiny::tags$span(class = "tb-tree-type", type_label(x)),
    shiny::tags$span(class = "tb-tree-value", short_value(x))
  )
  if (is.list(x) && !is.data.frame(x) && length(x) && depth < tree_depth) {
    shiny::tags$details(
      class = "tb-tree-node",
      shiny::tags$summary(label),
      tree_items_ui(x, depth + 1)
    )
  } else {
    shiny::tags$div(class = "tb-tree-leaf", label)
  }
}

tree_items_ui <- function(x, depth) {
  shown <- seq_len(min(length(x), tree_items))
  shiny::tags$div(
    class = "tb-tree-items",
    lapply(shown, function(i) tree_node(item_name(names(x), i), x[[i]], depth)),
    if (length(x) > tree_items) {
      shiny::tags$div(class = "tb-tree-more", sprintf("and %s more", format(length(x) - tree_items, big.mark = ",")))
    }
  )
}

#' A list's preview: its items, two levels deep, 50 at each level
#' @noRd
list_tree <- function(x) shiny::tags$div(class = "tb-tree", tree_items_ui(x, 1))

#' @rdname tar_board_preview
#' @export
tar_board_preview.list <- function(x, target, ...) {
  if (!length(x)) return(NULL)
  list_tree(x)
}

# The first class of x with a method for `generic`, other than the ones
# every object has.
method_class <- function(generic, x, envir = parent.frame()) {
  for (class in setdiff(class(x), c("list", "data.frame", "default"))) {
    if (!is.null(utils::getS3method(generic, class, optional = TRUE, envir = envir))) return(class)
  }
  NULL
}

# A table of a tidy() or glance() result, numbers to 4 significant digits.
estimates_table <- function(data) {
  data <- as.data.frame(data)
  columns <- lapply(names(data), function(column) {
    if (!is.numeric(data[[column]])) return(reactable::colDef(minWidth = 100))
    reactable::colDef(
      align = "right", minWidth = 80,
      cell = function(value) if (is.na(value)) "" else format_number(value)
    )
  })
  preview_table(data, columns = stats::setNames(columns, names(data)), pagination = nrow(data) > 25)
}

#' A model's views: its estimates and fit from broom, when broom knows the
#' model's class; else what summary() shows, when the class has a summary()
#' method
#' @noRd
model_views <- function(x) {
  if (!is.object(x)) return(NULL)
  if (rlang::is_installed("broom")) {
    broom <- asNamespace("broom")
    if (!is.null(method_class("tidy", x, broom))) {
      views <- list()
      tidied <- tryCatch(broom::tidy(x), error = function(e) NULL)
      if (is.data.frame(tidied) && nrow(tidied)) views$Estimates <- estimates_table(tidied)
      if (!is.null(method_class("glance", x, broom))) {
        glanced <- tryCatch(broom::glance(x), error = function(e) NULL)
        if (is.data.frame(glanced) && nrow(glanced) == 1) {
          values <- vapply(glanced, function(v) {
            v <- unname(v[[1]])
            if (is.numeric(v) && !is.na(v)) format_number(v) else format(v)
          }, character(1))
          views$Fit <- preview_table(
            data.frame(measure = names(values), value = unname(values), stringsAsFactors = FALSE),
            columns = list(
              measure = reactable::colDef(name = "Measure"),
              value = reactable::colDef(name = "Value", align = "right")
            ),
            pagination = FALSE
          )
        }
      }
      if (length(views)) return(views)
    }
  }
  if (!is.null(method_class("summary", x))) {
    return(list(Summary = shiny::tags$pre(class = "tb-preview-printed", printed_text(summary(x)))))
  }
  NULL
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.default <- function(x, target, ...) {
  # A model, or another object made of a list: as a tree.
  model_views(x) %||% if (is.list(x) && length(x)) list_tree(x)
}
