# torch values (#228): tar_torch() and tar_aws_torch() store nn_module
# models and tensors (format = "torch"). torch isn't a dependency: its
# objects are R6, and carry the methods these views call. Nothing moves to
# a GPU, nothing is written back, and a big tensor shows its first values.

# How many of a tensor's values it shows, and how deep a module's layers go.
tensor_values <- 20
module_depth <- 6

# "3 x 4", with a times sign, for a shape.
shape_label <- function(shape) {
  if (!length(shape)) return("scalar")
  paste(format(as.integer(shape), big.mark = ","), collapse = paste0(" ", intToUtf8(215L), " "))
}

count_label <- function(n, what) paste(format(n, big.mark = ",", scientific = FALSE), if (n == 1) what else paste0(what, "s"))

#' A tensor's view: its shape, dtype, device and first values
#' @noRd
tensor_view <- function(x) {
  n <- tryCatch(as.numeric(x$numel()), error = function(e) NA)
  values <- tryCatch({
    k <- min(tensor_values, n)
    as.vector(as.array(x$cpu()$flatten()[seq_len(k)]))
  }, error = function(e) NULL)
  shiny::tagList(
    shiny::tags$p(
      class = "tb-preview-facts",
      shiny::tags$span(shape_label(x$shape)),
      shiny::tags$span(as.character(x$dtype)),
      shiny::tags$span(tryCatch(x$device$type, error = function(e) "")),
      if (!is.na(n)) shiny::tags$span(count_label(n, "element"))
    ),
    if (length(values)) {
      shiny::tagList(
        if (n > length(values)) {
          shiny::tags$p(class = "tb-table-caption", sprintf(
            "The first %d of %s values.", length(values), format(n, big.mark = ",", scientific = FALSE)
          ))
        },
        shiny::tags$pre(class = "tb-preview-printed tb-preview-values", paste(short_value(values, n = tensor_values, width = 400)))
      )
    }
  )
}

# A module's parameters: their count, and how many train.
module_counts <- function(m) {
  parameters <- tryCatch(m$parameters, error = function(e) list())
  sizes <- vapply(parameters, function(p) as.numeric(p$numel()), numeric(1))
  trainable <- vapply(parameters, function(p) isTRUE(p$requires_grad), logical(1))
  list(total = sum(sizes), trainable = sum(sizes[trainable]))
}

# One layer of a module, and its own layers, as the list tree draws them.
module_node <- function(name, m, depth) {
  counts <- module_counts(m)
  children <- tryCatch(m$children, error = function(e) list())
  label <- shiny::tagList(
    shiny::tags$code(class = "tb-tree-name", name),
    shiny::tags$span(class = "tb-tree-type", class(m)[1]),
    shiny::tags$span(class = "tb-tree-value", if (counts$total > 0) count_label(counts$total, "parameter"))
  )
  if (length(children) && depth < module_depth) {
    shiny::tags$details(
      class = "tb-tree-node", open = if (depth == 1) NA,
      shiny::tags$summary(label),
      shiny::tags$div(class = "tb-tree-items", lapply(names(children), function(n) module_node(n, children[[n]], depth + 1)))
    )
  } else {
    shiny::tags$div(class = "tb-tree-leaf", label)
  }
}

#' A module's views: its layers as a tree, and a table of its parameters
#' @noRd
module_views <- function(x) {
  counts <- module_counts(x)
  parameters <- tryCatch(x$parameters, error = function(e) list())
  module <- shiny::tagList(
    shiny::tags$p(
      class = "tb-preview-facts",
      shiny::tags$span(class(x)[1]),
      shiny::tags$span(paste0(count_label(counts$total, "parameter"), ", ",
                              format(counts$trainable, big.mark = ",", scientific = FALSE), " trainable"))
    ),
    shiny::tags$div(class = "tb-tree", shiny::tags$div(class = "tb-tree-items", module_node("model", x, 1)))
  )
  if (!length(parameters)) return(list(Module = module))
  table <- data.frame(
    name = names(parameters),
    shape = vapply(parameters, function(p) shape_label(p$shape), character(1)),
    elements = vapply(parameters, function(p) format(as.numeric(p$numel()), big.mark = ",", scientific = FALSE), character(1)),
    trainable = vapply(parameters, function(p) if (isTRUE(p$requires_grad)) "yes" else "no", character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  list(
    Module = module,
    Parameters = preview_table(table, columns = list(
      name = reactable::colDef(name = "Parameter", minWidth = 160),
      shape = reactable::colDef(name = "Shape"),
      elements = reactable::colDef(name = "Elements", align = "right"),
      trainable = reactable::colDef(name = "Trainable")
    ), pagination = nrow(table) > 25)
  )
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.torch_tensor <- function(x, target, ...) list(Tensor = tensor_view(x))

#' @rdname tar_board_preview
#' @export
tar_board_preview.nn_module <- function(x, target, ...) module_views(x)

# The packages a storage format needs to read, as targets' own stores say
# (store_get_packages()): asked of targets 1.12.
format_packages <- list(
  qs = "qs2", feather = "arrow", parquet = "arrow", fst = "fst", fst_dt = c("data.table", "fst"),
  fst_tbl = c("tibble", "fst"), keras = "keras", torch = "torch"
)

#' Why a value can't be read here: its format's package isn't installed
#' @param format The target's format, from tar_meta().
#' @return A sentence, or NULL when nothing is missing.
#' @noRd
missing_format_package <- function(format) {
  if (length(format) != 1 || is.na(format) || is.null(format_packages[[format]])) return(NULL)
  needed <- format_packages[[format]]
  missing <- needed[!vapply(needed, rlang::is_installed, logical(1))]
  if (!length(missing)) return(NULL)
  what <- switch(format,
    torch = "This target is a torch model or tensor",
    keras = "This target is a keras model",
    paste0("This target is stored as ", format)
  )
  paste0(what, ": install the ", paste(missing, collapse = " and "),
         if (length(missing) == 1) " package" else " packages", " to preview it.")
}

#' The preview of a value whose format's package isn't installed: its
#' metadata, and which package to install (#228)
#' @noRd
missing_package_card <- function(target, message, store = NULL, ns = identity) {
  bslib::card(
    class = "tb-preview",
    preview_header(target, store = store, ns = ns),
    bslib::card_body(class = "tb-preview-gate", shiny::tags$p(message))
  )
}
