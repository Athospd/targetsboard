# Previews of data frames and of vectors, as reactable tables themed from the
# page's Bootstrap variables, so they follow dark mode. One column summary
# serves both: a data frame's Columns tab is one line of it per column, and a
# vector's preview is the same summary, for a single column.

# The most rows a data frame's Rows tab sends to the page.
preview_rows <- 1000

# reactable's colours, from the page's Bootstrap variables, so a table follows
# the theme and dark mode. Every table of the page uses them; `...` adds
# reactableTheme()'s other arguments.
board_table_theme <- function(..., headerStyle = list()) {
  reactable::reactableTheme(
    color = "var(--bs-body-color)",
    backgroundColor = "transparent",
    borderColor = "var(--bs-border-color-translucent)",
    stripedColor = "var(--bs-tertiary-bg)",
    highlightColor = "var(--bs-secondary-bg)",
    headerStyle = utils::modifyList(list(
      background = "var(--bs-body-bg)",
      borderBottomColor = "var(--bs-border-color)"
    ), headerStyle),
    inputStyle = list(
      background = "var(--bs-body-bg)", color = "var(--bs-body-color)",
      borderColor = "var(--bs-border-color)"
    ),
    pageButtonHoverStyle = list(background = "var(--bs-secondary-bg)"),
    pageButtonActiveStyle = list(background = "var(--bs-tertiary-bg)"),
    pageButtonCurrentStyle = list(background = "var(--bs-secondary-bg)"),
    ...
  )
}

# The value preview's tables: the board's colours, smaller and denser.
preview_table_theme <- function() {
  board_table_theme(
    cellPadding = "0.3rem 0.5rem",
    style = list(fontSize = "var(--tb-font-size-xs)"),
    headerStyle = list(fontWeight = 600),
    paginationStyle = list(fontSize = "var(--tb-font-size-xxs)")
  )
}

# A reactable with the preview's defaults; `...` overrides them.
preview_table <- function(data, columns = NULL, ...) {
  defaults <- list(
    theme = preview_table_theme(),
    compact = TRUE, striped = TRUE, highlight = TRUE, resizable = TRUE,
    wrap = FALSE, defaultPageSize = 25, showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 100),
    defaultColDef = reactable::colDef(minWidth = 80, headerClass = "tb-table-header")
  )
  do.call(reactable::reactable, c(list(data, columns = columns), utils::modifyList(defaults, list(...))))
}

# A short name for a column's type, as tibble prints it.
type_abbr <- function(x) {
  if (is.ordered(x)) return("ord")
  if (is.factor(x)) return("fct")
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXt")) return("dttm")
  if (inherits(x, "difftime")) return("drtn")
  if (is.logical(x)) return("lgl")
  if (is.integer(x)) return("int")
  if (is.double(x)) return("dbl")
  if (is.character(x)) return("chr")
  if (is.complex(x)) return("cpl")
  # A list column, from the cells a table shows: there can be many.
  if (is.list(x)) return(list_type(utils::head(x, preview_rows)))
  class(x)[1]
}

format_number <- function(x) {
  format(signif(x, 4), big.mark = ",", scientific = abs(x) >= 1e9 || (x != 0 && abs(x) < 1e-4), trim = TRUE)
}

is_continuous <- function(x) {
  (is.numeric(x) && !is.factor(x)) || inherits(x, c("Date", "POSIXt"))
}

# A tiny histogram, as inline SVG, for a number or date column.
spark_histogram <- function(x, width = 96, height = 22) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  # Too few values make bars, not a distribution.
  if (length(x) < 10 || length(unique(x)) < 3) return(NULL)
  # From zero: hist() overflows on large values, such as a date-time's
  # seconds since 1970. The shape is the same.
  counts <- graphics::hist(x - min(x), breaks = 16, plot = FALSE)$counts
  bar <- width / length(counts)
  heights <- counts / max(counts) * (height - 1)
  # Tags, not an HTML string: reactable draws a header's tags as React
  # elements, and would show an HTML string as text.
  bars <- lapply(seq_along(counts), function(i) {
    htmltools::tag("rect", list(
      x = sprintf("%.1f", (i - 1) * bar + 0.5), y = sprintf("%.1f", height - heights[i]),
      width = sprintf("%.1f", max(bar - 1, 0.5)), height = sprintf("%.1f", heights[i])
    ))
  })
  htmltools::tag("svg", c(
    list(
      class = "tb-spark", width = width, height = height,
      viewBox = sprintf("0 0 %d %d", width, height), role = "img", `aria-label` = "distribution"
    ),
    bars
  ))
}

# The few most common values of a text or factor column, with counts.
top_values <- function(x, n = 3) {
  counts <- sort(table(as.character(x), useNA = "no"), decreasing = TRUE)
  if (!length(counts)) return(NULL)
  shown <- utils::head(counts, n)
  paste0(names(shown), " (", format(as.integer(shown), big.mark = ",", trim = TRUE), ")", collapse = ", ")
}

#' One column's summary: its type, missing and unique counts, a few
#' statistics and, for numbers and dates, a tiny histogram
#' @noRd
column_summary <- function(x) {
  missing <- sum(is.na(x))
  present <- x[!is.na(x)]
  stats <- if (!length(present)) {
    NULL
  } else if (is.list(x)) {
    NULL
  } else if (is.logical(x)) {
    sprintf("%s TRUE, %s FALSE", format(sum(present), big.mark = ","), format(sum(!present), big.mark = ","))
  } else if (inherits(x, c("Date", "POSIXt"))) {
    paste(format(min(present)), "to", format(max(present)))
  } else if (is.numeric(x) && !is.factor(x)) {
    sprintf(
      "min %s \u00b7 mean %s \u00b7 max %s",
      format_number(min(present)), format_number(mean(present)), format_number(max(present))
    )
  } else {
    top_values(present)
  }
  list(
    type = type_abbr(x),
    missing = missing,
    unique = if (is.list(x)) NA_integer_ else length(unique(present)),
    stats = stats %||% "",
    spark = if (is_continuous(x)) spark_histogram(present)
  )
}

# A column's header: its name over its type.
column_header <- function(name, type, spark = NULL, rows = NULL) {
  htmltools::tagList(
    htmltools::tags$span(class = "tb-col-name", name),
    htmltools::tags$span(class = "tb-col-type", type),
    if (!is.null(spark)) {
      htmltools::tags$span(
        class = "tb-col-spark",
        title = sprintf("The distribution of all %s rows", format(rows, big.mark = ",")),
        spark
      )
    }
  )
}

#' A list-column cell, as tibble prints it: "tibble [7 x 10]", "lm",
#' "dbl [3]", "NULL" (#298)
#'
#' pillar's summary when it's installed (it is wherever tibble is), else
#' the list tree's (type_label()). Never formats the whole value.
#' @noRd
cell_summary <- function(v) {
  if (is.null(v)) return("NULL")
  if (inherits(v, "gg")) return("gg") # pillar abbreviates its package instead
  if (rlang::is_installed("pillar")) {
    summary <- tryCatch(pillar::obj_sum(v), error = function(e) NULL)
    if (is.character(summary) && length(summary) == 1) return(as.vector(summary)) # without its attributes
  }
  type_label(v)
}

# The type of a list column's cells, for its header: "list<tibble>",
# "list<lm>", "list<mixed>"; just "list" when every cell is NULL.
list_type <- function(x) {
  types <- unique(vapply(x[!vapply(x, is.null, logical(1))], function(v) sub(" .*$", "", cell_summary(v)), ""))
  if (!length(types)) return("list")
  paste0("list<", if (length(types) == 1) types else "mixed", ">")
}

# A list-column cell in the Rows tab: its summary, on a button that opens
# its own views (mod_preview.R), with its column and row.
cell_button <- function(v, column, row) {
  summary <- cell_summary(v)
  if (is.null(v)) return(as.character(htmltools::tags$span(class = "tb-cell-null", summary)))
  as.character(htmltools::tags$button(
    type = "button", class = "btn btn-link btn-sm p-0 tb-cell-open",
    title = "Open this cell's value",
    `data-tb-cell` = jsonlite::toJSON(list(column = column, row = row), auto_unbox = TRUE),
    paste0("<", summary, ">")
  ))
}

#' A data frame's Rows tab: its first rows, with each column's type
#' @noRd
rows_view <- function(x, limit = preview_rows) {
  shown <- utils::head(as.data.frame(x, stringsAsFactors = FALSE), limit)
  # List columns as their cells' summaries, each opening its value (#298).
  cells <- character()
  for (column in names(shown)) {
    if (is.list(shown[[column]])) {
      shown[[column]] <- vapply(seq_along(shown[[column]]), function(i) cell_button(shown[[column]][[i]], column, i), "")
      cells <- c(cells, column)
    } else if (!is.atomic(shown[[column]])) {
      # Anything else reactable can't show, as text.
      shown[[column]] <- vapply(shown[[column]], function(v) paste(format(v), collapse = ", "), character(1))
    }
  }
  # A number or date column is headed by its distribution, of every row:
  # the table shows only the first ones.
  columns <- stats::setNames(lapply(names(shown), function(column) {
    type <- type_abbr(x[[column]])
    spark <- if (is_continuous(x[[column]])) spark_histogram(x[[column]], width = 64, height = 16)
    cell <- column %in% cells # its cells are buttons: no filter or sort on their markup
    reactable::colDef(header = function(value) column_header(column, type, spark, nrow(x)), html = cell,
                      filterable = !cell, sortable = !cell, minWidth = if (cell) 150 else 100)
  }), names(shown))
  htmltools::tagList(
    if (nrow(x) > limit) {
      htmltools::tags$p(class = "tb-table-caption", sprintf(
        "The first %s of %s rows.", format(limit, big.mark = ","), format(nrow(x), big.mark = ",")
      ))
    },
    preview_table(
      shown, columns = columns, searchable = TRUE, filterable = TRUE, sortable = TRUE,
      defaultPageSize = 10
    )
  )
}

#' A data frame's Columns tab: one line per column
#' @noRd
columns_view <- function(x) {
  summaries <- lapply(x, column_summary)
  data <- data.frame(
    column = names(x),
    type = vapply(summaries, `[[`, character(1), "type"),
    missing = vapply(summaries, `[[`, numeric(1), "missing"),
    unique = vapply(summaries, function(s) as.numeric(s$unique), numeric(1)),
    stats = vapply(summaries, `[[`, character(1), "stats"),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  preview_table(
    data,
    pagination = nrow(data) > 50, defaultPageSize = 50, wrap = TRUE,
    columns = list(
      column = reactable::colDef(name = "Column", minWidth = 70),
      type = reactable::colDef(name = "Type", width = 48, class = "tb-col-type-cell"),
      missing = reactable::colDef(name = "Missing", width = 64, format = reactable::colFormat(separators = TRUE)),
      unique = reactable::colDef(name = "Unique", width = 60, format = reactable::colFormat(separators = TRUE)),
      # The statistics, with the histogram under them.
      stats = reactable::colDef(
        name = "Summary", minWidth = 140, sortable = FALSE, html = TRUE,
        cell = function(value, index) {
          paste0("<div>", htmltools::htmlEscape(value), "</div>", as.character(summaries[[index]]$spark %||% ""))
        }
      )
    )
  )
}

#' A vector's preview: its summary, its distribution, and its names or its
#' values' counts
#' @noRd
vector_view <- function(x) {
  if (length(x) == 1 && is.null(names(x))) {
    # In full: a single number is read for its digits.
    shown <- if (is.numeric(x) && !is.factor(x)) format(x, digits = 15, big.mark = ",") else format(x)
    return(htmltools::tags$div(class = "tb-scalar", shown))
  }
  summary <- column_summary(x)
  facts <- c(
    summary$type,
    paste("length", format(length(x), big.mark = ",")),
    if (summary$missing) paste(format(summary$missing, big.mark = ","), "missing"),
    paste(format(summary$unique, big.mark = ","), "unique")
  )
  values <- if (!is.null(names(x))) {
    preview_table(
      data.frame(name = names(x), value = unname(x), stringsAsFactors = FALSE),
      columns = list(name = reactable::colDef(name = "Name"), value = reactable::colDef(name = "Value")),
      sortable = TRUE
    )
  } else if (is.factor(x) || is.character(x) || is.logical(x)) {
    counts <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    preview_table(
      data.frame(value = names(counts), count = as.integer(counts), stringsAsFactors = FALSE),
      columns = list(
        value = reactable::colDef(name = "Value"),
        count = reactable::colDef(name = "Count", format = reactable::colFormat(separators = TRUE))
      ),
      sortable = TRUE
    )
  }
  # Numbers, dates and date-times: their values are what Printed shows.
  htmltools::tagList(
    htmltools::tags$div(class = "tb-vector-facts", lapply(facts, htmltools::tags$span)),
    if (nzchar(summary$stats)) htmltools::tags$p(class = "tb-vector-stats", summary$stats),
    if (!is.null(summary$spark)) htmltools::tags$div(class = "tb-vector-spark", spark_histogram(x[!is.na(x)], width = 280, height = 56)),
    values
  )
}

is_plain_vector <- function(x) is.null(dim(x)) && is.atomic(x)

#' @rdname tar_board_preview
#' @export
tar_board_preview.data.frame <- function(x, target, ...) {
  list(Rows = rows_view(x), Columns = columns_view(x))
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.numeric <- function(x, target, ...) {
  if (!is_plain_vector(x) || !length(x)) return(NULL)
  vector_view(x)
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.logical <- tar_board_preview.numeric

#' @rdname tar_board_preview
#' @export
tar_board_preview.factor <- tar_board_preview.numeric

#' @rdname tar_board_preview
#' @export
tar_board_preview.Date <- tar_board_preview.numeric

#' @rdname tar_board_preview
#' @export
tar_board_preview.POSIXct <- tar_board_preview.numeric

# How deep a cell's value can be opened, cell within cell (#298).
cell_depth <- 10

# One step into a cell, as a click sends it: its column and row.
cell_step <- function(x) list(column = as.character(x$column), row = as.integer(x$row))

#' The value of a list-column cell, cell within cell: `path` is a list of
#' steps, from the target's value
#' @noRd
cell_value <- function(value, path) {
  for (step in path) {
    column <- value[[step$column]]
    if (!is.data.frame(value) || !is.list(column) || !isTRUE(step$row >= 1 && step$row <= length(column))) {
      stop("That cell is no longer in the value: open the target again.", call. = FALSE)
    }
    value <- column[[step$row]]
  }
  value
}

#' A cell's value, in a dialog: where it is, as R code, and its views
#'
#' Its tables' cells open the next level (`cell_drill`); Back goes up one.
#' @noRd
cell_dialog <- function(name, path, built, ns = identity, plot_ids = character()) {
  where <- paste0(name, paste(vapply(path, function(s) sprintf("$%s[[%d]]", backquote_name(s$column), s$row), ""), collapse = ""))
  shiny::modalDialog(
    title = shiny::tags$code(where), size = "xl", easyClose = TRUE,
    shiny::tags$div(
      class = "tb-cell-dialog", `data-tb-cell-input` = ns("cell_drill"),
      shiny::tags$p(class = "tb-preview-facts", lapply(c(built$facts$class, built$facts$shape), shiny::tags$span)),
      do.call(bslib::navset_underline, view_panels(built$views, plot_ids, ns))
    ),
    footer = shiny::tagList(
      if (length(path) > 1) shiny::actionButton(ns("cell_back"), "Back", class = "btn-default"),
      shiny::modalButton("Close")
    )
  )
}

# A column name as R code after `$`: backquoted unless syntactic.
backquote_name <- function(x) if (make.names(x) == x) x else paste0("`", x, "`")
