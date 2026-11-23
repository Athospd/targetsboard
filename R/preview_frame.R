# The value preview's frame: what the board knows about a target from the
# store, the views of its value (tar_board_preview()), and the card that
# shows them. The board reads a value only through here.

#' The size above which the preview asks before reading a value
#' @noRd
preview_bytes_limit <- function() getOption("targetsboard.preview_bytes", 100e6)

#' What the store says about a target, without reading its value
#'
#' @return `list(name, format, bytes, time)`, or `NULL` when the store has no
#'   record of it. A pattern's bytes are the sum of its branches'.
#' @noRd
preview_target <- function(name, store = targets::tar_config_get("store")) {
  meta <- tryCatch(
    targets::tar_meta(fields = c("format", "bytes", "time"), store = store),
    error = function(e) NULL
  )
  if (is.null(meta) || !name %in% meta$name) return(NULL)
  row <- meta[meta$name == name, ]
  list(
    name = name,
    format = if ("format" %in% names(row)) na_null(row$format) else NULL,
    bytes = if ("bytes" %in% names(row)) na_null(row$bytes) else NULL,
    time = if ("time" %in% names(row)) na_null(row$time) else NULL
  )
}

# Identifies one build of a target: a rebuilt target is a new key, so the
# cache never shows it stale.
preview_key <- function(target) {
  paste(target$name, format(target$time %||% NA), target$bytes %||% NA, sep = "|")
}

# Whether a value is too large to read without asking.
preview_too_large <- function(target, limit = preview_bytes_limit()) {
  # A file target's value is only its paths: its size is its files'.
  !is_file_target(target) && !is.null(target$bytes) && isTRUE(target$bytes > limit)
}

format_bytes <- function(bytes) {
  if (is.null(bytes) || is.na(bytes)) return(NULL)
  format(structure(bytes, class = "object_size"), units = "auto", standard = "SI")
}

# Class and shape, for the frame's header: "data.frame, 200,000 x 2".
value_facts <- function(x) {
  shape <- if (!is.null(dim(x))) {
    paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " \u00d7 ")
  } else if (is.atomic(x) || is.list(x)) {
    paste("length", format(length(x), big.mark = ","))
  }
  list(class = class(x)[1], shape = shape)
}

# What print() shows, capped: a big value prints long. A value that draws
# when printed, such as a ggplot, draws on a null device: R's default one
# would write Rplots.pdf into the author's folder.
printed_text <- function(x, max_lines = 500) {
  old <- options(max.print = 2000, width = 80)
  grDevices::pdf(NULL)
  device <- grDevices::dev.cur()
  on.exit({
    options(old)
    if (device %in% grDevices::dev.list()) grDevices::dev.off(device)
  })
  lines <- utils::capture.output(print(x))
  if (length(lines) > max_lines) {
    lines <- c(lines[seq_len(max_lines)], sprintf("... %s more lines", format(length(lines) - max_lines, big.mark = ",")))
  }
  paste(lines, collapse = "\n")
}

is_preview_view <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list", "html", "htmlwidget", "tar_board_preview_plot", "tar_board_preview_files"))
}

#' The views of a value, with the Printed fallback
#'
#' Calls `tar_board_preview()`, checks what it returned, and tries each plot
#' once off screen, so that a method or a plot that fails falls back to the
#' printed value with a note, never to an error in the pane.
#'
#' @return `list(facts, views, note)`: `views` is a named list, Printed last.
#' @noRd
build_preview <- function(x, target) {
  note <- NULL
  fail <- function(e) {
    note <<- sprintf("The %s preview failed: %s", class(x)[1], conditionMessage(e))
    list()
  }
  views <- tryCatch({
    result <- tar_board_preview(dispatch_value(x, target), target)
    views <- if (is.null(result)) {
      list()
    } else if (is_preview_view(result)) {
      list(Preview = result)
    } else if (is.list(result) && length(result) && !is.null(names(result)) &&
               all(nzchar(names(result))) && all(vapply(result, is_preview_view, logical(1)))) {
      result
    } else {
      stop("tar_board_preview() returned neither a view nor a named list of views.", call. = FALSE)
    }
    for (view in views) if (inherits(view, "tar_board_preview_plot")) try_plot(view)
    views
  }, error = fail)
  printed <- tryCatch(printed_text(x), error = function(e) paste("print() failed:", conditionMessage(e)))
  views$Printed <- shiny::tags$pre(class = "tb-preview-printed", printed)
  list(facts = value_facts(x), views = views, note = note)
}

# Draws a plot on a null device: an error here is the plot's own.
try_plot <- function(view) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  draw_preview_plot(view)
}

# The last few previews built, so going back and forth between targets is
# instant. Keyed by preview_key().
preview_cache <- function(size = 5) {
  keys <- character()
  items <- list()
  list(
    get = function(key) items[[key]],
    set = function(key, value) {
      keys <<- c(setdiff(keys, key), key)
      items[[key]] <<- value
      for (old in utils::head(keys, max(0, length(keys) - size))) items[[old]] <<- NULL
      keys <<- utils::tail(keys, size)
      invisible(value)
    },
    clear = function() {
      keys <<- character()
      items <<- list()
    }
  )
}

# R code that reads the value in the author's console.
read_code <- function(name, store = NULL) {
  paste0("targets::tar_read(", paste(c(name, store_arg(store)), collapse = ", "), ")")
}

preview_header <- function(target, facts = NULL, store = NULL) {
  built <- if (!is.null(target$time)) format(as.POSIXct(target$time), "%Y-%m-%d %H:%M")
  items <- c(
    facts$class, facts$shape, format_bytes(target$bytes),
    if (!is.null(target$format)) paste(target$format, "format"),
    if (!is.null(built)) paste("built", built)
  )
  code <- read_code(target$name, store)
  bslib::card_header(
    class = "tb-preview-header",
    shiny::tags$div(
      class = "d-flex justify-content-between align-items-start gap-2",
      shiny::tags$code(class = "tb-preview-name", target$name),
      shiny::tags$button(
        class = "btn btn-sm btn-outline-secondary tb-copy", `data-copy` = code,
        title = code, onclick = copy_button_js, "Copy"
      )
    ),
    shiny::tags$div(class = "tb-preview-facts", lapply(items, shiny::tags$span))
  )
}

#' The frame of a value: its header, and a tab per view
#'
#' @param plot_ids The output ids of the plot views, in the order they come.
#' @noRd
preview_frame <- function(target, built, plot_ids = character(), store = NULL) {
  i <- 0
  panels <- lapply(names(built$views), function(title) {
    view <- built$views[[title]]
    if (inherits(view, "tar_board_preview_plot")) {
      i <<- i + 1
      view <- shiny::tags$div(class = "tb-preview-plot", shiny::imageOutput(plot_ids[[i]], height = "100%"))
    } else if (inherits(view, "htmlwidget")) {
      view <- shiny::tags$div(class = "tb-preview-widget", size_widget(view))
    } else if (inherits(view, "tar_board_preview_files")) {
      view <- files_sections(view)
    }
    bslib::nav_panel(title, shiny::tags$div(class = "tb-preview-view", view))
  })
  bslib::card(
    class = "tb-preview", full_screen = TRUE,
    preview_header(target, built$facts, store),
    bslib::card_body(
      class = "tb-preview-body", padding = 0, gap = 0,
      if (!is.null(built$note)) shiny::tags$p(class = "tb-preview-note", built$note),
      do.call(bslib::navset_underline, panels)
    )
  )
}

#' The frame of a value too large to read without asking
#' @noRd
preview_gate <- function(target, limit = preview_bytes_limit(), store = NULL) {
  bslib::card(
    class = "tb-preview",
    preview_header(target, store = store),
    bslib::card_body(
      class = "tb-preview-gate",
      shiny::tags$p(sprintf(
        "This value is %s in the store, more than the %s the preview reads on its own: reading it can take a while and use a lot of memory.",
        format_bytes(target$bytes), format_bytes(limit)
      )),
      shiny::actionButton("value_preview_read", "Load the value", class = "btn-sm")
    )
  )
}

#' Reads the preview methods of a project's `_targetsboard.R`
#'
#' Sources the file, and only it, into its own environment, which sees the
#' package's exported functions, and keeps its `tar_board_preview.<class>()`
#' functions in place of those read before: tar_board_preview() tries them
#' first.
#'
#' @return The classes read, invisibly; an error when the file fails, which
#'   keeps the methods read before.
#' @noRd
load_preview_methods <- function(path = "_targetsboard.R") {
  if (!file.exists(path)) {
    clear_project_methods()
    return(invisible(character()))
  }
  ns <- asNamespace("targetsboard")
  exports <- new.env(parent = globalenv())
  for (f in getNamespaceExports(ns)) assign(f, get(f, envir = ns), envir = exports)
  env <- new.env(parent = exports)
  sys.source(path, envir = env, keep.source = FALSE)
  methods <- ls(env, pattern = "^tar_board_preview[.]")
  methods <- methods[vapply(methods, function(m) is.function(get(m, envir = env)), logical(1))]
  classes <- sub("^tar_board_preview[.]", "", methods)
  clear_project_methods()
  for (i in seq_along(methods)) assign(classes[[i]], get(methods[[i]], envir = env), envir = project_methods)
  invisible(classes)
}

# A widget fills the preview's width, at a height of its own unless it set
# one; in full-screen, CSS lets it fill the frame.
size_widget <- function(widget) {
  widget$width <- "100%"
  if (is.null(widget$height)) widget$height <- 420
  widget
}

# The pixels per inch of a plot at its CSS size: 96, as browsers count, so a
# plot's text reads at the size its theme gives in points.
preview_plot_dpi <- 96

#' Draws a plot view at the size of its output, at twice the pixels
#'
#' The output's size comes from the page: the preview's CSS gives it 0.65 of
#' its width in height, or the frame in full-screen, and a change of either
#' draws the plot again. ragg draws it when installed, else png().
#' @noRd
render_preview_plot <- function(view, id, session) {
  shiny::renderImage({
    data <- session$clientData
    width <- data[[paste0("output_", id, "_width")]]
    height <- data[[paste0("output_", id, "_height")]]
    shiny::req(width > 0, height > 0)
    file <- tempfile(fileext = ".png")
    draw_png(view, file, width, height)
    list(src = file, width = width, height = height, alt = "A plot of the value", contentType = "image/png")
  }, deleteFile = TRUE)
}

draw_png <- function(view, file, width, height, scale = 2) {
  device <- if (rlang::is_installed("ragg")) ragg::agg_png else grDevices::png
  device(file, width = width * scale, height = height * scale, res = preview_plot_dpi * scale, units = "px")
  on.exit(grDevices::dev.off())
  draw_preview_plot(view)
  invisible(file)
}
