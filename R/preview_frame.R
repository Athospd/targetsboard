# The value preview's frame: what the board knows about a target from the
# store, the views of its value (tar_board_preview()), and the card that
# shows them. The board reads a value only through here.

#' The size above which the preview asks before reading a value
#' @noRd
preview_bytes_limit <- function() getOption("targetsboard.preview_bytes", 100e6)

#' What the store says about a target, without reading its value
#'
#' A target or branch that errored under `error = "null"` stored `NULL` in
#' its place (format "null"): its `stand_in` is its error message. With all
#' of a pattern's branches, `pattern$stand_ins` numbers the ones that did.
#'
#' @return `list(name, format, bytes, time, repository)`, or `NULL` when the
#'   store has no record of it. A pattern's bytes are the sum of its
#'   branches'.
#' @noRd
preview_target <- function(name, store = targets::tar_config_get("store"), branch = 1L) {
  meta <- tryCatch(
    targets::tar_meta(fields = c("type", "format", "bytes", "time", "children", "error", "repository", "path", "iteration"), store = store),
    error = function(e) NULL
  )
  if (is.null(meta) || !name %in% meta$name) return(NULL)
  field <- function(row, f) if (f %in% names(row)) na_null(row[[f]]) else NULL
  stand_in <- function(row) if (identical(field(row, "format"), "null")) field(row, "error") %||% ""
  row <- meta[meta$name == name, ]
  target <- list(
    name = name, format = field(row, "format"), bytes = field(row, "bytes"), time = field(row, "time"),
    repository = field(row, "repository"),
    paths = if ("path" %in% names(row)) Filter(Negate(is.na), row$path[[1]]) # a file target's files
  )
  target$stand_in <- stand_in(row)
  children <- if ("children" %in% names(row)) row$children[[1]] else NULL
  children <- children[!is.na(children)]
  # Reading it runs the store's custom R code: targets asks first (#237).
  target$custom_code <- has_custom_code(meta[meta$name %in% c(name, children), , drop = FALSE])
  if (!identical(field(row, "type"), "pattern") || !length(children)) return(target)

  # A pattern shows one branch at a time, as tar_read(name, branches = i)
  # reads it, or all of them combined (branch 0).
  branch <- as.integer(branch)
  if (is.na(branch) || branch < 0L || branch > length(children)) branch <- 1L
  target$pattern <- list(branches = length(children), branch = branch, total_bytes = target$bytes,
                         iteration = field(row, "iteration"))
  rows <- meta[match(children, meta$name), ]
  if (branch == 0L) {
    times <- rows$time[!is.na(rows$time)]
    target$time <- if (length(times)) max(times)
    if ("format" %in% names(rows)) target$pattern$stand_ins <- which(rows$format %in% "null")
    return(target)
  }
  child <- rows[branch, ]
  target$pattern$branch_name <- children[[branch]]
  target$format <- field(child, "format") %||% target$format
  target$bytes <- field(child, "bytes")
  target$time <- field(child, "time")
  target$repository <- field(child, "repository") %||% target$repository
  target$stand_in <- stand_in(child)
  target
}

#' What the frame says about stand-ins, or NULL: a value that is NULL only
#' because its target errored under `error = "null"`, or the branches that
#' all branches combined leave out (tar_read() drops their NULLs)
#' @noRd
stand_in_note <- function(target) {
  if (!is.null(target$stand_in)) {
    message <- sub("\n.*", "", target$stand_in) # its first line
    if (nchar(message) > 200) message <- paste0(substr(message, 1, 199), intToUtf8(8230L))
    return(sprintf(
      'This %s errored%s: with error = "null", targets stored NULL in its place and went on. NULL is a stand-in, not its result.',
      if (is.null(target$pattern)) "target" else "branch",
      if (nzchar(message)) sprintf(' ("%s")', message) else ""
    ))
  }
  numbers <- target$pattern$stand_ins
  if (!length(numbers)) return(NULL)
  shown <- utils::head(numbers, 10)
  listed <- paste(shown, collapse = ", ")
  if (length(numbers) > length(shown)) listed <- sprintf("%s and %d more", listed, length(numbers) - length(shown))
  sprintf(
    '%d of %s branches errored and are left out: with error = "null", each stored NULL in its place (%s %s).',
    length(numbers), format(target$pattern$branches, big.mark = ","),
    if (length(numbers) == 1) "branch" else "branches", listed
  )
}

# The branches argument of tar_read() for a pattern's branch, if one.
branch_arg <- function(target) {
  branch <- target$pattern$branch
  if (is.null(branch) || branch == 0L || by_branch_name(target)) NULL else paste("branches =", branch)
}

# A branch of a list-iteration pattern is read by its own name: tar_read(x,
# branches = i) wraps it in a list of one (#303). One branch, either way.
by_branch_name <- function(target) {
  p <- target$pattern
  identical(p$iteration, "list") && !is.null(p$branch_name) && !identical(p$branch, 0L)
}

# The name the console code reads: the target, or such a branch.
read_name <- function(target) if (by_branch_name(target)) target$pattern$branch_name else target$name

#' A store's custom R code (#237)
#'
#' targets runs the read code of `tar_format()` and `tar_repository_cas()`
#' targets, and asks first unless the `TAR_ASK` environment variable is
#' "false": at a console prompt in an interactive session, which the
#' browser doesn't see and which blocks the board; an error otherwise. The
#' board asks in the preview instead, and its reads of a store the author
#' trusted run with `TAR_ASK` "false", for those reads only: it never sets
#' `TAR_ASK` for the author.
#' @param meta `tar_meta()` rows with `format` and `repository`.
#' @noRd
has_custom_code <- function(meta) {
  any(grepl("^format_custom", meta$format)) || any(grepl("^repository_cas", meta$repository))
}

# Whether reading custom code needs the author's word: as targets decides.
store_needs_trust <- function(custom_code) {
  isTRUE(custom_code) && !identical(trimws(tolower(Sys.getenv("TAR_ASK"))), "false")
}

# One of the board's reads of a store the author trusted.
read_trusted <- function(expr) withr::with_envvar(c(TAR_ASK = "false"), expr)

#' The preview's question before running a store's custom code
#' @noRd
trust_gate <- function(target, store = NULL, ns = identity) {
  bslib::card(
    class = "tb-preview",
    preview_header(target, store = store, ns = ns),
    bslib::card_body(
      class = "tb-preview-gate",
      shiny::tags$p(
        "This store holds custom R code, from ", shiny::tags$code("tar_format()"), " or ",
        shiny::tags$code("tar_repository_cas()"), ", and reading this value runs it. ",
        "Read it only if you trust where the store came from."
      ),
      shiny::tags$p(
        class = "text-muted small",
        "The board then reads this store for the rest of the session. Your R session still asks: ",
        "targets' ", shiny::tags$code("TAR_ASK"), " setting is left as it is."
      ),
      shiny::actionButton(ns("trust"), "Trust this store and read", class = "btn-sm")
    )
  )
}

#' Reads the value the preview shows: a target's, or one branch of a pattern
#' @noRd
read_preview_value <- function(target, store = NULL) {
  branch <- target$pattern$branch
  if (by_branch_name(target)) return(targets::tar_read_raw(target$pattern$branch_name, store = store))
  if (is.null(branch) || branch == 0L) return(targets::tar_read_raw(target$name, store = store))
  targets::tar_read_raw(target$name, branches = branch, store = store)
}

# Identifies one build of a target: a rebuilt target is a new key, so the
# cache never shows it stale.
preview_key <- function(target) {
  paste(target$name, target$pattern$branch %||% "", format(target$time %||% NA), target$bytes %||% NA, sep = "|")
}

# Whether a value is in a cloud bucket: reading it downloads it (#131).
preview_in_cloud <- function(target) isTRUE(target$repository %in% c("aws", "gcp"))

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
read_code <- function(name, store = NULL, branches = NULL) {
  paste0("targets::tar_read(", paste(c(name, branches, store_arg(store)), collapse = ", "), ")")
}

preview_header <- function(target, facts = NULL, store = NULL, ns = identity) {
  built <- if (!is.null(target$time)) format(as.POSIXct(target$time), "%Y-%m-%d %H:%M")
  items <- c(
    facts$class, facts$shape, format_bytes(target$bytes),
    # tar_format()'s format is its code, encoded: name it instead (#237).
    if (!is.null(target$format)) paste(sub("^format_custom.*$", "custom (tar_format())", target$format), "format"),
    if (!is.null(built)) paste("built", built)
  )
  code <- read_code(read_name(target), store, branch_arg(target))
  bslib::card_header(
    class = "tb-preview-header",
    shiny::tags$div(
      class = "d-flex justify-content-between align-items-start gap-2",
      shiny::tags$code(class = "tb-preview-name", target$name),
      copy_code_button(code)
    ),
    shiny::tags$div(class = "tb-preview-facts", lapply(items, shiny::tags$span)),
    if (!is.null(target$pattern)) branch_picker(target$pattern, ns = ns)
  )
}

# A button that asks for the branch to show, as input `input_id`: 1 to n,
# or 0 for all of them.
branch_button <- function(value, input_id, ...) {
  shiny::tags$button(type = "button", ...) |> send_on_click(input_id, value)
}

#' A pattern's branch picker: previous and next, a branch by number, and
#' all the branches combined while their size is under the preview's limit
#' @param ns The preview module's namespace, for the input it sends.
#' @noRd
branch_picker <- function(pattern, limit = preview_bytes_limit(), ns = identity) {
  input_id <- ns("branch")
  n <- pattern$branches
  i <- pattern$branch
  fits <- is.null(pattern$total_bytes) || isTRUE(pattern$total_bytes <= limit)
  all_link <- function(label) {
    branch_button(0, input_id, class = "btn btn-link btn-sm p-0 tb-branch-all", label)
  }
  if (i == 0L) {
    return(shiny::tags$div(
      class = "tb-preview-branches",
      shiny::tags$span(sprintf("All %s branches", format(n, big.mark = ","))),
      branch_button(1, input_id, class = "btn btn-link btn-sm p-0", "One branch")
    ))
  }
  shiny::tags$div(
    class = "tb-preview-branches",
    branch_button(
      max(i - 1L, 1L), input_id, class = "btn btn-sm btn-default tb-branch-step", title = "Previous branch",
      `aria-label` = "Previous branch", disabled = if (i <= 1) NA, shiny::icon("chevron-left")
    ),
    shiny::tags$label(
      class = "tb-branch-label", "Branch",
      shiny::tags$input(
        type = "number", class = "form-control form-control-sm tb-branch-number", min = 1, max = n, value = i,
        # A change sends it, if it's a branch (srcjs/actions.js).
        `aria-label` = "Branch number", `data-tb-input` = input_id
      ),
      sprintf("of %s", format(n, big.mark = ","))
    ),
    branch_button(
      i + 1L, input_id, class = "btn btn-sm btn-default tb-branch-step", title = "Next branch",
      `aria-label` = "Next branch", disabled = if (i >= n) NA, shiny::icon("chevron-right")
    ),
    shiny::tags$code(class = "tb-branch-name", pattern$branch_name),
    if (fits) {
      all_link("All branches")
    } else {
      shiny::tags$span(
        class = "tb-branch-all-off",
        title = sprintf("All branches are %s, more than the %s the preview reads", format_bytes(pattern$total_bytes), format_bytes(limit)),
        "All branches: too large"
      )
    }
  )
}

#' A value's views as tabs: a plot in its output (`plot_ids`, in order), a
#' widget sized to the pane, a file target's sections
#' @noRd
view_panels <- function(views, plot_ids = character(), ns = identity) {
  i <- 0
  lapply(names(views), function(title) {
    view <- views[[title]]
    if (inherits(view, "tar_board_preview_plot")) {
      i <<- i + 1
      view <- shiny::tags$div(class = "tb-preview-plot", shiny::imageOutput(plot_ids[[i]], height = "100%"))
    } else if (inherits(view, "htmlwidget")) {
      view <- shiny::tags$div(class = "tb-preview-widget", size_widget(view))
    } else if (inherits(view, "tar_board_preview_files")) {
      view <- files_sections(view, ns)
    }
    bslib::nav_panel(title, shiny::tags$div(class = "tb-preview-view", view))
  })
}

#' The frame of a value: its header, and a tab per view
#'
#' @param plot_ids The output ids of the plot views, in the order they come.
#' @param ns The preview module's namespace, for the ids the frame writes.
#' @noRd
preview_frame <- function(target, built, plot_ids = character(), store = NULL, ns = identity,
                          tabs_for = function(titles) NULL, own_tabs = target_tabs(ns)) {
  panels <- view_panels(built$views, plot_ids, ns)
  # A card inside the board's card, against the usual advice: it sits in its
  # own sidebar, not the board's body, and full_screen needs a card, to
  # read a large value on the whole page.
  bslib::card(
    class = "tb-preview", full_screen = TRUE,
    preview_header(target, built$facts, store, ns),
    bslib::card_body(
      class = "tb-preview-body", padding = 0, gap = 0,
      `data-tb-cell-input` = ns("cell_open"), # a list-column cell's click (#298)
      lapply(c(stand_in_note(target), built$note), function(note) shiny::tags$p(class = "tb-preview-note", note)),
      value_tabs(panels, tabs_for, ns, own_tabs)
    )
  )
}

#' The frame of a value the preview doesn't read without asking
#'
#' One too large, or one in a cloud bucket (#131): reading it downloads it,
#' which the provider may bill and which needs its package and credentials.
#' targets reads a cloud value through a temporary file; a cloud file target
#' it would download over its own path, so the preview never does.
#' @noRd
preview_gate <- function(target, limit = preview_bytes_limit(), store = NULL, ns = identity) {
  size <- format_bytes(target$bytes)
  body <- if (preview_in_cloud(target) && is_file_target(target)) {
    list(shiny::tags$p(
      "This file is stored in the cloud (", target$repository, "): downloading it writes it over ",
      shiny::tags$code(paste(utils::head(target$paths, 3), collapse = ", ")), " in the project, so the preview doesn't. ",
      shiny::tags$code(read_code(target$name, store)), " in the console downloads it there."
    ))
  } else if (preview_in_cloud(target)) {
    list(
      shiny::tags$p(paste0(
        "This value is stored in the cloud (", target$repository, ")", if (!is.null(size)) paste0(", ", size),
        ": reading it downloads it, which your provider may bill, and needs its R package and your credentials. ",
        "It goes to a temporary file, never into the project.",
        if (preview_too_large(target, limit)) paste0(" It is also more than the ", format_bytes(limit), " the preview reads on its own.")
      )),
      shiny::actionButton(ns("read"), "Download and preview", class = "btn-sm")
    )
  } else {
    list(
      shiny::tags$p(sprintf(
        "This value is %s in the store, more than the %s the preview reads on its own: reading it can take a while and use a lot of memory.",
        size, format_bytes(limit)
      )),
      shiny::actionButton(ns("read"), "Load the value", class = "btn-sm")
    )
  }
  bslib::card(
    class = "tb-preview",
    preview_header(target, store = store, ns = ns),
    do.call(bslib::card_body, c(list(class = "tb-preview-gate"), body))
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
  # clientData knows outputs by their full id, the module's namespace in it.
  full_id <- session$ns(id)
  shiny::renderImage({
    data <- session$clientData
    width <- data[[paste0("output_", full_id, "_width")]]
    height <- data[[paste0("output_", full_id, "_height")]]
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
