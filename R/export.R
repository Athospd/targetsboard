#' Export the board as a standalone HTML page
#'
#' Writes a snapshot of the pipeline that anyone can open in a browser, with
#' no R and no Shiny: the graph with its statuses as they are now, and, for
#' each target, its Value pane (facts, views, printed value, command, and
#' error). A click on a target shows its pane. For sharing results with
#' someone who doesn't run R, attaching to a pull request or a CI run, or
#' keeping what a run produced.
#'
#' The page holds *data*: the values of the targets it includes. It says
#' which when it writes them; `names` picks the ones to share. Values stored
#' in the cloud (`repository = "aws"` or `"gcp"`) aren't included: reading
#' them costs, and needs credentials. Neither are values over `max_bytes`,
#' nor values whose store runs custom code to read them (`tar_format()`).
#' A pattern shows its first branch. A file target lists its files.
#'
#' @param file The page to write.
#' @param names The targets whose values to include, with tidyselect as in
#'   [targets::tar_make()]: e.g. `c(data, starts_with("model_"))`. `NULL`,
#'   the default, includes them all. The graph shows every target.
#' @param script,store The targets script and store, targets' defaults.
#' @param max_bytes A target whose value is larger in the store is listed
#'   with its facts only.
#' @param self_contained `TRUE`: one file. `FALSE`: the page and a folder
#'   of its scripts and styles beside it, `<file>_files`.
#' @param callr_function How to read the values: `callr::r`, the default,
#'   in a new R process, as [targets::tar_make()] does; `NULL` in this
#'   session. The board must be installed for `callr::r`.
#' @return `file`, invisibly.
#' @examples
#' \dontrun{
#' tar_board_export("board.html")
#' tar_board_export("results.html", names = starts_with("summary_"))
#' }
#' @export
tar_board_export <- function(file = "board.html", names = NULL,
                             script = targets::tar_config_get("script"),
                             store = targets::tar_config_get("store"),
                             max_bytes = 10e6, self_contained = TRUE,
                             callr_function = callr::r) {
  manifest <- targets::tar_manifest(names = !!rlang::enquo(names), script = script, fields = "name")
  included <- manifest$name
  message(
    file, " holds the values of ", length(included), " target", if (length(included) != 1) "s",
    if (length(included)) paste0(": ", and_list(included)), ". Share it as you would that data."
  )
  args <- list(file = file, included = included, script = script, store = store,
               max_bytes = max_bytes, self_contained = self_contained)
  if (is.null(callr_function)) {
    do.call(write_export, args)
  } else {
    callr_function(function(...) targetsboard:::write_export(...), args = args, show = TRUE)
  }
  invisible(file)
}

#' Builds and writes the page, in the process that reads the values
#' @noRd
write_export <- function(file, included, script, store, max_bytes, self_contained) {
  graph <- initial_metadata(script)
  commands <- target_commands(targets::tar_manifest(script = script, fields = c("name", "command")))
  panels <- lapply(included, function(name) export_panel(name, store, max_bytes, command = commands[name]))
  page <- export_page(graph, panels, store)
  libdir <- paste0(tools::file_path_sans_ext(basename(file)), "_files")
  if (!self_contained) return(htmltools::save_html(page, file, libdir = libdir))
  # Its scripts and styles inline: one file.
  rendered <- htmltools::renderTags(page)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  html <- c(
    "<!DOCTYPE html>", "<html>", "<head>", '<meta charset="utf-8"/>',
    vapply(deps, inline_dependency, character(1)),
    rendered$head, "</head>", "<body>", rendered$html, "</body>", "</html>"
  )
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(file)
}

# A dependency's scripts and styles, as tags in the page.
inline_dependency <- function(dep) {
  dir <- if (!is.null(dep$package)) system.file(dep$src$file, package = dep$package) else dep$src$file
  read <- function(path) paste(readLines(file.path(dir, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  scripts <- vapply(unlist(lapply(dep$script, function(s) if (is.list(s)) s$src else s)), function(path) {
    # A script that writes "</script>" would end its tag early.
    paste0("<script>", gsub("</script", "<\\\\/script", read(path), fixed = TRUE), "</script>")
  }, character(1))
  styles <- vapply(unlist(dep$stylesheet), function(path) {
    paste0("<style>", inline_urls(read(path), dirname(file.path(dir, path))), "</style>")
  }, character(1))
  paste(c(dep$head, styles, scripts), collapse = "\n")
}

# A stylesheet's relative url()s, such as its fonts, as data URIs.
inline_urls <- function(css, dir) {
  # woff2 only of the fonts: every browser that runs the page reads it. Not
  # the stylesheets it imports: Bootstrap's font.css is 8 MB of web fonts
  # the board's theme doesn't use.
  types <- c(woff2 = "font/woff2", svg = "image/svg+xml", png = "image/png", gif = "image/gif")
  pattern <- "url\\((['\"]?)([^)'\"]+)\\1\\)"
  found <- gregexpr(pattern, css, perl = TRUE)
  regmatches(css, found) <- list(vapply(regmatches(css, found)[[1]], function(match) {
    path <- sub(pattern, "\\2", match, perl = TRUE)
    local <- file.path(dir, sub("[?#].*$", "", path))
    type <- types[tolower(tools::file_ext(local))]
    if (grepl("^(data:|https?:|#)", path) || is.na(type) || !file.exists(local)) return(match)
    content <- readBin(local, "raw", file.size(local))
    paste0("url(", data_uri(content, type), ")")
  }, character(1)))
  css
}

#' The page: a header, the graph, and the panels, one shown at a time
#' @noRd
export_page <- function(graph, panels, store) {
  bslib::page_fillable(
    theme = app_theme("Targets board"),
    title = "targetsboard snapshot",
    shiny::tags$style(htmltools::HTML(paste(readLines(app_sys("app/www/index.css"), warn = FALSE), collapse = "\n"))),
    shiny::tags$p(
      class = "text-muted small mb-0",
      sprintf("Snapshot of %s, %s. Click a target to see its value.", store, format(Sys.time(), "%Y-%m-%d %H:%M"))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5), fill = TRUE,
      # No actions: nothing runs from the page.
      bslib::card(full_screen = TRUE, targetsboard("export", graph$nodes, graph$edges, options = targetsboard_options(download_image = TRUE))),
      shiny::tags$div(
        class = "tb-export-panels overflow-auto",
        shiny::tags$p(class = "tb-export-none text-muted", "Click a target to see its value."),
        panels
      )
    ),
    # The board's Copy buttons; their Shiny parts wait for a session that never comes.
    shiny::tags$script(htmltools::HTML(paste(readLines(app_sys("app/www/actions.js"), warn = FALSE), collapse = "
"))),
    shiny::tags$script(htmltools::HTML(
      "window.addEventListener('targetsboard:focus', function (e) {",
      "  var node = e.detail.node, shown = false;",
      "  document.querySelectorAll('[data-tb-export-target]').forEach(function (p) {",
      "    var on = p.getAttribute('data-tb-export-target') === node; p.hidden = !on; shown = shown || on;",
      "  });",
      "  document.querySelector('.tb-export-none').hidden = shown;",
      "  window.dispatchEvent(new Event('resize'));",
      "});"
    ))
  )
}

#' One target's panel: its Value pane, or its facts and why its value
#' isn't included
#' @noRd
export_panel <- function(name, store, max_bytes, command = NULL) {
  panel <- function(...) shiny::tags$div(`data-tb-export-target` = name, hidden = NA, ...)
  target <- preview_target(name, store)
  note <- function(text) shiny::tags$p(class = "tb-preview-note", text)
  if (is.null(target)) return(panel(shiny::tags$h5(name), note("Not built.")))
  facts_only <- function(text) panel(bslib::card(class = "tb-preview", preview_header(target, store = store), bslib::card_body(note(text))))
  meta <- tryCatch(targets::tar_meta(fields = "error", store = store), error = function(e) NULL)
  error <- if (!is.null(meta) && "error" %in% names(meta)) meta$error[meta$name == name] else NA
  if (length(error) && !is.na(error[1])) return(facts_only(paste("It errored:", cli::ansi_strip(error[1]))))
  if (preview_in_cloud(target)) return(facts_only("Stored in the cloud: not included."))
  if (isTRUE(target$custom_code)) return(facts_only("Its store runs custom code to read it: not included."))
  if (!is.null(target$bytes) && isTRUE(target$bytes > max_bytes)) {
    return(facts_only(sprintf("Not included: %s is over the export's %s.", format_bytes(target$bytes), format_bytes(max_bytes))))
  }
  value <- tryCatch(read_preview_value(target, store), error = function(e) e)
  if (inherits(value, "error")) return(facts_only(paste("Its value couldn't be read:", conditionMessage(value))))
  built <- build_preview(value, target)
  built$views <- static_views(built$views)
  # Its own ids in the page; Command filled here, not by a session.
  ns <- shiny::NS(paste0("export-", gsub("[^A-Za-z0-9_]", "_", name)))
  command <- unname(command)
  own_tabs <- list(bslib::nav_panel("Command", shiny::tags$div(
    class = "tb-preview-view",
    if (length(command) && !is.na(command)) code_block("Its command", "command", command) else note("Its command isn't known.")
  )))
  panel(
    if (!is.null(target$pattern)) note(sprintf("The first branch of %d.", target$pattern$branches)),
    bs5_tabs(preview_frame(target, built, store = store, ns = ns, own_tabs = own_tabs))
  )
}

# Tabs as Bootstrap 5 marks them: Shiny writes Bootstrap 3's, which its
# session converts in the board. bslib's card hides them from tagQuery(),
# so in its HTML, on the tabs' own links (href="#tab-...") only.
bs5_tabs <- function(tag) {
  html <- as.character(tag)
  html <- gsub('<li class="active">([[:space:]]*)<a href="#tab-', '<li class="nav-item">\\1<a class="nav-link active" href="#tab-', html)
  html <- gsub('<li>([[:space:]]*)<a href="#tab-', '<li class="nav-item">\\1<a class="nav-link" href="#tab-', html)
  htmltools::attachDependencies(htmltools::HTML(html), htmltools::findDependencies(tag))
}

#' Views that need the board's session, made static: a plot drawn to an
#' image in the page; a file target's files listed
#' @noRd
static_views <- function(views) {
  lapply(views, function(view) {
    if (inherits(view, "tar_board_preview_plot")) {
      file <- tempfile(fileext = ".png")
      on.exit(unlink(file))
      draw_png(view, file, 640, 416)
      src <- data_uri(readBin(file, "raw", file.size(file)), "image/png")
      return(shiny::tags$img(src = src, alt = "A plot of the value", style = "width: 100%"))
    }
    if (inherits(view, "tar_board_preview_files")) {
      return(shiny::tags$ul(class = "tb-export-files", lapply(view$paths, function(p) shiny::tags$li(shiny::tags$code(p)))))
    }
    view
  })
}

# Bytes as a data URI. base64_enc() wraps its lines: a URI has none.
data_uri <- function(bytes, type) {
  paste0("data:", type, ";base64,", gsub("[[:space:]]", "", jsonlite::base64_enc(bytes)))
}
