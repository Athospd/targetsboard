# Previews of file targets. A file target's value is its paths: the board
# shows each file by its type. Building a file's view reads the file, and a
# target can have thousands, so the frame holds them as sections, and a
# section's view is built in the session only when it opens.

# The formats whose value is paths to files the target wrote.
file_formats <- c("file", "file_fast")

is_file_target <- function(target) isTRUE(target$format %in% file_formats)

#' What the board dispatches a value on: for a file target, its paths, of
#' class `tar_board_files`; else the value itself
#' @noRd
dispatch_value <- function(x, target) {
  if (is_file_target(target) && is.character(x) && length(x)) {
    return(structure(unclass(x), class = c("tar_board_files", "character")))
  }
  x
}

#' One path, with a class from its extension, as `tar_board_preview()` sees
#' it: `c("tar_board_file_csv", "tar_board_file", "character")`
#' @noRd
board_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  structure(path, class = c(if (nzchar(ext)) paste0("tar_board_file_", ext), "tar_board_file", "character"))
}

# Internal: the files of a target, as a view the frame shows as sections.
files_view <- function(paths) structure(list(paths = unclass(paths)), class = "tar_board_preview_files")

#' @rdname tar_board_preview
#' @export
tar_board_preview.tar_board_files <- function(x, target, ...) files_view(x)

# The files shown at once; "Show more" shows as many again.
files_page <- 50

#' The sections of a files view: one per path, 50 at a time, the first open.
#' A section holds an output for its view, which the session fills when it
#' opens; with a single path, the view alone.
#' @noRd
files_sections <- function(view) {
  paths <- view$paths
  if (length(paths) == 1) return(shiny::uiOutput(file_output_id(1)))
  # Opening a section adds its output, binds it, and asks for its view.
  ontoggle <- paste0(
    "if (this.open && !this.querySelector('.tb-preview-file-body')) {",
    " const d = document.createElement('div');",
    " d.id = 'value_preview_file_' + this.dataset.index;",
    " d.className = 'shiny-html-output tb-preview-file-body';",
    " this.appendChild(d); Shiny.bindAll(this);",
    " Shiny.setInputValue('value_preview_file_open', Number(this.dataset.index), {priority: 'event'}); }"
  )
  more <- paste0(
    "const list = this.previousElementSibling;",
    " list.querySelectorAll(':scope > details[hidden]').forEach((d, i) => { if (i < ", files_page, ") d.hidden = false; });",
    " if (!list.querySelector(':scope > details[hidden]')) this.remove();"
  )
  shiny::tagList(
    shiny::tags$p(class = "tb-table-caption", sprintf("%s files.", format(length(paths), big.mark = ","))),
    shiny::tags$div(
      class = "tb-preview-files",
      lapply(seq_along(paths), function(i) {
        shiny::tags$details(
          class = "tb-preview-file", `data-index` = i,
          open = if (i == 1) NA, hidden = if (i > files_page) NA,
          ontoggle = if (i > 1) ontoggle,
          shiny::tags$summary(shiny::tags$code(basename(paths[[i]]))),
          if (i == 1) shiny::uiOutput(file_output_id(1), class = "tb-preview-file-body")
        )
      })
    ),
    if (length(paths) > files_page) {
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary tb-preview-files-more",
        onclick = more, sprintf("Show %d more", files_page)
      )
    }
  )
}

file_output_id <- function(i) paste0("value_preview_file_", i)

#' A file's view, built in the session when its section opens: its path,
#' size and modified time, and what `tar_board_preview()` gives for its type
#' @noRd
file_section_view <- function(path, target) {
  if (!file.exists(path)) {
    return(shiny::tags$p(class = "tb-preview-note", sprintf("The file isn't at %s any more.", path)))
  }
  body <- tryCatch(
    tar_board_preview(board_file(path), target),
    error = function(e) {
      shiny::tags$p(class = "tb-preview-note", paste("The file's preview failed:", conditionMessage(e)))
    }
  )
  shiny::tagList(file_facts(path), body)
}

file_facts <- function(path) {
  info <- file.info(path)
  shiny::tags$div(
    class = "tb-file-facts",
    shiny::tags$code(path),
    if (!isTRUE(info$isdir)) shiny::tags$span(format_bytes(info$size)),
    shiny::tags$span(paste("modified", format(info$mtime, "%Y-%m-%d %H:%M")))
  )
}

image_types <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif",
                 svg = "image/svg+xml", webp = "image/webp")
served_types <- c(image_types, pdf = "application/pdf")

#' The URL of one file, for this session only: the board serves the file
#' itself, not its folder
#' @noRd
serve_file <- function(path, session = shiny::getDefaultReactiveDomain()) {
  type <- served_types[[tolower(tools::file_ext(path))]]
  if (is.null(session)) return(serve_html_report(path)) # outside a session: its folder
  session$registerDataObj(
    name = paste0("tb_file_", substr(digest::digest(path), 1, 12)),
    data = normalizePath(path, winslash = "/"),
    filterFunc = function(data, req) {
      shiny::httpResponse(200, type, readBin(data, "raw", file.size(data)))
    }
  )
}

#' A PDF, in the browser's PDF viewer
#'
#' Some browsers have none: RStudio's and Positron's viewers are Electron
#' views with Chromium's PDF viewer turned off. There, the board's preview.js
#' puts the PDF's first pages in the frame's place, drawn as PNG images by
#' pdftools: so the frame carries its page count and the URL of its pages.
#' Without pdftools, a link to open it in a browser takes its place.
#' @param path The PDF.
#' @noRd
pdf_view <- function(path) {
  session <- shiny::getDefaultReactiveDomain()
  pages <- if (!is.null(session) && has_pdftools()) {
    tryCatch(pdftools::pdf_info(path)$pages, error = function(e) NULL)
  }
  # No src: preview.js sets it where the browser can show it. Loaded where
  # it can't, a PDF could start a download.
  shiny::tags$iframe(
    class = "tb-preview-document tb-preview-pdf", title = basename(path), `data-pdf-src` = serve_file(path),
    `data-pdf-pages` = pages,
    `data-pdf-page-url` = if (!is.null(pages)) serve_pdf_pages(path, session)
  )
}

has_pdftools <- function() requireNamespace("pdftools", quietly = TRUE)

#' A URL for a PDF's pages as PNG images: add "&page=" and a page number
#' @noRd
serve_pdf_pages <- function(path, session) {
  session$registerDataObj(
    name = paste0("tb_pdf_", substr(digest::digest(path), 1, 12)),
    data = normalizePath(path, winslash = "/"),
    filterFunc = function(data, req) {
      query <- shiny::parseQueryString(req$QUERY_STRING %||% "")
      page <- suppressWarnings(as.integer(query$page))
      if (length(page) != 1 || is.na(page) || page < 1) page <- 1L
      shiny::httpResponse(200, "image/png", pdf_page_png(data, page))
    }
  )
}

#' A PDF's page as PNG bytes, drawn by pdftools; past its last page, the last
#' @noRd
pdf_page_png <- function(path, page) {
  page <- min(page, pdftools::pdf_info(path)$pages)
  dir <- tempfile("tb_pdf_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # A single file name is a sprintf() format, of the page and the format.
  file <- pdftools::pdf_convert(
    path, format = "png", pages = page, dpi = 110, verbose = FALSE,
    filenames = file.path(gsub("%", "%%", dir, fixed = TRUE), "page_%d.%s")
  )
  readBin(file, "raw", file.size(file))
}

# The first lines of a text file, and whether there are more.
first_lines <- function(path, n) {
  lines <- readLines(path, n = n + 1, warn = FALSE, encoding = "UTF-8")
  list(lines = utils::head(lines, n), more = length(lines) > n)
}

text_lines <- 200
csv_rows <- 100

text_file_view <- function(path, language = NULL) {
  text <- first_lines(path, text_lines)
  shiny::tagList(
    if (text$more) shiny::tags$p(class = "tb-table-caption", sprintf("The first %d lines.", text_lines)),
    # Text alone in <pre>: a nested tag would indent the first line.
    shiny::tags$pre(class = paste(c("tb-preview-text", if (!is.null(language)) paste0("language-", language)), collapse = " "),
                    paste(text$lines, collapse = "\n"))
  )
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.tar_board_file <- function(x, target, ...) {
  path <- unclass(x)
  ext <- tolower(tools::file_ext(path))
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE)
    return(shiny::tags$p(sprintf("A folder of %s files.", format(length(files), big.mark = ","))))
  }
  if (ext %in% names(image_types)) {
    url <- serve_file(path)
    return(shiny::tags$a(
      href = url, target = "_blank", title = "Open at full size",
      shiny::tags$img(class = "tb-preview-image", src = url, alt = basename(path))
    ))
  }
  if (ext == "pdf") return(pdf_view(path))
  if (ext %in% c("html", "htm")) {
    return(shiny::tags$iframe(class = "tb-preview-document", src = serve_html_report(path), title = basename(path)))
  }
  if (ext %in% c("csv", "tsv")) {
    read <- if (ext == "csv") utils::read.csv else utils::read.delim
    data <- read(path, nrows = csv_rows, check.names = FALSE, stringsAsFactors = FALSE)
    return(shiny::tagList(
      if (nrow(data) == csv_rows) shiny::tags$p(class = "tb-table-caption", sprintf("The first %d rows.", csv_rows)),
      rows_view(data)
    ))
  }
  if (ext %in% c("md", "markdown") && file.size(path) < 1e6) {
    return(shiny::tags$div(class = "tb-preview-markdown", shiny::markdown(readLines(path, warn = FALSE, encoding = "UTF-8"))))
  }
  if (ext %in% c("r", "rmd", "qmd")) return(text_file_view(path, "r"))
  if (ext %in% c("txt", "log", "json", "yaml", "yml", "md", "markdown", "toml", "sql", "py", "sh")) {
    return(text_file_view(path))
  }
  NULL
}
