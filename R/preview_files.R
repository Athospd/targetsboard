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
#' opens; with a single path, the view alone. A rendered report (#275)
#' shows at once, its sources and inputs in one closed list below it.
#' @noRd
files_sections <- function(view, ns = identity) {
  paths <- view$paths
  if (length(paths) == 1) return(shiny::uiOutput(ns(file_output_id(1))))
  labels <- file_labels(paths)
  if (rendered_report(paths)) {
    return(shiny::tagList(
      shiny::uiOutput(ns(file_output_id(1))),
      shiny::tags$details(
        class = "tb-preview-sources",
        shiny::tags$summary(sprintf("Sources and inputs (%s)", format(length(paths) - 1, big.mark = ","))),
        file_section_list(paths, labels, ns, which = seq_along(paths)[-1], first_open = FALSE)
      )
    ))
  }
  shiny::tagList(
    shiny::tags$p(class = "tb-table-caption", sprintf("%s files.", format(length(paths), big.mark = ","))),
    file_section_list(paths, labels, ns, which = seq_along(paths), first_open = TRUE)
  )
}

# The sections of the paths `which`, 50 shown at a time. The first open
# already holds its output; the others ask for theirs when they open
# (srcjs/actions.js).
file_section_list <- function(paths, labels, ns, which, first_open) {
  shiny::tagList(
    shiny::tags$div(
      class = "tb-preview-files",
      lapply(seq_along(which), function(k) {
        i <- which[[k]]
        built <- first_open && k == 1
        shiny::tags$details(
          class = "tb-preview-file", `data-index` = i,
          open = if (built) NA, hidden = if (k > files_page) NA,
          `data-tb-input` = if (!built) ns("file_open"),
          `data-tb-value` = if (!built) i,
          `data-tb-output` = if (!built) ns(file_output_id(i)),
          shiny::tags$summary(shiny::tags$code(labels[[i]])),
          if (built) shiny::uiOutput(ns(file_output_id(i)), class = "tb-preview-file-body")
        )
      })
    ),
    if (length(which) > files_page) {
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-default tb-preview-files-more",
        `data-tb-more` = files_page, sprintf("Show %d more", files_page)
      )
    }
  )
}

#' Each path's label: its path in the folder the paths share, so that
#' posts/a/index.qmd and posts/b/index.qmd tell apart (#275)
#' @noRd
file_labels <- function(paths) {
  parts <- strsplit(gsub("\\\\", "/", paths), "/", fixed = TRUE)
  folders <- lapply(parts, function(p) utils::head(p, -1))
  shared <- 0
  repeat {
    k <- shared + 1
    heads <- vapply(folders, function(f) if (length(f) >= k) f[[k]] else NA_character_, character(1))
    if (anyNA(heads) || length(unique(heads)) != 1) break
    shared <- k
  }
  vapply(parts, function(p) paste(p[seq_along(p) > shared], collapse = "/"), character(1))
}

#' Whether a file target's first path is a rendered report, as
#' tar_quarto() and tar_render() return them: a folder with an index.html
#' (a site), or an HTML, PDF or Word document with a .qmd or .Rmd among the
#' paths
#' @noRd
rendered_report <- function(paths) {
  if (length(paths) < 2) return(FALSE)
  first <- paths[[1]]
  if (dir.exists(first)) return(file.exists(file.path(first, "index.html")))
  tolower(tools::file_ext(first)) %in% c("html", "htm", "pdf", "docx") &&
    any(tolower(tools::file_ext(paths[-1])) %in% c("qmd", "rmd"))
}

# A file section's output, in the preview module's namespace.
file_output_id <- function(i) paste0("file_", i)

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
  # A table's size and time are in the frame's header already: its shape
  # isn't.
  facts <- if (tolower(tools::file_ext(path)) %in% c("csv", "tsv") && !isTRUE(info$isdir)) {
    list(shiny::tags$span(table_file_shape(path)))
  } else {
    list(
      if (!isTRUE(info$isdir)) shiny::tags$span(format_bytes(info$size)),
      shiny::tags$span(paste("modified", format(info$mtime, "%Y-%m-%d %H:%M")))
    )
  }
  shiny::tags$div(class = "tb-file-facts", shiny::tags$code(path), facts)
}

#' A CSV or TSV file's rows and columns, as "1,500 rows x 3 columns"
#'
#' Rows are its lines after the header, counted in chunks without reading the
#' table; a quoted field that spans lines counts as more than one.
#' @param path The file.
#' @noRd
table_file_shape <- function(path) {
  sep <- if (tolower(tools::file_ext(path)) == "tsv") "	" else ","
  header <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  columns <- if (length(header)) length(utils::read.table(text = header, sep = sep, quote = "\"", comment.char = "")) else 0
  con <- file(path, "rb")
  on.exit(close(con))
  lines <- 0
  last <- as.raw(10)
  repeat {
    chunk <- readBin(con, "raw", 1048576)
    if (!length(chunk)) break
    lines <- lines + sum(chunk == as.raw(10))
    last <- chunk[length(chunk)]
  }
  if (last != as.raw(10)) lines <- lines + 1 # no line break at the end
  rows <- max(lines - 1, 0)
  sprintf(
    "%s %s x %s %s",
    format(rows, big.mark = ","), if (rows == 1) "row" else "rows",
    format(columns, big.mark = ","), if (columns == 1) "column" else "columns"
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
    name = paste0("tb_file_", substr(rlang::hash(path), 1, 12)),
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
    name = paste0("tb_pdf_", substr(rlang::hash(path), 1, 12)),
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

#' Markdown, rendered, with its R code blocks coloured
#'
#' highlight.js colours the board's `<pre class="language-r">`; Markdown puts
#' a block's language on its `<code>`, so it moves to the `<pre>`.
#' @param lines The Markdown's lines.
#' @noRd
markdown_view <- function(lines) {
  html <- as.character(shiny::markdown(lines))
  html <- gsub('<pre><code class="language-r">', '<pre class="language-r"><code>', html, fixed = TRUE)
  shiny::tags$div(class = "tb-preview-markdown", shiny::HTML(html))
}

#' A Quarto or R Markdown document: what it was rendered to, or its source
#'
#' The rendered document is what the target made, with its chunks' output:
#' the board doesn't render it again. It's the file next to the source with
#' its name, HTML or else PDF, or, in a Quarto project, the one in the
#' project's output folder (`_site/`, `_book/`). tar_quarto() and
#' tar_render() return it too, first: it has its own section then, and the
#' source's shows the source.
#' @param path The source.
#' @param target The target, with `files`, its paths, in a file section.
#' @noRd
document_view <- function(path, target) {
  stem <- tools::file_path_sans_ext(path)
  candidates <- c(paste0(stem, c(".html", ".pdf")), project_output(path))
  rendered <- candidates[file.exists(candidates)][1]
  note <- function(...) shiny::tags$p(class = "tb-table-caption", ...)
  same_file <- function(a, b) normalizePath(a, winslash = "/", mustWork = FALSE) %in% normalizePath(b, winslash = "/", mustWork = FALSE)
  if (!is.na(rendered) && !same_file(rendered, target$files %||% character())) {
    older <- file.mtime(rendered) < file.mtime(path)
    return(shiny::tagList(
      note(sprintf("Rendered as %s%s.", names(rendered) %||% basename(rendered), if (older) ", older than its source" else "")),
      tar_board_preview(board_file(rendered), target)
    ))
  }
  source <- if (file.size(path) < 1e6) {
    markdown_view(document_markdown(readLines(path, warn = FALSE, encoding = "UTF-8")))
  } else {
    text_file_view(path, "r")
  }
  shiny::tagList(
    if (!is.na(rendered)) {
      note(sprintf("Its source: %s has its own section.", basename(rendered)))
    } else {
      note("Not rendered: its source, with its chunks as code.")
    },
    source
  )
}

#' Where a Quarto project rendered a source to
#'
#' The nearest folder above the source, at most five up, with a
#' `_quarto.yml` is the project; its output folder is `output-dir`, or
#' `_site` for a website and `_book` for a book, else the project's folder.
#' The page keeps the source's place under it: `posts/first.qmd` renders to
#' `_site/posts/first.html`.
#' @param path The source.
#' @return The HTML and PDF it may have rendered to, each named by its path
#'   in the project (`_site/posts/first.html`); none outside a project.
#' @noRd
project_output <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  here <- dirname(path)
  for (i in 1:5) {
    config <- file.path(here, "_quarto.yml")
    if (file.exists(config)) {
      out <- quarto_output_dir(readLines(config, warn = FALSE))
      page <- paste0(substring(tools::file_path_sans_ext(path), nchar(here) + 2), c(".html", ".pdf"))
      shown <- if (out == ".") page else file.path(out, page)
      return(stats::setNames(file.path(here, shown), shown))
    }
    up <- dirname(here)
    if (identical(up, here)) break
    here <- up
  }
  character()
}

#' A Quarto project's output folder, from its `_quarto.yml`'s lines
#'
#' Read by pattern, for the two fields it needs (`output-dir` and the
#' project's `type`), so the board needs no YAML parser.
#' @noRd
quarto_output_dir <- function(lines) {
  field <- function(name) {
    line <- grep(sprintf("^[[:space:]]+%s:", name), lines, value = TRUE)[1]
    if (is.na(line)) return(NA_character_)
    trimws(gsub("[\"']", "", sub(sprintf("^[[:space:]]+%s:", name), "", line)))
  }
  out <- field("output-dir")
  if (!is.na(out) && nzchar(out)) return(sub("/+$", "", out))
  type <- field("type")
  if (identical(type, "website")) "_site" else if (identical(type, "book")) "_book" else "."
}

#' A Quarto or R Markdown document's lines as plain Markdown
#'
#' As the rendered document reads, without running it: the front matter
#' gives its title as a heading and is otherwise left out, and each chunk
#' (```` ```{r label, echo=FALSE} ````) becomes a code block in its
#' language. Chunks show their code, not their output.
#' @param lines The document's lines.
#' @noRd
document_markdown <- function(lines) {
  title <- NULL
  if (length(lines) > 1 && grepl("^---\\s*$", lines[[1]])) {
    end <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines[-1]))[1] + 1
    if (!is.na(end)) {
      front <- lines[seq_len(end - 2) + 1]
      field <- grep("^title:", front, value = TRUE)[1]
      if (!is.na(field)) {
        title <- trimws(sub("^title:", "", field))
        title <- sub("^([\"'])(.*)\\1$", "\\2", title)
      }
      lines <- lines[-seq_len(end)]
    }
  }
  lines <- sub("^(\\s*`{3,})\\s*\\{([A-Za-z0-9_]+)[^}]*\\}\\s*$", "\\1\\2", lines)
  if (!is.null(title) && nzchar(title)) lines <- c(paste("#", title), "", lines)
  lines
}

#' @rdname tar_board_preview
#' @export
tar_board_preview.tar_board_file <- function(x, target, ...) {
  path <- unclass(x)
  ext <- tolower(tools::file_ext(path))
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE)
    count <- sprintf("A folder of %s files", format(length(files), big.mark = ","))
    # A rendered website or book (tar_quarto() on a project returns its
    # output folder): its front page, with its links.
    index <- file.path(path, "index.html")
    if (file.exists(index)) {
      return(shiny::tagList(shiny::tags$p(class = "tb-table-caption", paste0(count, ": a site, from its index.html.")), html_view(index)))
    }
    return(shiny::tags$p(paste0(count, ".")))
  }
  if (ext %in% names(image_types)) {
    url <- serve_file(path)
    return(shiny::tags$a(
      href = url, target = "_blank", title = "Open at full size",
      shiny::tags$img(class = "tb-preview-image", src = url, alt = basename(path))
    ))
  }
  if (ext == "pdf") return(pdf_view(path))
  if (ext %in% c("html", "htm")) return(html_view(path))
  if (ext %in% c("csv", "tsv")) {
    read <- if (ext == "csv") utils::read.csv else utils::read.delim
    data <- read(path, nrows = csv_rows, check.names = FALSE, stringsAsFactors = FALSE)
    return(shiny::tagList(
      if (nrow(data) == csv_rows) shiny::tags$p(class = "tb-table-caption", sprintf("The first %d rows.", csv_rows)),
      rows_view(data)
    ))
  }
  if (ext %in% c("qmd", "rmd")) return(document_view(path, target))
  if (ext %in% c("md", "markdown") && file.size(path) < 1e6) {
    return(markdown_view(readLines(path, warn = FALSE, encoding = "UTF-8")))
  }
  if (ext %in% c("r", "rmd", "qmd")) return(text_file_view(path, "r"))
  if (ext %in% c("json", "geojson", "ndjson", "jsonl")) return(json_view(path, lines = ext %in% c("ndjson", "jsonl")))
  if (ext %in% c("txt", "log", "yaml", "yml", "md", "markdown", "toml", "sql", "py", "sh")) {
    return(text_file_view(path))
  }
  NULL
}

# JSON as a tree (#250): how big a file it parses, how many lines of NDJSON,
# and how many nodes it draws in all, whatever the shape.
json_bytes <- function() 5e6
json_lines <- 100
json_depth <- 8
json_nodes <- 2000

#' What a JSON value is, in a few characters: `{3}` an object, `[3]` an
#' array, string, number, bool, null; from jsonlite's lists
#' (`simplifyVector = FALSE`), where an object has names, even none
#' @noRd
json_type_label <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) return(sprintf(if (is.null(names(x))) "[%s]" else "{%s}", format(length(x), big.mark = ",")))
  if (is.logical(x)) return("bool")
  if (is.numeric(x)) return("number")
  if (is.character(x)) return("string")
  type_label(x)
}

#' A JSON file as a tree you can fold, its text a click away
#'
#' A file too big to parse, or not valid JSON, keeps the text view, with
#' why. NDJSON (`lines = TRUE`): one item a line, the first `json_lines`.
#' @noRd
json_view <- function(path, lines = FALSE) {
  text_view <- function(note) shiny::tagList(shiny::tags$p(class = "tb-table-caption", note), text_file_view(path))
  if (isTRUE(file.size(path) > json_bytes())) {
    return(text_view("Too large to show as a tree: its text."))
  }
  value <- tryCatch({
    if (lines) {
      text <- first_lines(path, json_lines)
      lapply(text$lines[nzchar(trimws(text$lines))], jsonlite::parse_json, simplifyVector = FALSE)
    } else {
      jsonlite::read_json(path, simplifyVector = FALSE)
    }
  }, error = function(e) structure(list(), error = conditionMessage(e)))
  if (!is.null(attr(value, "error"))) {
    why <- gsub("[[:space:]]+", " ", cli::ansi_strip(attr(value, "error")))
    return(text_view(paste("Not valid JSON:", substr(why, 1, 200))))
  }
  # JSON spells its bools true and false.
  shown <- function(x) if (is.logical(x) && length(x) == 1 && !is.na(x)) tolower(x) else short_value(x)
  options <- tree_options(json_type_label, json_depth, budget = list2env(list(left = json_nodes)), value = shown)
  shiny::tagList(
    if (lines && isTRUE(first_lines(path, json_lines)$more)) {
      shiny::tags$p(class = "tb-table-caption", sprintf("The first %d lines.", json_lines))
    },
    if (is.list(value) && length(value)) {
      shiny::tags$div(class = "tb-tree", tree_items_ui(value, 1, options))
    } else {
      shiny::tags$div(class = "tb-tree", tree_node("value", value, 1, options))
    },
    shiny::tags$details(class = "tb-preview-raw", shiny::tags$summary("Raw text"), text_file_view(path))
  )
}
