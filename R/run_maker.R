#' Which targets function runs the pipeline (#162)
#'
#' `targets::tar_make()`, unless the script sets up future
#' (`future::plan()`) or clustermq (`options(clustermq.scheduler = )`): then
#' `tar_make_future()` or `tar_make_clustermq()`, which targets keeps for
#' projects set up for them (crew is its newer way). Read from the script's
#' calls, never by running it, and not inside the functions it defines.
#'
#' Their `workers`: `options(targetsboard.workers = )`, else a literal
#' `workers =` in the `plan()` call, else 1.
#' @param script Path to the targets script.
#' @return `list(fun, workers)`: the function's name, and its workers (NULL
#'   for `tar_make()`).
#' @noRd
run_maker <- function(script) {
  exprs <- tryCatch(parse(script, keep.source = FALSE), error = function(e) NULL, warning = function(w) NULL)
  found <- NULL
  workers <- NULL
  walk <- function(x) {
    if (!is.call(x)) return()
    head <- x[[1]]
    if (identical(head, as.name("function"))) return() # a function's body isn't the script's
    name <- if (is.name(head)) {
      as.character(head)
    } else if (is.call(head) && as.character(head[[1]]) %in% c("::", ":::")) {
      as.character(head[[3]])
    }
    if (identical(name, "plan") && is.null(found)) {
      found <<- "tar_make_future"
      w <- x$workers
      if (is.numeric(w) && length(w) == 1 && !is.na(w)) workers <<- as.integer(w)
    }
    if (identical(name, "options") && "clustermq.scheduler" %in% names(x) && is.null(found)) {
      found <<- "tar_make_clustermq"
    }
    for (arg in as.list(x)[-1]) walk(arg)
  }
  for (e in exprs) walk(e)
  if (is.null(found)) return(list(fun = "tar_make", workers = NULL))
  option <- getOption("targetsboard.workers")
  if (!is.null(option)) workers <- as.integer(option)
  list(fun = found, workers = workers %||% 1L)
}

# "tar_make_future(), 2 workers", or NULL for tar_make().
run_maker_label <- function(maker) {
  if (is.null(maker) || identical(maker$fun, "tar_make")) return(NULL)
  paste0(maker$fun, "(), ", maker$workers, if (maker$workers == 1) " worker" else " workers")
}
