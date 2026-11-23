# Run one operation in a fresh R process, watched: its time, the peak memory
# of the process (sampled every 0.25 s), and a kill when it goes over a
# memory cap or a timeout.
#
# `fun` runs in the child with `args`, from the directory `dir`. It may
# return a list of extra measurements (e.g. sizes); the child adds its own
# wall time (`seconds`, excluding R start-up) and R's heap peak (`heap_mb`,
# from gc()'s "max used").

run_guarded <- function(fun, args = list(), dir = getwd(), cap_gb = 8, timeout = 1800,
                        label = deparse(substitute(fun))) {
  wrapper <- function(fun, args, dir) {
    setwd(dir)
    # Load what the operations use first: time is the operation's, not R's
    # loading of packages.
    for (pkg in c("targets", "targetsboard", "jsonlite", "skimr", "tarchetypes")) {
      if (requireNamespace(pkg, quietly = TRUE)) loadNamespace(pkg)
    }
    invisible(gc(reset = TRUE, full = TRUE))
    start <- Sys.time()
    extra <- do.call(fun, args)
    seconds <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    used <- gc()
    # "max used" in MB: the column next to "max used" counts, as gc() prints it.
    heap_mb <- sum(used[, ncol(used)])
    c(list(seconds = seconds, heap_mb = heap_mb), if (is.list(extra)) extra)
  }
  process <- callr::r_bg(wrapper, list(fun = fun, args = args, dir = dir), supervise = TRUE)
  pid <- process$get_pid()
  peak <- 0
  killed <- NULL
  start <- Sys.time()
  while (process$is_alive()) {
    # The process alone: the operations start no processes of their own (the
    # build runs with callr_function = NULL). On Windows, ps_children() spoils
    # the handle it is given and ps::ps() takes seconds, so neither is used.
    rss <- tryCatch(as.numeric(ps::ps_memory_info(ps::ps_handle(pid))[["rss"]]), error = function(e) 0)
    peak <- max(peak, rss)
    if (rss > cap_gb * 2^30) killed <- sprintf("over the %s GB memory cap", cap_gb)
    if (difftime(Sys.time(), start, units = "secs") > timeout) killed <- sprintf("over the %s s timeout", timeout)
    if (!is.null(killed)) {
      process$kill_tree()
      break
    }
    Sys.sleep(0.25)
  }
  result <- if (is.null(killed)) tryCatch(process$get_result(), error = function(e) list(error = conditionMessage(e))) else list()
  c(
    list(label = label, peak_rss_mb = round(peak / 2^20), killed = killed %||% NA_character_),
    result
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
