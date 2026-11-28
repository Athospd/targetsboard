#' tar_map() families, read from the script (#113)
#'
#' targets records no family: `tar_map()` names each copy of a target
#' `<template>_<values>`, so a family is the targets named after a template
#' that a `tar_map()` call in the script holds. Read from the script's
#' text, never by running it, like cue_rules(): a `tar_map()` in a function
#' in `R/` isn't seen.
#' @param script Path to the targets script.
#' @return `list(templates, others)`: the target names inside `tar_map*()`
#'   calls, and those outside, which are never a family's members.
#' @noRd
tar_map_templates <- function(script) {
  found <- list(templates = character(), others = character())
  if (is.null(script) || !file.exists(script)) return(found)
  exprs <- tryCatch(parse(script, keep.source = FALSE), error = function(e) expression())
  fname <- function(x) {
    fn <- x[[1]]
    if (is.name(fn)) return(as.character(fn))
    if (is.call(fn) && length(fn) == 3 && is.name(fn[[1]]) && as.character(fn[[1]]) %in% c("::", ":::")) return(as.character(fn[[3]]))
    ""
  }
  # A target's name: tar_*(name, ...), tar_*(name = ...) or tar_target_raw("name", ...).
  target_name <- function(x) {
    args <- drop_empty(as.list(x)[-1])
    named <- names(args) %||% rep("", length(args))
    name <- if ("name" %in% named) args[["name"]] else if (any(!nzchar(named))) args[!nzchar(named)][[1]]
    if (is.name(name)) as.character(name) else if (is.character(name) && length(name) == 1) name
  }
  visit <- function(x, mapped) {
    if (!is.call(x)) return(invisible())
    fn <- fname(x)
    in_map <- mapped || startsWith(fn, "tar_map")
    if (startsWith(fn, "tar_") && !startsWith(fn, "tar_map") && !fn %in% c("tar_option_set", "tar_source", "tar_plan")) {
      name <- target_name(x)
      if (!is.null(name)) {
        part <- if (in_map) "templates" else "others"
        found[[part]] <<- union(found[[part]], name)
      }
    }
    for (part in as.list(x)[-1]) if (!missing(part)) visit(part, in_map)
  }
  for (expr in exprs) visit(expr, FALSE)
  found
}

#' Each target's family: the template it is a copy of, or NA
#'
#' The longest template whose name and `_` start the target's; a family of
#' one isn't one.
#' @param ids The graph's target ids.
#' @param found tar_map_templates()'s.
#' @noRd
map_families <- function(ids, found) {
  family <- rep(NA_character_, length(ids))
  templates <- found$templates[order(-nchar(found$templates))]
  copies <- !ids %in% c(found$templates, found$others)
  for (template in templates) {
    hit <- is.na(family) & copies & startsWith(ids, paste0(template, "_"))
    family[hit] <- template
  }
  sizes <- table(family)
  family[family %in% names(sizes)[sizes < 2]] <- NA_character_
  family
}

# The nodes of a graph, with their family.
add_families <- function(nodes, script) {
  if (!is.null(nodes) && nrow(nodes)) nodes$family <- map_families(nodes$id, tar_map_templates(script))
  nodes
}
