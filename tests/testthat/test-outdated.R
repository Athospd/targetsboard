# Why outdated targets are: outdated_reasons() on what the poller writes,
# here read from a real pipeline changed in each way.

# What the poller writes (tar_visnetwork_bg()), for the pipeline in the
# working directory.
poller_reasons <- function() {
  network <- targets::tar_network(targets_only = FALSE)
  meta <- targets::tar_meta(fields = "parent", targets_only = TRUE)
  list(
    sitrep = as.data.frame(targets::tar_sitrep()),
    vertices = as.data.frame(network$vertices[, c("name", "type", "status")]),
    edges = as.data.frame(network$edges),
    parents = as.data.frame(meta[!is.na(meta$parent), c("name", "parent")])
  )
}

test_that("each outdated target says why: its command, what changed, upstream, no record", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c(
    "library(targets)",
    "f <- function(x) x + 1",
    "list(tar_target(a, 1), tar_target(b, f(a)), tar_target(c, b * 2), tar_target(e, a - 1),",
    "  tar_target(p, a + 1:3, pattern = map(a)))"
  ), "_targets.R")
  targets::tar_make(reporter = "silent")
  expect_equal(outdated_reasons(poller_reasons()), list())

  # f changed: b depends on it; c only upstream of b; e's command changed;
  # d is new; p's command changed, on its branch.
  writeLines(c(
    "library(targets)",
    "f <- function(x) x + 2",
    "list(tar_target(a, 1), tar_target(b, f(a)), tar_target(c, b * 2), tar_target(e, a - 2),",
    "  tar_target(p, a + 2:4, pattern = map(a)), tar_target(d, 5))"
  ), "_targets.R")
  reasons <- outdated_reasons(poller_reasons())
  expect_setequal(names(reasons), c("b", "c", "d", "e", "p"))
  expect_equal(reasons$b, c(depend = "`f` changed."))
  expect_equal(reasons$c, c(upstream = "Upstream, `b` is outdated."))
  expect_equal(reasons$e, c(command = "Its command changed."))
  expect_match(reasons$d, "no record in the store", fixed = TRUE)
  expect_equal(reasons$p, c(command = "Its command changed."))
})

test_that("outdated_reasons() falls back to plain words, and handles no data", {
  expect_equal(outdated_reasons(NULL), list())
  reasons <- list(
    sitrep = data.frame(name = c("x", "y"), meta = FALSE, always = c(TRUE, FALSE), depend = c(FALSE, TRUE)),
    vertices = data.frame(name = c("x", "y", "z"), type = "stem", status = c("outdated", "outdated", "outdated")),
    edges = data.frame(from = character(), to = character()),
    parents = NULL
  )
  got <- outdated_reasons(reasons)
  expect_equal(got$x, c(always = "Its cue is \"always\": it runs every time."))
  expect_equal(got$y, c(depend = "Something it depends on changed.")) # what changed isn't outdated any more
  expect_equal(got$z, c(upstream = "Something upstream changed.")) # no row of its own
})

test_that("names are listed in words, and the reasons read as a tooltip and in the pane", {
  expect_equal(backticked("a"), "`a`")
  expect_equal(backticked(c("a", "b", "c")), "`a`, `b` and `c`")
  expect_equal(reasons_tooltip(c("`f` changed.", "Its command changed.")), "Outdated: f changed. Its command changed.")
  html <- as.character(reasons_ui(c(depend = "`f` and `g` changed.", upstream = "Upstream, `b` is outdated.")))
  expect_match(html, "Why it is outdated", fixed = TRUE)
  expect_match(html, "<span><code>f</code> and <code>g</code> changed.</span>", fixed = TRUE)
  expect_match(html, 'data-kind="depend"', fixed = TRUE)
  expect_match(html, "fa-arrow-turn-up", fixed = TRUE) # upstream's icon
  expect_match(html, "fa-pen", fixed = TRUE) # depend's
  expect_null(reasons_ui(NULL))
})
