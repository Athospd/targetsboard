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

test_that("an up-to-date target says why a run skips it, and names its cue", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(a, 1),",
    "  tar_target(b, a + 1, cue = tar_cue(mode = 'never')),",
    "  tar_target(c, b * 2, cue = tar_cue(command = FALSE, depend = FALSE)),",
    "  tar_target(d, c + 1),",
    "  tar_target(p, a + 1:3, pattern = map(a))",
    ")"
  ), "_targets.R")
  targets::tar_make(reporter = "silent")
  reasons <- poller_reasons()
  reasons$manifest <- as.data.frame(targets::tar_manifest(fields = c(
    "name", "format", "command", "cue_mode", "cue_command", "cue_depend",
    "cue_file", "cue_format", "cue_repository", "cue_iteration"
  )))
  skipped <- skipped_reasons(reasons)
  expect_setequal(names(skipped), c("a", "b", "c", "d", "p"))
  expect_match(skipped$a, "^Up to date: its command, the functions, objects and targets it uses, and its settings haven't changed since its last build")
  expect_equal(skipped$b, "Its cue is \"never\": a run skips it, even when something it uses changes.")
  expect_equal(skipped$c, c(
    "Up to date: nothing its cue checks changed since its last build, so a run skips it.",
    "Its cue doesn't check its command or what it uses: a change there doesn't run it again."
  ))

  # An outdated target has its reasons instead.
  writeLines(sub("tar_target\\(d, c \\+ 1\\)", "tar_target(d, c + 2)", readLines("_targets.R")), "_targets.R")
  expect_false("d" %in% names(skipped_reasons(poller_reasons())))

  # A tarchetypes skip rule has its own line.
  rules <- data.frame(name = "a", rule = "skip", arg = "TRUE", `for` = NA_character_, check.names = FALSE)
  expect_false("a" %in% names(skipped_reasons(reasons, rules)))
})

test_that("why a run skips a target reads in the pane, with its last build", {
  expect_null(skipped_ui(NULL))
  expect_equal(skipped_reasons(list()), list())
  html <- as.character(skipped_ui(
    c("Up to date: nothing its cue checks changed since its last build, so a run skips it.", "Its cue doesn't check its command: a change there doesn't run it again."),
    built = as.POSIXct("2026-11-26 12:01:30")
  ))
  expect_match(html, "since its last build, 2026-11-26 12:01, so a run skips it.", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('class="tb-rule"', html))), 2)
  expect_match(html, "circle-check", fixed = TRUE)
  expect_no_match(as.character(skipped_ui("Up to date: x since its last build.")), "since its last build,", fixed = TRUE)
  expect_equal(and_list(c("a", "b", "c")), "a, b and c")
  expect_equal(and_list(c("a", "b"), "or"), "a or b")
  expect_equal(backticked(c("f", "g")), "`f` and `g`")
})
