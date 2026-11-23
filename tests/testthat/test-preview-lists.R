test_that("a list previews as a tree, two levels deep", {
  x <- list(
    alpha = 1:3, beta = "text", nested = list(deep = list(deeper = 1), frame = data.frame(a = 1:2)),
    list(4), empty = NULL
  )
  html <- as.character(tar_board_preview(x, list(name = "x")))
  expect_match(html, '<div class="tb-tree">', fixed = TRUE)
  # Each item: its name, type and first values.
  expect_match(html, '<code class="tb-tree-name">alpha</code>[[:space:]]*<span class="tb-tree-type">int \\[3\\]</span>[[:space:]]*<span class="tb-tree-value">1, 2, 3</span>')
  expect_match(html, '<code class="tb-tree-name">[[4]]</code>', fixed = TRUE) # unnamed
  expect_match(html, '<span class="tb-tree-type">NULL</span>', fixed = TRUE)
  # A list at the first level opens on its items; one at the second is a line.
  expect_match(html, '<details class="tb-tree-node">[[:space:]]*<summary>[[:space:]]*<code class="tb-tree-name">nested</code>')
  expect_match(html, '<code class="tb-tree-name">deep</code>[[:space:]]*<span class="tb-tree-type">list \\[1\\]</span>')
  expect_no_match(html, "deeper", fixed = TRUE)
  expect_match(html, "data.frame 2 \u00d7 1", fixed = TRUE)
  expect_null(tar_board_preview(list(), list(name = "x")))
})

test_that("a level shows 50 items, and says how many more", {
  html <- as.character(list_tree(as.list(seq_len(120))))
  expect_equal(lengths(regmatches(html, gregexpr('class="tb-tree-leaf"', html))), 50)
  expect_match(html, "and 70 more", fixed = TRUE)
})

test_that("an item's first values fit on one line", {
  expect_equal(short_value(c(1.23456, 2, NA)), "1.235, 2, NA")
  expect_equal(short_value(1:10), "1, 2, 3, 4, 5, \u2026")
  expect_equal(nchar(short_value(strrep("a", 200))), 80)
  expect_null(short_value(list(1)))
  expect_equal(short_value(c("demo", "gallery")), "demo, gallery") # not padded
  expect_equal(type_label(matrix(1:6, 2)), "matrix 2 \u00d7 3")
  expect_equal(type_label(mean), "function")
})

test_that("a model previews as its estimates and fit, with broom", {
  skip_if_not_installed("broom")
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  built <- build_preview(fit, list(name = "fit"))
  expect_equal(names(built$views), c("Estimates", "Fit", "Printed"))
  expect_null(built$note)
  estimates <- built$views$Estimates
  expect_s3_class(estimates, "reactable")
  html <- as.character(htmltools::as.tags(estimates))
  for (column in c("term", "estimate", "p.value", "(Intercept)")) expect_match(html, column, fixed = TRUE)
  fit_html <- as.character(htmltools::as.tags(built$views$Fit))
  expect_match(fit_html, "r.squared", fixed = TRUE)
  expect_no_match(fit_html, "statistic.value", fixed = TRUE) # glance's names, as they are
})

test_that("an object broom doesn't know previews as its summary(), else as a tree", {
  summary.tb_thing <- function(object, ...) structure(list(), class = "summary.tb_thing")
  print.summary.tb_thing <- function(x, ...) cat("a thing, summarised\n")
  registerS3method("summary", "tb_thing", summary.tb_thing)
  registerS3method("print", "summary.tb_thing", print.summary.tb_thing)
  views <- tar_board_preview(structure(list(a = 1), class = "tb_thing"), list(name = "x"))
  expect_named(views, "Summary")
  expect_match(as.character(views$Summary), "a thing, summarised", fixed = TRUE)

  tree <- tar_board_preview(structure(list(a = 1, b = "x"), class = "tb_other"), list(name = "x"))
  expect_match(as.character(tree), '<div class="tb-tree">', fixed = TRUE)
  expect_null(tar_board_preview(quote(a + b), list(name = "x")))
})
