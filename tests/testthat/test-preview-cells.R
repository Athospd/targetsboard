# List-column cells in the Rows tab (#298): a summary as tibble prints one,
# a button that opens the cell's own views, cell within cell.

nested <- function() {
  x <- data.frame(cyl = c(4, 6, 8))
  x$data <- lapply(c(4, 6, 8), function(n) mtcars[mtcars$cyl == n, 1:3])
  x$model <- lapply(x$data, function(d) stats::lm(mpg ~ disp, d))
  x$extra <- list(NULL, 1:3, "a")
  x
}

test_that("a cell is summarised as tibble prints it, never formatted whole", {
  skip_if_not_installed("pillar")
  expect_equal(cell_summary(NULL), "NULL")
  expect_equal(cell_summary(c(1.5, 2)), "dbl [2]")
  expect_equal(cell_summary(stats::lm(mpg ~ wt, mtcars)), "lm")
  expect_match(cell_summary(mtcars[1:7, 1:3]), "^df \\[7 .* 3\\]$")
  skip_if_not_installed("ggplot2")
  expect_equal(cell_summary(ggplot2::ggplot()), "gg")
})

test_that("a list column's type names its cells' type, or mixed", {
  skip_if_not_installed("pillar")
  x <- nested()
  expect_equal(type_abbr(x$data), "list<df>")
  expect_equal(type_abbr(x$model), "list<lm>")
  expect_equal(type_abbr(x$extra), "list<mixed>")
  expect_equal(type_abbr(list(NULL, NULL)), "list")
})

test_that("the Rows tab shows list cells as buttons, and NULL plainly", {
  skip_if_not_installed("pillar")
  button <- cell_button(mtcars[1:7, 1:3], "data", 2L)
  expect_match(button, 'class="btn btn-link btn-sm p-0 tb-cell-open"', fixed = TRUE)
  expect_match(button, 'data-tb-cell="{&quot;column&quot;:&quot;data&quot;,&quot;row&quot;:2}"', fixed = TRUE)
  expect_match(button, "&lt;df [7", fixed = TRUE)
  expect_match(cell_button(NULL, "extra", 1L), '<span class="tb-cell-null">NULL</span>', fixed = TRUE)

  html <- htmltools::renderTags(rows_view(nested()))$html
  expect_match(html, "tb-cell-open", fixed = TRUE)
  expect_no_match(html, "Mazda|Coefficients|28\\.4") # no row names, no lm internals
})

test_that("a cell's value is found by its path, and a stale path says so", {
  x <- nested()
  x$data[[2]]$inner <- list(1:2, list(a = 1), NULL, 4, 5, 6, 7)
  expect_equal(cell_value(x, list(list(column = "data", row = 2L))), x$data[[2]])
  path <- list(list(column = "data", row = 2L), list(column = "inner", row = 2L))
  expect_equal(cell_value(x, path), list(a = 1))
  expect_error(cell_value(x, list(list(column = "data", row = 9L))), "no longer in the value")
  expect_error(cell_value(x, list(list(column = "cyl", row = 1L))), "no longer in the value")
  expect_equal(cell_step(list(column = "data", row = 2)), list(column = "data", row = 2L))
})

test_that("a cell's dialog says where it is as R code, and goes back only from deeper", {
  skip_if_not_installed("pillar")
  ns <- shiny::NS("preview")
  built <- build_preview(mtcars[1:3, 1:2], list(name = "fits"))
  one <- htmltools::renderTags(cell_dialog("fits", list(list(column = "data", row = 2L)), built, ns))$html
  expect_match(one, "<code>fits$data[[2]]</code>", fixed = TRUE)
  expect_match(one, 'data-tb-cell-input="preview-cell_drill"', fixed = TRUE)
  expect_no_match(one, "preview-cell_back", fixed = TRUE)
  expect_match(one, ">Rows<", fixed = TRUE)
  two <- htmltools::renderTags(cell_dialog(
    "fits", list(list(column = "data", row = 2L), list(column = "my col", row = 1L)), built, ns
  ))$html
  expect_match(two, "<code>fits$data[[2]]$`my col`[[1]]</code>", fixed = TRUE)
  expect_match(two, "preview-cell_back", fixed = TRUE)
})

test_that("the Value pane's frame names the input its cells send to", {
  target <- list(name = "x", format = "rds", bytes = 10)
  built <- build_preview(nested(), target)
  html <- htmltools::renderTags(preview_frame(target, built, ns = shiny::NS("preview")))$html
  expect_match(html, 'data-tb-cell-input="preview-cell_open"', fixed = TRUE)
})
