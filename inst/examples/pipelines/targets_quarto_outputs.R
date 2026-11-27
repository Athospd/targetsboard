# Quarto outputs richer than one report: a revealjs deck and a blog (a
# Quarto website with a listing of posts), both rendered by tar_quarto().
# Double-click each to see it in the Value pane: the deck in a frame (F for
# fullscreen), the blog from its index.html, its pages linked.
#
#   Sys.setenv(TAR_CONFIG = "inst/examples/pipelines/_targets.yaml", TAR_PROJECT = "quarto_outputs")
#   targets::tar_make()
#   targetsboard::tar_board()
#
# The deck and the posts read targets with tar_read(): the graph shows them
# downstream of the data, and a change to it renders them again. Their
# sources are written into the store when this script is read, only when
# they change, so they go with tar_destroy(). Needs tarchetypes, quarto and
# the Quarto CLI.

library(targets)

if (!requireNamespace("tarchetypes", quietly = TRUE) || !requireNamespace("quarto", quietly = TRUE) ||
    is.null(quarto::quarto_path())) {
  stop("This example needs tarchetypes, quarto and the Quarto CLI.", call. = FALSE)
}

source_file <- function(name, lines) {
  path <- file.path(tar_path_store(), "quarto", name)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(path) || !identical(readLines(path, warn = FALSE), lines)) writeLines(lines, path)
  path
}

deck <- source_file("deck/deck.qmd", c(
  "---",
  "title: \"Fuel economy\"",
  "subtitle: \"A revealjs deck rendered by tar_quarto()\"",
  "format: revealjs",
  "---",
  "",
  "## The data",
  "",
  "```{r}",
  "cars <- targets::tar_read(cars)",
  "knitr::kable(head(cars[, c(\"mpg\", \"cyl\", \"wt\")]))",
  "```",
  "",
  "## Mileage by cylinders",
  "",
  "```{r}",
  "#| fig-height: 4",
  "boxplot(mpg ~ cyl, data = targets::tar_read(cars), col = \"#88c0d0\")",
  "```",
  "",
  "## The model",
  "",
  "::: {.incremental}",
  "- Heavier cars go fewer miles per gallon.",
  "- Each extra 1000 lbs costs about this many mpg:",
  ":::",
  "",
  "```{r}",
  "round(coef(targets::tar_read(fit))[[\"wt\"]], 2)",
  "```"
))

blog <- dirname(source_file("blog/_quarto.yml", c(
  "project:",
  "  type: website",
  "website:",
  "  title: \"Pipeline notes\"",
  "  navbar:",
  "    left:",
  "      - href: index.qmd",
  "        text: Posts",
  "format:",
  "  html:",
  "    theme: cosmo"
)))
source_file("blog/index.qmd", c(
  "---",
  "title: \"Pipeline notes\"",
  "listing:",
  "  contents: posts",
  "  sort: \"date desc\"",
  "  type: default",
  "---"
))
source_file("blog/posts/first-look/index.qmd", c(
  "---",
  "title: \"A first look at the data\"",
  "date: \"2026-11-20\"",
  "categories: [data]",
  "---",
  "",
  "The pipeline's `cars` target, as the blog reads it:",
  "",
  "```{r}",
  "summary(targets::tar_read(cars)$mpg)",
  "```"
))
source_file("blog/posts/the-model/index.qmd", c(
  "---",
  "title: \"What the model says\"",
  "date: \"2026-11-27\"",
  "categories: [model]",
  "---",
  "",
  "::: {.callout-tip}",
  "Each post reads targets: a rebuilt `fit` renders the blog again.",
  ":::",
  "",
  "```{r}",
  "fit <- targets::tar_read(fit)",
  "plot(fitted(fit), resid(fit), xlab = \"Fitted\", ylab = \"Residual\")",
  "abline(h = 0, lty = 2)",
  "```"
))

list(
  tar_target(cars, transform(mtcars, cyl = factor(cyl)), description = "mtcars, with cyl a factor"),
  tar_target(fit, lm(mpg ~ wt + cyl, data = cars)),
  tarchetypes::tar_quarto(slides, deck, description = "A revealjs deck: F in the frame for fullscreen"),
  tarchetypes::tar_quarto(blog_site, blog, description = "A Quarto blog: a website with a listing of posts")
)
