# From the tests: local_problem_store() in tests/testthat/test-problem.R. Every
# kind of problem: model errors deep in the pipeline's own functions, warned
# warns, and one branch of the pattern branch errors.
#
# Try: the Problems tab (3 rows, a pattern counting "1 of 3 errored"); each
# Problem pane: model's traceback (analyze() and fit() shown, the calls in
# packages folded), the Reproduce and Debug run snippets, warned's snippet
# from its inputs and seed, branch's list of failed branches.

library(targets)
tar_option_set(error = "continue")

analyze <- function(data) fit(data)
fit <- function(data) stats::lm(y ~ x, data = data) # there is no y: errors

list(
  tar_target(data, data.frame(x = 1:3)),
  tar_target(model, analyze(data)),
  tar_target(warned, {warning("2 rows dropped"); data}),
  tar_target(nums, 1:3),
  tar_target(branch, if (nums == 2) stop("bad branch") else nums, pattern = map(nums))
)
