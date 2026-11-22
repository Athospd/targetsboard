# From the tests: local_problem_pipeline() in tests/testthat/test-run-modes.R.
# bad errors, warned warns, and good and after are fine.
#
# Try the run mode switches under Run all:
# - off: the run stops at bad, maybe before good;
# - Keep going after errors: good, warned and after build, bad errors;
# - Stop at warnings: warned errors where it warned, with a workspace and a
#   traceback in its Problem pane. A plain run afterwards builds it again.

library(targets)
list(
  tar_target(bad, stop("boom")),
  tar_target(good, 1),
  tar_target(warned, {warning("2 rows dropped"); good + 1}),
  tar_target(after, warned * 2)
)
