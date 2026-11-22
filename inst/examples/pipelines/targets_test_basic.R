# From the tests: local_pipeline() in tests/testthat/helper-fixtures.R, the
# pipeline most server and browser tests use. Three targets in a chain.
#
# Try: run all, a target's menu and toolbar (Run, Invalidate, Value),
# Invalidate all, the value preview, saving the layout.

library(targets)
list(tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2))
