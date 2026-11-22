# From the tests: the warning-mark test in tests/testthat/test-server.R. a
# warns; b depends on it.
#
# Try: the warning mark on a after a run, the Problems tab (a yellow count),
# and a's Problem pane with its Reproduce snippet.

library(targets)
list(tar_target(a, {warning("careful"); 1}), tar_target(b, a + 1))
