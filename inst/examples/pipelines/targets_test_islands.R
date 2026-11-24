# From the tests: local_group_pipeline() in tests/testthat/helper-browser.R, used
# by "a selected group has its own toolbar, menu and highlighting". Three
# unconnected parts: a chain a -> b -> c, a pair d -> e, and f alone.
#
# Try: Ctrl-click (Cmd on a Mac) b and d, or Shift-drag a box: the group's
# toolbar and right-click menu ("Run 2 targets", "Select their lineage"),
# Escape to clear it.

library(targets)
list(
  tar_target(a, 1), tar_target(b, a + 1), tar_target(c, b * 2),
  tar_target(d, 10), tar_target(e, d + 1),
  tar_target(f, 0)
)
