# From the tests: local_chain_pipeline(8) in tests/testthat/helper-fixtures.R,
# used by "a click on a tar_meta() row shows its target on the board".
# Eight targets in one long chain.
#
# Try: after a run, click t6's row in the tar_meta() tab: the board zooms in
# on t6 and its neighbours, with t1 out of view. Also Tidy vertically and
# horizontally on a long, thin graph.

library(targets)
list(
  tar_target(t1, 1),
  tar_target(t2, t1),
  tar_target(t3, t2),
  tar_target(t4, t3),
  tar_target(t5, t4),
  tar_target(t6, t5),
  tar_target(t7, t6),
  tar_target(t8, t7)
)
