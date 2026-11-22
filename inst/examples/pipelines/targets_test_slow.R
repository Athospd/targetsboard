# From the tests: local_slow_pipeline() in tests/testthat/test-run.R, used by
# the tests of stopping a run. b takes a minute.
#
# Try: Run all, watch b run, then Stop: b shows as canceled and c never runs.
# Also start the run from the console (targets::tar_make(), with the same
# TAR_CONFIG and TAR_PROJECT) and stop it from the board.

library(targets)
list(tar_target(a, 1), tar_target(b, {Sys.sleep(60); a}), tar_target(c, b))
