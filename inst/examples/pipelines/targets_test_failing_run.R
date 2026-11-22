# From the tests: "Tell how the board's own run ended" in
# tests/testthat/test-run.R. The only target errors.
#
# Try: Run all: the notification "Run failed in a: boom", with its Show
# problem link to the Problem pane.

library(targets)
list(tar_target(a, stop("boom")))
