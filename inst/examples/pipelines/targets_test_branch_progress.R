# From the tests: "during a run, patterns show their branch counts" in
# tests/testthat/test-server.R. A pattern of 4 branches, a second each.
#
# Try: Run all and watch slow's count go 0/4 to 4/4; it goes away when the
# run ends.

library(targets)
list(tar_target(nums, 1:4), tar_target(slow, {Sys.sleep(1); nums}, pattern = map(nums)))
