# From the tests: "dense layers stay compact, in the columns of
# tar_visnetwork()'s levels" in tests/testthat/test-browser-layout.R. Three layers of
# three targets, each feeding all of the next: the shape that used to spread
# into a widening diamond.
#
# Try: the first layout, and Tidy vertically and horizontally.

library(targets)
list(
  tar_target(a1, 1), tar_target(a2, 2), tar_target(a3, 3),
  tar_target(b1, a1 + a2 + a3), tar_target(b2, a1 + a2 + a3), tar_target(b3, a1 + a2 + a3),
  tar_target(c1, b1 + b2 + b3), tar_target(c2, b1 + b2 + b3), tar_target(c3, b1 + b2 + b3)
)
