# The app's own CSS parses: an unclosed block nests every rule after it
# inside, and they stop applying (a merge dropped a "}" once).

test_that("every rule block in the app's CSS is closed", {
  files <- list.files(test_path("..", "..", "inst", "app", "www"), pattern = "[.]css$", full.names = TRUE)
  files <- c(files, system.file("app", "www", "index.css", package = "targetsboard"))
  files <- unique(files[nzchar(files) & file.exists(files)])
  expect_gt(length(files), 0)
  for (file in files) {
    css <- paste(readLines(file, warn = FALSE), collapse = "\n")
    css <- gsub("/[*].*?[*]/", "", css, perl = TRUE)
    chars <- strsplit(css, "")[[1]]
    depth <- cumsum((chars == "{") - (chars == "}"))
    expect_true(all(depth >= 0) && utils::tail(depth, 1) == 0, info = file)
  }
})
