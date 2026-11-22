# A bootstrap analysis with every kind of target the board has to show:
# dynamic branching (map, cross, head; a pattern over a pattern), row
# groups (tar_group_by), batched reps (tar_rep), static branching
# (tar_map + tar_combine), a file target, and values of unusual types
# (ggplot objects, a function, NULL, a nested list). Some branches warn and
# a few error, handled two ways, so problems show up at the pattern level.
#
# For discussing how the board should present patterns and branches. Run it
# from the project folder, as the "bootstrap" project of _targets.yaml:
#
#   Sys.setenv(TAR_CONFIG = "inst/examples/pipelines/_targets.yaml", TAR_PROJECT = "bootstrap")
#   targets::tar_make()
#   targetsboard::tar_board()
#
# Branches sleep a little (`pause`), so their progress is visible on the board;
# a full run takes about a minute. `n_boot` sets the number of resamples.

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("stats", "ggplot2"),
  error = "continue" # other targets go on when one errors
)

n_boot <- 200    # bootstrap resamples: the main dynamic pattern
pause <- 0.1     # seconds per branch, to watch the progress
warn_every <- 25 # every 25th resample warns
fail_rep <- 37   # this resample errors (its fit is NULL: error = "null")

# ---- Helpers ----------------------------------------------------------------

simulate_data <- function(n = 300) {
  set.seed(42)
  x <- rnorm(n)
  data.frame(
    x = x,
    y = 1.5 + 2 * x + rnorm(n, sd = 1 + abs(x)),
    region = sample(c("north", "south", "east", "west"), n, replace = TRUE)
  )
}

resample <- function(data) {
  Sys.sleep(pause)
  data[sample.int(nrow(data), replace = TRUE), ]
}

fit_slope <- function(boot, rep) {
  Sys.sleep(pause)
  if (rep == fail_rep) stop("the resample has no variance in x (rep ", rep, ")")
  if (rep %% warn_every == 0) warning("resample ", rep, " has many repeated rows")
  data.frame(rep = rep, slope = unname(coef(lm(y ~ x, data = boot))[["x"]]))
}

percentile_ci <- function(slopes, alpha, method) {
  Sys.sleep(pause)
  bounds <- switch(method,
    percentile = unname(quantile(slopes, c(alpha / 2, 1 - alpha / 2))),
    normal = mean(slopes) + c(-1, 1) * qnorm(1 - alpha / 2) * sd(slopes)
  )
  data.frame(alpha = alpha, method = method, lower = bounds[1], upper = bounds[2])
}

plot_resample <- function(boot, rep) {
  ggplot(boot, aes(x, y)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(title = paste("Resample", rep))
}

# ---- Static branching: one model per smoother, as separate targets ----------

smoothers <- tar_map(
  values = data.frame(smoother = c("lm", "loess", "glm")),
  names = "smoother",
  tar_target(smooth_fit, {
    Sys.sleep(pause * 5)
    switch(smoother,
      lm = lm(y ~ x, data = data),
      loess = loess(y ~ x, data = data),
      glm = glm(y ~ x, data = data)
    )
  }),
  tar_target(smooth_rmse, {
    if (smoother == "loess") warning("loess: span chosen by default")
    sqrt(mean(residuals(smooth_fit)^2))
  })
)

# ---- The pipeline -----------------------------------------------------------

list(
  # A file target: the data written to disk, then read back from it.
  tar_target(data_file, {
    path <- file.path(tar_path_store(), "user", "bootstrap_data.csv")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(simulate_data(), path, row.names = FALSE)
    path
  }, format = "file"),
  tar_target(data, utils::read.csv(data_file)),

  # Dynamic branching: n_boot resamples (a list of data frames), then a fit
  # per resample (a pattern over a pattern), combined into one data frame.
  tar_target(rep, seq_len(n_boot)),
  tar_target(boot, resample(data), pattern = map(rep), iteration = "list"),
  tar_target(slope, fit_slope(boot, rep), pattern = map(boot, rep), error = "null"),
  tar_target(slopes, slope$slope),

  # cross(): every confidence level with every interval method.
  tar_target(alpha, c(0.01, 0.05, 0.10)),
  tar_target(method, c("percentile", "normal")),
  tar_target(ci, percentile_ci(slopes, alpha, method), pattern = cross(alpha, method)),

  # head(): plots of the first 4 resamples only, one ggplot per branch.
  tar_target(
    resample_plot, plot_resample(boot, rep),
    pattern = head(map(boot, rep), n = 4), iteration = "list"
  ),

  # Row groups: one branch per region of the data (iteration = "group").
  tar_group_by(by_region, data, region),
  tar_target(
    region_slope,
    {
      Sys.sleep(pause * 3)
      data.frame(region = by_region$region[1], slope = coef(lm(y ~ x, data = by_region))[["x"]])
    },
    pattern = map(by_region)
  ),

  # Batched reps: 4 batches of 25 simulated null slopes (tar_rep).
  tar_rep(
    null_slope,
    {
      Sys.sleep(pause)
      d <- simulate_data(100)
      d$y <- sample(d$y) # no relation left
      data.frame(slope = coef(lm(y ~ x, data = d))[["x"]])
    },
    batches = 4, reps = 25
  ),

  # Static branching, then its results combined.
  smoothers,
  tar_combine(smooth_rmses, smoothers[["smooth_rmse"]], command = c(!!!.x)),

  # Values of unusual types.
  tar_target(slope_density, ggplot(data.frame(slope = slopes), aes(slope)) + geom_density()),
  tar_target(predictor, function(x) mean(slopes) * x), # a function
  tar_target(nothing, NULL),                            # NULL
  tar_target(report, list(                              # a nested list
    n_boot = n_boot,
    failed = sum(is.na(match(seq_len(n_boot), slope$rep))),
    ci = ci,
    regions = region_slope,
    null_p = mean(abs(null_slope$slope) >= abs(mean(slopes))),
    rmse = smooth_rmses
  )),

  # A target that always errors, downstream of the patterns.
  tar_target(published, stop("no credentials to publish ", length(slopes), " slopes"))
)
