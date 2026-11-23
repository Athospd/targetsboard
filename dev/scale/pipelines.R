# Scale fixtures: one bootstrap analysis, written two ways.
#
# 500 simulated rows are resampled `n` times, a linear model is fitted to each
# resample, and the coefficients are summarised. Every `warn_every`-th rep
# warns and every `error_every`-th rep errors, so the board's problem
# features see realistic counts too (0 turns them off).
#
# - "dynamic": `pattern = map(rep)`. 6 targets on the graph, 2 * n branches.
# - "static":  `tarchetypes::tar_map()`. 2 * n + 3 targets on the graph.
#
# The data are small on purpose (a resample is ~12 kB): the point is the
# number of targets and branches, not the size of their values.

write_scale_pipeline <- function(dir, kind = c("dynamic", "static"), n = 100,
                                 warn_every = 100, error_every = 1000) {
  kind <- match.arg(kind)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  common <- c(
    "library(targets)",
    # As bootstraps usually do: a failed rep is NULL, and the rest combine.
    "tar_option_set(error = 'null', packages = 'stats')",
    sprintf("n_reps <- %d; warn_every <- %d; error_every <- %d", n, warn_every, error_every),
    "simulate_data <- function() {",
    "  set.seed(1)",
    "  x <- rnorm(500)",
    "  data.frame(x = x, y = 2 + 3 * x + rnorm(500), group = sample(letters[1:5], 500, TRUE))",
    "}",
    "resample <- function(data) data[sample.int(nrow(data), replace = TRUE), ]",
    "fit_boot <- function(boot, rep) {",
    "  if (error_every > 0 && rep %% error_every == 0) stop('model did not converge on rep ', rep)",
    "  if (warn_every > 0 && rep %% warn_every == 0) warning('resample ', rep, ' has many ties')",
    "  coefs <- coef(lm(y ~ x, data = boot))",
    "  data.frame(rep = rep, intercept = coefs[[1]], slope = coefs[[2]])",
    "}",
    "summarise_boot <- function(estimates) {",
    "  data.frame(",
    "    term = c('intercept', 'slope'),",
    "    estimate = c(mean(estimates$intercept), mean(estimates$slope)),",
    "    lower = c(quantile(estimates$intercept, 0.025), quantile(estimates$slope, 0.025)),",
    "    upper = c(quantile(estimates$intercept, 0.975), quantile(estimates$slope, 0.975))",
    "  )",
    "}"
  )
  targets <- switch(kind,
    dynamic = c(
      "list(",
      "  tar_target(data, simulate_data()),",
      "  tar_target(rep, seq_len(n_reps)),",
      "  tar_target(boot, resample(data), pattern = map(rep), iteration = 'list'),",
      "  tar_target(fit, fit_boot(boot, rep), pattern = map(boot, rep)),",
      "  tar_target(estimates, fit),",
      "  tar_target(ci, summarise_boot(estimates))",
      ")"
    ),
    static = c(
      "library(tarchetypes)",
      "reps <- tarchetypes::tar_map(",
      "  values = data.frame(rep = seq_len(n_reps)),",
      "  tar_target(boot, resample(data)),",
      "  tar_target(fit, fit_boot(boot, rep))",
      ")",
      "list(",
      "  tar_target(data, simulate_data()),",
      "  reps,",
      "  tarchetypes::tar_combine(estimates, reps[['fit']]),",
      "  tar_target(ci, summarise_boot(estimates))",
      ")"
    )
  )
  writeLines(c(common, targets), file.path(dir, "_targets.R"))
  invisible(dir)
}
