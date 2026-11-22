# A dynamically branched bootstrap at scale, for how the board copes with many
# targets or branches: 1,000 reps (change n_reps below).
# A bootstrap: 500 rows resampled n_reps times, a model fitted to each;
# every 100th rep warns and every 1000th errors (its result is NULL).
#
# Dynamic: 6 targets on the graph, 2 * n_reps branches. Try: the branch
# counts during a run, the Problems tab, and the preview of boot (a list of
# n_reps data frames: the scale tests found it very slow to print).
#
# A full build takes about 30 s at 1,000 reps.

library(targets)
tar_option_set(error = 'null', packages = 'stats')
n_reps <- 1000; warn_every <- 100; error_every <- 1000
simulate_data <- function() {
  set.seed(1)
  x <- rnorm(500)
  data.frame(x = x, y = 2 + 3 * x + rnorm(500), group = sample(letters[1:5], 500, TRUE))
}
resample <- function(data) data[sample.int(nrow(data), replace = TRUE), ]
fit_boot <- function(boot, rep) {
  if (error_every > 0 && rep %% error_every == 0) stop('model did not converge on rep ', rep)
  if (warn_every > 0 && rep %% warn_every == 0) warning('resample ', rep, ' has many ties')
  coefs <- coef(lm(y ~ x, data = boot))
  data.frame(rep = rep, intercept = coefs[[1]], slope = coefs[[2]])
}
summarise_boot <- function(estimates) {
  data.frame(
    term = c('intercept', 'slope'),
    estimate = c(mean(estimates$intercept), mean(estimates$slope)),
    lower = c(quantile(estimates$intercept, 0.025), quantile(estimates$slope, 0.025)),
    upper = c(quantile(estimates$intercept, 0.975), quantile(estimates$slope, 0.975))
  )
}
list(
  tar_target(data, simulate_data()),
  tar_target(rep, seq_len(n_reps)),
  tar_target(boot, resample(data), pattern = map(rep), iteration = 'list'),
  tar_target(fit, fit_boot(boot, rep), pattern = map(boot, rep)),
  tar_target(estimates, fit),
  tar_target(ci, summarise_boot(estimates))
)
