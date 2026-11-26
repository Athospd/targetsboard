# Take four's pipeline: the air-quality study of take three, grown to look
# like a real project: about 25 targets, a pattern over months, a tar_map()
# over the weather variables, a slow model, an R Markdown report, and
# descriptions. write_demo_v4() writes <dir>/_targets.R and its report:
#   fixed = FALSE  forecast is broken (wind, not Wind)
#   tweak = TRUE   plot_trend() draws thicker lines: its targets go outdated,
#                  and the board says why
write_demo_v4 <- function(dir, fixed = FALSE, tweak = FALSE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  forecast <- if (fixed) "data.frame(Temp = 90, Wind = 8)" else "data.frame(Temp = 90, wind = 8)"
  linewidth <- if (tweak) "1.6" else "0.9"
  writeLines(c(
    "library(targets)",
    "library(tarchetypes)",
    "suppressWarnings(suppressPackageStartupMessages({ library(ggplot2); library(broom) }))",
    "tar_option_set(error = 'continue')",
    "",
    "clean_air <- function(raw) {",
    "  Sys.sleep(1.5)",
    "  dropped <- sum(is.na(raw$Ozone))",
    "  if (dropped > 0) warning(dropped, ' days with no ozone reading dropped')",
    "  raw <- raw[!is.na(raw$Ozone) & !is.na(raw$Solar.R), ]",
    "  raw$month <- factor(month.name[raw$Month], levels = month.name[5:9])",
    "  raw",
    "}",
    "fit_month <- function(clean, month) {",
    "  Sys.sleep(1.2)",
    "  fit <- lm(Ozone ~ Temp + Wind, data = clean[clean$month == month, ])",
    "  cbind(month = month, broom::tidy(fit))",
    "}",
    "monthly_trend <- function(clean, var) {",
    "  Sys.sleep(0.8)",
    "  out <- aggregate(clean[[var]], list(month = clean$month), mean)",
    "  names(out)[2] <- var",
    "  out",
    "}",
    "plot_trend <- function(trend, var) {",
    "  Sys.sleep(0.6)",
    "  ggplot(trend, aes(month, .data[[var]], group = 1)) +",
    sprintf("    geom_line(linewidth = %s, colour = '#2F80ED') + geom_point(size = 3, colour = '#2F80ED') +", linewidth),
    "    labs(title = paste('Monthly mean of', var), x = NULL, y = var) +",
    "    theme_minimal(base_size = 14)",
    "}",
    "plot_ozone <- function(clean) {",
    "  Sys.sleep(1.2)",
    "  ggplot(clean, aes(Temp, Ozone, colour = month)) +",
    "    geom_point(alpha = 0.7, size = 2.5) +",
    "    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, linewidth = 1) +",
    "    scale_colour_viridis_d(end = 0.9) +",
    "    labs(title = 'Hotter days, more ozone', subtitle = 'New York, May to September 1973',",
    "         x = 'Temperature (F)', y = 'Ozone (ppb)', colour = NULL) +",
    "    theme_minimal(base_size = 14)",
    "}",
    "plot_effects <- function(coefs) {",
    "  Sys.sleep(0.8)",
    "  ggplot(coefs[coefs$term != '(Intercept)', ], aes(estimate, month, colour = term)) +",
    "    geom_vline(xintercept = 0, linetype = 2) +",
    "    geom_pointrange(aes(xmin = estimate - 2 * std.error, xmax = estimate + 2 * std.error),",
    "                    position = position_dodge(width = 0.5), linewidth = 1) +",
    "    labs(title = 'Effect on ozone, month by month', x = 'Estimate (ppb per unit)', y = NULL) +",
    "    theme_minimal(base_size = 14)",
    "}",
    "fit_ozone <- function(clean) {",
    "  Sys.sleep(7) # the slow one",
    "  lm(Ozone ~ Temp + Wind + Solar.R + month, data = clean)",
    "}",
    "",
    "list(",
    "  tar_target(raw, { Sys.sleep(1); datasets::airquality },",
    "             description = 'Daily air quality, New York, 1973'),",
    "  tar_target(clean, clean_air(raw), description = 'Days with an ozone reading'),",
    "  tar_target(summary_table, skimr::skim(clean), description = 'Every variable at a glance'),",
    "  tar_target(months, levels(clean$month)),",
    "  tar_target(ozone_model, fit_month(clean, months), pattern = map(months),",
    "             description = 'One model per month'),",
    "  tar_target(ozone_plot, plot_ozone(clean)),",
    "  tar_target(effects_plot, plot_effects(ozone_model)),",
    "  tar_map(",
    "    values = list(var = c('Temp', 'Wind', 'Solar.R')),",
    "    tar_target(trend, monthly_trend(clean, var), description = 'Monthly means'),",
    "    tar_target(trend_plot, plot_trend(trend, var))",
    "  ),",
    "  tar_target(hot_days, clean[clean$Temp >= 90, ], description = 'Days at 90 F or more'),",
    "  tar_target(hot_share, nrow(hot_days) / nrow(clean)),",
    "  tar_target(model, fit_ozone(clean), description = 'Ozone from weather, all months'),",
    "  tar_target(coefs, broom::tidy(model)),",
    "  tar_target(fit_quality, broom::glance(model)),",
    "  tar_target(residuals, data.frame(fitted = fitted(model), residual = resid(model))),",
    sprintf("  tar_target(forecast, predict(model, newdata = cbind(%s, Solar.R = 200, month = factor('July', levels = month.name[5:9]))),", forecast),
    "             description = 'Ozone on a 90 F day in July'),",
    "  tar_render(report, 'report.Rmd', description = 'The study, as a report')",
    ")"
  ), file.path(dir, "_targets.R"))
  writeLines(c(
    "---",
    "title: \"Ozone in New York, 1973\"",
    "output: html_document",
    "---",
    "",
    "```{r, echo = FALSE, message = FALSE}",
    "library(targets)",
    "```",
    "",
    "Hotter days bring more ozone.",
    "",
    "```{r, echo = FALSE}",
    "tar_read(ozone_plot)",
    "tar_read(effects_plot)",
    "```",
    "",
    "The model explains `r round(100 * tar_read(fit_quality)$r.squared)`% of the variation.",
    "Days at 90 F or more: `r round(100 * tar_read(hot_share))`%."
  ), file.path(dir, "report.Rmd"))
  invisible(dir)
}

# A second project beside the study, for the projects scene: _targets.yaml
# with main (the study) and quick, a three-target sketch with its own store.
write_projects_v4 <- function(dir) {
  writeLines(c(
    "main:", "  script: _targets.R", "  store: _targets",
    "quick:", "  script: quick.R", "  store: _targets_quick"
  ), file.path(dir, "_targets.yaml"))
  writeLines(c(
    "library(targets)",
    "list(",
    "  tar_target(sample_days, head(datasets::airquality, 30)),",
    "  tar_target(mean_ozone, mean(sample_days$Ozone, na.rm = TRUE)),",
    "  tar_target(note, sprintf('Mean ozone: %.1f ppb', mean_ozone))",
    ")"
  ), file.path(dir, "quick.R"))
  invisible(dir)
}
