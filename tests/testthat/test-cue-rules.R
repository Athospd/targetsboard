# tarchetypes' rerun rules, said in the board's words (#155).

rules_script <- function() {
  script <- withr::local_tempfile(fileext = ".R", .local_envir = parent.frame())
  writeLines(c(
    "library(targets); library(tarchetypes)",
    "flag <- TRUE",
    "list(",
    "  tar_age(aged, f(x), age = as.difftime(1, units = 'days')),",
    "  tar_target(aged2, f(x), cue = tar_cue_age(aged2, as.difftime(3, units = 'hours'))),",
    "  tarchetypes::tar_force(forced, f(x), force = flag),",
    "  tar_target(forced2, f(x), cue = tarchetypes::tar_cue_force(condition = flag)),",
    "  tar_skip(skipped, f(x), flag),",
    "  tar_target(skipped2, f(x), cue = tar_cue_skip(flag)),",
    "  tar_change(changed, f(x), change = Sys.Date()),",
    "  tar_age(aged3, f(x), age = my_age),",
    "  tar_target(plain, f(x))",
    ")"
  ), script)
  script
}

test_that("cue_rules() reads each factory and cue from the script, without running it", {
  rules <- cue_rules(rules_script())
  expect_equal(rules$name, c("aged", "aged2", "forced", "forced_change", "forced2", "skipped", "skipped2",
                             "changed", "changed_change", "aged3"))
  expect_equal(rules$rule, c("age", "age", "force", "helper", "force", "skip", "skip", "change", "helper", "age"))
  expect_equal(rules$arg[rules$name == "aged"], 'as.difftime(1, units = "days")')
  expect_equal(rules$arg[rules$name == "forced2"], "flag")
  expect_equal(rules$`for`[rules$name == "changed_change"], "changed")
  expect_equal(nrow(cue_rules(withr::local_tempfile())), 0) # no file
})

test_that("rule_sentence() says the rule, and when an aged target is due", {
  rules <- cue_rules(rules_script())
  now <- as.POSIXct("2026-11-26 12:00:00", tz = "UTC")
  rule <- function(name) target_rule(rules, name)
  expect_equal(rule_sentence(rule("aged"), built = now - 3 * 86400, now = now),
               "Runs again once its last build is older than 1 day. Built 3 days ago: it is due.")
  expect_equal(rule_sentence(rule("aged2"), built = now - 3600, now = now),
               "Runs again once its last build is older than 3 hours. Due in 2 hours.")
  expect_equal(rule_sentence(rule("aged2")), "Runs again once its last build is older than 3 hours.")
  expect_equal(rule_sentence(rule("aged3")), "Runs again once its last build is older than `my_age`.")
  expect_equal(rule_sentence(rule("forced")), "Runs every time while `flag` is true.")
  expect_equal(rule_sentence(rule("skipped2")), "Skipped while `flag` is true, even when what it depends on changes.")
  expect_equal(rule_sentence(rule("changed_change")), "Checks `Sys.Date()` for `changed` on every run.")
  expect_equal(rule_sentence(data.frame(name = "a", rule = "force", arg = "TRUE")), "Runs every time.")
  expect_null(target_rule(rules, "plain"))
  expect_match(as.character(rule_ui("Runs every time while `flag` is true.")), "<code>flag</code>", fixed = TRUE)
})

test_that("outdated_reasons() names the rule, not the cue it set or its helper", {
  rules <- cue_rules(rules_script())
  reasons <- list(
    vertices = data.frame(name = c("aged", "forced", "forced_change", "forced2", "changed", "changed_change", "dl"),
                          type = "stem", status = "outdated"),
    edges = data.frame(from = c("forced_change", "changed_change"), to = c("forced", "changed")),
    sitrep = data.frame(name = c("aged", "forced", "forced_change", "forced2", "changed", "changed_change", "dl"),
                        always = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE),
                        file = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
    manifest = data.frame(name = "dl", format = "url", command = "'https://example.com'")
  )
  why <- outdated_reasons(reasons, rules)
  expect_equal(why$aged, c(age = "Its last build is older than 1 day."))
  expect_equal(why$forced, c(force = "Forced: `flag` is true."))
  expect_equal(why$forced2, c(force = "Forced: `flag` is true."))
  expect_equal(why$changed, c(change = "`Sys.Date()` is checked on every run: it runs again if its value changed."))
  expect_equal(why$forced_change, c(helper = "Checks `flag` for `forced` on every run."))
  expect_equal(why$dl, c(file = "Its URL's remote file changed, or can't be reached."))
  # Without rules, as before.
  expect_equal(outdated_reasons(reasons)$forced2, c(always = "Its cue is \"always\": it runs every time."))
})

test_that("the Value pane says a skipped target's rule, though it looks up to date", {
  skip_if_not_installed("tarchetypes")
  dir <- local_pipeline()
  writeLines(c(
    "library(targets); library(tarchetypes)",
    "list(tar_target(x, 1), tar_target(y, x + 1, cue = tar_cue_skip(TRUE)))"
  ), file.path(dir, "_targets.R"))
  withr::with_dir(dir, targets::tar_make(reporter = "silent"))
  app <- local_board_app(dir, "cue-rules")
  app$wait_for_js("document.querySelector(\".react-flow__node[data-id='y']\") !== null", timeout = 30000)
  app$click(selector = ".react-flow__node[data-id='y']")
  app$wait_for_js("document.querySelector(\".tb-node-toolbar [data-action='value']\") !== null")
  app$click(selector = ".tb-node-toolbar [data-action='value']")
  app$wait_for_js("document.querySelector('.tb-rule') !== null", timeout = 20000)
  expect_equal(app$get_js("document.querySelector('.tb-rule').innerText.trim()"),
               "Always skipped, even when what it depends on changes.")
})

test_that("cue_rules() and sourced_paths() read a pkg::f(...)(...) call (#238)", {
  dir <- withr::local_tempdir()
  script <- file.path(dir, "_targets.R")
  writeLines(c("library(targets)", "purrr::partial(paste, sep = '-')('a', 'b')", "list(tar_target(a, 1))"), script)
  expect_no_error(cue_rules(script))
  expect_no_error(sourced_paths(script))
})

test_that("a tar_age() target says it is too old only when its age cue fired (#240)", {
  rules <- cue_rules(rules_script())
  reasons <- function(...) list(
    vertices = data.frame(name = "aged", type = "stem", status = "outdated"),
    edges = data.frame(from = character(), to = character()),
    sitrep = data.frame(name = "aged", ...)
  )
  expect_equal(outdated_reasons(reasons(always = FALSE, command = TRUE), rules)$aged, c(command = "Its command changed."))
  expect_equal(outdated_reasons(reasons(always = FALSE, meta = TRUE), rules)$aged,
               c(meta = "It has no record in the store: it is new, was invalidated, or never finished."))
  expect_equal(outdated_reasons(reasons(always = TRUE, command = TRUE), rules)$aged,
               c(age = "Its last build is older than 1 day.", command = "Its command changed."))
})

test_that("positional arguments are matched as R matches them, around named ones (#243)", {
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "list(",
    "  tarchetypes::tar_force(x, command = f(), TRUE),",
    "  tarchetypes::tar_age(y, age = as.difftime(1, units = 'days'), g()),",
    "  tarchetypes::tar_skip(command = h(), z, skip = cond),",
    "  targets::tar_target(w, k(), cue = tarchetypes::tar_cue_age(age = as.difftime(2, units = 'hours'), w))",
    ")"
  ), script)
  rules <- cue_rules(script)
  expect_equal(rules$arg[rules$name == "x"], "TRUE")
  expect_equal(rules$rule[rules$name == "x"], "force")
  expect_equal(rules$arg[rules$name == "y"], "as.difftime(1, units = \"days\")")
  expect_equal(rules$arg[rules$name == "z"], "cond")
  expect_equal(rules$arg[rules$name == "w"], "as.difftime(2, units = \"hours\")")
})
