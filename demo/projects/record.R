# The projects tutorial: how to use and try targets projects on the board
# (#114). It follows dev/projects/play.R on the same sandbox, so the video
# and the script match. The overlays are drawn by ../v3/assemble.py from
# the events logged here (../driver.R).
#
# Uses the installed targetsboard: R CMD INSTALL . first. make-demo-projects.py
# runs it from the package folder, with DEMO_DIR and DEMO_OUT set.
library(shinytest2)
DEMO <- Sys.getenv("DEMO_DIR", "demo")
OUT <- Sys.getenv("DEMO_OUT", file.path(DEMO, "projects", "build"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
SCALE <- 2 # 2560 x 1440 frames, sharp under the camera's zooms
chromote::set_chrome_args(c(chromote::default_chrome_args(), sprintf("--force-device-scale-factor=%d", SCALE)))

# The sandbox of dev/projects/setup.R, in the take's own folder.
dir <- normalizePath(file.path(OUT, "project"), winslash = "/", mustWork = FALSE)
Sys.setenv(PROJECTS_SANDBOX = dir)
source("dev/projects/setup.R")
writeLines("targetsboard::tar_board_app()", file.path(dir, "app.R"))
app <- AppDriver$new(dir, name = "demo-projects", width = 1280, height = 720, load_timeout = 60000)
app$wait_for_js("document.querySelectorAll('.react-flow__node').length === 3", timeout = 30000)
s <- app$get_chromote_session()
Sys.sleep(1.5)
app$run_js("document.querySelector('.react-flow__controls-fitview').click()")
Sys.sleep(1.5)
source(file.path(DEMO, "driver.R"))

# ---- This take's moves --------------------------------------------------------------
field <- "#board-script-project"
path <- "#board-script-path"
# Pick a project in the field, as a click and a choice would (a native
# select's list isn't in the screencast).
pick <- function(project) {
  click(field, dur = 0.5)
  hold(0.5)
  js(sprintf("(() => { const s = document.querySelector('%s'); s.value = '%s'; s.dispatchEvent(new Event('change', { bubbles: true })); })()",
             field, project))
}
# Type a path into the script bar, a letter at a time, and press Load.
load_script <- function(script) {
  click(path, dur = 0.5)
  js(sprintf("document.querySelector('%s').select()", path))
  for (i in seq_len(nchar(script))) {
    js(sprintf("(() => { const e = document.querySelector('%s'); e.value = '%s'; e.dispatchEvent(new Event('input', { bubbles: true })); })()",
               path, substr(script, 1, i)))
    hold(0.09)
  }
  js(sprintf("document.querySelector('%s').dispatchEvent(new Event('change', { bubbles: true }))", path))
  hold(0.4)
  click("#board-script-load", dur = 0.4)
}
on_project <- function(project) sprintf("document.querySelector('%s').value === '%s'", field, project)
tab <- function(label) sprintf("#board-view .nav-link[data-value='%s']", label)
bar <- c(".tb-project", ".tb-address")

# ---- Scenes --------------------------------------------------------------------------
s$Page$startScreencast(format = "jpeg", quality = 90, maxWidth = 1280 * SCALE, maxHeight = 720 * SCALE, everyNthFrame = 1)
track(pos)
hold(0.4)

chapter("targets projects", "#F2C94C")
hold(1.4)
caption("One folder, several pipelines")
console("_targets.yaml", c(
  "fast:", "  script: fast.R", "  store: _targets_fast",
  "slow:", "  script: slow.R", "  store: _targets_slow",
  "slow_copy:", "  inherits: slow", "  store: _targets_slow_copy"
))
hold(3.2)
caption("Each project: its own script and store")
hold(3.0)
console_off()
hold(0.3)
console("R console", c("> targetsboard::tar_board()", "# no arguments: on main, as tar_watch()"))
caption("Start the board as always")
hold(3.0)
console_off()
hold(0.4)
frame_on(bar, max_zoom = 2.2, dur = 0.8)
caption("With projects, a Project field shows up")
hold(2.8)
cam(640, 360, 1, 0.6)
hold(0.4)

chapter("1 · Switch project", "#78B7C5")
hold(1.3)
caption("Pick one: its graph, its script, its store")
pick("fast")
wait_cut(has(node("fast_b")), 30)
hold(1.2)
frame_on(bar, max_zoom = 2.2, dur = 0.7)
caption("The script bar follows: fast.R")
hold(2.2)
cam(640, 360, 1, 0.5)
click(tab("script"), dur = 0.5)
until(has(".tb-script-view .tb-paths"), 10)
hold(0.4)
frame_on(".tb-script-view .tb-paths", max_zoom = 1.8, dur = 0.7)
caption("The Script tab: where its store is")
hold(2.6)
caption("…and the line to switch your console too")
hold(2.8)
cam(640, 360, 1, 0.5)
click(tab("board"), dur = 0.5)
hold(0.6)

chapter("2 · Load follows the script", "#354823")
hold(1.3)
caption("A script no project names stays on this project")
load_script("other.R")
wait_cut(has(node("other")), 30)
frame_on(bar, max_zoom = 2.2, dur = 0.6)
hold(2.2)
cam(640, 360, 1, 0.5)
caption("Another project's script switches to its project")
load_script("slow.R")
wait_cut(paste(has(node("slow_4")), "&&", on_project("slow")), 30)
frame_on(bar, max_zoom = 2.2, dur = 0.6)
hold(2.4)
cam(640, 360, 1, 0.5)
hold(0.3)

chapter("3 · A run holds the project", "#D49536")
hold(1.3)
caption("Run all on slow")
click("#board-run-all", dur = 0.6)
until(sprintf("document.querySelector('%s').disabled", field), 15)
hold(0.4)
frame_on(".tb-project", max_zoom = 2.2, dur = 0.6)
caption("During a run, the project can't change")
hold(2.6)
caption("Switch after the run ends")
hold(1.6)
cam(640, 360, 1, 0.5)
frame_on(".tb-legend", max_zoom = 2, dur = 0.6)
caption("The legend counts the targets as they build")
speed(2)
until(sprintf("!document.querySelector('%s').disabled", field), 60)
speed(1)
frame_on(".tb-project", max_zoom = 2.2, dur = 0.6)
caption("Done: free to switch again")
hold(2.0)
cam(640, 360, 1, 0.5)
hold(0.3)

chapter("4 · inherits", "#8E6BB0")
hold(1.3)
caption("slow_copy inherits slow's script…")
pick("slow_copy")
wait_cut(paste(on_project("slow_copy"), "&&", has(".react-flow__node-outdated[data-id='slow_4']")), 30)
hold(1.0)
caption("…with its own store: the same graph, not built yet")
hold(3.0)

chapter("5 · _targets.yaml, live", "#C93312")
hold(1.3)
console("R console", c(
  "> targets::tar_config_set(",
  "+   script = \"fast.R\", store = \"_targets_extra\",",
  "+   project = \"extra\"",
  "+ )"
))
caption("Add a project while the board is open")
hold(2.4)
targets::tar_config_set(script = "fast.R", store = "_targets_extra", project = "extra",
                        config = file.path(dir, "_targets.yaml"))
hold(1.2)
console_off()
until(sprintf("[...document.querySelectorAll('%s option')].some(o => o.value === 'extra')", field), 10)
caption("It's in the Project field within a second")
pick("extra")
wait_cut(paste(on_project("extra"), "&&", has(node("fast_b"))), 30)
frame_on(bar, max_zoom = 2.2, dur = 0.6)
hold(2.2)
cam(640, 360, 1, 0.5)
hold(0.3)

chapter("6 · Start on a project", "#F2C94C")
hold(1.3)
console("R console", c(
  "> targetsboard::tar_board(project = \"slow\")",
  "# or, as targets itself does:",
  "> Sys.setenv(TAR_PROJECT = \"slow\")",
  "> targetsboard::tar_board()"
))
caption("Start on any project")
hold(3.6)
caption("The board never changes your session's TAR_PROJECT")
hold(3.2)
console_off()
hold(0.4)

chapter("Try it yourself", "#78B7C5")
hold(1.3)
console("R console", c(
  "# from the targetsboard repository:",
  "> source(\"dev/projects/setup.R\")  # the sandbox",
  "> source(\"dev/projects/play.R\")   # this tour",
  "# the targets manual: books.ropensci.org/targets/projects.html"
))
caption("Every step of this video, in dev/projects/")
hold(4.5)
console_off()
caption("")
hold(1.2)
finish_take()
app$stop()
