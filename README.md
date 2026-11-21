
<!-- README.md is generated from README.Rmd. Please edit that file -->

# targetsboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Athospd/targetsboard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Athospd/targetsboard/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Athospd/targetsboard/graph/badge.svg)](https://app.codecov.io/gh/Athospd/targetsboard)
<!-- badges: end -->

A board for the author of a [targets](https://docs.ropensci.org/targets/)
pipeline, to understand it while working on it: its structure, what is
outdated, what each target produced and what a run is doing. It is meant to
replace `targets::tar_watch()` for the author. The DAG is drawn with
[React Flow](https://reactflow.dev): targets are coloured by status (up to
date, outdated, running or errored), running targets show a spinner and the
edges feeding them are animated, and targets whose last build raised
warnings carry a warning mark. Clicking a target focuses it: what it depends
on and what depends on it stay highlighted, and its value is previewed.
When the pipeline changes, targets already drawn keep their place.

<figure>
<img src="man/figures/README-board.png"
alt="The board while tar_make() runs: model_a is running" />
<figcaption aria-hidden="true">The board while tar_make() runs: model_a
is running</figcaption>
</figure>

**Early stage.** Help, support and ideas are welcome!

## Installation

``` r
remotes::install_github("Athospd/targetsboard")
```

## Usage

In your normal targets workflow, launch the board with `tar_board()`. It
runs in a background R process, so your session stays free:

``` r
library(targetsboard)

app <- tar_board()                     # opens the board in your browser

targets::tar_make()                    # the board follows along

app$is_alive()
readLines(app$get_output_file())       # the board's log
browseURL(attr(app, "url"))            # reopen it after closing the tab
app$kill()                             # shut it down
```

You can also run the pipeline from the board. **Run all** makes the
whole pipeline, **Run selected** makes the targets selected in the DAG
and whatever they depend on that is outdated, and **Stop** kills the run
going, wherever it was started. Runs from the board go on if the board
stops.

Like `targets::tar_watch()`, `tar_board()` needs no arguments: it shows
the current project's pipeline, using the script and store from its targets
configuration (`_targets.R` and `_targets/` unless `_targets.yaml` says
otherwise). The graph comes from the script, since the store only records
statuses; without a script yet, the board starts empty and draws the pipeline
once the script appears. As `tar_watch()` does, `tar_board()` serves it on a
random free port (or the `shiny.port` option), waits until it is up, opens it
in your browser and prints its url; `browse = FALSE` skips the browser.
It also takes a `script`, a `port`, a `host` and `targets_only`. To run the board in the foreground, or deploy it, use the
Shiny app object instead:

``` r
shiny::runApp(tar_board_app(script = "_targets.R"))
```

### The DAG widget in your own Shiny app

`targetsboard()` and `update_targetsboard()` take the nodes and edges of
`targets::tar_visnetwork()`:

``` r
library(shiny)
net <- targets::tar_visnetwork(targets_only = TRUE)$x

ui <- fluidPage(targetsboard("dag", net$nodes, net$edges, height = "600px"))

server <- function(input, output, session) {
  observe({
    invalidateLater(2000)
    net <- targets::tar_visnetwork(targets_only = TRUE)$x
    update_targetsboard(session, "dag", net$nodes, net$edges, height = "600px")
  })
  observe(print(input$dag_focused_node)) # id of the focused target
}

shinyApp(ui, server)
```

`tar_visnetwork()` hashes the whole pipeline, which gets slow on big
ones. To follow a running pipeline, push only the statuses from
`targets::tar_progress()` with `update_targetsboard_status()`: the
widget updates them in place.

``` r
progress <- targets::tar_progress()
update_targetsboard_status(session, "dag", setNames(progress$progress, progress$name))
```

## Development

- The React front end lives in `srcjs/`. The built bundle in
  `inst/htmlwidgets/` is committed, so installing needs no Node. After
  changing `srcjs/`, run `npm ci` once, then `npm run build` and commit
  the bundle (CI checks it is up to date). `npm run dev` /
  `npm run watch` make a development build with a source map.
- `dev/` holds scripts for running the board while developing; example
  pipelines are in `inst/examples/`.
- Tests: `devtools::test()`. The browser test needs Chrome and is
  skipped on CRAN.
