
<!-- README.md is generated from README.Rmd. Please edit that file -->

# targetsboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An experimental alternative to `targets::tar_watch()` to monitor and
(eventually) control DAGs from the UI.

**Ultra-early stage** with some performance issues and clunkyness! Still finding out if any useful.
Help/Support/Ideas are welcomed!

## Installation

You can install the development version of targetsboard like so:

``` r
remotes::install_github("Athospd/targetsboard")
```

## Usage

In your normal, day-to-day targets workflow, launch the targetsboard
through `tar_board()`.

``` r
library(targetsboard)

# launch the app
app <- tar_board()
```

``` r
# go to localhost:9999 where the board is being served
browseURL("http://localhost:9999")

# you can check it is alive, running on background
app$is_alive()
```

It will keep track of your targets’ metadata just like `tar_watch()`
would do.

``` r
tar_make()

# tar_destroy()
# tar_invalidate()
# tar_read()
# tar_... and so on
```

``` r
# you can eventually shut the app down like so
app$kill()
```
