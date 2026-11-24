## Submission

This is a new release: targetsboard 0.2.0, a board for a 'targets'
pipeline, meant to replace targets::tar_watch() for the pipeline's author.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

* local: Windows 11, R 4.5.1
* GitHub Actions (R CMD check --as-cran): ubuntu-latest (R-devel, R-release,
  R-oldrel-1), windows-latest (R-release), macos-latest (R-release)
* win-builder: R-release, R-devel

## Notes for the reviewer

* The package bundles JavaScript libraries (React, React Flow and others) in
  inst/htmlwidgets/targetsboard.js, and highlight.js in inst/app/www. Their
  copyright holders are in Authors@R as 'cph', and inst/COPYRIGHTS lists
  each with its version and licence. The JavaScript sources and the files
  that build the bundles are in the package's repository (URL), folder srcjs/.
* The examples start Shiny apps, so they run only in interactive sessions
  (`if (interactive())`).
* Tests that start a Shiny app in a browser, or a pipeline in a background
  R process, are skipped on CRAN.

## Downstream dependencies

There are currently no downstream dependencies for this package.
