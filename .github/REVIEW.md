# Review checklist

For every review of a targetsboard change, by a person or by an agent
(`/code-review`). It holds what a reviewer without the project's history
misses. A trial of blind reviews caught a slow preview (#176) but missed a
focus that showed the whole lineage (#172) and a run summary that flickered
(#173), and called the flickering design sound (#207). Check each line that
the change touches; say which ones you checked.

## Shiny reactivity

- [ ] **No redraw on a timer or poll.** A `renderUI()` that a timer or the
  poller invalidates rebuilds its HTML and flickers. Update in place instead
  (a custom message, `update*()`, an output of its own), as the run summary
  does (`run_summary_values()`).
- [ ] **Reads that re-render.** Every reactive a render reads makes it run
  again: `board()`, `running()`, `progress()`, the poller's results. Read
  only what the output shows; `isolate()` the rest, or split the output.
- [ ] **Equal values aren't sent again.** A `reactiveVal` set to the same
  value, or a message with the same payload, still costs a render or a
  round trip.
- [ ] **No render loop with an input.** A `renderUI()` that builds an input
  it also reads (a select it re-creates) loops; use `update*()` (#126).
- [ ] **Hidden outputs.** Shiny doesn't render an output in a hidden tab,
  panel or `display: none` block. An output that must fill while hidden
  needs `outputOptions(suspendWhenHidden = FALSE)`; a browser test must
  click its tab first.

## Cost on render paths

- [ ] **No script sourcing on a click.** No `tar_manifest()`,
  `tar_network()`, `tar_branches()`, `tar_visnetwork()` or anything that
  sources the script when a preview opens, a target is focused or a tab
  changes. Reuse the poller's results (`R/poller.R`), or compute on demand
  once and cache.
- [ ] **The poller stays lean.** One process per recompute
  (`poller_why()`), nothing new that blocks the session.
- [ ] **The canvas on large pipelines.** Code that runs on every render of
  the widget (every drag frame, every status tick) keeps unchanged nodes and
  edges as the same objects (#206).

## Intended behaviour

- [ ] **Focus is the direct neighbours**, as `tar_visnetwork()` highlights
  them, not the whole lineage (#172). The lineage is a menu action.
- [ ] **Statuses and their colours** come from the `--tb-status-*` tokens,
  so every theme and the legend follow.
- [ ] **The board only looks** (ADR 0001); a control that changes the store
  or the script asks first and says what goes.
- [ ] **`tar_board()` with no arguments just works**, like `tar_watch()`.

## Project rules

- [ ] `R CMD check --as-cran` stays clean; R code is ASCII.
- [ ] **Lean dependencies**: shiny, bslib and htmltools, or our own code,
  before a new Imports; anything optional is in Suggests.
- [ ] **UI labels in plain text** ("Board", "Keep going after errors");
  snake_case is for ids and values only.
- [ ] **Media off main**: videos and large images go on the `media` branch.
- [ ] **JS bundle rebuilt** (`npm ci && npm run build`) when `srcjs/`
  changes; a bundle conflict is resolved by rebuilding, never by a side.
- [ ] **The app's CSS parses** (`test-css.R`): a lost `}` silently drops
  every rule after it.
- [ ] **NEWS** has an entry under the development version, not under a
  released one (`## Bug fixes` appears in both).

## Tests

- [ ] A test that fails before the change and passes after it.
- [ ] **Browser tests run the installed package**: install the branch to a
  scratch library before trusting a UI change (`R CMD INSTALL
  --library=<lib> .`, then `R_LIBS=<lib>`).
- [ ] A UI change is looked at in light and dark mode.
