# targetsboard (development version)

## New features

* A status legend on the canvas, under the Tidy buttons: each status's
  colour, the nodes' own in every theme and mode, with how many targets
  have it. A click on one selects those targets, for the group's Run and
  Invalidate. It folds away, and the browser remembers; hide it with
  `targetsboard_options(legend = FALSE)` (#102).

* An icon in each target's top-right corner tells its kind: a pattern, a file
  (`format = "file"`), a Quarto or R Markdown report, a function or an
  object. A plain target has none. Hover it for its name. The icons take the
  node's text colour, so they read in every status, theme and mode (#103).

* A run summary in the Control Panel, under the run buttons: while a run
  goes, its clock and how many targets it built, skipped, is running, saw
  error and has still to go, with a thin bar of those parts; after it, the
  last run's time and counts, until the next one. Only its clock ticks: the
  rest is drawn again when a count changes. As `tar_progress_summary()` counts, from targets' own
  files, so a run started from the console shows too (#140).

* Build times and sizes: two switches in the legend, Times and Sizes, show
  each built target's last build time and stored size in small text in its
  bottom-left corner (a pattern's summed over its branches). The slowest
  and largest tenth get a warm dot. The browser remembers the switches. The
  Metadata tab (was `tar_meta()`) shows `seconds` and `bytes` readably,
  slowest first, and the run summary estimates the time left from the
  targets' last builds (#107).

* tarchetypes' rerun rules are said in the board's words. The Value pane
  states a target's rule whatever its status: `tar_age()` (and when it is
  due), `tar_force()`, `tar_skip()` (a skipped target looks up to date),
  `tar_change()`, and the cues `tar_cue_age()`, `tar_cue_force()`,
  `tar_cue_skip()`. Why it is outdated names the rule rather than the
  "always" cue or the `_change` helper it sets, and a URL's file says it is
  remote (#155).

* A target's description (`tar_target(description = )`, or
  `tarchetypes::tar_map(descriptions = )`'s) shows when hovering it on the
  canvas, before why it is outdated, and at the top of its Value pane (#143).

* The board redraws when a function changes in any file the script sources:
  the literal paths of its `tar_source()` and `source()` calls (a folder, or
  a file), not only `R/` (#152).

* The Value pane has a Command section, closed at first: the target's
  command as it runs, from `tar_manifest()`, highlighted and copyable. For
  targets made by `tar_map()`, `tar_eval()`, `tar_combine()` or hooks, that
  is not always what the author wrote (#144).

* The Value pane has a Debug section, closed at first, for any target and
  not only failing ones: the Problems pane's Reproduce code (its globals,
  inputs and seed, then its command) and debug run, for a target that runs
  but gives a wrong value. A branch gets its debug run, and Reproduce when
  it has a workspace (#150).

## Bug fixes

* The run summary of a run of a few targets, from the graph's menus or
  toolbars, counted the rest of the pipeline as still to go ("13 to go"
  after its three were built), and estimated the time left from it. It now
  counts only the targets asked for and what they depend on. A run from the
  console still counts the whole pipeline (#181).

* Focusing a target highlighted its whole lineage, everything upstream and
  downstream at every level, so a target near the start of the pipeline
  lit up nearly the whole graph. It now highlights its direct inputs and
  outputs and the edges to them, as `tar_visnetwork()` does; a selected
  group, the same around each of its targets. The menu's Select its lineage
  still selects the whole lineage (#172).

* A redraw of the graph could drop an outdated target's hover text (why it
  is outdated) until the board next worked it out.

* A click on a legend entry with a single target selected it without marking it
  on the canvas: now it focuses it too, as a click on the target does.

* The Problems and Metadata tables could turn white after picking another
  theme on the board, barely readable in a dark one: reactable's own
  stylesheet, loaded again by the switch, won over the theme's colours.
  The board's rules now win whatever the order (#165).

* `tar_board(names = , allow = , exclude = , ...)` kept the subset only for
  the first drawing: the first change to the pipeline brought back the whole
  graph. The board now redraws with them every time, tidyselect expressions
  such as `names = starts_with("model")` included. Another script loaded
  from the board is drawn whole (#148).

## Internal

* Four fewer dependencies: dplyr, purrr, zeallot and digest. The widget's
  graph conversion is base R and about 15 times as fast (5,000 targets:
  1.2 s to 0.08 s), with the same JSON (#160).

# targetsboard 0.3.0

## New features

* targets projects: the board follows `_targets.yaml`'s projects (#114).
  It starts on the project `TAR_PROJECT` names, `"main"` unless set, or on
  `tar_board(project = )` (also `tar_board_app(project = )`), with that
  project's script and store. A Project field left of the script bar
  switches project, store included; it shows only when `_targets.yaml` has
  more than one project or the board isn't on `"main"`, so a plain pipeline
  looks as before. Loading another project's script switches to that
  project; a script no project names stays on the current project's store.
  The field waits for a run to end, and follows `_targets.yaml` as it
  changes. The board's project is its own: your R session's `TAR_PROJECT`
  is never changed, and the Script tab gives the
  `Sys.setenv(TAR_PROJECT = )` line to make it follow. Copied code (a debug
  run, `tar_read()` in the preview) names the project's store.

## Bug fixes

* In a narrow window with the sidebar open, the board fits the window:
  its card no longer runs off the right edge on every tab, and the tab bar
  wraps (#125).

# targetsboard 0.2.0

## New features

* A Script tab: the script the board has loaded, as it is on disk, read
  only and coloured as R, with a Copy button; above it, where things are:
  the script, the project folder, the data store and its metadata,
  progress, values and workspaces, the `_targets.yaml` in use and the
  active `TAR_PROJECT`, each path copied with a click. It follows Load and
  edits to the files (#123).
* An outdated target says why: hovered on the board, and above its value
  in the preview. Its command changed, which function or object changed
  (`f` changed), what upstream is outdated, no record in the store, its
  cue, file, format or seed: targets' cues (`tar_sitrep()`), with what
  changed named from `tar_network()`. Computed in the background after the
  graph, which doesn't wait for it. In the preview, the reasons sit in a
  card in the outdated status's colour, each with an icon of its kind.
* Themes: the board's look is the "Targets board" theme, and a few
  Bootswatch themes, checked for readable text in light and dark mode, can
  replace it: pick one in the top bar, and the browser remembers it, or start
  the board with one with `options(targetsboard.theme = )`, e.g. `"Cosmo"`.
  A Bootswatch theme also colours the canvas and the minimap from its own
  colours, and a dark one keeps the page in dark mode and hides the switch.
  Six editor themes, Catppuccin, Tokyo Night, Nord, Gruvbox, Solarized and
  Glow Night (dark only), bring their own palettes, light and dark, to the
  page and the canvas; and "Flow", the board's own, takes React Flow's
  website's pink and slate.
  A theme other than "Targets board" colours the whole canvas from its own
  colours: the targets by status (its success, info, danger and warning),
  their outlines and labels, edges and running edges, the focus ring and
  selection, and the zoom controls, checked for contrast in both modes.
* The script the board draws is chosen in a bar above the board's tabs, as
  a browser's address bar: Refresh, the script's path, and Load. The path is
  free text. Focused, it lists the known scripts, filtered as one types;
  Enter, or a pick from the list, loads it. The sidebar keeps the run controls.
* The focused target's toolbar has a Copy button, which copies its name,
  quoted as an R string, as "Copy its name" in its right-click menu. A
  selected group's toolbar has none.
* The value preview shows a value in a frame: a header with its class, shape,
  size in the store, format and build time, its `tar_read()` call on a button
  that copies it, with a copy icon, and a
  tab per view of the value, the last one "Printed", with what `print()`
  shows. A preview that fails falls back to Printed with a note, and the
  frame opens full-screen.
* `tar_board_preview()` is a public hook: a method for a class gives the
  views of its values, and `tar_board_preview_plot()` makes a plot one of
  them. Methods come from packages, or from a project's `_targetsboard.R`,
  which the board reads at start and on Refresh.
* A data frame previews as two tables: Rows, its first 1,000 rows, 10 a
  page, with each column's type and, for numbers and dates, a histogram of
  all its rows in the header, searchable, filterable by column and sortable;
  and Columns, one line per column
  with its type, missing and unique counts, a summary and a small histogram.
  A vector previews as its summary and its distribution, with its names or
  its values' counts (its values are in Printed); a single value shows in
  full. Tables follow dark mode. skimr is no
  longer suggested.
* A plot in the value preview is drawn at the preview's width, 0.65 of it
  high, at twice the pixels, with ragg if installed; its text reads at the
  size its theme gives. A plot or an htmlwidget fills the frame in
  full-screen, and Escape leaves full-screen without closing the preview.
* A file target previews its files by type: images, PDF and HTML files in
  place (a PDF as images of its first 20 pages, with pdftools, in viewers
  without a PDF viewer, such as RStudio's and Positron's), CSV and TSV files
  as tables, with their rows and columns counted, Markdown rendered, a Quarto
  or R Markdown file as the document it was rendered to (or its source, with
  its chunks as code, if it wasn't), and the first 200
  lines of text files. Several files show as sections, 50 at a time, each
  read only when opened. Each path is dispatched as
  `tar_board_file_<extension>`, so a method adds a type.
* R code is in colour: R files in the preview, and the Problem pane's code
  and traceback (highlight.js, bundled).
* A list previews as a tree, two levels deep and 50 items a level, each
  item with its type and first values. A model previews as its estimates and
  fit, from broom when it knows the model (lm, glm, tests, kmeans and many
  more), or else as its `summary()`; other objects made of a list, as a tree.
* The preview gallery, an example pipeline with one target per kind of view
  (`TAR_PROJECT = "preview_gallery"` in `inst/examples/pipelines`), is built
  and checked by the tests.
* A pattern previews one branch at a time, as `tar_read(name, branches = i)`
  reads it, opening on the first, with previous and next and a branch
  number; "All branches" combines them while their size is under the
  preview's limit.
* Opening a value preview no longer runs the pipeline's script, unless the
  target has a problem to show: it took about 2 s for every target, and
  4 s for a pattern.
* The value preview reads a value larger than 100 MB in the store only when
  asked; set the limit with `options(targetsboard.preview_bytes = )`. It shows
  a spinner while it reads, and keeps the last few previews, so going back to
  a target is instant until it is built again.
* `tar_board()` and `tar_board_app()` gain `scripts`: other targets scripts
  to switch to. The Script field lists them under `script`, the one shown on
  launch and filters them as you type; only these can be chosen. Load
  switches to the chosen one.
* Load shows it is loading until the chosen script's graph is drawn or the
  script turns out not to load: a large pipeline can take a while. The button
  shows a spinner, and a shinycssloaders spinner over the Board pane keeps
  the old graph and its preview from being used meanwhile.
* Load asks before drawing a script with more than 500 targets on its graph,
  which can use up the browser's memory; Cancel keeps the current graph. Set
  the limit with `options(targetsboard.confirm_nodes = )`.

## Minor changes

* A brand: the Pulse mark, a live signal reaching a target, in one
  colour. With the name it makes the wordmark that replaces the top
  bar's title, in the theme's text colour; it's the browser tab's icon; the loading spinner is the mark,
  animated; and a new hex sticker carries it, in the style of targetfy's
  (#105). The scripts that make every file from the one mark, and the other
  concepts, are kept on the `logo-concepts` branch.
* The Value pane's button that copies the `targets::tar_read()` call is in
  the theme's red, its danger colour, so it stands out; readable in every
  theme and mode (#104).
* The confirmation before invalidating counts the targets in its title
  ("Invalidate 5 targets?"), lists up to 50 of them in a list that scrolls,
  and closes on Escape or a click outside. Modals have a compact title
  with an icon, and dividers, like the board's cards.
* The board's look is evened out: one type scale (`--tb-font-size-*`),
  Bootstrap's radii, sizes in rem, one button style in two sizes (the
  page's, bslib's default; the canvas's, the toolbar's, which Tidy now
  shares), and the app's title above the sidebar's in weight and size. The
  canvas's menus and buttons take the page's Bootstrap surfaces in light
  mode too, so they follow a Bootstrap theme.
* The graph's edges, targets' outlines and handles, the running spinner and
  the group-selection box are colour tokens too; in dark mode the edges are
  lighter, as React Flow's nearly vanished on the canvas.
* The board's colours (canvas, statuses, menus, toolbars) are colour tokens,
  `--tb-*` CSS variables with light and dark values, in one place, so the
  board's look changes there only.
* The graph opens fitted to the view, as the Fit View button makes it: on
  first render, with a saved layout too (the targets go back where they were
  left, but the saved zoom and pan no longer come back), and when Load loads
  another pipeline, which is now laid out afresh.
* The JavaScript libraries the package bundles, React, React Flow and
  others, are credited in Authors@R and listed with their licences in
  `inst/COPYRIGHTS`. The page pins Bootstrap 5.
* The board's app is built of Shiny modules, one per part: the script,
  runs, graph, value preview and each tab. Its input and output ids are
  namespaced, under `board-` (`board-graph-flow`, `board-run-all`).
* Run all keeps its plain label: it no longer names the run modes that are
  on ("Run all · keep going"). The switches below it show them.

## Bug fixes

* Copy buttons copy in IDE viewers, such as Positron's and RStudio's, which
  show the board in an iframe that the clipboard API refuses: the board
  falls back to copying a selected text (#118). The same goes for copying
  targets' names from the canvas.
* Dark mode: the Problems and `tar_meta()` tables use the value preview's
  colours, so their text no longer sits light on white; the focused target's
  toolbar and the right-click menu take the page's dark surfaces; the
  `tar_visnetwork()` tab's labels and legend are light; an error's message is
  Bootstrap's danger text on its subtle background; and the outline buttons
  (Refresh, Load, Copy) no longer look disabled.
* In dark mode, the ring around a target's warning mark is the canvas's
  colour, not the light canvas's beige.
* Sorting a table in the value preview no longer moves the sorted column's
  name and histogram aside: the sort arrow sits in the header's corner.
* `update_targetsboard()` and the widget's other update and command
  functions find a widget inside a Shiny module: they address it by its
  namespaced id, as the widget knows itself.
* On a narrow window the Board pane keeps a usable height. It collapsed to a
  strip above the Control Panel.
* The Load spinner no longer covers the Board pane for a moment when the
  board opens, which blocked clicks and drags until the server's first
  update. It shows only while Load loads.
* A pattern whose run stopped at a branch's error (targets' default,
  `error = "stop"`) shows that branch in its Problem: its error, traceback
  and code to copy. It showed "No value stored for this target yet", because
  the pattern has no metadata of its own then.
* The code a Problem offers to copy (Reproduce and Debug run) runs as pasted
  for a script other than `_targets.R` or a store other than `_targets`: it
  names them, and says which folder to run it from. Reproduce failed with
  "cannot open file '_targets.R'" there.
* A script that stops loading, such as one with a typo, is notified, with
  its error, until it loads again. The board kept its last graph without a
  word.
* The Script field names the script the board draws, in every tab: a tab
  opened after a Load, or open during one, showed the launch script over the
  loaded script's graph.
* Invalidating a target that errored shows it as outdated. It stayed red,
  because `targets::tar_invalidate()` keeps its line in the progress file: a
  target now shows as errored only while the metadata still records its
  error, or one of its branches'.

# targetsboard 0.1.0

First release: the 2024 prototype, cleaned up into a board for the author of
a pipeline, with runs, selection of several targets and debugging.

## Breaking changes

* The public API is `tar_board()` and `tar_board_app()`; the widget,
  `targetsboard()`, with `update_targetsboard()`,
  `update_targetsboard_status()`, `update_targetsboard_focus()`,
  `update_targetsboard_selection()`, `update_targetsboard_options()`,
  `targetsboard_options()`, `targetsboard_fit_view()`,
  `targetsboard_export_image()` and `save_targetsboard_image()`; and
  `debug_run_script()`. The prototype's `f()`, `%not_in%` and `%<-%` helpers
  are no longer exported.
  `app_ui()`, `app_server()`, `tar_visnetwork_bg()`, the
  `dataframe_to_reactflow_*()` converters and `render_and_output_bundle()`
  are internal, and the non-working React component wrappers (`MiniMap()`,
  `Controls()`, ...) and htmlwidget bindings are gone.
* `targetsboard()` and `update_targetsboard()` take the `nodes` and `edges`
  data frames of `targets::tar_visnetwork()$x`; `update_targetsboard()` no
  longer takes `value` and `configuration`.
* `tar_board()`'s `metadata` argument moved after `targets_only`.
* The widget reports its selection as `input$<inputId>_selected_nodes`
  (was `input$selected_nodes`).
* The widget calls running targets "running": their node type is `running`
  (was `dispatched`) and it reports them as `input$<inputId>_running_nodes`
  (was `input$<inputId>_dispatched_nodes`; `_dispatched_nodes_incomers` is
  gone). Targets can no longer be connected by dragging between them.
* The board's value preview is driven by the focused target, not the first
  selected one.

## New features

* `inst/examples/pipelines/` has pipelines to try the board with, each a
  project of its `_targets.yaml` with its own store: a bootstrap with
  dynamic and static branching, row groups, batches and values of unusual
  types; one pipeline per situation the tests cover; and two bootstraps at
  scale.
* While a run goes, a pattern shows how many of its branches are done, e.g.
  "3/6", in red once one errored. `update_targetsboard_status()` gains
  `branches`, for the counts of `targets::tar_progress_branches()`.
* A pattern carries the warning mark when its branches warned: targets
  records the warnings on the branches alone.
* **Run all** names the run modes that are on, e.g. "Run all · keep going",
  and the Log tab starts with what the run was: when it started, what it
  builds and its modes.
* The Copy buttons of the Problem pane say "Copied", or that copying failed.
* The Tidy buttons look like the graph's other controls, in light and dark
  mode, and no longer sit under the value preview's toggle.
* A pattern whose branches errored shows only its problem, without "No value
  stored for this target yet." under it.
* A **Problems** tab lists the targets that errored or warned in their
  latest build, errors first, with a count in its title; a pattern counts its
  branches with problems. A click shows the target on the board with its
  problem. A **Log** tab shows the output of the board's latest run.
* When a board run ends with errors, also a run that keeps going after
  them, its notification names the targets that errored and links to the
  first one's problem.

* The value preview of a target that errored or warned starts with its
  problem: the error or warnings, the traceback (the pipeline's own functions
  shown, calls in packages folded), the seed and links to its inputs. Two
  snippets to copy into the console follow: **Reproduce** loads what the
  target had (its workspace, or its inputs, globals and seed) and runs its
  command; **Debug run** runs the pipeline in your session, stopped in the
  debugger inside the target. A pattern lists its branches with problems,
  each with its own. New `debug_run_script()` writes the script a debug run
  uses, so your own script is never changed.
* Two run modes, switched on in the sidebar under **Run all**, change how the
  board's runs go, for debugging: **Keep going after errors** builds the other
  targets after one errors, so a run shows every error at once; **Stop at
  warnings** turns a target's first warning into an error, which leaves a
  workspace and a traceback where it warned. They apply to every run the
  board starts and are off on every start. The board never edits the script:
  the run loads it through a wrapper that changes its targets (ADR 0002). If
  a version of targets changes what the wrapper relies on, the switches are
  disabled and say what to add to the script instead.
* Several targets can be selected and handled as a group: Ctrl-click (Cmd-click
  on a Mac) adds a target, Shift-drag selects those in a box, and the menu's
  "Select what it depends on", "Select what depends on it" and "Select its
  lineage" select at every level. A group keeps its lineage highlighted, gets a
  toolbar of its own (Run, Invalidate) and, with a right-click on one of its
  targets, the group's menu, without Value. A right-click on any other target
  selects it alone, and Escape clears the selection and the focus. "Copy its
  name" copies R code: `"model"`, or `c("a", "b")` for a group. In the widget,
  the new `node_group_actions` and `toolbar_group_actions` of
  `targetsboard_options()` hold a group's menu and toolbar, `<id>_node_action`
  gains `nodes`, every target an action covers, and the new
  `update_targetsboard_selection()` selects targets from the server.
* The board remembers where you left the targets. It saves their positions,
  the view and the direction of the last layout in
  `_targets/user/targetsboard_layout.json` after each drag, pan, zoom or tidy,
  and opens with them, also on a reload. Targets new since then are placed
  next to their neighbours; the "Tidy" buttons still lay the graph out afresh.
  The widget reports its layout as `input$<inputId>_layout`, and
  `targetsboard()` takes one back as `layout`.
* The widget draws the whole graph as an image, whatever the zoom:
  `targetsboard_export_image()` asks it for a PNG or an SVG, which arrives as
  `input$<inputId>_image`, and `save_targetsboard_image()` writes that to a
  file. With the new `download_image` of `targetsboard_options()`, a button in
  its controls downloads the PNG; the board has it.
* A toolbar over the focused target, from the new `toolbar_actions` of
  `targetsboard_options()`, reports its buttons as `<id>_node_action`, like
  the right-click menu. The board's toolbar runs the target, invalidates it
  (after confirmation) or shows its value.
* `update_targetsboard_focus()` focuses a target from the server, and
  `targetsboard_fit_view()` brings some targets, or all, into view. On the
  board, a click on a row of the **tar_meta()** table shows its target on the
  **Board** tab: focused, with its direct neighbours in view.
* The widget reports more to the server: `<id>_double_clicked_node`,
  `<id>_context_node` (a right-click, which also focuses the target),
  `<id>_viewport` after each pan or zoom, and `<id>_node_action` when an item
  of a target's right-click menu is chosen. The menu's items come from the
  new `node_actions` of `targetsboard_options()`; `copy_name` copies the
  target's name in the browser.
* On the board, a click only focuses a target; a double-click shows its value.
  Right-click a target to run it, invalidate it (after confirmation), show its
  value or copy its name.
* `targetsboard()` takes display options from the new
  `targetsboard_options()`: `color_mode` (light, dark or the system's),
  whether to show the minimap and the zoom controls, the `background` pattern,
  the `edge_type`, and `only_render_visible` for big pipelines. The new
  `update_targetsboard_options()` changes some of them from the server. The
  minimap colours targets by status, and the board has a dark mode switch in
  its top bar.
* The board's page is one card of tabs: **Board**, the board's own graph;
  **tar_visnetwork()**, the graph as that function draws it, the view
  `tar_watch()` shows; and **tar_meta()**. An **Internals** tab, with the
  app's own inputs, shows only with `options(targetsboard.dev = TRUE)`, to
  develop the board. The node table is gone. A
  power button in the top bar turns the board off, which also ends
  `tar_board()`'s background process.
* An **Invalidate all** button, under **Stop**, makes every target outdated
  with `targets::tar_invalidate()`, so the next run builds them all again;
  until then, their values can no longer be read. It asks for confirmation,
  is disabled while a run is going, and refreshes the graph. Some targets are
  invalidated from their menu or toolbar.
* The graph is laid out like `tar_visnetwork()`'s: each target in the column
  of its level, and each column stacked compactly and centred, with edge
  crossings reduced. Dagre, which spread dense pipelines (every target of a
  layer feeding every target of the next) into a wide diamond, is gone. The
  board can also zoom out far enough to show big pipelines whole.
* Like `targets::tar_watch()`, `tar_board()` serves the board on a random
  free port (or the `shiny.port` option) instead of 9999, takes a `host`
  (or the `shiny.host` option), waits until the board is up, opens it in the
  browser (`browse = TRUE`) and prints its url and log (`verbose = TRUE`).
  The url is kept on the returned process: `browseURL(attr(app, "url"))`.
* Like `targets::tar_watch()`, `tar_board()` and `tar_board_app()` need no
  arguments: `script` defaults to the project's configured script
  (`targets::tar_config_get("script")`, so `_targets.yaml` is respected)
  instead of `"_targets.R"`. Without a script yet, the board starts empty
  and draws the pipeline once the script appears, instead of erroring.
* Clicking a target focuses it: what it depends on and what depends on it
  stay highlighted and the rest of the graph dims. The widget reports it as
  `input$<inputId>_focused_node`; clicking the background clears it.
* Targets whose last build raised warnings carry a warning mark. The widget
  functions take `warnings`, and `update_targetsboard_status()` can send
  the marks alone.
* When the pipeline changes, targets already drawn keep their place and only
  new ones are placed, next to their neighbours. The layout buttons are now
  **Tidy vertically** and **Tidy horizontally**, to lay out everything again.
* The project's language is in `GLOSSARY.md`, and its scope in
  `docs/adr/0001-local-read-only-companion.md`.
* `tar_board_app()` returns the board as a Shiny app object, to run in the
  foreground or deploy.
* The board follows changes to the pipeline itself: added, removed and
  rewired targets appear in the DAG.
* A **Load** button switches the pipeline script; the script box no longer
  reloads on every keystroke.
* The board's output goes to a log file: `readLines(app$get_output_file())`.
* `update_targetsboard_status()` updates target statuses in place, without
  resending or laying out the graph again.
* A **Refresh** button recomputes the graph on demand.
* **Run all** and **Stop** buttons run the pipeline from the board:
  `tar_make()` of everything; a target's or a group's menu and toolbar run
  just those and what they depend on. Stop also stops a run started outside the board. The
  buttons follow whether a run is going, and the board tells how its own runs
  ended.
* The board tells a running pipeline by its process (`tar_process()`), not
  by targets marked dispatched: a killed run's targets show as canceled
  instead of running forever.

## Bug fixes and performance

* The board no longer opens, now and then, zoomed all the way in on a
  corner of the graph: the first fit of the view raced the first layout.
* Backspace and Delete no longer remove the selected targets and their edges
  from the board. React Flow deletes them on those keys by default, which a
  read-only board must not do.
* `tar_board()` lists `parallelly` among its dependencies. Its default port
  comes from `targets::tar_random_port()`, which needs `parallelly`, but
  `targets` only suggests it, so on a fresh install `tar_board()` failed with
  "The package "parallelly" is required".
* `tar_board()` errors when its port is already in use, typically by a
  board started before, instead of returning a background process that has
  already died.

* The background poller no longer keeps a CPU core busy, is shared by all
  browser sessions and stops with the app. It no longer recomputes
  `tar_visnetwork()` every second: only when the script or a file in `R/`
  changes, when a run ends, or on **Refresh**. In between, the board follows
  runs through the cheap `<store>/meta/progress` file, twice a second. A
  broken script no longer stops it.
* Every targets status has a node style: `completed`, `skipped`, `canceled`
  and others no longer fall back to React Flow's default node.
* The board no longer freezes after a while (unread output pipes) or errors
  on half-written files.
* Only the previewed HTML report's folder is served over HTTP, not the whole
  working directory.
* Per-status text colours, the initial layout and the animation of the edges
  feeding running targets work reliably; the tar_meta table keeps its sorting
  and scrolling.
* All dependencies are declared; golem is no longer needed. ggplot2, plotly
  and skimr are optional.
