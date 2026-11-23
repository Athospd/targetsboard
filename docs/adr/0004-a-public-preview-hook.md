# A target's value is previewed through a public S3 hook that returns views, in a frame the board owns

The value preview had an internal generic that returned a Shiny render and output
pair per class. Authors could not add a preview for their own classes, and each
renderer had to do its own Shiny wiring. `tar_board_preview(x, target, ...)` is now
an exported S3 generic. A method returns one view, or a named list of views shown
as tabs. A view is HTML (a tag, a tag list or an htmlwidget) or a plot wrapped in
`tar_board_preview_plot()`, which the board draws at the preview's size. `target`
carries what the store knows: the name, the format, the size and the build time.
The board owns everything around the views: the frame with the target's facts and a
Copy for `tar_read()`, a last "Printed" tab with what `print()` shows, the fallback to
it alone when a method or a plot fails, the size check before reading a large value,
full-screen, the spinner and a cache of the last previews.

Methods come from packages, through `S3method()`, or from a project's
`_targetsboard.R`: the board sources that one file at start and on Refresh, and
registers the `tar_board_preview.<class>()` functions it defines.

## Considered Options

- **Keep the hook internal**: rejected. Every new class would need a change to
  targetsboard, and the targets-templates idea needs packages to bring their own previews.
- **Methods return Shiny render and output pairs, as before**: rejected. A method
  would have to know Shiny's output wiring, and could not be tested as plain R.
- **Plots returned as images by the method**: rejected. A fixed image cannot follow
  the preview's size, or full-screen.
- **Load the pipeline's globals into the board, to find methods defined in the
  pipeline** (`tar_load_globals()`): rejected. It would run the pipeline's package
  loads and side effects inside the board, which ADR 0001 keeps apart.

## Consequences

The generic, its arguments and the view contract are now public: changing them
breaks other people's methods, so they change only with a deprecation. A method in
`_targetsboard.R` replaces the board's own, or a package's, for the same class: the
generic looks it up first, by the value's full class, because R finds a method by name
in targetsboard's namespace before any registered one. Each reading of the file
replaces the methods read before, so a method removed from it is dropped. The frame reads a value
only after checking its size in the store (`targetsboard.preview_bytes`, 100 MB),
so a method can assume the value fits in memory.

## Later

- A JSON tree view for lists, offered as another view beside the tree.
- JSON and YAML file previews with their own formatting.
- The second tier of renderers: gt, gtsummary, flextable and kable tables;
  tidymodels workflows and tuning results; sf and terra maps; matrices; recorded
  base and grid plots.
