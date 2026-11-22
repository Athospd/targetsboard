# Board runs change targets' settings through a wrapper script, by editing the target objects

Debugging needs runs that behave differently from `tar_make()`: keep going after an
error, turn a target's warnings into errors so they leave a workspace and a traceback,
or stop inside one target and skip the rest. In targets these are `tar_option_set()`
calls in `_targets.R`, and the board is read-only towards the author's code
(ADR 0001), so it never edits the script. Instead, a board run gives `tar_make()` a
temporary wrapper script that sources the author's script and returns its targets
changed. Each target fixes its settings when the author's script creates it, so a
`tar_option_set()` in the wrapper comes too late. The wrapper therefore edits
settings inside the target objects (`target$settings$error`, the cue, the command),
which belong to targets' internal structure, not its public API. That is confined to
one small layer, with a test that fails loudly when targets changes the structure.
Where the layer cannot apply a setting, the board shows the line for the author to add
themselves. We will ask targets upstream for a public way, which would retire the
layer.

## Considered Options

- **Edit `_targets.R`** to add the options for a run, then restore it: rejected. The
  board would write the author's code, and a crash mid-run would leave it changed.
- **`tar_option_set()` in the wrapper, around the author's script**: does not work.
  Settings are fixed when each target is created. Checked with targets 1.12.1:
  `error = "continue"` set after sourcing left the run stopping at the first error.
- **`options(warn = 2)` in the wrapper**, for stopping at warnings: does not work,
  because targets catches warnings itself, so they never become errors.
- **Only public options**: the debug target (`debug`), which targets reads at run time.
  Everything else would be lines for the author to add, so the run modes would not be
  switches at all.

## Consequences

An update of targets can break the run modes. The layer's test is what notices, and
until it is fixed the board falls back to showing lines to add. Debug runs started
from the author's console use the same wrapper, so they also depend on the layer.
