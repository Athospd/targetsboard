# Trying targets projects on the board (#114), step by step. Run a section
# at a time from the repository root, with the board open in your browser.
# Each step says what you should see. Uses the installed package: after a
# change, install it again and restart R.

library(targetsboard)
repo <- getwd()
sandbox <- source("dev/projects/setup.R")$value
setwd(sandbox)

# 1. Easy entry ---------------------------------------------------------------
# No arguments: the board starts on main, like tar_watch(). A Project field
# sits left of the script bar (there is more than one project), on "main".
app <- tar_board(port = 9999)

# 2. Switch in the board -------------------------------------------------------
# In the Project field, pick "fast": the graph turns to fast_a and fast_b,
# outdated, and the script bar says fast.R. Pick "main" again: green.
# The Script tab: its Data store row says _targets_fast while on fast, and a
# Project row gives Sys.setenv(TAR_PROJECT = "fast") to copy.

# Your session is not touched: still unset, whatever the board is on.
Sys.getenv("TAR_PROJECT")

# 3. Load follows the script's project ----------------------------------------
# Type slow.R in the script bar and press Load: the field jumps to "slow".
# Load other.R: the field stays where it is (no project names other.R), and
# its store is that project's.

# 4. A run holds the project --------------------------------------------------
# Pick "slow" and press Run all (about 12 s): the Project field greys out,
# with "Switch project after the run" on hover. Loading fast.R meanwhile
# says to load it after the run. Once it ends, the field is free again.
# The legend's Running count follows the run too.

# 5. inherits -----------------------------------------------------------------
# Pick "slow_copy": the same graph as slow (it inherits slow's script) but
# outdated: its store, _targets_slow_copy, is its own.

# 6. _targets.yaml changes while the board is open ----------------------------
# A new project shows in the field within a second:
targets::tar_config_set(script = "fast.R", store = "_targets_extra", project = "extra")
# Edit the file by hand and delete the project the board is on (say you are
# on "fast"): the field keeps it, labelled "fast (not in _targets.yaml)".
file.edit("_targets.yaml")
# No _targets.yaml at all: only main is left, and the field hides.
file.rename("_targets.yaml", "_targets.yaml.off")
file.rename("_targets.yaml.off", "_targets.yaml") # and back

app$kill()

# 7. Start on a project -------------------------------------------------------
# By argument: the board opens on slow, with slow.R in the bar.
app <- tar_board(project = "slow", port = 9999)
app$kill()
# Or as targets itself would, through TAR_PROJECT:
Sys.setenv(TAR_PROJECT = "fast")
app <- tar_board(port = 9999)
app$kill()
Sys.unsetenv("TAR_PROJECT")

# 8. A project that isn't there -----------------------------------------------
# An error in your session, before anything starts, naming the projects:
try(tar_board(project = "nope"))

# 9. Copied code names the project's store -------------------------------------
# On "fast", run it, then double-click fast_b: the value preview's Copy gives
# targets::tar_read(fast_b, store = "_targets_fast").

# Done -------------------------------------------------------------------------
setwd(repo)
# unlink("dev/projects/sandbox", recursive = TRUE)
