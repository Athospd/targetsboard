# dev/projects/: trying targets projects

A playground for the board's support of targets projects (`_targets.yaml`, `TAR_PROJECT`; #114, released in 0.3.0).

- `setup.R` builds `sandbox/`, a pipeline folder with four projects:
  - `main`, built;
  - `fast`;
  - `slow`, whose targets sleep 3 s each, so a run lasts long enough to watch;
  - `slow_copy`, which inherits `slow` but keeps its own store.

  There is also `other.R`, which no project names. Running it again starts over.
- `play.R` goes through the scenarios one section at a time, and each section says what you should see. They cover:
  - starting with no arguments;
  - switching project in the board;
  - Load following a script's project;
  - the field held during a run;
  - `inherits`;
  - `_targets.yaml` edited live;
  - starting on a project by argument or with `TAR_PROJECT`;
  - an unknown project;
  - copied code naming the store.

`sandbox/` is ignored by git. The scripts use the installed targetsboard, so after a change install it again and restart R.

## What targets projects are, in brief

`_targets.yaml` can name several projects. Each one sets targets options, mostly `script` and `store`:

```yaml
fast:
  script: fast.R
  store: _targets_fast
slow_copy:
  inherits: slow
  store: _targets_slow_copy
```

- `TAR_PROJECT` picks the active one (`"main"` when unset), and every `tar_*()` call follows it.
- `inherits` takes another project's settings.
- `targets::tar_config_set(..., project = )` writes the file.
- `targets::tar_config_get("store", project = )` reads it.
- See the targets manual: https://books.ropensci.org/targets/projects.html

## Where the board keeps its project

The board keeps its project in its own process's `TAR_PROJECT`. The poller and the runs inherit it from there. Your R session's `TAR_PROJECT` is never changed. The code is in `R/projects.R`, and the Project field is in `R/mod_script.R`.
