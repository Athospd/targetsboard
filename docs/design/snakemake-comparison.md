# Snakemake, and a targetsboard for it

The first take on two questions, kept as given on 22 November 2026, after the
comparisons with Airflow and Prefect. The author asked: "Now a comparison with
python's snakemake. Here I would also ask: what if I wanted to make a python
version of targetsboard for snakemake? would it be possible?"

**What it compares:**
- targetsboard 0.1.0, with the fixes of PRs #42 to #44.
- Snakemake 9.27.2.

**How Snakemake was checked:**
- Installed in a throwaway virtual environment on Windows.
- A three-rule workflow (`raw` → `count` → `summary`, with a `{s}` wildcard) was run.
- Its params and code were edited, then these were read:
  - the dry-run's reasons and `--summary`;
  - the metadata kept per output file;
  - the methods of `snakemake.api.DAGApi`;
  - the members of `LogEvent` in `snakemake-interface-logger-plugins` 2.1.0;
  - the JSON that `--d3dag` prints.

**Docs pages read:**
- the docs index: https://snakemake.readthedocs.io/en/stable/
- the changelog: https://snakemake.readthedocs.io/en/stable/project_info/history.html
- the tutorial: https://snakemake.readthedocs.io/en/stable/tutorial/basics.html
- the plugin catalog: https://snakemake.github.io/snakemake-plugin-catalog/
- the `snkmt` and `panoptes` logger plugins, in the catalog.

---

**Short answer:** Snakemake is the closest relative `targets` has. Both are make-style build systems with a static DAG that skip work already up to date, so it doesn't compete with targetsboard on the same axis as Airflow or Prefect. And yes, a Python board for Snakemake is clearly possible. I checked by installing Snakemake 9.27.2 and running a small workflow: every piece a board needs is there, and a few are better than what `targets` offers.

## Snakemake compared with `targets` plus the board

| | Snakemake 9.27 | `targets` + targetsboard |
|---|---|---|
| **What a step is** | A rule with input and output **files**; wildcards (`{sample}`) expand one rule into many jobs | A target: an R **object** or a file; patterns expand into branches |
| **How the DAG is found** | By matching file names | By reading the R code's dependencies |
| **Rerun logic** | `--rerun-triggers`: `mtime`, `input`, `params`, `code`, `software-env` | Hashes of commands, dependencies, functions and globals |
| **Why a job reruns** | Very specific. I saw "Params have changed since last execution: … before: 2 now: 3", "Input files updated by another job: count/a.txt", and in `--summary`, "rule implementation changed" | Coarser: the board shows "depend changed" |
| **Execution** | Strong on clusters and clouds: executor plugins for SLURM, Kubernetes, AWS Batch, Google Batch, HTCondor, LSF and more, with a conda environment or container per rule | `crew` and `clustermq` |
| **Live view of a run** | None built in. Third-party logger plugins: `panoptes` (sends events to its own server), `snkmt` (under development), and others not yet checked (`flowo`, `snakesee`) | The board |
| **Reports** | An HTML report (`--report`) built after the run | Previews of every value, live |
| **Debugging** | Log files, the failed shell command, a debugging guide | Problem pane, workspaces, debug runs |

Snakemake is stronger at scaling out and at saying exactly why a job reruns. You're stronger at the live, interactive side, which Snakemake leaves to third parties: watching a run, seeing every value, and debugging. That gap is exactly a board.

## A Python targetsboard for Snakemake

Tested here:

| What the board needs | Snakemake 9.27, tested here | Fit |
|---|---|---|
| The graph without running | `snakemake.api` → `DAGApi.printd3dag()` gives JSON (`nodes`, `edges`); also `printrulegraph`, `printfilegraph` | Good, though the JSON is thin; full detail needs Snakemake's internal DAG |
| Status, and why outdated | `DAGApi.summary()` (per output: "ok", "params changed", "rule implementation changed", "missing", plus the plan); the dry-run's reason per job; `list_changes` | **Better than `targets`** |
| What each output's last build was | Metadata per output file: `code`, `params`, `input_checksums`, `shellcmd`, `starttime`/`endtime`, `log`, `incomplete`, `software_stack_hash` | Good: durations and "what changed" for free |
| Live progress | A logger plugin (`--logger`) receives typed events: `JOB_STARTED`, `JOB_FINISHED`, `JOB_ERROR`, `SHELLCMD`, `PROGRESS`, `RULEGRAPH`, `RUN_INFO` and others | **Better than `targets`'** progress file: pushed events, not polling |
| Run, Stop, Invalidate | `execute_workflow`; `--keep-going`, `--forcerun`, `--until`; `delete_output`; `unlock`; Stop = kill the process, with `--rerun-incomplete` afterwards | Good |
| Value previews | Outputs are files: previews by file type (CSV and Parquet as tables, images, HTML, text) | Good, and simpler than previewing R objects |
| Problem pane | `JOB_ERROR` plus the rule's `log:` files and its shell command | Partial: no workspaces |
| Patterns and branches | Wildcard jobs folded by rule, like a pattern on the board | Good match |

**What doesn't carry over:**
- **Workspaces and debug runs:** Snakemake runs files and shell commands, so there's no "load the inputs into your session". The closest is giving the exact command to rerun one job (`--forcerun` plus `--until`).
- **Stop at warnings:** no equivalent, because jobs have exit codes, not R warnings.
- **Checkpoints:** these let the DAG change mid-run, so the graph has to be rebuilt during a run, which a `targets` board never does.

**The hard parts:**
- **Internal API:** a full-detail graph uses Snakemake's internal DAG object, which can change between versions. This mirrors ADR 0002's wrapper problem, and needs the same fix: confine it to one small, tested layer.
- **Cost of building the DAG:** big workflows can take seconds to minutes, so the graph needs the same background poller you already have.
- **Existing tools:** `panoptes` and `snkmt` exist. Look at them before building, though neither seems to aim at the interactive board you have.

## How I'd build it

Share the front end and the status contract, and write one back end per tool.
- **The front end is reusable as is.** The React Flow widget in `srcjs/` is plain React; only its `htmlwidgets` binding is R.
- **One contract both back ends speak:** nodes, edges, statuses, reasons, branch counts, problems and previews.
- **Hosting:** under Shiny for Python, it keeps the same custom messages (`targetsboard:status` and the rest).
- **The Snakemake back end, in Python:**
  - the poller builds the graph and `summary()`;
  - a small logger plugin writes live events;
  - a runner wraps `execute_workflow`.

That turns targetsboard into a board for make-style pipelines in both R and Python, a much bigger audience. It's also a decision worth an ADR, because it would freeze today's internal widget messages into a public contract.

**Before committing, a throwaway prototype:** Shiny for Python, the existing JS bundle, a logger plugin and the three-rule workflow I tested here. A day's work would show whether the widget runs unchanged and whether live events arrive cleanly.

**Not verified:** the docs pages on checkpoints, the per-rule `log:` and `benchmark:` directives, and `--report`. Everything in the second table I ran myself, on Windows, with a small workflow, not a large one.

---

## Appendix: what the check found

**Reasons for a rerun, as Snakemake printed them:**
- From the dry-run, after `params: factor` went from 2 to 3: "Params have changed since last execution: Union of exclusive params before and now across all output: before: 2 now: 3".
- For the jobs downstream: "Input files updated by another job: count/a.txt, count/b.txt".
- `--summary` (columns `output_file`, `date`, `rule`, `log-file(s)`, `status`, `plan`), after the params and code edits:
  - `ok` / `no update` for the untouched outputs;
  - `params changed` / `update pending` for the outputs whose params changed;
  - `rule implementation changed` / `update pending` for the output whose rule's code changed;
  - before the first run, `missing` / `update pending`.

**Kept per output file** (`.snakemake/metadata/`, file names base64 of the output path): `code`, `conda_env`, `container_img_url`, `starttime`, `endtime`, `external_jobid`, `incomplete`, `input`, `input_checksums`, `job_hash`, `log`, `params`, `record_format_version`, `rule`, `shellcmd`, `software_stack_hash`.

**`snakemake.api.DAGApi` methods:** `archive`, `cleanup_metadata`, `cleanup_shadow`, `conda_cleanup_envs`, `conda_create_envs`, `conda_list_envs`, `container_cleanup_images`, `containerize`, `create_report`, `delete_output`, `execute_workflow`, `export_to_cwl`, `generate_unit_tests`, `list_changes`, `list_untracked`, `printd3dag`, `printdag`, `printfilegraph`, `printrulegraph`, `summary`, `unlock`.

**Logger events** (`LogEvent`): `RUN_INFO`, `WORKFLOW_STARTED`, `SHELLCMD`, `JOB_INFO`, `JOB_ERROR`, `JOB_STARTED`, `JOB_FINISHED`, `GROUP_INFO`, `GROUP_ERROR`, `RESOURCES_INFO`, `DEBUG_DAG`, `PROGRESS`, `RULEGRAPH`, `ERROR`.

**`--d3dag`:** JSON with `nodes` (each `{"id", "value": {"jobid", "label", "rule"}}`) and `edges` (`{"u", "v"}`). It has no status, so statuses come from `summary()` or the dry-run.

**From the docs:**
- Version 9.0 (March 2025) brought the logging refactor with `LoggerPluginInterface`.
- The plugin types are executor, storage, report, logger and scheduler.
- Monitoring logger plugins: `panoptes` (sends events to a separate server) and `snkmt` (under development). Neither is official.
