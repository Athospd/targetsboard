# How far is targetsboard from Airflow?

The first take on the question, kept as given on 22 November 2026. The author
asked: "read the docs of Airflow; I have an ambition of evolving the
targetsboard to serve as a competitor to Airflow. How far I am from it?"
Its conclusions led to `stage-two-runs-without-the-author.md`.

**What it compares:**
- targetsboard 0.1.0, with the fixes of PRs #42 to #44.
- Apache Airflow 3.4.0, from the stable docs at https://airflow.apache.org/docs/.

**Pages read:**
- the overview: https://airflow.apache.org/docs/apache-airflow/stable/core-concepts/overview.html
- DAGs: https://airflow.apache.org/docs/apache-airflow/stable/core-concepts/dags.html
- executors: https://airflow.apache.org/docs/apache-airflow/stable/core-concepts/executor/index.html
- asset scheduling: https://airflow.apache.org/docs/apache-airflow/stable/authoring-and-scheduling/asset-scheduling.html
- deferring: https://airflow.apache.org/docs/apache-airflow/stable/authoring-and-scheduling/deferring.html
- language SDKs: https://airflow.apache.org/docs/apache-airflow/stable/authoring-and-scheduling/language-sdks/index.html
- the release notes: https://airflow.apache.org/docs/apache-airflow/stable/release_notes.html

The pages on pools, retries, connections, the REST API and auth were not read; what this says about them is general knowledge.

---

**Short answer:** as a UI on top of a DAG, you're fairly close. As an orchestrator, you're far: most of Airflow's weight is in layers you haven't started. That's less bad than it sounds, because `targets` and `crew` already cover a good part of the middle.

The one structural difference that matters: Airflow is a **scheduler of runs**. Every run is a dated record, kept forever, started by a clock, an event or a person. `targets` is a **build system**: it keeps only the latest state and skips whatever is up to date. Most of the gap comes from that difference, not from missing buttons.

I compared against the Airflow 3.4 docs: the overview, DAGs, executors, assets, deferral, language SDKs and release notes. I didn't read the pages on pools, retries, connections, the REST API and auth; what I say about those is from general knowledge.

**Gap map**

| Airflow area | Airflow | What you have today | Distance |
|---|---|---|---|
| **Authoring the DAG** | Python DAGs, TaskFlow, dynamic task mapping, task groups, 16 trigger rules | `targets`: inferred dependencies, dynamic branching, `tarchetypes` | **Covered by `targets`**, plus something Airflow lacks: skipping up-to-date work |
| **Graph and run UI** | Graph and Grid views, logs, Code view, assets view, backfill from the UI | Live board: statuses, branch counts, value previews, Problem pane with traceback and reproduce code, group actions, run modes | **Close for one run, far for history**: Airflow's Grid view is *runs × tasks*, and you have one run |
| **Execution** | Local, Celery, Kubernetes, ECS and Edge executors, several at once, chosen per task | `tar_make()` from the board; `crew` and `clustermq` could do distributed runs | **Partly covered by `targets`/`crew`**; the board doesn't expose workers or resources yet |
| **Reliability** | Retries, timeouts, pools, priority, sensors, deferrable tasks, draining | `error = "continue"`, Stop, Stop at warnings | **Far**: no retries, pools or waits on outside events |
| **Scheduling** | Cron, timetables, data intervals, catchup, backfill, asset-driven runs, triggers with params | Manual runs, by a person at the board | **Not started**, and it's the core of Airflow |
| **Run history and state** | Metadata database (Postgres) of every DAG run, task try and log | The store's latest `tar_meta()` and progress, plus one Log tab | **Not started**, and everything else depends on it |
| **Always-on server** | Scheduler, DAG processor, triggerer, API server and workers as separate services | A Shiny app plus a background process per app, alive only while the board runs | **Far** |
| **Multi-user operation** | Auth managers, roles, multi-team mode, audit logs | One author, locally | **Not started** (deliberately, for stage one) |
| **Integrations** | REST API, CLI, Python client, ~110 providers, connections, secrets backends | R packages do the integrating; no API or connections | **Far**, though R's packages make up for some of it |
| **Alerting and observability** | Callbacks, deadline alerts, metrics, OpenLineage | Notifications in the browser only | **Far** |
| **Deployment** | Official Docker image, Helm chart, managed offerings | `install_github()` and `tar_board()` | **Far** |

**In stages**, from your memory note's vision ("an Airflow-class orchestrator for analytical DAGs"):

1. **Stage one, a local board for one author:** essentially done and tested. Today's three fixes and the earlier scale work polish it.
2. **Stage two, a server that runs pipelines without you:** the big jump. It needs three things:
   - a daemon that outlives the browser;
   - a **run-history store**, because `targets` overwrites its own;
   - scheduling: cron first, then "run when this file or table changes", which is Airflow's assets.

   That's a new architecture, not a feature. I'd estimate it at several times the work of stage one.
3. **Stage three, team operation:** auth, several projects, an API, alerts, retries and resource pools, a Docker image. That makes it an orchestrator.
4. **Stage four, parity:** executors on clusters through `crew`, an integration ecosystem, high availability. Airflow took a decade and hundreds of contributors to get there, so I wouldn't aim for parity.

**Positioning**

Win on what Airflow can't do, not on its own terms:
- **Airflow doesn't run R.** Its 3.4 language SDKs are Java, Go and TypeScript, and even those are experimental. R users get by with Bash or Docker tasks and lose all visibility inside them.
- **Airflow reruns everything on every run.** `targets` knows what is up to date, so a scheduled run that only rebuilds what changed is a real advantage for analytical work.
- **Airflow ships no debugger for the code inside a task.** Value previews, the traceback through your own code and one-click reproduce code are things Airflow users would want.

So the realistic goal is **"Airflow for R analytical pipelines"**: scheduled, observable, history-keeping runs of `targets` pipelines, possibly with Airflow as an optional outer scheduler.

**Next step:** the run-history store. Scheduling, the Grid view, alerting, retries and a multi-user server all need it, and it's the first thing that turns the board from a view of the current state into a record of runs.

---

## Appendix: Airflow 3.4 as the docs describe it

Kept as the reference this take compared against.

- **Components:**
  - The **scheduler** submits tasks; the executor runs inside it.
  - The **Dag processor** parses Dag files from a **Dag bundle**, such as a folder or a versioned Git repository, so the scheduler never runs author code.
  - The **API server** serves the REST API, the UI and the Execution API that tasks use.
  - A **metadata database** (PostgreSQL or MySQL) holds the state.
  - Optional parts: **workers** (Celery, Kubernetes) and the **triggerer**, which runs deferred triggers in asyncio. Each task runs in a new subprocess.
- **Authoring:**
  - Dags are written with `with DAG(...)` or `@dag`; tasks are operators or TaskFlow `@task`.
  - Dependencies use `>>`.
  - Data passes as XComs, explicitly or implicitly through TaskFlow.
  - **Dynamic task mapping** (`.partial().expand()`), TaskGroups, 16 trigger rules, branching, `depends_on_past`, and setup/teardown tasks.
- **Scheduling:**
  - A `schedule` is cron, a preset, a timedelta, a timetable or an asset expression.
  - Runs have a **data interval** and a **logical date**; catchup and backfill are supported.
  - **Assets**: a task names them in `outlets=[...]`, and a Dag runs when the assets it consumes update, combined with `&` and `|`. There is also `AssetAlias`, and asset events can be pushed through the REST API or pulled with an `AssetWatcher`.
  - Manual triggers can take params. **Draining** (new in 3.4) lets active runs finish, then pauses the Dag.
- **Execution:**
  - Executors: Local (the default), Celery, Kubernetes, Batch, ECS and Edge. Several can run at once, chosen per task.
  - Sensors wait for events; **deferrable operators** free their worker slot while a trigger waits.
  - Retries create a new try. Pools, priorities, timeouts and **deadline alerts** are available.
- **Operations:**
  - UI: Graph view, Grid view (runs × tasks, with logs), Dag overview, Code view, asset views, and backfill from the UI.
  - Integration: the REST API, a Python client, `airflowctl` and the CLI.
  - Access: auth managers (simple, or FAB with roles) and a multi-team mode.
- **Ecosystem:** about 110 provider packages (AWS, Google, Azure, Databricks, Snowflake, Kafka, Spark, Slack, OpenLineage, and others), an official Docker image and a Helm chart.
- **Other languages:**
  - Experimental since 3.4: tasks in Java, Go and TypeScript, through coordinators and `@task.stub`, and whole Dags in those languages or in YAML.
  - Several features are unsupported in those SDKs, such as Dag params, asset scheduling and mapped tasks.
  - There is no R SDK.
