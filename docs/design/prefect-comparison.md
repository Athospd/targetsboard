# How far is targetsboard from Prefect?

The first take on the question, kept as given on 22 November 2026, right
after the comparison with Airflow (`airflow-comparison.md`). The author asked:
"Ok! Now, what about Python's Prefect?" Its lessons are folded into
`stage-two-runs-without-the-author.md`.

**What it compares:**
- targetsboard 0.1.0, with the fixes of PRs #42 to #44.
- Prefect 3.9, from the docs at https://www.prefect.io/docs.

**Pages read:**
- the intro: https://www.prefect.io/docs
- caching: https://www.prefect.io/docs/v3/concepts/caching
- states: https://www.prefect.io/docs/v3/concepts/states
- work pools: https://www.prefect.io/docs/v3/concepts/work-pools
- workers: https://www.prefect.io/docs/v3/concepts/workers
- schedules: https://www.prefect.io/docs/v3/concepts/schedules
- event triggers: https://www.prefect.io/docs/v3/concepts/event-triggers

**Not read:** flows and tasks, results, transactions, retries and concurrency limits, artifacts, the UI, and the self-hosted server. What this says about them is general knowledge or comes from the docs index.

---

**Short answer:** Prefect is closer to what targetsboard could become than Airflow is, and it's a better model for stage two. On features the gap looks like the Airflow one: no run history, scheduler, workers or automations. But Prefect shows the lighter path: start from a local process, keep history in a small database, and add servers only when needed.

Prefect is also built on the opposite bet from `targets` about DAGs, and that gives you a lasting advantage.

I compared against the Prefect 3.9 docs: the intro, caching, states, work pools, workers, schedules and event triggers. I didn't read the pages on flows and tasks, results, transactions, retries and concurrency limits, artifacts, the UI, or the self-hosted server. What I say about those comes from general knowledge and the docs index.

**The bet that separates them**
- **Prefect's bet:** for 2.0 it "removed a key constraint that workflows be written explicitly as DAGs, fully embracing native Python control flow". A flow is a plain Python function, and its graph is discovered while it runs.
- **What that costs Prefect:** it cannot tell you, before a run, what is outdated and why.
- **`targets`' bet:** the static DAG. That's exactly what the board shows best: statuses before running, the reasons a target is outdated, what each target produced, and skipping work that's up to date.

**Caching, the closest feature to `targets`**
- **What Prefect does:** it can skip a task. A cache hit puts the task in a "Cached" state with its stored value.
- **Limits of its default:** the cache is scoped to one flow run, because the default key includes the run's id. Reusing results across runs means choosing `INPUTS` + `TASK_SOURCE` yourself.
- **What its source hashing misses:** it covers only the task's own lines, not the functions it calls.
- **Persistence is off by default.**
- **Compared with `targets`:** `targets` tracks functions, globals and upstream hashes across runs, with no configuration. For analytical pipelines, that's a real edge.

**Gap map**

| Prefect area | Prefect 3.9 | targetsboard plus `targets` | Distance |
|---|---|---|---|
| Authoring | `@flow`/`@task` on plain functions; graph built at run time with `.submit`/`.map` | Static DAG inferred from code; dynamic branching | Different bet; yours explains staleness, theirs is more flexible |
| Skipping work | Opt-in cache policies; per run by default | Automatic, across runs, aware of dependencies | **Ahead** |
| Run history and states | Every flow run and task run kept, with a rich state model: Completed, Cached, Failed (code) vs Crashed (infrastructure), Late, AwaitingRetry, TimedOut | Latest state only | **Not started** (stage two, part 1) |
| Running without you | `serve` (a process serves schedules) or work pools with polling workers; push pools for serverless | Runs only from the board | **Far**; `serve` is the model to copy |
| Scheduling | Cron, Interval, RRule, with timezones; parameters per schedule | None | **Not started** |
| Events | Automations with Reactive and **Proactive** triggers (fire when expected events *don't* arrive), composite triggers, webhooks (Cloud) | None | **Not started** |
| Observability | Run graph and timeline, logs, **artifacts** (markdown, tables, images shown in the UI) | Live board, previews of every value, Problem pane | **Ahead for inspecting values**: you show every value automatically, while Prefect artifacts are created by hand |
| Operations | Self-hosted server (Postgres and Redis in the Docker Compose guide) or Prefect Cloud: workspaces, RBAC, SSO, audit logs, 160+ integrations | Local, one author | **Far** (stage three) |
| Non-Python | Python-first; shell and containers through integrations | R-first | Neither covers the other's users, which is your opening |

**What Prefect teaches the stage-two design (PR #45):**
1. **Start with a `serve` mode, then a separate server.** Prefect's `serve()` runs a process that executes its own schedules without workers. Part 3 of the design could begin the same way, with schedules run by the board's own process, before the separate board server.
2. **Borrow the state taxonomy for run records.** Split errored (the code failed) from crashed (killed, out of memory, R session lost), and add late (a scheduled run that didn't start on time). The design already has crashed.
3. **Add proactive alerts.** For scheduled pipelines, "no successful run in 24 hours" catches more than "a run failed". It belongs in part 4.
4. **Keep the history database minimal.** My understanding is that Prefect's local server ships with SQLite and grows to Postgres. That matches the design's choice; I couldn't confirm it in the pages read.

**Positioning:** Prefect positions itself as "Airflow, but Pythonic and dynamic". targetsboard can be "Prefect's experience for R", with something neither has: knowing what is outdated and why, and skipping it across runs.

---

## Appendix: Prefect 3.9 as the docs describe it

Kept as the reference this take compared against.

- **Positioning:** "an open-source orchestration engine that turns your Python functions into production-grade data pipelines". It is "Pythonic" (no DSL or YAML), with a "Dynamic Runtime": tasks and branches are created during execution. Prefect 2.0 "removed a key constraint that workflows be written explicitly as DAGs, fully embracing native Python control flow".
- **Authoring:** `@flow` and `@task` on plain functions; concurrency through `.submit` and `.map`. The graph comes from control flow at run time, not from a declared DAG.
- **Caching** needs result persistence, which is off by default (`PREFECT_RESULTS_PERSIST_BY_DEFAULT`, `persist_result`).
  - A cache hit puts the task in the "Cached" state, of type COMPLETED, with its stored value.
  - The `DEFAULT` policy hashes the inputs, the task's code and the flow run's id, so its cache is scoped to one run.
  - Other policies: `INPUTS`, `TASK_SOURCE` (the task's own lines, not nested tasks), `FLOW_PARAMETERS` and `NO_CACHE`. They compose: `TASK_SOURCE + INPUTS`, or `INPUTS - 'debug'` to ignore an input.
  - Also available: `cache_expiration`, `cache_key_fn`, records in `~/.prefect/storage/` or a storage block, and isolation levels `READ_COMMITTED` and `SERIALIZABLE`, the latter with memory, file-system or Redis lock managers.
  - There is no tracking of files or of a dependency graph: invalidation goes by cache key.
- **States** are typed:
  - not finished: SCHEDULED (Scheduled, Late, AwaitingRetry, AwaitingConcurrencySlot), PENDING, RUNNING (Running, Retrying), PAUSED and CANCELLING;
  - finished: Completed, Cached, RolledBack, Failed, TimedOut, Crashed and Cancelled.
  - "Failed" means the code failed; "Crashed" means the infrastructure did, or a signal stopped the run.
- **Execution:** `serve` runs deployments from one process.
  - **Work pools** bridge orchestration and infrastructure. Their types: process, Docker, Kubernetes, ECS, Cloud Run, Azure Container Instances, Vertex AI, Coiled.
  - Hybrid pools need polling **workers** (`prefect worker start -p my-pool`, every 15 s by default). Push pools and Prefect Managed pools need none.
  - Work queues have priorities and concurrency limits.
- **Schedules:** Cron, Interval and RRule, with timezones, attached to deployments. The scheduler creates runs ahead of time, at least 3 runs and at least an hour ahead, up to 100 runs and 100 days.
- **Automations:** one trigger and its actions.
  - Event triggers can be **Reactive** or **Proactive**; a proactive trigger fires when expected events don't arrive within a window.
  - There are compound and sequence composite triggers. Metric triggers are Cloud only.
- **Operations:**
  - Self-hosted server: the Docker Compose guide runs PostgreSQL, Redis, the server, background services and a worker.
  - Prefect Cloud adds workspaces, roles, SSO, audit logs, webhooks and metric triggers.
  - The docs index lists over 160 integration pages (`prefect-aws`, `prefect-dbt`, `prefect-shell`, and others), a REST API and an MCP server.
- **Other languages:** Python-first. Shell commands and containers run through integrations and work pools.
