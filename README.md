# Caravan

Caravan is a durable, distributed job queue for OCaml 5, built on [Eio](https://github.com/ocaml-multicore/eio).

Your application enqueues jobs into PostgreSQL. Worker nodes on any number of machines claim them, run them with timeouts and retries, and record what happened. If a node crashes mid-job, the others notice and run the job elsewhere.

```ocaml
type email = { to_ : string; subject : string } [@@deriving yojson]

let send_email =
  Caravan.Job.make ~name:"send_email" ~queue:"mailers" ~max_attempts:5
    ~codec:(Caravan.Codec.of_yojson email_to_yojson email_of_yojson)
    ~perform:(fun _ctx e ->
      Mailer.send e;
      Caravan.Outcome.Ok)
    ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pg = Caravan_postgres.connect ~sw ~clock:env#clock "postgresql://localhost/myapp" in
  let backend = Caravan_postgres.backend pg in
  let client = Caravan.Client.make backend in
  ignore (Caravan.Client.enqueue client send_email { to_ = "ada@example.com"; subject = "Hi" });
  Caravan.Node.start ~sw ~clock:env#clock backend
    ~jobs:[ Caravan.Job.pack send_email ]
    ~queues:[ ("mailers", 10) ]
  |> Caravan.Node.run_until_signal
```

## Features

- **Durable.** Jobs live in PostgreSQL and survive restarts and deploys.
- **Distributed.**
  - Any number of nodes can share one database.
  - Nodes claim work with `FOR UPDATE SKIP LOCKED`, so no two nodes ever get the same job.
  - `LISTEN/NOTIFY` wakes idle nodes as soon as a job arrives, and they fall back to polling when notifications aren't available.
- **Crash recovery.**
  - Nodes heartbeat.
  - If a node stops heartbeating, the leader rescues its jobs and they run elsewhere.
  - The interrupted attempt counts toward the retry limit, so a job that keeps crashing its node is eventually discarded.
- **Graceful shutdown.** On SIGTERM a node stops claiming new jobs and gives running jobs a grace period. Any still unfinished are handed back to the queue without using up an attempt.
- **Retries.**
  - Backoff can be exponential with jitter, linear, constant or custom.
  - Jobs can have per-attempt timeouts.
  - A job can also return `Snooze`, `Cancel` or `Discard`.
- **Scheduling.**
  - You can delay a job, or run it at a set time.
  - Priorities run from 0 to 9.
  - Cron jobs run once per firing time across the whole cluster, even when leadership changes.
- **Unique jobs.** You can guarantee at most one active job per argument set or per key.
- **Transactional enqueue.** A job can be inserted in the same transaction as your own writes.
- **Multicore.** CPU-bound queues can run on an `Eio.Executor_pool` so they don't hold up heartbeats and I/O.
- **Operations.**
  - A web dashboard and a JSON API.
  - Prometheus metrics.
  - A `caravan` CLI.
  - Middleware and telemetry hooks for your own logging and tracing.

## Packages

| Package | What it is |
|---|---|
| `caravan` | Job definitions, the node runtime, and the in-memory backend. Depends only on Eio. |
| `caravan-postgres` | The PostgreSQL backend. It uses libpq directly, non-blocking under Eio. |
| `caravan-web` | The dashboard, JSON API and `/metrics` endpoint, on cohttp-eio. |
| `caravan-cli` | The `caravan` command: `migrate`, `stats`, `jobs`, `queues`, `nodes`, `prune` and `web`. |

## Getting started

```sh
export CARAVAN_DATABASE_URL=postgresql://localhost/myapp
caravan migrate                                   # create the tables (safe to run concurrently)
dune exec examples/multi_node/main.exe -- worker  # start as many as you like
dune exec examples/multi_node/main.exe -- enqueue 1000
caravan web --port 4000                           # http://localhost:4000
```

The dashboard shows queue depths by state, the live nodes, job lists and each job's error history. From there you can retry or cancel a job and pause or resume a queue.

## Delivery guarantee: at-least-once

A job can run more than once. For example, a node might finish the work and then crash before it records that. **Make `perform` idempotent.** Caravan does guarantee three things:

- Two nodes never hold the same attempt of a job at the same time.
- Every outcome is recorded against a claim (job id, node, attempt). If a node was wrongly presumed dead, it can't overwrite the result of the job's next attempt.
- A job that was enqueued is never lost. It ends up completed, discarded or cancelled.

## How it works

Every node runs:

- **A producer for each queue.** It claims up to the queue's concurrency limit and runs each job in its own fiber, applying the timeout and the middleware.
- **A heartbeat.**
- **A leader loop.** A lease elects one leader, and the leader does the cluster-wide maintenance:
  - moving due scheduled jobs and retries back to available
  - rescuing jobs from dead nodes
  - pruning old finished jobs
  - inserting cron jobs

  Every maintenance task is idempotent, so a brief overlap between two leaders is harmless.

Job states: `available → executing → completed | retryable | discarded | cancelled`. On top of those, `scheduled` holds jobs waiting for their run time. The state machine in [`State`](src/core/state.mli) is the single source of truth.

## Testing

```sh
dune test                                                   # unit, runtime and web tests
CARAVAN_PG_URL=postgresql://localhost/caravan_test dune test --force
CARAVAN_PG_URL=... dune exec test/chaos/chaos.exe           # kill -9 workers, check that no job is lost
```

- **Runtime tests** run on `Eio_mock` with a virtual clock. Hours of retries, backoff, rescues and cron run deterministically in milliseconds.
- **The model-based test** runs random sequences of operations against both the in-memory backend and PostgreSQL. After every step it requires identical results and identical job tables.
  - It found a real bug before release: requeuing a unique job could create a second active job with the same key.
- **The chaos test** runs 4 worker processes and keeps SIGKILLing random ones while 5,000 jobs run. It then checks that every job completed and appears in an independent audit table.
  - In a typical run, 18 kills interrupted 144 jobs mid-attempt, and all 5,000 still completed.

## Performance

These are no-op jobs, so the numbers measure Caravan and the database rather than job work. They're from a laptop (Apple M-series) with a local PostgreSQL 16, running `dune exec --release bench/bench.exe`:

| Operation | Throughput |
|---|---|
| Enqueue, batches of 1000 | ~10,500 jobs/s |
| Enqueue, one at a time | ~9,200 jobs/s |
| Process, 1 node × 50 concurrency | ~27,000 jobs/s |
| Process, 4 nodes × 50 concurrency | ~26,000 jobs/s |

A single database can already keep several nodes busy, so real workloads are limited by the jobs themselves.

## Operational notes

- **Connections.**
  - Each node uses a pool (10 connections by default) plus one `LISTEN` connection.
  - If you connect through PgBouncer in transaction mode, set `~listen:false`. Nodes then poll every `poll_interval`.
- **Clocks.** Nodes timestamp heartbeats and scheduled times with their own clocks, so run NTP. Small skew only delays scheduled jobs. Skew that approaches `node_timeout` (60s by default) could make a healthy node look dead, and its jobs would then run twice.
- **CPU-bound jobs.** A CPU-heavy job on the node's own domain delays that node's heartbeat, and a long enough delay gets its jobs rescued elsewhere. Put such queues on an executor pool with `Node.start ~executor:(pool, ["queue"])`, or give them a generous `node_timeout`.
- **Rolling deploys.** A node that doesn't know a job's name retries it with backoff instead of failing it, so a newer node can pick it up.
- **Security.**
  - Serve the dashboard behind TLS.
  - Enable basic auth with `CARAVAN_WEB_USER` and `CARAVAN_WEB_PASSWORD`.
  - Use `--read-only` for view-only deployments.

## License

MIT
