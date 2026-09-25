## 0.1.0 (unreleased)

First release.

- `caravan` covers job definitions, the codecs, the state machine, backoff, cron, the in-memory backend, and the Eio node runtime. The runtime includes producers, timeouts, retries, middleware, telemetry, leader-based maintenance and graceful shutdown.
- `caravan-postgres` is the PostgreSQL backend. It runs on non-blocking libpq, claims jobs with `SKIP LOCKED`, and uses LISTEN/NOTIFY wakeups, transactional enqueue and versioned migrations.
- `caravan-web` provides the dashboard, the JSON API and Prometheus metrics.
- `caravan-cli` provides the `caravan` command.
