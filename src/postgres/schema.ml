(* Versioned migrations. Append new versions; never edit a released one. *)

let migrations : (int * string * string) list =
  [
    ( 1,
      "initial schema",
      {sql|
CREATE TABLE caravan_jobs (
  id            bigserial PRIMARY KEY,
  state         text NOT NULL CHECK (state IN
                  ('available', 'scheduled', 'executing', 'retryable',
                   'completed', 'discarded', 'cancelled')),
  queue         text NOT NULL CHECK (length(queue) > 0),
  worker        text NOT NULL CHECK (length(worker) > 0),
  args          jsonb NOT NULL DEFAULT 'null',
  meta          jsonb NOT NULL DEFAULT 'null',
  tags          jsonb NOT NULL DEFAULT '[]',
  priority      smallint NOT NULL DEFAULT 0 CHECK (priority BETWEEN 0 AND 9),
  attempt       integer NOT NULL DEFAULT 0 CHECK (attempt >= 0),
  max_attempts  integer NOT NULL DEFAULT 20 CHECK (max_attempts > 0),
  errors        jsonb NOT NULL DEFAULT '[]',
  inserted_at   timestamptz NOT NULL,
  scheduled_at  timestamptz NOT NULL,
  attempted_at  timestamptz,
  attempted_by  text,
  finished_at   timestamptz,
  unique_key    text
);

-- Claiming: the next available jobs of one queue in priority order.
CREATE INDEX caravan_jobs_fetch_idx ON caravan_jobs
  (queue, priority, scheduled_at, id) WHERE state = 'available';

-- Staging: scheduled and retryable jobs whose time has come.
CREATE INDEX caravan_jobs_stage_idx ON caravan_jobs
  (scheduled_at) WHERE state IN ('scheduled', 'retryable');

-- Rescue and release: jobs executing on a given node.
CREATE INDEX caravan_jobs_executing_idx ON caravan_jobs
  (attempted_by) WHERE state = 'executing';

-- Pruning: finished jobs by age.
CREATE INDEX caravan_jobs_prune_idx ON caravan_jobs
  (finished_at) WHERE state IN ('completed', 'discarded', 'cancelled');

-- Uniqueness holds among active jobs only.
CREATE UNIQUE INDEX caravan_jobs_unique_idx ON caravan_jobs (unique_key)
  WHERE unique_key IS NOT NULL
    AND state IN ('available', 'scheduled', 'executing', 'retryable');

CREATE INDEX caravan_jobs_state_queue_idx ON caravan_jobs (state, queue);
CREATE INDEX caravan_jobs_worker_idx ON caravan_jobs (worker, id);

CREATE TABLE caravan_nodes (
  node          text PRIMARY KEY,
  info          jsonb NOT NULL,
  heartbeat_at  timestamptz NOT NULL
);

CREATE TABLE caravan_queues (
  queue   text PRIMARY KEY,
  paused  boolean NOT NULL DEFAULT false
);

CREATE TABLE caravan_leader (
  name        text PRIMARY KEY,
  node        text NOT NULL,
  expires_at  timestamptz NOT NULL
);

-- Wake idle producers when a job becomes available. Postgres collapses
-- identical notifications within a transaction, so bulk inserts send one
-- notification per queue.
CREATE FUNCTION caravan_notify() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM pg_notify('caravan_jobs', NEW.queue);
  RETURN NULL;
END;
$$;

CREATE TRIGGER caravan_jobs_notify
  AFTER INSERT OR UPDATE OF state ON caravan_jobs
  FOR EACH ROW WHEN (NEW.state = 'available')
  EXECUTE FUNCTION caravan_notify();
|sql}
    );
  ]

let latest = List.fold_left (fun m (v, _, _) -> max m v) 0 migrations

let src = Logs.Src.create "caravan.postgres.migrate"

module Log = (val Logs.src_log src)

let bootstrap =
  {sql|
CREATE TABLE IF NOT EXISTS caravan_migrations (
  version     integer PRIMARY KEY,
  name        text NOT NULL,
  applied_at  timestamptz NOT NULL DEFAULT now()
)|sql}

let current_version conn =
  Pg.exec conn bootstrap;
  match
    (Pg.query conn "SELECT coalesce(max(version), 0) FROM caravan_migrations")
      .rows
  with
  | [| [| Some v |] |] -> int_of_string v
  | _ -> 0

(* Apply pending migrations in one transaction, serialised across processes
   by an advisory lock so concurrent deploys cannot race. *)
let migrate conn =
  Pg.exec conn bootstrap;
  Pg.with_transaction conn (fun () ->
      ignore
        (Pg.query conn "SELECT pg_advisory_xact_lock(hashtext('caravan_migrate'))");
      let current = current_version conn in
      let pending = List.filter (fun (v, _, _) -> v > current) migrations in
      List.iter
        (fun (version, name, sql) ->
          Log.info (fun m -> m "applying migration %d: %s" version name);
          Pg.exec conn sql;
          ignore
            (Pg.query conn
               ~params:[ Some (string_of_int version); Some name ]
               "INSERT INTO caravan_migrations (version, name) VALUES ($1, $2)"))
        pending;
      List.map (fun (v, _, _) -> v) pending)
