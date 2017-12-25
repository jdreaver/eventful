#!/usr/bin/env bash

set -eu;

# Benchmark various configurations of postgres as an event store.

export PGHOST=${PGHOST:-localhost}
export PGDATABASE=${PGDATABASE:-postgres_event_store_bench}
export PGUSER=${PGUSER:-postgres}
export PGPASSWORD=${PGPASSWORD:-password}

PSQL="psql --no-psqlrc --quiet"

NUM_CLIENTS=${NUM_CLIENTS:-4}
BENCH_TIME_SECONDS=${BENCH_TIME_SECONDS:-10}
PGBENCH="pgbench --no-vacuum $PGDATABASE --client $NUM_CLIENTS --jobs $NUM_CLIENTS -f pgbench-script.sql --time $BENCH_TIME_SECONDS --report-latencies"

SYNCHRONOUS_COMMIT=${SYNCHRONOUS_COMMIT:-off}
TRACK_COMMIT_TIMESTAMP=${TRACK_COMMIT_TIMESTAMP:-false}

SMALL_EVENT='{"type":"mytype","value":"hello"}'
LARGE_EVENT='{"type":"mytype","value":"hello","a":1,"b":2,"c":3,"d":true,"e":"Mary had a little lamb her fleece was white as snow and everywhere that mary went her lamb was sure to go","f":true,"g":false}'

# Spin up a docker container to run postgres tests
if [ "$(docker ps -aq -f name=postgres_event_store_bench)" ]; then
  docker stop postgres_event_store_bench
  docker rm postgres_event_store_bench
fi
docker run \
  --name postgres_event_store_bench \
  -e "POSTGRES_DB=$PGDATABASE" \
  -p 5432:5432/tcp \
  -d postgres:10 \
  postgres \
  -c "synchronous_commit=$SYNCHRONOUS_COMMIT" \
  -c "track_commit_timestamp=$TRACK_COMMIT_TIMESTAMP"
  # -c shared_buffers=2000MB \
  # -c wal_buffers=16MB \
  # -c work_mem=20MB \
  # -c wal_writer_delay=2s

# Wait for postgres to be available
until nc -z "$PGHOST" 5432; do
  echo "waiting for postgres..."
  sleep 1
done

# Give time for postgres to start
sleep 10

recreate_db() {
  local real_pgdatabase="$PGDATABASE"
  PGDATABASE=postgres $PSQL -c "DROP DATABASE IF EXISTS $real_pgdatabase;"
  PGDATABASE=postgres $PSQL -c "CREATE DATABASE $real_pgdatabase;"
}

# Test insertion speed with simple schema and full table lock
recreate_db
$PSQL <<EOF
CREATE TABLE events (
  sequence_number serial PRIMARY KEY,
  event jsonb NOT NULL
  -- created_at timestamp with time zone default now() NOT NULL
);
EOF

cat <<EOF > pgbench-script.sql
BEGIN;
LOCK events IN EXCLUSIVE MODE;
INSERT INTO events (event) VALUES ('$SMALL_EVENT');
COMMIT;
EOF

echo "Full table lock"
$PGBENCH

# Schema with logical logs embedded (UUID and version)
recreate_db
$PSQL <<EOF
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE events (
  sequence_number serial PRIMARY KEY,
  log_id uuid NOT NULL,
  version int NOT NULL,
  event jsonb NOT NULL,
  UNIQUE (log_id, version)
);
EOF

# TODO: I think the main bottleneck here is running uuid_generate_v4(). We
# wouldn't have to do this in a real system because we would already know the
# UUID.

cat <<EOF > pgbench-script.sql
BEGIN;
LOCK events IN EXCLUSIVE MODE;
INSERT INTO events (log_id, version, event) VALUES (uuid_generate_v4(), 0, '$SMALL_EVENT');
COMMIT;
EOF

echo "Full table lock, complex schema, random insertion"
$PGBENCH

# Schema with UUID but no versions
recreate_db
$PSQL <<EOF
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE events (
  sequence_number serial PRIMARY KEY,
  log_id uuid NOT NULL,
  event jsonb NOT NULL
);

CREATE INDEX events_log_id ON events (log_id);
EOF

cat <<EOF > pgbench-script.sql
BEGIN;
LOCK events IN EXCLUSIVE MODE;
INSERT INTO events (log_id, event) VALUES ('00000000-0000-0000-0000-000000000000', '$SMALL_EVENT');
COMMIT;
EOF

echo "Full table lock, UUID but no version"
$PGBENCH

# Test insertion with trigger as sequence number
recreate_db
$PSQL <<EOF
CREATE TABLE events (
  id serial PRIMARY KEY,
  sequence_number bigint,
  event jsonb NOT NULL,
  UNIQUE (sequence_number)
);

CREATE SEQUENCE events_sequence_number_seq
  START WITH 1
  INCREMENT BY 1
  NO MINVALUE
  NO MAXVALUE
  CACHE 1;

CREATE OR REPLACE FUNCTION update_events_sequence_number()
  RETURNS trigger AS
\$BODY\$
BEGIN
 PERFORM pg_advisory_lock(1);

 UPDATE events
 SET sequence_number = nextval('events_sequence_number_seq'::regclass)
 WHERE id = NEW.id;
 RETURN NEW;
END;
\$BODY\$
LANGUAGE 'plpgsql';

CREATE TRIGGER update_events_sequence_number
AFTER INSERT ON events
FOR EACH ROW
EXECUTE PROCEDURE update_events_sequence_number();
EOF

cat <<EOF > pgbench-script.sql
BEGIN;
INSERT INTO events (event) VALUES ('$SMALL_EVENT');
COMMIT;
EOF

echo "Trigger with advisory lock"
$PGBENCH

# TODO: Test with millions (billions?) of rows already inserted
# TODO: Insert multiple rows per transaction
# TODO: BRIN indexes
# TODO: Query and streaming throughput

# Notes of stuff I've discovered so far:

# * Setting synchronous_commit=off makes all the difference in the world. It
# increases insert throughput by like 10 times. During one test, the simple
# full table lock version went from 400 tps to 4000 tps on my laptop. Note that
# this means we will see data loss of up to 3 times wal_writer_delay, and if
# this is acceptable or not is domain specific. Also, in most systems like
# Kinesis and Kafka an ACK of the event means it is stored durably.

# * I'm not sure if in the trigger version the advisory lock on the sequence is
# necessary to achieve monotoinc reads. My gut says it is. With the advisory
# lock it is definitely slower than just a full table lock.

# * (Needs more testing) Event size doesn't have too much of an effect on write
# speed. That means if you have large events then the total bytes per second
# when using postgres could be fairly large. Also, if you batch event writes
# (like you probably would anyway with something like Kafka or Kinesis), then
# the total events per second and bytes per second is potentially event larger.
