# postgres-effects

**Status:** Implemented guide

Show the smallest app that opts into real database effects while staying close to the existing repo.

Current repo alignment:

- `HarchWeb.Database` defines the reusable typed effect contract: an application
  supplies a result-indexed operation algebra and an interpreter, while each
  operation returns its own result type plus stable query metadata.
- the app already has a PostgreSQL-backed path,
- the migration flow already uses `WEB_API_MIGRATION_DATABASE_*`,
- the combined example app already proves the runtime path works.

Suggested snippet:

- [env/.env.local.md](env/.env.local.md)

## Grounded repo flow

If you want a local PostgreSQL instance that matches the current repo defaults, start one like this:

```bash
docker run --name web-api-postgres \
  -e POSTGRES_USER=web_api_owner \
  -e POSTGRES_PASSWORD=web_api_owner \
  -e POSTGRES_DB=web_api_dev \
  -p 127.0.0.1:5432:5432 \
  -d docker.io/library/postgres:17
```

- With Podman, replace `docker` with `podman`.

Then export the owner-level migration credentials and run the Haskell-managed migrations:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner

cabal run exe:haskell-web-api-db -- migrate-and-seed
```

The important split is:

- runtime app config uses the lower-privilege `DATABASE_*` values,
- migration work uses separate `WEB_API_MIGRATION_DATABASE_*` owner credentials.

After that, the combined example app can run against the migrated database with the repo's normal
runtime startup flow.

## Account-activity audit: application operations, not a framework service

The `web-api` reference application also demonstrates a durable account-security
history in the *same* PostgreSQL database. It is not part of
`HarchWeb.Database`, not a generic Harch audit service, and not a customer
history feature. Its closed event vocabulary, atomic account-session/audit
operation, `account_audit` schema, RLS policy, retention and capacity policy,
and deployment schedule are application-owned.

The migration command above creates the following least-privilege split. Use
separate credentials and protected connection configuration for each role;
never put an owner, reader, or scheduler password in the runtime application
environment.

| Identity | Permitted reference responsibility | Not permitted |
| --- | --- | --- |
| `account_audit_owner` / migration connection | Own and migrate audit schema, policies, partitions, scope maps, and recovery procedures. | Runtime application traffic or ordinary reporting. |
| `web_api_runtime` | Call only the controlled append/atomic session-and-audit functions through application code. | Direct audit-table reads or mutations, role/scope selection, DDL, or partition maintenance. |
| `web_api_audit_reader` | Operator/reporting `SELECT` through the owner-managed scope-grant RLS policy. | Any audit mutation, policy/role change, or partition inspection. |
| `web_api_audit_scheduler` | Register and invoke the safe no-argument maintenance wrapper. | Arbitrary maintenance timestamps, audit reads/writes, or owner privileges. |

The owner provisions a runtime role's one append scope in
`account_audit.runtime_scope` and a reader's permitted scopes in
`account_audit.reader_scope_grant` as a reviewed deployment/migration change.
The caller cannot choose a scope with a request value or a custom PostgreSQL
setting. RLS is forced on the parent table; direct runtime reads and writes are
expected to fail. PostgreSQL superusers and database owners can still bypass
these controls, so this is not tamper-proof storage or a claim of automatic
SOC compliance.

For example, an owner can grant the fixed reader login a reviewed scope with
`psql` variables (which quote values rather than interpolating shell text into
SQL). The migration normally maps the configured runtime role to its `default`
append scope; changing that mapping is an owner-reviewed deployment change too.

```sh
psql "$AUDIT_OWNER_DATABASE_URL" --no-psqlrc --set=ON_ERROR_STOP=1 \
  --set=reader_role=web_api_audit_reader --set=audit_scope=default <<'SQL'
INSERT INTO account_audit.reader_scope_grant (reader_role_name, audit_scope_id)
VALUES (:'reader_role'::name, :'audit_scope')
ON CONFLICT DO NOTHING;
SQL
```

Verify the opposite side of the boundary with the runtime application's own
connection: this direct table read must fail. A successful result means the
role grants/RLS deployment is unsafe and should be repaired before serving
traffic.

```sh
if psql "$RUNTIME_DATABASE_URL" --no-psqlrc --set=ON_ERROR_STOP=1 \
  --command "SELECT account_id FROM account_audit.activity LIMIT 1;"; then
  printf '%s\n' 'runtime audit-table read unexpectedly succeeded' >&2
  exit 1
fi
```

### Operator queries and capacity response

Use an operator-managed reader connection (shown here as an externally supplied
connection URI) for a bounded investigation query. The database's RLS grants,
not the query, decide which scopes are visible:

```sh
psql "$AUDIT_READER_DATABASE_URL" --no-psqlrc --set=ON_ERROR_STOP=1 \
  --command "SELECT occurred_at, account_id, request_id, event_code, payload_detail, route_template
             FROM account_audit.activity
             WHERE occurred_at >= statement_timestamp() - interval '24 hours'
             ORDER BY occurred_at DESC
             LIMIT 100;"
```

This is operator/reporting access only. Do not turn it into an account-facing
activity page or API, and do not export account IDs, request IDs, or activity
values as metric labels or ordinary logs.

### Request-ID audit lookup

Given an identifier copied from the response header or a framework-owned error
body, use the protected reader connection and a `psql` variable rather than
splicing it into SQL. The audit lookup is deliberately non-unique: one request
can have several activities, while RLS hides rows in scopes not granted to this
reader.

```sh
test -n "$request_id"
psql "$AUDIT_READER_DATABASE_URL" --no-psqlrc --set=ON_ERROR_STOP=1 \
  --set=request_id="$request_id" <<'SQL'
SELECT occurred_at, account_id, request_id, event_code, payload_detail,
       route_template
FROM account_audit.activity
WHERE request_id = :'request_id'::text
ORDER BY occurred_at, activity_id
LIMIT 100;
SQL
```

The same ID in an ungranted scope must not appear. A zero-row result is not
evidence that the identifier is invalid: it can mean that the request had no
account subject, required audit persistence rolled back, the record is outside
retention, or the reader lacks the relevant scope. Use the
[logging-and-telemetry support workflow](../logging-and-telemetry/README.md#support-workflow)
to correlate any available private logs and OTLP spans without treating either
best-effort channel as an audit substitute.

The owner, not the runtime or reader role, inspects partition capacity before
the configured high-water mark is exceeded:

```sh
psql "$AUDIT_OWNER_DATABASE_URL" --no-psqlrc --set=ON_ERROR_STOP=1 \
  --command "SELECT registry.partition_name, registry.lower_bound, registry.upper_bound,
                    registry.accepted_row_count, policy.partition_row_limit,
                    (registry.accepted_row_count * 100 / policy.partition_row_limit) AS utilization_percent,
                    policy.utilization_high_water_percent
             FROM account_audit.partition_registry AS registry
             CROSS JOIN account_audit.audit_policy AS policy
             WHERE policy.policy_key
             ORDER BY registry.lower_bound;"
```

Capacity exhaustion is intentionally a failed-closed outcome for an
audit-required mutation. The operator must inspect storage/retention policy,
repair the underlying deployment condition, and use the owner-controlled
maintenance/recovery procedure; do not grant the runtime role a bypass or
delete individual audit rows to make a request succeed.

### Runtime monitoring and alerting inventory

Signals are identifiers, not severity levels. Alert policy belongs to the
deployment, and every request-bound investigation starts from the private
request log/span's `RequestId`, never from a metric label or an audit subject.
The table deliberately calls out query-based checks as well as emitted
signals: a missing signal is not evidence that the underlying control is
healthy.

| Condition to monitor | Signal or owner query | Immediate operator response | Evidence |
| --- | --- | --- | --- |
| Explicit logout revocation succeeded but its optional audit append failed | `account.logout.audit-append-failed`, with only `operation=append` and failure kind `unavailable`, `capacity-exhausted`, or `corrupt-result`. The browser logout still succeeds; the event is intentionally absent. | Investigate database availability/capacity or the controlled append result with the private request ID. Do not restore the browser credential or fabricate an audit row. | [logout workflow](../../packages/web-api/src/WebApi/AccountPages/Actions/Workflows.hs), [workflow test](../../packages/web-api/test/Unit/WebApi/AccountPagesSpec.hs) |
| A controlled append reaches the partition hard cap | `audit_capacity_exceeded`; the same owner capacity query above shows the affected partition and its configured limit. | Treat required audit mutations as unavailable until capacity, disk, or retention maintenance is repaired. Raising the owner-controlled limit is an explicit policy decision; do not delete retained rows or grant a runtime bypass. | [logout signal path](../../packages/web-api/src/WebApi/AccountPages/Actions/Workflows.hs), [capacity/role integration proof](../../packages/web-api/test/Integration/WebApiSpec.hs) |
| Capacity approaches its configured high-water mark | The owner capacity query above, compared with `utilization_high_water_percent`. This is currently a query-based warning boundary, not a delivery-guaranteed application signal. | Forecast growth and storage headroom, verify the next partition, and schedule a reviewed capacity/retention change before the hard cap is reached. | [audit policy and partition registry](../../packages/web-api/src/WebApi/Postgres/ActivityAuditMigration.hs), [partition proof](../../packages/web-api/test/Integration/WebApiSpec.hs) |
| Maintenance is missing, late, or failing; current/next writable partitions are absent | Inspect the named `cron.job` rows and their bounded `cron.job_run_details`, then compare `partition_registry` bounds with the current UTC month. | Repair the scheduler connection/role, job command, or maintenance failure; call the owned maintenance function directly for recovery. Do not wait for `pg_cron` to run during diagnosis. | [scheduler setup](../../packages/web-api/src/WebApi/DatabaseSetup.hs), [exact-job and direct-maintenance test](../../packages/web-api/test/Integration/WebApiSpec.hs) |
| Retention drift, disk pressure, or partition-maintenance lock pressure | Monitor the oldest/newest `partition_registry` bounds alongside PostgreSQL disk and lock telemetry. This is deployment monitoring, not a runtime metric emitted by Harch. | Resolve storage or blocking maintenance work before the writable partition is exhausted; keep the documented twelve-complete-month policy unless an owner-approved retention change is made. | [maintenance contract](../../packages/web-api/src/WebApi/Postgres/ActivityAuditMigration.hs), [retention-boundary test](../../packages/web-api/test/Integration/WebApiSpec.hs) |

The first two signals carry no account, session, route, page, raw exception, or
request-ID attribute. The framework adds request correlation only to private
diagnostics and spans. Other audit-producing workflows must adopt this same
inventory and control policy before they are described as audit-capable.

### Retention and scheduler setup

`migrate` and `migrate-and-seed` reconcile the scheduler login and install the
example `pg_cron` jobs with the separate
`WEB_API_AUDIT_SCHEDULER_DATABASE_*` connection documented in [SETUP.md](../../SETUP.md#local-postgresql-startup-example):

- `account-audit-maintenance` calls only
  `account_audit.maintain_activity_partitions()` at `0 3 * * *` UTC;
- the scheduler-owned run-detail cleanup runs at `41 3 * * *` UTC.

The wrapper creates current/next monthly partitions and drops partitions at the
configured twelve-complete-month retention boundary. The setup command checks
that the jobs are installed; it does not wait for or test `pg_cron`'s clock.
Another deployment may use Kubernetes, systemd, or a managed scheduler, but it
must invoke the same no-argument wrapper with separately reviewed credentials,
schedule, timezone, and recovery procedure. The repository's integration test
calls the owned maintenance function directly at deterministic timestamps and
tests the scheduler's installed command/permissions rather than reproducing
the third party's scheduler suite.

What this example should emphasize:

1. the runtime database user can stay different from the migration owner user,
2. seed or migrate once before starting the app,
3. the simplest example should still keep page rendering and effect seams easy to follow.
