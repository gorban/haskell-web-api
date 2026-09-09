# logging-and-telemetry

**Status:** Implemented guide

Show how to run the app locally with logs and traces visible, while keeping the smallest example
free from telemetry by default.

Current repo alignment:

- OTLP tracing is already wired,
- the repo already documents local Jaeger usage,
- the custom trace export layer is intentional and should remain documented as such.

Suggested snippet:

- [env/.env.local.md](env/.env.local.md)

## Grounded repo flow

Turn tracing on with the local default endpoint:

```dotenv
OTLP_TRACING_ENABLED=true
```

Then start a local Jaeger all-in-one instance with OTLP ingest enabled:

```bash
docker run --name web-api-jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4318:4318 \
  -d jaegertracing/all-in-one
```

- With Podman, replace `docker` with `podman`.

Useful endpoints after startup:

- Jaeger UI: `http://127.0.0.1:16686`
- OTLP HTTP ingest: `http://127.0.0.1:4318/v1/traces`

Use this example to explain:

1. how the app emits traces to the local default endpoint when `OTLP_TRACING_ENABLED=true`,
2. how to start the app and then inspect traces in Jaeger,
3. why the repo intentionally keeps its custom OTLP export layer instead of generic WAI tracing
   middleware,
4. where request logs should be observed once the logging surface is formalized further.

## Request correlation is not an audit ledger

Harch creates an opaque `RequestId` at request ingress and makes it available
to trusted application context and telemetry-safe route observation. `web-api`
uses that ID to correlate selected account-security audit rows with the request
that produced them, but the framework does not persist audit history or expose
an audit reader. The remaining response/log/span/audit presentation sweep is
tracked separately; do not promise a universal public error-body join from this
guide.

| Signal | Owner and delivery policy | Data/retention boundary |
| --- | --- | --- |
| Request logs and OTLP traces/metrics | Application observability configuration; diagnostics are best effort and must not change an otherwise valid authorization decision. | Low-cardinality route/status attributes and private diagnostics according to the deployment's telemetry policy. |
| `web-api` account activity | Application PostgreSQL transaction policy; selected state changes require the audit append to commit atomically, while a known-account denial stays denied if its optional audit write is unavailable. | Closed event vocabulary with opaque account/request IDs and bounded trusted route metadata; operator/reporting access only. |

Never substitute a successful telemetry export for a required audit commit, or
an audit row for an OTLP span. Neither system should receive raw credentials,
submitted identifiers, token material, request paths/queries, raw IP addresses,
or user-agent values as audit payloads or metric labels. For the reference
schema, RLS roles, retention, capacity inspection, and scheduler procedure, see
the [PostgreSQL effects guide](../postgres-effects/README.md#account-activity-audit-application-operations-not-a-framework-service).
