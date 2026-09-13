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
an audit reader. Framework-owned HTTP errors present the same safe identifier
in their body; application protocols retain ownership of their own body format.

### Support workflow

Start with the response header (or the identifier displayed by a
framework-owned error page), never a caller-provided `X-Request-ID` value. For
example, preserve response headers from a reproducible request and copy the
server-generated value:

```sh
curl --silent --show-error --dump-header response.headers --output /dev/null \
  http://127.0.0.1:5001/api/status
request_id="$(awk 'tolower($1) == "x-request-id:" { print $2 }' response.headers | tr -d '\r' | head -n 1)"
test -n "$request_id"
printf '%s\n' "$request_id"
```

The application reporter writes to stderr. A request-specific application
diagnostic is rendered as `ERROR request.id=<UUID> <message>`, so use an exact
field prefix rather than a broad substring match:

```sh
rg -F "request.id=$request_id " web-api.stderr.log
```

For the local Jaeger instance above, the configured OTLP service name is
`web-api` and the exported server span attribute is `harch.request.id`. The
following query uses Jaeger's actual HTTP API and URL-encodes the JSON tag
filter; it is also a convenient check that the copied ID reaches the configured
backend:

```sh
curl --fail --silent --show-error --get \
  --data-urlencode 'service=web-api' \
  --data-urlencode "tags={\"harch.request.id\":\"$request_id\"}" \
  --data-urlencode 'limit=20' \
  http://127.0.0.1:16686/api/traces | jq .
```

The PostgreSQL reader lookup belongs to the separate
[account-activity guide](../postgres-effects/README.md#request-id-audit-lookup),
which uses a reader login and RLS; possession of an identifier does not grant
access. A log or span can legitimately be absent when its delivery is disabled,
sampled out, queued and dropped, or rejected by its exporter. An unknown-account
rejection has an ID but no account-audit row, and a required audit failure rolls
back the mutation rather than manufacturing a successful audit record.

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
