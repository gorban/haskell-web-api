# 04-logging-and-telemetry

**Status:** Current

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
