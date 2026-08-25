# Runtime configuration

The full `web-api` application has localhost-friendly defaults. Startup reads configuration in this
order, with each later layer overriding the earlier one:

1. Defaults compiled into the application.
2. `./.env`, for committed non-secret development settings.
3. `./.env.local`, for machine-specific or deployed settings and secrets. This file is gitignored.
4. Process environment variables, for deployment and one-off overrides. This is the highest-precedence
   layer.

Both files use `KEY=value` lines, allow blank lines, and treat lines beginning with `#` as comments.
The smaller `two-pages-example` has its own fixed local configuration and does not need this table.

## Environment variables

| Config value | Description | Default |
| --- | --- | --- |
| `APP_MODE` | App behavior mode: `development`, `test`, or `production`. | `development` |
| `DATABASE_HOST` | PostgreSQL host. A loopback address keeps it local to the machine. | `127.0.0.1` |
| `DATABASE_PORT` | PostgreSQL port. | `5432` |
| `DATABASE_NAME` | Runtime database name. | `web_api_dev` |
| `DATABASE_USER` | Runtime database username. | `web_api_runtime` |
| `DATABASE_PASSWORD` | Runtime database password. | `web_api` |
| `WEB_API_MIGRATION_DATABASE_HOST` | Migration-only PostgreSQL host, read by setup and migration commands rather than the runtime request path. | unset |
| `WEB_API_MIGRATION_DATABASE_PORT` | Migration-only PostgreSQL port. | unset |
| `WEB_API_MIGRATION_DATABASE_NAME` | Migration-only PostgreSQL database name. | unset |
| `WEB_API_MIGRATION_DATABASE_USER` | Migration-only PostgreSQL user. | unset |
| `WEB_API_MIGRATION_DATABASE_PASSWORD` | Migration-only PostgreSQL password. | unset |
| `SMTP_HOST` | SMTP server host for application email delivery. | `127.0.0.1` |
| `SMTP_PORT` | SMTP server port. | `5025` |
| `SMTP_HELO_NAME` | HELO/EHLO name sent to the SMTP server. | `localhost` |
| `SMTP_USER` | SMTP username. | `test@localhost` |
| `SMTP_PASSWORD` | SMTP password. | `password` |
| `EMAIL_FROM` | Sender address for application email. | `noreply@localhost` |
| `PUBLIC_BASE_URL` | Public origin used when rendering absolute links in emails and workflows. | `http://127.0.0.1:5001` |
| `TOTP_ENCRYPTION_KEY` | URL-safe unpadded base64 encoding of a 32-byte encryption key for stored TOTP secrets. The committed development fixture is rejected when `APP_MODE=production`. | development fixture only |
| `APP_TITLE_PREFIX` | Prefix for rendered HTML page titles. | `web-api` |
| `LISTENER_<n>_HOST` | Interface for listener `n`. | listener 0: `127.0.0.1` |
| `LISTENER_<n>_PORT` | Port for listener `n`. | listener 0: `5001` |
| `LISTENER_<n>_SCHEME` | `http` or `https`. | listener 0: `http` |
| `LISTENER_<n>_TLS_SOURCE` | HTTPS source: `manual`, `shared`, `shared-wait`, or `shared-fail-fast`. `shared` is the legacy waiting alias. The older HTTPS `acme` shape is accepted, but an ACME HTTP publisher plus shared HTTPS consumer is preferred. | unset |
| `LISTENER_<n>_TLS_CERTIFICATE_FILE` | Certificate path for manual TLS. | unset |
| `LISTENER_<n>_TLS_PRIVATE_KEY_FILE` | Private-key path for manual TLS. | unset |
| `LISTENER_<n>_TLS_CERTIFICATE_DIRECTORY` | Directory containing `fullchain.pem` and `privkey.pem` for shared TLS. With exactly one ACME publisher, an unset consumer reuses its effective directory. | listener-aware |
| `LISTENER_<n>_TLS_SHARED_WAIT_SECONDS` | Optional `shared`/`shared-wait` startup timeout. Unset waits indefinitely. `shared-fail-fast` rejects this setting. | unset |
| `LISTENER_<n>_ACME_DIRECTORY_URL` | ACME directory for a certificate-publishing listener. Prefer the HTTP listener serving `http-01`. | Let's Encrypt production directory |
| `LISTENER_<n>_ACME_CONTACT_EMAILS` | Comma-delimited ACME contact addresses. | unset |
| `LISTENER_<n>_ACME_DOMAINS` | Comma-delimited certificate domains. | unset |
| `LISTENER_<n>_ACME_CERTIFICATE_DIRECTORY` | Directory where ACME publishes `fullchain.pem` and `privkey.pem`. | `./.tls/<cert-name>` |
| `LISTENER_<n>_ACME_CERTBOT_EXECUTABLE` | Certbot executable path. | `certbot` |
| `LISTENER_<n>_ACME_CERTBOT_ARGUMENTS` | Comma-delimited certbot overrides. Otherwise startup derives a non-interactive webroot invocation from ACME config. | unset |
| `STATIC_ASSET_ROOT_<n>_URL_PREFIX` | URL prefix for static root `n`. | unset |
| `STATIC_ASSET_ROOT_<n>_DIRECTORY` | Filesystem directory for static root `n`. | unset |
| `STATIC_ASSET_CONTENT_TYPE_<n>_EXTENSION` | Allowed extension, including its leading dot. An empty value opts into extensionless files. | `.css`, `.html`, `.js`, `.json`, `.svg`, `.txt` |
| `STATIC_ASSET_CONTENT_TYPE_<n>_MIME_TYPE` | MIME type paired with the indexed extension. | types for the default extensions |
| `STATIC_CACHE_CONTROL_SECONDS` | Static response `Cache-Control` max-age. | unset |
| `TRUST_FORWARDED_HEADERS` | Trust forwarded host/proto/prefix headers from a configured reverse proxy. Keep `false` unless the direct peer is trusted. | `false` |
| `REQUEST_TARGET_MAX_BYTES` | Optional maximum for raw path plus query bytes. A request above it receives `414` before routing, logging, or tracing. | unset (unbounded) |
| `REQUEST_HEADER_MAX_BYTES` | Optional total request-header budget. Warp applies it while parsing the wire request; the framework also checks it before middleware. | unset (unbounded) |
| `REQUEST_HEADER_MAX_COUNT` | Optional maximum number of request header fields. | unset (unbounded) |
| `REQUEST_HEADER_VALUE_MAX_BYTES` | Optional maximum for one header value, including cookies, forwarded headers, and trace context. | unset (unbounded) |
| `REQUEST_COOKIE_MAX_COUNT` | Optional maximum number of syntactically valid cookie pairs across every `Cookie` header field. | unset (unbounded) |
| `REQUEST_COOKIE_NAME_MAX_BYTES` | Optional maximum bytes in one syntactically valid cookie name. | unset (unbounded) |
| `REQUEST_COOKIE_VALUE_MAX_BYTES` | Optional maximum bytes in one syntactically valid cookie value. | unset (unbounded) |
| `REQUEST_PATH_SEGMENT_MAX_COUNT` | Optional maximum number of raw non-empty path segments before route parsing. | unset (unbounded) |
| `REQUEST_PATH_SEGMENT_MAX_BYTES` | Optional maximum bytes in a raw path segment before route parsing. | unset (unbounded) |
| `REQUEST_QUERY_FIELD_MAX_COUNT` | Optional maximum number of raw query-field slots, including empty slots separated by `&`. | unset (unbounded) |
| `REQUEST_QUERY_FIELD_MAX_BYTES` | Optional maximum bytes in one raw query-field slot before query parsing. | unset (unbounded) |
| `REQUEST_NETWORK_TIMEOUT_SECONDS` | Optional Warp network-progress timeout. `0` disables Warp's timer; choose a positive value for slow-request containment. | unset (Warp default: 30 seconds) |
| `REQUEST_SLOWLORIS_MAX_BYTES` | Optional number of bytes that must arrive before Warp treats a connection as making progress for its timeout timer. | unset (Warp default: 2048 bytes) |
| `REQUEST_MAX_CONCURRENT` | Optional maximum number of requests admitted at once across every listener (HTTP, manual TLS, ACME-managed TLS). A request beyond the limit receives `503` before route parsing, middleware, observability, or body reads. | unset (unbounded; relies on Warp/OS defaults) |
| `REDIRECT_HTTP_TO_HTTPS` | Boolean HTTP redirect policy. If unset with both HTTP and HTTPS listeners, redirects default on. | listener-aware |
| `HSTS_MAX_AGE_SECONDS` | `Strict-Transport-Security` max-age for effective HTTPS. | unset |
| `HSTS_INCLUDE_SUBDOMAINS` | Add HSTS `includeSubDomains`; requires max-age. | `false` |
| `HSTS_PRELOAD` | Add HSTS `preload`; requires max-age. | `false` |
| `CORS_ALLOWED_ORIGINS` | Comma-delimited exact origins allowed to read responses. Unset stays same-origin. | unset |
| `CORS_ALLOWED_METHODS` | Methods returned for allowed preflights. | `GET,HEAD,OPTIONS` |
| `CORS_ALLOWED_HEADERS` | Headers returned for allowed preflights. | `Content-Type,X-Requested-With` |
| `CORS_MAX_AGE_SECONDS` | Optional preflight cache duration. | unset |
| `CONTENT_SECURITY_POLICY` | CSP. Full documents add a fresh `script-src` nonce for the capture kernel. | strict same-origin policy |
| `X_CONTENT_TYPE_OPTIONS_NOSNIFF` | Emit `X-Content-Type-Options: nosniff`. | `true` |
| `X_XSS_PROTECTION` | Compatibility `X-XSS-Protection` value. | `1; mode=block` |
| `REFERRER_POLICY` | Referrer policy. | `strict-origin-when-cross-origin` |
| `PERMISSIONS_POLICY` | Browser capability policy. | common powerful features disabled |
| `X_FRAME_OPTIONS` | Compatibility frame policy; default CSP also denies frame ancestors. | `DENY` |
| `OTLP_TRACING_ENABLED` | Explicit tracing switch. True uses the local endpoint unless overridden; false wins over endpoint/header settings. | unset |
| `OTLP_TRACING_ENDPOINT` | OTLP HTTP trace endpoint. | unset |
| `OTLP_TRACING_HEADERS` | Comma-delimited trace headers in `name=value` form. | unset |
| `OTLP_METRICS_ENDPOINT` | OTLP HTTP metrics endpoint. | unset |
| `OTLP_METRICS_HEADERS` | Comma-delimited metric headers in `name=value` form. | unset |
| `SETUP_AUTOSTART_DATABASE` | Allow setup tooling to plan local PostgreSQL startup if unreachable. | `true` |
| `SETUP_AUTOSTART_JAEGER` | Allow setup tooling to plan local Jaeger startup if configured but unreachable. | `false` |

The `SETUP_AUTOSTART_*` values and `WEB_API_MIGRATION_DATABASE_*` credentials belong to setup and
migration planning, not the runtime request path read by `cabal run exe:haskell-web-api`. Setup hooks
and verification paths read them from the same four layers. Migration overrides are all-or-nothing:
setting any one `WEB_API_MIGRATION_DATABASE_*` value requires all five. Runtime and migration PostgreSQL
identities are deliberately separate; see [SETUP.md](../SETUP.md) for database creation, migration, and
test prerequisites. The supported PostgreSQL major version is currently 17.

The compiled SMTP credentials and `TOTP_ENCRYPTION_KEY` are localhost development fixtures, never
production secrets. `APP_MODE=production` rejects the committed TOTP key at startup; set a fresh,
independent 32-byte key in `.env.local` or the process environment. For deployment, prefer process
environment injection or a secret manager over a committed file.

## Request resource limits

The request-head settings are deliberately opt-in: no framework-wide production default is imposed
by this release. Select a budget from the whole deployment path rather than copying an arbitrary
number: reverse proxy, load balancer, Warp listener, application middleware, endpoint codec, and
container must agree on which layer rejects first. Set the proxy boundary no larger than the
application boundary where possible; a proxy is defense in depth, not a replacement for the
application limit.

Warp rejects an over-limit header block before it builds a WAI request (its parser response is `400`).
Once WAI has constructed a request, Harch rejects an over-limit target with `414` and count or
individual header-value limits with `431`, without reflecting request data. Warp 3.4.12 (the pinned
version) has exactly one header-related transport setting, `setMaxTotalHeaderLength` (wired here as
`requestHeaderByteLimit`, always at least Warp's own 50 KiB default even when unconfigured): it caps
cumulative header bytes before allocation, but Warp has no setting that bounds header *count* or an
individual header's byte size on its own — those can only be rejected after WAI has already built the
request, by the `431` gate above. This is a real, tested residual exposure rather than an oversight:
many small headers that stay under the total-byte cap reach the WAI layer intact, where the
count-aware gate is what rejects them (see the real-socket test proving this ordering in
`HarchWebSpec.hs`). Endpoint body budgets are separate because JSON, form, streaming, and multipart
endpoints have different valid sizes. The framework's client-action reader remains capped at 64 KiB,
and the native subscription example uses an explicit 8 KiB incremental body reader; multipart body,
field, and file bounds — including independent field-count and file-count limits — are enforced by
`HarchWeb.Api.Multipart`'s `MultipartLimits`.

Cookie budgets are an application-wide request-head policy, independent from the generic header
budgets. `REQUEST_COOKIE_MAX_COUNT` counts only syntactically valid `name=value` pairs over every
case-insensitive `Cookie` header field; repeated names and empty values remain valid, while malformed
fragments remain ignored as they are by typed API decoding. Choose the count and byte limits from the
largest legitimate session, preferences, and proxy-added cookies, then configure the reverse proxy's
total-header allowance no higher than the application can safely accept. Keep the generic total and
per-header-value budgets too: they run first and bound malformed cookie material, forwarded headers,
and trace context that the cookie-specific policy intentionally does not reinterpret. A cookie
rejection is a non-reflective `431` before routing, middleware, request observability, or cookie
decoding.

`REQUEST_NETWORK_TIMEOUT_SECONDS` and `REQUEST_SLOWLORIS_MAX_BYTES` are listener-level network
controls, not application body budgets. They apply consistently to HTTP, manual TLS, and
ACME-managed TLS listeners. Leaving either unset deliberately retains the installed Warp default;
set both only after measuring legitimate slow clients and proxy behavior. A timeout closes the
connection rather than producing an application response.

`REQUEST_MAX_CONCURRENT` bounds concurrent in-flight requests, not connections: it is a WAI-level
admission gate shared across every listener, acquired before route parsing, middleware,
observability, or body reads and released only once the response finishes, so a slow handler or a
large streamed response correctly keeps its slot occupied for its whole duration. Warp 3.4.12 (the
pinned version) has no built-in concurrent-request or connection-count setting of its own — this is
a real, documented limitation, not an oversight — so leaving `REQUEST_MAX_CONCURRENT` unset means the
runtime forks an unbounded worker per accepted connection, relying entirely on Warp's and the
listening socket's own accept-queue behavior and the OS/container's process and memory limits.
Choose a value from real load testing against the deployment's actual per-request memory and CPU
cost; an admission-gate rejection is a `503`, cheap for the server to produce, in contrast to the
uncontained memory growth an unbounded worker count risks under sustained load.

Response bodies are buffered in memory by this framework's design: `ResponseBody`'s `responseBody`
is an ordinary `Text` value, and every non-streaming response path (`ResponseRendering.hs`) renders
it through `Wai.responseLBS` from a fully-realized value before any bytes are sent — only the SSE and
explicit `ProtocolResponseStream` paths stream incrementally. There is no framework-level response
size cap, and none of the framework's own response construction paths build an in-memory value whose
size scales with an attacker-controlled *count* (as opposed to ordinary handler-supplied content).
The framework does not amplify a bounded request into an unbounded response on its own; an endpoint
handler that builds a response proportional to request-derived data (a page size, a repeat count, a
list length pulled from a query parameter) owns bounding that construction itself, the same way it
owns every other domain-specific validation.

Container memory limits are last-resort containment only: a cgroup OOM kills every in-flight request
in that container. They should be observed in deployment drills, but are not a substitute for request
limits and are intentionally not run by the test suite.

### Manual rootless Podman containment drill

On a cgroup-v2, rootless Podman host, perform this only as a manual deployment drill. The August 2026
verification used Podman 5.8.4 with the systemd cgroup manager and `crun`. It starts a named
disposable container with no network, a read-only root filesystem, no swap beyond the 32 MiB memory
ceiling, and a small process ceiling. The Python allocation deliberately grows beyond the cgroup
budget; it is not an application request test.

```sh
podman run --name harch-web-oom-probe --network none --read-only \
  --memory 32m --memory-swap 32m --pids-limit 32 \
  docker.io/library/python:3.12-alpine \
  python -c 'payload = bytearray(128 * 1024 * 1024); print(len(payload))'
podman inspect harch-web-oom-probe --format '{{.State.Status}} exit={{.State.ExitCode}} oom={{.State.OOMKilled}}'
podman rm harch-web-oom-probe
```

The verification returned `exited exit=137 oom=true`. The host's root `memory.events` was readable,
but rootless Podman with systemd reported a namespace-relative container cgroup path, so its
per-container `memory.events` was not directly available from the host shell after exit. Where your
Podman/cgroup setup maps that path into the host hierarchy, inspect `memory.events` before and after
the allocation as an additional signal. Always remove the named probe, including after a failed
command. An OOM kill is observable emergency containment that abandons all work in that container,
not graceful per-request admission control.

## Minimal local overrides

```dotenv
APP_MODE=development
DATABASE_HOST=127.0.0.1
DATABASE_PORT=5432
DATABASE_NAME=web_api_dev
DATABASE_USER=web_api_runtime
DATABASE_PASSWORD=web_api
APP_TITLE_PREFIX=web-api-dev
LISTENER_0_HOST=127.0.0.1
LISTENER_0_PORT=5001
LISTENER_0_SCHEME=http
```

Run the full application with:

```sh
cabal run exe:haskell-web-api
```

## Listener and certificate examples

Manual HTTPS uses an explicit certificate/key pair:

```dotenv
LISTENER_0_HOST=0.0.0.0
LISTENER_0_PORT=5443
LISTENER_0_SCHEME=https
LISTENER_0_TLS_SOURCE=manual
LISTENER_0_TLS_CERTIFICATE_FILE=/etc/web-api/tls/fullchain.pem
LISTENER_0_TLS_PRIVATE_KEY_FILE=/etc/web-api/tls/privkey.pem
```

For ACME, an HTTP listener publishes certificate files while an HTTPS listener waits for and reloads
them:

```dotenv
LISTENER_0_HOST=0.0.0.0
LISTENER_0_PORT=80
LISTENER_0_SCHEME=http
LISTENER_0_ACME_CONTACT_EMAILS=ops@example.com
LISTENER_0_ACME_DOMAINS=example.com,www.example.com
LISTENER_0_ACME_CERTIFICATE_DIRECTORY=/etc/web-api/acme/example.com

LISTENER_1_HOST=0.0.0.0
LISTENER_1_PORT=443
LISTENER_1_SCHEME=https
LISTENER_1_TLS_SOURCE=shared-wait
LISTENER_1_TLS_CERTIFICATE_DIRECTORY=/etc/web-api/acme/example.com
LISTENER_1_TLS_SHARED_WAIT_SECONDS=120
```

Omitting `LISTENER_<n>_ACME_DIRECTORY_URL` uses the production Let's Encrypt directory. Set it
explicitly for staging or another ACME service. The runtime image contains certbot; ordinary HTTP-only
source builds do not require it.

When `REDIRECT_HTTP_TO_HTTPS` is unset:

- HTTP-only listener sets do not redirect.
- A mixed HTTP/HTTPS set redirects to its unique HTTPS port.
- Multiple distinct HTTPS ports still enable redirects, but omit a port so the target uses 443.
- `/.well-known/acme-challenge/*` remains on HTTP for `http-01` validation.
- An explicit `false` disables the derived redirect.

See the focused [certificate and HTTPS guides](../examples/README.md#implemented-guides) for manual,
shared, ACME, HSTS, redirect, and local mkcert workflows.

## Assets, proxy policy, and observability

```dotenv
STATIC_ASSET_ROOT_0_URL_PREFIX=/assets
STATIC_ASSET_ROOT_0_DIRECTORY=public
STATIC_CACHE_CONTROL_SECONDS=3600

REDIRECT_HTTP_TO_HTTPS=true
HSTS_MAX_AGE_SECONDS=31536000
HSTS_INCLUDE_SUBDOMAINS=true
HSTS_PRELOAD=true

OTLP_TRACING_ENABLED=true
OTLP_TRACING_HEADERS=authorization=Bearer demo-token,x-service-name=web-api
OTLP_METRICS_ENDPOINT=http://127.0.0.1:4318/v1/metrics
OTLP_METRICS_HEADERS=authorization=Bearer demo-token,x-service-name=web-api
```

Static requests are limited to configured roots and content-type extensions. Hidden path segments such
as `.env` and `.well-known` are rejected, and extensionless files require an explicit empty-extension
entry. Full SSR documents receive a fresh CSP nonce for the inline capture kernel; deferred modules
remain external. CORS is same-origin unless exact origins are configured.

OTLP request span names use stable route values. Unmatched requests group under `not-found`, while the
concrete URL remains on `url.path`. The custom exporter also covers redirects, assets, CORS preflights,
ACME challenges, connection-level TLS failures, and certificate lifecycle events that sit outside
ordinary page handling.

Scenario-oriented `.env.local` templates live in `examples/runtime-config/`, including local HTTP,
OTLP, manual TLS, shared certificates, ACME, and reverse-proxy/TLS-offload configurations. For example:

```sh
cp examples/runtime-config/manual-tls.env ./.env.local
```
