# ADR-AY: Require authenticated PostgreSQL transport

- Status: **Implemented and verified**
- Task: [AY — PostgreSQL connection lifecycle and transport](../../TASKS/pr-3-correctness-and-security-defects.md)
- Date: 2026-08-26

## Executive problem statement

Runtime and migration connections now have timeouts, transactional migration ownership, and a
bounded runtime pool, but libpq still inherits `sslmode=prefer`. If a server does not offer TLS,
libpq silently falls back to plaintext and can expose credentials and data. Simply defaulting to
`sslmode=require` would make every repository-owned database unreachable because local startup and
CI use a stock PostgreSQL 17 container with TLS disabled. The application cannot enforce secure
transport until deployment provisions server certificates and client trust configuration.

Decision made: expose a closed configuration representation of libpq's supported `sslmode` values.
When no mode is configured, omit the TLS parameters and preserve libpq's own default. A deployment
that needs authenticated TLS selects `verify-full` and supplies a root certificate either explicitly
or through libpq's default location; a deployment that selects `require` makes the weaker,
encryption-only choice explicit.

## Design guidance that constrains the decision

- **Extend the existing adapter/config boundary.** `DatabaseConfig`, the pool, and the migration
  interpreter already own libpq connection parameters. Do not add a second database client path.
- **Application and deployment ownership stay distinct.** The application can require and validate
  a transport policy; deployment owns server keys, certificates, CA distribution, DNS/SAN names,
  rotation, and `pg_hba.conf`.
- **Do not silently substitute properties.** `require` encrypts but does not normally authenticate
  the server; `verify-full` also validates the CA chain and requested host. The chosen guarantee
  must be named accurately.
- **Name the selected guarantee accurately.** A closed mode ADT prevents misspelling or silently
  inventing libpq semantics, but lets the deployment select the documented guarantee it owns.
- **Name partial delivery honestly.** Pooling, timeouts, and the real TLS proof are complete; the
  local CI-equivalent gate sequence has passed. GitHub Actions also passed for the implementation
  commit (`118984c`, [run 33126747936](https://github.com/gorban/haskell-web-api/actions/runs/33126747936)).

## Current evidence

- [`DatabaseConfig`](../../packages/web-api/src/WebApi/Config/Internal.hs) now carries a closed
  `DatabaseTransportSecurity` value parsed from runtime or migration settings.
- [`runtimeConnectionString`](../../packages/web-api/src/WebApi/Postgres/Pool.hs) emits exact
  `sslmode`/`sslrootcert` parameters only when configured; otherwise it leaves libpq's normal
  environment/default resolution intact. The `psql` path receives the equivalent libpq variables.
- CI, automatic local startup, and the runnable PostgreSQL example use an unmodified
  `postgres:17` image. Direct inspection recorded `SHOW ssl = off`; no production PostgreSQL
  deployment or certificate lifecycle exists in this repository.
- PostgreSQL documents that `prefer` can fall back to plaintext, `require` protects only against
  eavesdropping, and `verify-full` additionally verifies CA trust and host identity.
  [PostgreSQL 17 SSL support](https://www.postgresql.org/docs/17/libpq-ssl.html) recommends
  `verify-full` for most security-sensitive environments.
- PostgreSQL also requires both server and client configuration before a connection is known to be
  secure; unsupported strict modes fail rather than fall back.
- `withPostgresTlsFixtures` starts isolated PostgreSQL 17 TLS and non-TLS containers. Its real
  libpq regression proves `verify-full` success (including `pg_stat_ssl.ssl = true`), and fails
  closed for an untrusted but syntactically valid CA, a hostname/SAN mismatch, and a TLS-disabled
  server.

## Options and consequences

### Option A — Require `verify-full` for every network connection

Provision TLS in CI/local fixtures and production. Add CA/host configuration to the existing
database config and emit `sslmode=verify-full` plus the selected `sslrootcert`; require certificate
SANs that match the configured hostname or IP.

Consequences:

- Protects credentials/data from eavesdropping and authenticates the intended server.
- Gives tests the same strict transport property production claims.
- Requires certificate issuance, CA distribution, SAN naming, secure server-key ownership,
  rotation, and PostgreSQL server configuration in every environment.
- Makes a current stock-container or incorrectly rotated deployment fail closed.

### Option B — Require encryption with `sslmode=require`

Provision server TLS but do not require explicit CA/hostname verification.

Consequences:

- Meets AY's literal `sslmode=require` wording and prevents plaintext fallback.
- Does not normally prevent a man-in-the-middle server; PostgreSQL documents `require` as encryption
  without server identity unless compatibility CA behavior happens to apply.
- Creates a weaker contract that may later require another breaking configuration migration.

### Option C — Typed strict production policy plus explicit local plaintext

Model transport as a closed policy in `DatabaseConfig`: verified TLS for network production, and a
conspicuously named plaintext mode accepted only in Development/Test when the host is loopback (or a
Unix-domain socket, where `sslmode` is ignored). Exercise verified TLS in CI even though ordinary
interactive local startup may retain the explicit escape hatch.

Consequences:

- Makes insecure production configuration unrepresentable while preserving low-friction local
  development.
- Keeps the exception visible in types/config parsing instead of relying on libpq fallback.
- Adds mode-aware validation and tests; CI must manage test-only certificates and a matching host.
- A non-loopback development database must provision TLS rather than silently downgrade.

### Option D — Add a configurable `DATABASE_SSL_MODE` while preserving libpq defaults

Consequences:

- Is backward compatible and lets prepared deployments select `verify-full`, `require`, or another
  documented libpq mode without application code changes.
- An omitted value emits no TLS conninfo parameter, preserving libpq's own default rather than
  copying it into the application configuration.
- Makes the weaker `prefer`, `allow`, and `require` choices explicit whenever they are selected,
  but leaves deployment responsible for choosing a secure policy.

### Option E — Terminate TLS in a sidecar or database proxy

Consequences:

- Can centralize certificates and pooling for some deployments.
- Leaves the application-to-proxy hop outside the libpq guarantee and makes correctness depend on
  topology this repository does not own.
- Useful deployment option only when the local hop is independently trusted and documented; not a
  universal application default.

## Recommendation

Adopt **Option D**. `DatabaseTransportSecurity` is a closed ADT containing either an omitted libpq
policy or one exact libpq mode, with an optional root-certificate path only for an explicit mode.
The shared conninfo encoder applies it to runtime and migration libpq connections; `psql` receives
the corresponding documented `PGSSLMODE`/`PGSSLROOTCERT` environment variables. PostgreSQL's
`verify-full` remains the documented recommendation for security-sensitive deployments, while
`require` is intentionally labelled encryption-only rather than verified transport.

## Approved implementation plan

1. Define the closed `DatabaseTransportSecurity`/`DatabaseSslMode` values in `DatabaseConfig`.
2. Parse the runtime and migration mode/root-certificate environment values, rejecting unknown,
   empty, and root-certificate-without-mode combinations.
3. Apply the same policy through runtime/migration conninfo and `psql` environment values without
   logging passwords or certificate contents.
4. Document the exact modes and their interoperability/security consequences.
5. Add a real PostgreSQL TLS fixture for `verify-full` success, untrusted CA, hostname mismatch,
   and TLS-disabled failure. It verifies deployment provisioning, not configuration parsing.
6. Run the complete CI-equivalent and module-health gates before committing and pushing.

The configuration and real transport proof are complete. AY closes after the normal repository
gates, commit, push, and green CI evidence.
