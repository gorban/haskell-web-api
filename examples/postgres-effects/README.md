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

What this example should emphasize:

1. the runtime database user can stay different from the migration owner user,
2. seed or migrate once before starting the app,
3. the simplest example should still keep page rendering and effect seams easy to follow.
