# 14-reverse-proxy-awareness

**Status:** Current

Show how to run behind nginx while preventing clients from spoofing forwarding headers directly.

Current repo alignment:

- the repo already includes nginx examples,
- forwarded-header trust is intentionally default-off,
- this example should explain both the happy path and the security reason for the configuration.

Suggested snippet:

- [nginx/default.conf.md](nginx/default.conf.md)

## Grounded repo flow

The canonical runtime files for this example already exist under `examples/reverse-proxy/`.

Generate the local development certificate chain first:

```bash
./examples/reverse-proxy/generate-local-tls.sh
```

Then bring up PostgreSQL and Jaeger before the app/proxy pair:

```bash
podman compose -f examples/reverse-proxy/podman-compose.yml up -d postgres jaeger
# or: docker compose -f examples/reverse-proxy/docker-compose.yml up -d postgres jaeger
```

Seed the database with owner-level credentials:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner

cabal run exe:haskell-web-api-db -- migrate-and-seed
```

Then start the app and nginx:

```bash
podman compose -f examples/reverse-proxy/podman-compose.yml up -d web-api nginx
# or: docker compose -f examples/reverse-proxy/docker-compose.yml up -d web-api nginx
```

Verify the proxy behavior:

```bash
curl -I http://127.0.0.1/
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/api/status
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/second
xdg-open http://127.0.0.1:16686
```

For the `/app/*` subpath variant, switch from `nginx/default.conf` to `nginx/prefixed.conf` in the
compose file and re-run the `web-api` / `nginx` services.

What to explain:

1. the proxy, not the browser, must set `X-Forwarded-*`,
2. the proxy should overwrite `X-Forwarded-For` with its trusted view of the client chain,
3. subpath mounting uses `X-Forwarded-Prefix`,
4. `REDIRECT_HTTP_TO_HTTPS` should follow the effective forwarded scheme when the proxy terminates TLS.
