# testing

**Status:** Implemented guide

Show what app-specific tests should look like on top of the existing repo testing stack.

Current repo alignment:

- the repo already has unit, integration, and browser E2E suites,
- `spec-preprocessor` already demonstrates a repo-local boilerplate-reduction pattern,
- coverage generation is already part of the standard workflow.

Suggested snippet:

- [test/Unit/AppSpec.hs.md](test/Unit/AppSpec.hs.md)

## Grounded repo flow

The current repo testing shape is:

```bash
cabal test all
```

For focused app work, the current example app should normally be exercised in slices:

```bash
cabal test web-api --test-show-details=direct --test-options="--match Unit"
cabal test web-api --test-show-details=direct --test-options="--match Integration"
cabal test web-api --test-show-details=direct --test-options="--match E2E"
```

The full coverage helper still relies on the owner-level migration environment variables before it
runs:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner

./generate-code-coverage.sh
```

Use this example to explain:

1. Unit tests should cover pure route/page/rendering behavior first.
2. Integration tests should hit the real executable/runtime boundary.
3. Browser E2E tests stay Haskell-authored while using the current Node-backed browser runner.
4. Coverage generation intentionally runs package-by-package so each package gets a real report.

Recommended walkthrough topics:

1. one focused unit spec for pure route/page behavior,
2. one integration spec hitting the real executable boundary,
3. one browser spec for user-visible behavior,
4. how to run the coverage script and open the report.
